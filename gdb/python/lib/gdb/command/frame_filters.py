# Frame-filter commands.
# Copyright (C) 2012 Free Software Foundation, Inc.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

"""GDB commands for working with frame-filters."""

import gdb
import copy
import gdb.FrameFilter
from gdb.FrameIterator import FrameIterator
from gdb.BaseFrameWrapper import BaseFrameWrapper
import itertools

def _parse_arg (cmd_name, arg):
    """ Internal Worker function to take an argument and return a
    tuple of arguments.

    Arguments:
        cmd_name: Name of the command invoking this function.
        args: The argument as a string
    """

    argv = gdb.string_to_argv(arg);
    argc = len(argv)
    if argc != 2:
        raise gdb.GdbError(cmd_name + " takes exactly " + \
                          "two arguments.")

    object_list = argv[0]
    argument = argv[1]

    return (object_list, argument)

def _get_sort_priority(filter_item):
    """ Internal Worker function to return the frame-filter's priority
    for a List's sort function

    Arguments:
        filter_item: A tuple, with the first element being the name,
        and the second being the frame-filter object.
    """
    # Do not fail here, as the sort will fail.  If a filter has not
    # (incorrectly) set a priority, set it to zero.
    if hasattr(filter_item[1], "priority"):
        return filter_item[1].priority
    else:
        return 0

def _get_priority(filter_item):
    """ Internal Worker function to return the frame-filter's priority.

    Arguments:
        filter_item: An object conforming to the frame filter
        interface.
    """
    if hasattr(filter_item, "priority"):
        return filter_item.priority
    else:
        raise gdb.GdbError("Cannot find class attribute 'priority'")

def _set_priority(filter_item, priority):
    """ Internal Worker function to set the frame-filter's priority.

    Arguments:
        filter_item: An object conforming to the frame filter
        interface.
        priority: The priority to assign as an integer.
    """

    if hasattr(filter_item, "priority"):
        filter_item.priority = priority
    else:
        raise gdb.GdbError("Cannot find class attribute 'priority'")

def _get_enabled(filter_item):
    """ Internal Worker function to return the frame-filter's enabled
    state.

    Arguments:
        filter_item: An object conforming to the frame filter
        interface.
    """

    if hasattr(filter_item, "enabled"):
        return filter_item.enabled
    else:
        raise gdb.GdbError("Cannot find class attribute 'enabled'")

def _get_filter_enabled (filter_item):
    """ Internal Worker function to return the frame-filter's enabled
    state for filter operations.

    Arguments:
        filter_item: A tuple, with the first element being the name,
        and the second being the frame-filter object.
    """
    # If the filter class is badly implemented, do not cease filter
    # operations, just set enabled to False.
    try:
        enabled = _get_enabled (filter_item[1])
    except gdbError as e:
        enabled = False

    return enabled

def _set_enabled(filter_item, state):
    """ Internal Worker function to set the frame-filter's enabled
    state.

    Arguments:
        filter_item: An object conforming to the frame filter
        interface.
        state: True or False, depending on desired state.
    """
    if hasattr(filter_item, "enabled"):
        filter_item.enabled = state
    else:
        raise gdb.GdbError("Cannot find class attribute 'enabled'")

def _get_name (frame_filter):
    """ Internal Worker function to return the name of the
    frame-filter.

    Arguments:
        filter_item: An object conforming to the frame filter
        interface.
    """
    if hasattr(frame_filter, "name"):
        return frame_filter.name
    raise gdb.GdbError("Cannot find class attribute 'name'")

def _return_list (name):
    """ Internal Worker function to return the frame-filter name,
    depending on the name supplied as an argument.  If the name is not
    "global" or "progspace", it is assumed to name an object-file.

    Arguments:
        name: The name of the list, as specified by GDB user commands.
    """

    if name  == "global":
        return gdb.frame_filters
    else:
        if name == "progspace":
            cp = gdb.current_progspace()
            return cp.frame_filters
        else:
            for objfile in gdb.objfiles():
                if name == objfile.filename:
                    return objfile.frame_filters

    msg = "Cannot find frame-filter dictionary for '" + name + "'"
    raise gdb.GdbError(msg)

def _sort_list ():
    """ Internal Worker function to merge all known frame-filter
    lists, prune any filters with the state set to "disabled", and
    sort the list on the frame-filter's "priority" attribute.
    """

    all_filters = []
    for objfile in gdb.objfiles():
        all_filters = all_filters + objfile.frame_filters.items()
    cp = gdb.current_progspace()

    all_filters = all_filters + cp.frame_filters.items()
    all_filters = all_filters + gdb.frame_filters.items()

    sorted_frame_filters = copy.copy(all_filters)
    sorted_frame_filters.sort(key = _get_sort_priority,
                                   reverse = True)
    sorted_frame_filters = filter (_get_filter_enabled, sorted_frame_filters)
    return sorted_frame_filters

def invoke (frame):
    """ Public internal function that will execute the chain of frame
    filters.  Each filter is executed in priority order.

    Arguments:
        frame_iterator:  An iterator of frames.
    """

    sorted_list = _sort_list()

    # Check to see if there are any frame-filters.  If not, just
    # return None and let default backtrace printing occur.
    if len(sorted_list) == 0:
        return None

    frame_iterator = FrameIterator (frame)

    # Apply base filter to all gdb.Frames.  This unifies the in
    # interface
    frame_iterator = itertools.imap (BaseFrameWrapper, frame_iterator)

    for ff in sorted_list:
        frame_iterator = ff[1].filter (frame_iterator)

    return frame_iterator

class InfoFrameFilter(gdb.Command):
    """GDB command to list all registered frame-filters.

    Usage: info frame-filters
    """
    @staticmethod
    def enabled_string(state):
        """Return "Yes" if PRINTER is enabled, otherwise "No"."""
        if state:
            return "Yes"
        else:
            return "No"

    def __init__ (self):
        super(InfoFrameFilter, self).__init__("info frame-filter",
                                              gdb.COMMAND_DATA)

    def list_frame_filters(self, frame_filters):
        """Print a list of frame_filters."""

        sorted_frame_filters = frame_filters.items()
        sorted_frame_filters.sort(key = _get_sort_priority, reverse = True)

        print "  Priority  Enabled  Name"
        print "  ========  =======  ===="
        if len(sorted_frame_filters) == 0:
            print "  No frame filters registered."
        else:
            for frame_filter in sorted_frame_filters:
                name = frame_filter[0]
                try:
                    priority = '{:<8}'.format(
                        str(_get_priority (frame_filter[1])))
                    enabled = '{:<7}'.format(
                        self.enabled_string(_get_enabled(frame_filter[1])))
                except Exception as e:
                    print "  Error printing filter '"+name+"': ",e
                else:
                    print "  %s  %s  %s" % (priority, enabled, name)

    def print_list(self, title, filter_list):
        """Print a list."""
        if filter_list:
            print title
            self.list_frame_filters(filter_list)

    def invoke(self, arg, from_tty):
        """GDB calls this to perform the command."""
        self.print_list("global frame-filters:", gdb.frame_filters)

        cp = gdb.current_progspace()
        self.print_list("progspace %s frame-filters:" % cp.filename,
                        cp.frame_filters)

        for objfile in gdb.objfiles():
            self.print_list("objfile %s frame-filters:" % objfile.filename,
                            objfile.frame_filters)

def do_enable_frame_filter_1 (frame_filters, name, flag):
    """Worker for enabling/disabling frame_filters.

    Arguments:
        frame_filters: list of frame_filters
        name: object to select printers
        flag: True for Enable, False for Disable
    """
    try:
        ff = frame_filters[name]
    except KeyError:
        msg = "frame-filter '" + str(name) + "' not found."
        raise gdb.GdbError(msg)

    _set_enabled(ff, flag)

def do_enable_frame_filter (command_tuple, flag):
    """Internal worker for enabling/disabling frame-filters."""

    list_op = command_tuple[0]
    frame_filter = command_tuple[1]

    op_list = _return_list(list_op)
    do_enable_frame_filter_1(op_list, frame_filter, flag)

class SetFilterPrefixCmd (gdb.Command):
    def __init__ (self):
        super (SetFilterPrefixCmd, self).__init__ ("set python frame-filter",
                                                   gdb.COMMAND_DATA,
                                                   gdb.COMPLETE_COMMAND, True)
class ShowFilterPrefixCmd (gdb.Command):
    def __init__ (self):
        super (ShowFilterPrefixCmd, self).__init__ ("show python frame-filter",
                                                    gdb.COMMAND_DATA,
                                                    gdb.COMPLETE_COMMAND, True)

class EnableFrameFilter (gdb.Command):
    """GDB command to disable the specified frame-filter.

    Usage: enable frame-filter enable [list] [name]

    LIST is the name of the frame-filter list to operate.  Named lists
    are: "global" for the global frame-filter list, "progspace" for
    the program space's file frame-filter list.  If either of these
    two are not specified, the list name is assumed to be the name of
    the object-file name.

    NAME matches the name of the frame-filter to operate on.
    """
    def __init__(self):
        super(EnableFrameFilter, self).__init__("enable frame-filter",
                                                 gdb.COMMAND_DATA)

    def invoke(self, arg, from_tty):
        """GDB calls this to perform the command."""
        command_tuple = _parse_arg("enable frame-filter", arg)
        do_enable_frame_filter(command_tuple, True)


class DisableFrameFilter (gdb.Command):
    """GDB command to disable the specified frame-filter.

    Usage: disable frame-filter disable [list] [name]

    LIST is the name of the frame-filter list to operate.  Named lists
    are: "global" for the global frame-filter list, "progspace" for
    the program space's file frame-filter list.  If either of these
    two are not specified, the list name is assumed to be the name of
    the object-file name.

    NAME matches the name of the frame-filter to operate on.
    """
    def __init__(self):
        super(DisableFrameFilter, self).__init__("disable frame-filter",
                                                  gdb.COMMAND_DATA)

    def invoke(self, arg, from_tty):
        """GDB calls this to perform the command."""
        command_tuple = _parse_arg("disable frame-filter", arg)
        do_enable_frame_filter(command_tuple, False)

class SetFrameFilterPriority (gdb.Command):
    """GDB command to set the priority of the specified frame-filter.

    Usage: set python frame-filter priority list name priority

    LIST is the name of the frame-filter list to operate.  Named lists
    are: "global" for the global frame-filter list, "progspace" for
    the program space's file frame-filter list.  If either of these
    two are not specified, the list name is assumed to be the name of
    the object-file name.

    NAME matches the name of the frame-filter to operate on.

    PRIORITY is the new priority to set the frame-filter.
    """

    def __init__(self):
        super(SetFrameFilterPriority, self).__init__("set python " \
                                                     "frame-filter priority",
                                                     gdb.COMMAND_DATA,
                                                     gdb.COMPLETE_COMMAND)
    def _parse_pri_arg (self, arg):
        argv = gdb.string_to_argv(arg);
        argc = len(argv)
        if argc != 3:
            raise gdb.GdbError("set python frame-filter priority " \
                              "takes exactly three arguments.")

        object_list = argv[0]
        name = argv[1]
        priority = argv[2]
        return (object_list, name, priority)

    def _set_filter_priority_1 (self, frame_filters, name, priority):
        """Worker for setting priority of frame_filters.

        Arguments:

        frame_filters: list of frame_filters.
        name: object to select printers.
        priority: Priority of filter.
        """
        try:
            ff = frame_filters[name]
        except KeyError:
            msg = "frame-filter '" + str(name) + "' not found."
            raise gdb.GdbError(msg)

        _set_priority(ff, priority)

    def _set_filter_priority (self,command_tuple):
        """Internal worker for setting priority of frame-filters."""

        list_op = command_tuple[0]
        frame_filter = command_tuple[1]
        priority = command_tuple [2]

        op_list = _return_list (list_op)

        self._set_filter_priority_1(op_list, frame_filter, priority)

    def invoke(self, arg, from_tty):
        """GDB calls this to perform the command."""
        command_tuple = self._parse_pri_arg(arg)
        try:
            self._set_filter_priority (command_tuple)
        except e:
            # Print the error, instead of raising it.
            gdb.write(e.message+"\n")

class ShowFrameFilterPriority (gdb.Command):
    """GDB command to show the priority of the specified frame-filter.

    Usage: show python frame-filter priority list name

    LIST is the name of the frame-filter list to operate.  Named lists
    are: "global" for the global frame-filter list, "progspace" for
    the program space's file frame-filter list.  If either of these
    two are not specified, the list name is assumed to be the name of
    the object-file name.

    NAME matches the name of the frame-filter to operate on.
    """

    def __init__(self):
        super(ShowFrameFilterPriority, self).__init__("show python " \
                                                      "frame-filter priority",
                                                      gdb.COMMAND_DATA,
                                                      gdb.COMPLETE_COMMAND)
    def _parse_pri_arg (self, arg):
        argv = gdb.string_to_argv(arg);
        argc = len(argv)
        if argc != 2:
            raise gdb.GdbError("show python frame-filter priority " \
                              "takes exactly two arguments.")

        object_list = argv[0]
        name = argv[1]
        return (object_list, name)

    def get_filter_priority (self, frame_filters, name):
        """Worker for retrieving the priority of frame_filters.

        Arguments:

        frame_filters: name of frame filter list.
        name: object to select printers.
        """
        op_list = _return_list (frame_filters)

        try:
            ff = op_list[name]
        except KeyError:
            msg = "frame-filter '" + str(name) + "' not found."
            raise gdb.GdbError(msg)

        return _get_priority(ff)

    def invoke(self, arg, from_tty):
        """GDB calls this to perform the command."""
        try:
            command_tuple = self._parse_pri_arg(arg)
        except gdb.GdbError as e:
            # Print the error instead of raising it.
            gdb.write(e.message+"\n")
            return
        filter_name = command_tuple[1]
        list_name = command_tuple[0]
        try:
            priority = self.get_filter_priority (list_name, filter_name);
        except Exception as e:
            print "Error printing filter priority for '"+name+"':",e
        else:
            print "Priority of filter '" + filter_name + "' in list '" \
                + list_name + "' is: " + str(priority)

def register_frame_filter_commands():
    """Call from a top level script to install the frame-filter commands."""
    InfoFrameFilter()
    SetFilterPrefixCmd()
    ShowFilterPrefixCmd()
    EnableFrameFilter()
    DisableFrameFilter()
    SetFrameFilterPriority ()
    ShowFrameFilterPriority ()

register_frame_filter_commands()
