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

def _parse_arg(cmd_name, arg):
    """ Internal worker function to take an argument and return a
    tuple of arguments.

    Arguments:
        cmd_name: Name of the command invoking this function.
        args: The argument as a string.

    Returns:
        A tuple containing the dictionary, and the argument.
    """

    argv = gdb.string_to_argv(arg);
    argc = len(argv)
    if argc != 2:
        raise gdb.GdbError(cmd_name + " takes exactly two arguments.")

    object_list = argv[0]
    argument = argv[1]

    return(object_list, argument)

def _get_sort_priority(filter_item):
    """ Internal worker function to return the frame-filter's priority
    from a frame filter tuple object.

    Arguments:
        filter_item: A tuple, with the first element being the name,
                     and the second being the frame-filter object.

    Returns:
        The priority of the frame filter from the "priority"
        attribute.
    """
    # Do not fail here, as the sort will fail.  If a filter has not
    # (incorrectly) set a priority, set it to zero.
    if hasattr(filter_item[1], "priority"):
        return filter_item[1].priority
    else:
        return 0

def _get_priority(filter_item):
    """ Internal worker function to return the frame-filter's priority.

    Arguments:
        filter_item: An object conforming to the frame filter
        interface.

    Returns:
        The priority of the frame filter from the "priority"
        attribute.

    Raises:
        gdb.GdbError: When the priority attribute has not been
                      implemented.
    """
    if hasattr(filter_item, "priority"):
        return filter_item.priority
    else:
        raise gdb.GdbError("Cannot find class attribute 'priority'")

def _set_priority(filter_item, priority):
    """ Internal worker function to set the frame-filter's priority.

    Arguments:
        filter_item: An object conforming to the frame filter
                     interface.
        priority: The priority to assign as an integer.

    Raises:
        gdb.GdbError: When the priority attribute has not been
                      implemented.
    """

    if hasattr(filter_item, "priority"):
        filter_item.priority = priority
    else:
        raise gdb.GdbError("Cannot find class attribute 'priority'")

def _get_enabled(filter_item):
    """ Internal worker function to return the frame-filter's enabled
    state.

    Arguments:
        filter_item: An object conforming to the frame filter
                     interface.

    Returns:
        The enabled state of the frame filter from the "enabled"
        attribute.

    Raises:
        gdb.GdbError: When the enabled attribute has not been
                      implemented.
    """

    if hasattr(filter_item, "enabled"):
        return filter_item.enabled
    else:
        raise gdb.GdbError("Cannot find class attribute 'enabled'")

def _get_filter_enabled(filter_item):
    """ Internal Worker function to return the frame-filter's enabled
    state for filter operations.

    Arguments:
        filter_item: A tuple, with the first element being the name,
                     and the second being the frame-filter object.
    Returns:
        The enabled state of the frame filter from the "enabled"
        attribute.

    """
    # If the filter class is badly implemented, do not cease filter
    # operations, just set enabled to False.
    try:
        enabled = _get_enabled(filter_item[1])
    except gdb.GdbError as e:
        enabled = False

    return enabled

def _set_enabled(filter_item, state):
    """ Internal Worker function to set the frame-filter's enabled
    state.

    Arguments:
        filter_item: An object conforming to the frame filter
                     interface.
        state: True or False, depending on desired state.

    Raises:
        gdb.GdbError: When the enabled attribute has not been
                      implemented.
    """

    if hasattr(filter_item, "enabled"):
        filter_item.enabled = state
    else:
        raise gdb.GdbError("Cannot find class attribute 'enabled'")

def _get_name(frame_filter):
    """ Internal Worker function to return the name of the
    frame-filter.

    Arguments:
        filter_item: An object conforming to the frame filter
                     interface.

    Returns:
        The name of the frame filter from the "name" attribute.

    Raises:
        gdb.GdbError: When the name attribute has not been
                      implemented.
    """
    if hasattr(frame_filter, "name"):
        return frame_filter.name
    raise gdb.GdbError("Cannot find class attribute 'name'")

def _return_list(name):
    """ Internal Worker function to return the frame filter
    dictionary, depending on the name supplied as an argument.  If the
    name is not "global" or "progspace", it is assumed to name an
    object-file.

    Arguments:
        name: The name of the list, as specified by GDB user commands.

    Returns:
        A dictionary object.

    Raises:
        gdb.GdbError:  A dictionary of that name cannot be found.
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

def _sort_list():
    """ Internal Worker function to merge all known frame-filter
    lists, prune any filters with the state set to "disabled", and
    sort the list on the frame-filter's "priority" attribute.

    Returns:
        sorted_list: A sorted, pruned list of frame filters to
                     execute.
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
    sorted_frame_filters = filter(_get_filter_enabled,
                                  sorted_frame_filters)

    return sorted_frame_filters

def invoke(frame):
    """ Public internal function that will execute the chain of frame
    filters.  Each filter is executed in priority order.

    Arguments:
        frame: The initial frame.

    Returns:
        frame_iterator: The iterator after all frame filters have
                        had a change to execute, or None if no frame
                        filters are registered.
    """

    # Get a sorted list of frame filters.
    sorted_list = _sort_list()

    # Check to see if there are any frame-filters.  If not, just
    # return None and let default backtrace printing occur.
    if len(sorted_list) == 0:
        return None

    frame_iterator = FrameIterator(frame)

    # Apply base filter to all gdb.Frames.  This unifies the
    # interface.
    frame_iterator = itertools.imap(BaseFrameWrapper, frame_iterator)

    for ff in sorted_list:
        frame_iterator = ff[1].filter(frame_iterator)

    return frame_iterator

# GDB Commands.
class SetFilterPrefixCmd(gdb.Command):
    def __init__(self):
        super(SetFilterPrefixCmd, self).__init__("set python frame-filter",
                                                 gdb.COMMAND_DATA,
                                                 gdb.COMPLETE_COMMAND, True)
class ShowFilterPrefixCmd(gdb.Command):
    def __init__(self):
        super(ShowFilterPrefixCmd, self).__init__("show python frame-filter",
                                                  gdb.COMMAND_DATA,
                                                  gdb.COMPLETE_COMMAND, True)
class InfoFrameFilter(gdb.Command):
    """GDB command to list all registered frame-filters.

    Usage: info frame-filters
    """
    @staticmethod
    def enabled_string(state):
        """Return "Yes" if filter is enabled, otherwise "No"."""
        if state:
            return "Yes"
        else:
            return "No"

    def __init__(self):
        super(InfoFrameFilter, self).__init__("info frame-filter",
                                              gdb.COMMAND_DATA)

    def list_frame_filters(self, frame_filters):
        """ Internal worker function to list and print frame filters
        in a dictionary.

        Arguments:
            frame_filters: The name of the dictionary, as
                           specified by GDB user commands.
        """

        sorted_frame_filters = frame_filters.items()
        sorted_frame_filters.sort(key = _get_sort_priority,
                                  reverse = True)

        print "  Priority  Enabled  Name"
        print "  ========  =======  ===="
        if len(sorted_frame_filters) == 0:
            print "  No frame filters registered."
        else:
            for frame_filter in sorted_frame_filters:
                name = frame_filter[0]
                try:
                    priority = '{:<8}'.format(
                        str(_get_priority(frame_filter[1])))
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

# Internal enable/disable functions.

def do_enable_frame_filter_1(frame_filters, name, flag):
    """Internal worker for enabling/disabling frame_filters.

    Arguments:
        frame_filters: Dictionary that this frame filter is contained
                       within.
        name: Name of the frame filter.
        flag: True for Enable, False for Disable.

    Raises:
        gdb.GdbError: A frame filter cannot be found.
    """

    try:
        ff = frame_filters[name]
    except KeyError:
        msg = "frame-filter '" + str(name) + "' not found."
        raise gdb.GdbError(msg)

    _set_enabled(ff, flag)

def do_enable_frame_filter(command_tuple, flag):
    """Worker for enabling/disabling frame_filters.

    Arguments:
        command_type: A tuple with the first element being the
                      frame filter dictionary, and the second being
                      the frame filter name.
        flag: True for Enable, False for Disable.
    """

    list_op = command_tuple[0]
    frame_filter = command_tuple[1]

    op_list = _return_list(list_op)
    do_enable_frame_filter_1(op_list, frame_filter, flag)

class EnableFrameFilter(gdb.Command):
    """GDB command to disable the specified frame-filter.

    Usage: enable frame-filter enable [dictionary] [name]

    DICTIONARY is the name of the frame filter dictionary on which to
    operate.  Named dictionaries are: "global" for the global frame
    filter dictionary, "progspace" for the program space's framefilter
    dictionary.  If either of these two are not specified, the
    dictionary name is assumed to be the name of the object-file name.

    NAME matches the name of the frame-filter to operate on.
    """
    def __init__(self):
        super(EnableFrameFilter, self).__init__("enable frame-filter",
                                                 gdb.COMMAND_DATA)

    def invoke(self, arg, from_tty):
        """GDB calls this to perform the command."""
        command_tuple = _parse_arg("enable frame-filter", arg)
        do_enable_frame_filter(command_tuple, True)


class DisableFrameFilter(gdb.Command):
    """GDB command to disable the specified frame-filter.

    Usage: disable frame-filter disable [dictionary] [name]

    DICTIONARY is the name of the frame filter dictionary on which to
    operate.  Named dictionaries are: "global" for the global frame
    filter dictionary, "progspace" for the program space's framefilter
    dictionary.  If either of these two are not specified, the
    dictionary name is assumed to be the name of the object-file name.

    NAME matches the name of the frame-filter to operate on.
    """
    def __init__(self):
        super(DisableFrameFilter, self).__init__("disable frame-filter",
                                                  gdb.COMMAND_DATA)

    def invoke(self, arg, from_tty):
        """GDB calls this to perform the command."""
        command_tuple = _parse_arg("disable frame-filter", arg)
        do_enable_frame_filter(command_tuple, False)

class SetFrameFilterPriority(gdb.Command):
    """GDB command to set the priority of the specified frame-filter.

    Usage: set python frame-filter priority dictionary name priority

    DICTIONARY is the name of the frame filter dictionary on which to
    operate.  Named dictionaries are: "global" for the global frame
    filter dictionary, "progspace" for the program space's framefilter
    dictionary.  If either of these two are not specified, the
    dictionary name is assumed to be the name of the object-file name.

    NAME matches the name of the frame filter to operate on.

    PRIORITY is the new priority to set the frame filter.
    """

    def __init__(self):
        super(SetFrameFilterPriority, self).__init__("set python " \
                                                     "frame-filter priority",
                                                     gdb.COMMAND_DATA,
                                                     gdb.COMPLETE_COMMAND)
    def _parse_pri_arg(self, arg):
        """Internal worker to parse a priority from a tuple.

        Arguments:
            arg: Tuple which contains the arguments from the command.

        Returns:
            A tuple containing the dictionary, name and priority from
            the arguments.

        Raises:
            gdb.GdbError: An error parsing the arguments.
        """

        argv = gdb.string_to_argv(arg);
        argc = len(argv)
        if argc != 3:
            raise gdb.GdbError("set python frame-filter priority " \
                              "takes exactly three arguments.")

        object_list = argv[0]
        name = argv[1]
        priority = argv[2]
        return(object_list, name, priority)

    def _set_filter_priority_1(self, frame_filters, name, priority):
        """Internal worker for setting priority of frame_filters.

        Arguments:
            frame_filters: The frame_filter dictionary.
            name: The name of the filter.
            priority: Priority of filter.

        Raises:
            gdb.GdbError: An error finding the frame filter.
        """
        try:
            ff = frame_filters[name]
        except KeyError:
            msg = "frame-filter '" + str(name) + "' not found."
            raise gdb.GdbError(msg)

        _set_priority(ff, priority)

    def _set_filter_priority(self,command_tuple):
        """Internal worker for setting priority of frame-filters, by
        parsing a tuple and calling _set_filter_priority_1 with the
        parsed tuple.

        Arguments:
            command_tuple: Tuple which contains the arguments from the
                           command.
        """

        list_op = command_tuple[0]
        frame_filter = command_tuple[1]
        priority = command_tuple [2]

        op_list = _return_list(list_op)

        self._set_filter_priority_1(op_list, frame_filter, priority)

    def invoke(self, arg, from_tty):
        """GDB calls this to perform the command."""

        command_tuple = self._parse_pri_arg(arg)
        try:
            self._set_filter_priority(command_tuple)
        except gdb.GdbError as e:
            # Print the error, instead of raising it.
            gdb.write(e.message+"\n")

class ShowFrameFilterPriority(gdb.Command):
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
    def _parse_pri_arg(self, arg):
        """Internal worker to parse a dictionary and name from a
        tuple.

        Arguments:
            arg: Tuple which contains the arguments from the command.

        Returns:
            A tuple containing the dictionary,  and frame filter name.

        Raises:
            gdb.GdbError: An error parsing the arguments.
        """

        argv = gdb.string_to_argv(arg);
        argc = len(argv)
        if argc != 2:
            raise gdb.GdbError("show python frame-filter priority " \
                              "takes exactly two arguments.")

        object_list = argv[0]
        name = argv[1]
        return (object_list, name)

    def get_filter_priority(self, frame_filters, name):
        """Worker for retrieving the priority of frame_filters.

        Arguments:
            frame_filters: Name of frame filter dictionary.
            name: object to select printers.

        Returns:
            The priority of the frame filter.

        Raises:
            gdb.GdbError: A frame filter cannot be found.
        """

        op_list = _return_list(frame_filters)

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
            priority = self.get_filter_priority(list_name, filter_name);
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
    SetFrameFilterPriority()
    ShowFrameFilterPriority()

register_frame_filter_commands()
