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

def _parse_arg (cmd_name, arg):
    argv = gdb.string_to_argv(arg);
    argc = len(argv)
    if argc != 2:
        raise SyntaxError(cmd_name + " takes exactly " + \
                          "two arguments.")
    
    object_list = argv[0]
    argument = argv[1]
    
    return (object_list, argument)

def _get_priority(filter_item):
    if hasattr(filter_item, "priority"):
        return filter_item.priority
    else:
        return 1000

def _set_priority(filter_item, priority):
    if priority < 0 and priority > 10000:
        raise SyntaxError("Priority must be between 0 - 10000")
    if hasattr(filter_item, "priority"):
        filter_item.priority = priority

def _get_enabled(filter_item):
    if hasattr(filter_item, "enabled"):
        return filter_item.enabled
    else:
        return True

def _set_enabled(filter_item, state):
    if hasattr(filter_item, "enabled"):
        filter_item.enabled = state

def _get_name (frame_filter):
    """Return the filter's name."""
    if hasattr(frame_filter, "name"):
        return frame_filter.name
    if hasattr(frame_filter, "__name__"):
        return frame_filter.__name__
    # Deal with anonymous objects/functions
    return "Unknown"

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

        sorted_frame_filters = copy.copy(frame_filters)
        sorted_frame_filters.sort(key = _get_priority, reverse = True)

        print "  Priority  Enabled  Name"
        print "  ========  =======  ===="
        for frame_filter in sorted_frame_filters:
            priority = '{:<8}'.format(str(_get_priority (frame_filter)))
            name = _get_name(frame_filter)
            enabled = '{:<7}'.format(self.enabled_string(_get_enabled(frame_filter)))
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
            self.print_list("  objfile %s frame-filters:" % objfile.filename,
                            objfile.frame_filters)

def do_enable_frame_filter_1 (frame_filters, name, flag):
    """Worker for enabling/disabling frame_filters.

    Arguments:
        frame_filters: list of frame_filters
        name: object to select printers
        flag: True for Enable, False for Disable
    """
    for ff in frame_filters:
        if (hasattr(ff, "name") and _get_name(ff) == name or
            hasattr(ff, "__name__") and ff.__name__ == name):
               _set_enabled(ff,flag)

def do_enable_frame_filter (command_tuple, flag):
    """Internal worker for enabling/disabling frame-filters."""

    list_op = command_tuple[0]
    frame_filter = command_tuple[1]

    if list_op == "all" or list_op == "global":
        do_enable_frame_filter_1(gdb.frame_filters, 
                                 frame_filter, flag)
    if list_op == "all" or list_op == "progspace":
        cp = gdb.current_progspace()
        do_enable_frame_filter_1(cp.frame_filters,
                                 frame_filter, flag)
    if list_op == "all" or list_op == "objfile":
        for objfile in gdb.objfiles():
            if object_list == objfile.filename:
                do_enable_frame_filter_1(objfile.frame_filters,
                                         frame_filter, flag)

class FilterPrefixCmd (gdb.Command):
    def __init__ (self):
        super (FilterPrefixCmd, self).__init__ ("set python frame-filter",
                                                gdb.COMMAND_DATA, 
                                                gdb.COMPLETE_COMMAND, True)

class EnableFrameFilter (gdb.Command):
    """GDB command to disable the specified frame-filter.

    Usage: set python frame-filter enable [list] [name]

    LIST is the name of the frame-filter list to operate on, or 'all'
    to operate on all frame-filter lists. Named lists are: "global"
    for the global frame-filter list, "progspace" for the program
    space's file frame-filter list, and "objfile" for the objfiles
    within that program space frame-filter list.

    NAME matches the name of the frame-filter to operate on.
    """
    def __init__(self):
        super(EnableFrameFilter, self).__init__("set python frame-filter " \
                                                "enable", gdb.COMMAND_DATA,
                  gdb.COMPLETE_COMMAND)

    def invoke(self, arg, from_tty):
        """GDB calls this to perform the command."""
        command_tuple = _parse_arg("set python frame-filter disable", arg)
        do_enable_frame_filter(command_tuple, True)


class DisableFrameFilter (gdb.Command):
    """GDB command to disable the specified frame-filter.

    Usage: set python frame-filter disable [list] [name]

    LIST is the name of the frame-filter list to operate on, or 'all'
    to operate on all frame-filter lists. Named lists are: "global"
    for the global frame-filter list, "progspace" for the program
    space's file frame-filter list, and "objfile" for the objfiles
    within that program space frame-filter list.

    NAME matches the name of the frame-filter to operate on.
    """
    def __init__(self):
        super(DisableFrameFilter, self).__init__("set python frame-filter " \
                                                 "disable", gdb.COMMAND_DATA,
                                                 gdb.COMPLETE_COMMAND)

    def invoke(self, arg, from_tty):
        """GDB calls this to perform the command."""
        command_tuple = _parse_arg("set python frame-filter disable", arg)
        do_enable_frame_filter(command_tuple, False)

class SetFrameFilterPriority (gdb.Command):
    """GDB command to set the priority of the  specified frame-filter.

    Usage: set python frame-filter priority [list] [name] [priority]

    LIST is the name of the frame-filter list to operate on, or 'all'
    to operate on all frame-filter lists. Named lists are: "global"
    for the global frame-filter list, "progspace" for the program
    space's file frame-filter list, and "objfile" for the objfiles
    within that program space frame-filter list.

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
            raise SyntaxError("set python frame-filter priority " \
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

        for ff in frame_filters:
            if (hasattr(ff, "name") and _get_name(ff) == name or
                hasattr(ff, "__name__") and ff.__name__ == name):
                _set_priority(ff, priority)

    def _set_filter_priority (self,command_tuple):
        """Internal worker for setting priority of frame-filters."""

        list_op = command_tuple[0]
        frame_filter = command_tuple[1]
        priority = command_tuple [2]

        if list_op == "all" or list_op == "global":
            self._set_filter_priority_1(gdb.frame_filters, 
                                        frame_filter, priority)
        if list_op == "all" or list_op == "progspace":
            cp = gdb.current_progspace()
            self._set_filter_priority_filter_1(cp.frame_filters,
                                               frame_filter, priority)
        if list_op == "all" or list_op == "objfile":
            for objfile in gdb.objfiles():
                if object_list == objfile.filename:
                    self._set_filter_priority_1(objfile.frame_filters,
                                                frame_filter, priority)

    def invoke(self, arg, from_tty):
        """GDB calls this to perform the command."""
        command_tuple = self._parse_pri_arg(arg)
        self._set_filter_priority (command_tuple)

def register_frame_filter_commands():
    """Call from a top level script to install the frame-filter commands."""
    InfoFrameFilter()
    FilterPrefixCmd()
    EnableFrameFilter()
    DisableFrameFilter()
    SetFrameFilterPriority ()

register_frame_filter_commands()
