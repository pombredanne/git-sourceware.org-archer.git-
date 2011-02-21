# New backtrace command.

# Copyright (C) 2008, 2009, 2010, 2011 Free Software Foundation, Inc.

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

import gdb
import gdb.frame
import itertools
import sys

class ReverseBacktraceParameter(gdb.Parameter):
    """The new-backtrace command can show backtraces in 'reverse' order.
This means that the innermost frame will be printed last.
Note that reverse backtraces are more expensive to compute."""

    set_doc = "Enable or disable reverse backtraces."
    show_doc = "Show whether backtraces will be printed in reverse order."

    def __init__(self):
        super(ReverseBacktraceParameter, self).__init__("reverse-backtrace",
                                                        gdb.COMMAND_STACK,
                                                        gdb.PARAM_BOOLEAN)

        # Default to compatibility with gdb.
        self.value = False

class FilteringBacktrace(gdb.Command):
    """Print backtrace of all stack frames, or innermost COUNT frames.

Usage: new-backtrace [COUNT|QUALIFIER]...

With a negative argument, print outermost -COUNT frames.

Valid qualifiers are:

   full        Also print the values of the local variables.
   raw         Avoid any filtering by loadable modules.
   reverse     Reverse the stack trace.  If a reverse trace was
               already selected by `set reverse-backtrace', then an
               ordinary stack trace is done.  Note that reverse
               backtraces are more expensive to compute.
   all         Show frames that have been filtered out.

"""

    def __init__(self):
        # FIXME: this is not working quite well enough to replace
        # "backtrace" yet.
        super(FilteringBacktrace, self).__init__("new-backtrace",
                                                 gdb.COMMAND_STACK)

        self.reverse = ReverseBacktraceParameter()

    def reverse_iter(self, iter):
        result = list(iter)
        result.reverse()
        return result

    def final_n(self, iter, x):
        result = list(iter)
        return result[x:]

    def invoke(self, arg, from_tty):
        i = 0
        count = 0
        filter = True
        full = False
        reverse = self.reverse.value
        showall = False

        for word in arg.split(" "):
            if word == '':
                continue
            elif word == 'raw':
                filter = False
            elif word == 'full':
                full = True
            elif word == 'reverse':
                reverse = not reverse
            elif word == 'all':
                showall = True
            else:
                count = int(word)

        # FIXME: try/catch and wrap in gdb error
        newest_frame = gdb.newest_frame()
        iter = itertools.imap(gdb.frame.FrameWrapper,
                              gdb.frame.FrameIterator(newest_frame))

        if filter:
            iter = gdb.frame.create_frame_filter(iter)

        # Now wrap in an iterator that numbers the frames.
        iter = itertools.izip(itertools.count(0), iter)

        # Reverse if the user wanted that.
        if reverse:
            iter = self.reverse_iter(iter)

        # Extract sub-range user wants.
        if count < 0:
            iter = self.final_n(iter, count)
        elif count > 0:
            iter = itertools.islice(iter, 0, count)

        for pair in iter:
            sys.stdout.write("#%-2d" % pair[0])
            gdb.frame.print_frame(pair[1], sys.stdout, full)
            if showall:
                for f in pair[1].children():
                    gdb.frame.print_frame(f, sys.stdout, full, '    ')


FilteringBacktrace()


def lame():
    class _Holder(gdb.Command):
        def __init__(self, what):
            super(_Holder, self).__init__(what + " backtrace filter",
                                          gdb.COMMAND_STACK,
                                          prefix = True)

        def invoke(self, arg, from_tty):
            # FIXME
            pass

    _Holder("set")
    _Holder("show")

    class ShowFilter(gdb.Command):
        def __init__(self):
            super(ShowFilter, self).__init__("show backtrace filters",
                                             gdb.COMMAND_STACK)

        def invoke(self, arg, from_tty):
            gdb.frame.print_filters()

    ShowFilter()
