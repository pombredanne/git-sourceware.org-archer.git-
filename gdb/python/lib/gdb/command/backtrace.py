# New backtrace command.

# Copyright (C) 2008 Free Software Foundation, Inc.

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
import gdb.backtrace
import itertools
from gdb.FrameIterator import FrameIterator
import sys

class FrameWrapper:
    def __init__ (self, frame):
        self.frame = frame;

    def write_symbol (self, stream, sym, block):
        if len (sym.get_linkage_name ()):
            nsym, is_field_of_this = gdb.lookup_symbol (sym.get_linkage_name (), block, gdb.SYMBOL_VAR_DOMAIN)
            if nsym.get_class () != gdb.SYMBOL_LOC_REGISTER:
                sym = nsym

        stream.write (sym.get_print_name () + "=")
        try:
            val = self.frame.read_var_value (sym)
            if val != None:
                val = str (val)
        # FIXME: would be nice to have a more precise exception here.
        except RuntimeError, text:
            val = text
        if val == None:
            stream.write ("???")
        else:
            stream.write (str (val))

    def print_frame_locals (self, stream, func):
        if not func:
            return

        first = True
        block = func.get_value ()

        for sym in block:
            if sym.is_argument ():
                continue;

            self.write_symbol (stream, sym, block)
            stream.write ('\n')

    def print_frame_args (self, stream, func):
        if not func:
            return

        first = True
        block = func.get_value ()

        for sym in block:
            if not sym.is_argument ():
                continue;

            if not first:
                stream.write (", ")

            self.write_symbol (stream, sym, block)
            first = False

    # FIXME: this should probably just be a method on gdb.Frame.
    # But then we need stream wrappers.
    def describe (self, stream, full):
        if self.frame.get_type () == gdb.DUMMY_FRAME:
            stream.write (" <function called from gdb>\n")
        elif self.frame.get_type () == gdb.SIGTRAMP_FRAME:
            stream.write (" <signal handler called>\n")
        else:
            sal = self.frame.find_sal ()
            pc = self.frame.get_pc ()
            name = self.frame.get_name ()
            if not name:
                name = "??"
            if pc != sal.get_pc () or not sal.symtab:
                stream.write (" 0x%08x in" % pc)
            stream.write (" " + name + " (")

            func = gdb.find_pc_function (self.frame.get_address_in_block ())
            self.print_frame_args (stream, func)

            stream.write (")")

            if sal.symtab and sal.symtab.get_filename ():
                stream.write (" at " + sal.symtab.get_filename ())
                stream.write (":" + str (sal.get_line ()))

            if not self.frame.get_name () or (not sal.symtab or not sal.symtab.get_filename ()):
                lib = gdb.solib_address (pc)
                if lib:
                    stream.write (" from " + lib)

            stream.write ("\n")

            if full:
                self.print_frame_locals (stream, func)

    def __getattribute__ (self, name):
        return getattr (self.frame, name)

class ReverseBacktraceParameter (gdb.Parameter):
    """The new-backtrace command can show backtraces in 'reverse' order.
This means that the innermost frame will be printed last.
Note that reverse backtraces are more expensive to compute."""

    set_doc = "Enable or disable reverse backtraces."
    show_doc = "Show whether backtraces will be printed in reverse order."

    def __init__(self):
        gdb.Parameter.__init__ (self, "reverse-backtrace",
                                gdb.COMMAND_STACK, gdb.PARAM_BOOLEAN)
        # Default to compatibility with gdb.
        self.value = False

class FilteringBacktrace (gdb.Command):
    """Print backtrace of all stack frames, or innermost COUNT frames.
With a negative argument, print outermost -COUNT frames.
Use of the 'full' qualifier also prints the values of the local variables.
Use of the 'raw' qualifier avoids any filtering by loadable modules.
"""

    def __init__ (self):
        # FIXME: this is not working quite well enough to replace
        # "backtrace" yet.
        gdb.Command.__init__ (self, "new-backtrace", gdb.COMMAND_STACK)
        self.reverse = ReverseBacktraceParameter()

    def reverse_iter (self, iter):
        result = []
        for item in iter:
            result.append (item)
        result.reverse()
        return result

    def final_n (self, iter, x):
        result = []
        for item in iter:
            result.append (item)
        return result[x:]

    def invoke (self, arg, from_tty):
        i = 0
        count = 0
        filter = True
        full = False

        if arg == None:
            arg = ''
        for word in arg.split (" "):
            if word == '':
                continue
            elif word == 'raw':
                filter = False
            elif word == 'full':
                full = True
            else:
                count = int (word)

        # FIXME: provide option to start at selected frame
        # However, should still number as if starting from current
        iter = itertools.imap (FrameWrapper,
                               FrameIterator (gdb.get_current_frame ()))
        if filter:
            iter = gdb.backtrace.create_frame_filter (iter)

        # Now wrap in an iterator that numbers the frames.
        iter = itertools.izip (itertools.count (0), iter)

        # Reverse if the user wanted that.
        if self.reverse.value:
            iter = self.reverse_iter (iter)

        # Extract sub-range user wants.
        if count < 0:
            iter = self.final_n (iter, count)
        elif count > 0:
            iter = itertools.islice (iter, 0, count)

        for pair in iter:
            sys.stdout.write ("#%-2d" % pair[0])
            pair[1].describe (sys.stdout, full)

FilteringBacktrace()
