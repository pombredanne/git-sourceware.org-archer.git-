# upto command.

# Copyright (C) 2009 Free Software Foundation, Inc.

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
import re
from gdb.FrameIterator import FrameIterator
from gdb.FrameWrapper import FrameWrapper

class UptoPrefix (gdb.Command):
    def __init__ (self):
        super (UptoPrefix, self).__init__ ("upto", gdb.COMMAND_STACK,
                                           prefix = True)

class UptoImplementation (gdb.Command):
    def __init__ (self, subcommand):
        super (UptoImplementation, self).__init__ ("upto " + subcommand,
                                                   gdb.COMMAND_STACK)

    def search (self):
        saved = gdb.selected_frame ()
        iter = FrameIterator (saved)
        found = False
        try:
            for frame in iter:
                frame.select ()
                try:
                    if self.filter (frame):
                        wrapper = FrameWrapper (frame)
                        wrapper.describe (sys.stdout, False)
                        return
                except:
                    pass
        except:
            pass
        saved.select ()
        raise RuntimeError, 'Could not find a matching frame'

    def invoke (self, arg, from_tty):
        self.rx = re.compile (arg)
        self.search ()

class UptoSymbolCommand (UptoImplementation):
    """Select and print some calling stack frame, based on symbol.
The argument is a regular expression.  This command moves up the
stack, stopping at the first frame whose symbol matches the regular
expression."""

    def __init__ (self):
        super (UptoSymbolCommand, self).__init__ ("symbol")

    def filter (self, frame):
        name = frame.name ()
        if name is not None:
            if self.rx.search (name) is not None:
                return True
        return False

class UptoSourceCommand (UptoImplementation):
    """Select and print some calling stack frame, based on source file.
The argument is a regular expression.  This command moves up the
stack, stopping at the first frame whose source file name matches the
regular expression."""

    def __init__ (self):
        super (UptoSourceCommand, self).__init__ ("source")

    def filter (self, frame):
        name = frame.find_sal ().symtab.filename
        if name is not None:
            if self.rx.search (name) is not None:
                return True
        return False

class UptoObjectCommand (UptoImplementation):
    """Select and print some calling stack frame, based on object file.
The argument is a regular expression.  This command moves up the
stack, stopping at the first frame whose object file name matches the
regular expression."""

    def __init__ (self):
        super (UptoObjectCommand, self).__init__ ("object")

    def filter (self, frame):
        name = frame.find_sal ().symtab.objfile.filename
        if name is not None:
            if self.rx.search (name) is not None:
                return True
        return False

class UptoWhereCommand (UptoImplementation):
    """Select and print some calling stack frame, based on expression.
The argument is an expression.  This command moves up the stack,
parsing and evaluating the expression in each frame.  This stops when
the expression evaluates to a non-zero (true) value."""

    def __init__ (self):
        super (UptoWhereCommand, self).__init__ ("where")

    def filter (self, frame):
        try:
            if gdb.parse_and_eval (self.expression):
                return True
        except:
            pass
        return False

    def invoke (self, arg, from_tty):
        self.expression = arg
        self.search ()

UptoPrefix ()
UptoSymbolCommand ()
UptoSourceCommand ()
UptoObjectCommand ()
UptoWhereCommand ()
