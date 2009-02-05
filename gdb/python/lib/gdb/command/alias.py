# Alias command.

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

class AliasImplementation (gdb.Command):
    def __init__ (self, name, real, doc):
        # Have to set __doc__ before the super init call.
        # It would be nice if gdb's help looked up __doc__ dynamically.
        self.__doc__ = doc
        # Note: no good way to complete :(
        super (AliasImplementation, self).__init__ (name, gdb.COMMAND_NONE)
        self.real = real

    def invoke(self, arg, from_tty):
        gdb.execute (self.real + ' ' + arg, from_tty)

class AliasCommand (gdb.Command):
    """Alias one command to another.
In the simplest form, the first word is the name of the alias, and
the remaining words are the the expansion.
An '=' by itself can be used to define a multi-word alias; words
before the '=' are the name of the new command."""

    def __init__ (self):
        # Completion is not quite right here.
        super (AliasCommand, self).__init__ ("alias", gdb.COMMAND_NONE,
                                             gdb.COMPLETE_COMMAND)

    def invoke (self, arg, from_tty):
        self.dont_repeat ()
        # Without some form of quoting we can't alias a multi-word
        # command to another command.
        args = arg.split()
        try:
            start = args.index ('=')
            end = start + 1
        except ValueError:
            start = 1
            end = 1
        target = " ".join(args[end:])
        AliasImplementation (" ".join (args[0:start]), target,
                             "This command is an alias for '%s'." % target)

AliasCommand()
