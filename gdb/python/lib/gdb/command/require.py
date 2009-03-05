# Demand-loading commands.

# Copyright (C) 2008, 2009 Free Software Foundation, Inc.

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
import os

class RequireCommand (gdb.Command):
    """Prefix command for requiring features."""

    def __init__ (self):
        super (RequireCommand, self).__init__ ("require",
                                               gdb.COMMAND_SUPPORT,
                                               gdb.COMPLETE_NONE,
                                               True)

class RequireSubcommand (gdb.Command):
    """Demand-load a command by name."""

    def __init__ (self, name):
        self.__doc__ = "Demand-load a %s by name." % name
        super (RequireSubcommand, self).__init__ ("require %s" % name,
                                                  gdb.COMMAND_SUPPORT)
        self.name = name

    def invoke (self, arg, from_tty):
        for cmd in arg.split():
            exec ('import gdb.' + self.name + '.' + cmd, globals ())

    def complete (self, text, word):
        dir = gdb.pythondir + '/gdb/' + self.name
        result = []
        for file in os.listdir(dir):
            if not file.startswith (word) or not file.endswith ('.py'):
                continue
            feature = file[0:-3]
            if feature == 'require' or feature == '__init__':
                continue
            result.append (feature)
        return result

RequireCommand()
RequireSubcommand("command")
RequireSubcommand("function")
