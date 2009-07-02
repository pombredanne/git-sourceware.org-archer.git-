# Save breakpoints.

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

from __future__ import with_statement
import gdb

class SavePrefixCommand (gdb.Command):
  "Prefix command for saving things."

  def __init__ (self):
    super (SavePrefixCommand, self).__init__ ("save",
                                              gdb.COMMAND_SUPPORT,
                                              gdb.COMPLETE_NONE, True)

class SaveBreakpointsCommand (gdb.Command):
    """Save the current breakpoints to a file.
This command takes a single argument, a file name.
The breakpoints can be restored using the 'source' command."""

    def __init__ (self):
        super (SaveBreakpointsCommand, self).__init__ ("save breakpoints",
                                                       gdb.COMMAND_SUPPORT,
                                                       gdb.COMPLETE_FILENAME)

    def invoke (self, arg, from_tty):
        self.dont_repeat ()
        bps = gdb.breakpoints ()
        if bps is None:
            raise RuntimeError, 'No breakpoints to save'
        with open (arg.strip (), 'w') as f:
            for bp in bps:
                print >> f, "break", bp.location,
                if bp.thread is not None:
                    print >> f, " thread", bp.thread,
                if bp.condition is not None:
                    print >> f, " if", bp.condition,
                print >> f
                if not bp.enabled:
                    print >> f, "disable $bpnum"
                # Note: we don't save the ignore count; there doesn't
                # seem to be much point.
                commands = bp.commands
                if commands is not None:
                    print >> f, "commands"
                    # Note that COMMANDS has a trailing newline.
                    print >> f, commands,
                    print >> f, "end"
                print >> f

SavePrefixCommand ()
SaveBreakpointsCommand ()
