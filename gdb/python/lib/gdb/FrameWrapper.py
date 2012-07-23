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

import gdb

class FrameWrapper (object):
    """Interface for a Frame Wrapper"""

    """ A frame wrapper wraps a frame and provides additional and
    convenience methods. """
    def __init__(self, frame):
        super(FrameWrapper, self).__init__()
        self.frame = frame

    def elided (self):
        """ The 'elided' function groups frames together in a
        hierarchical system.  An example would be an interpreter call
        that occurs over many frames but might be better represented
        as a group of frames distinct from the other frames.

        Arguments: None

        Returns: The 'elide' function must return an iterator that
        contains the frames that are being elided.  Elided frames are
        indented from normal frames in a backtrace, to show affinity
        with the frame that elided them.  Note that it is the Frame
        Filter's task to filter out the elided frames from the
        iterator the source frames were acquired, and also to provide
        the iterator of elided frames in this function.

        If this function returns a Python None, no frames will be
        elided.
        """

        pass

    def function (self):
        """ The name of the function in the frame.

        Arguments: None.

        Returns: A string describing the function.

        If this function returns a Python None, no data will be
        displayed for this field at printing.
        """
        pass

    def address (self):
        """ The address of the frame.

        Arguments: None.

        Returns: A numeric integer type of sufficient size to describe
        the address of the frame.

        If this function returns a Python None, no data will be
        displayed for this field at printing.
        """

        pass

    def filename (self):
        """ The filename associated with the function and line number
        addressed by this frame.

        Arguments: None.

        Returns: A string containing the filename, and optionally, the
        path to the filename of the frame.

        If this function returns a Python None, the field output from
        the function will be left blank.""
        """

        pass

    def line (self):
        """ The line number associated with the current position
        within the function addressed by this frame.

        Arguments: None.

        Returns: A number integer type line number

        If this function returns a Python None, the field output from
        the function will be left blank.""
        """

        pass

    def frame_args (self):
        """ The arguments of the function in this frame.

        Arguments: None.

        Returns: An iterator that returns a tuple pairing of
        "argument", and "value".  The "argument" element can be either
        a gdb.Symbol or a string.  The "value" argument must be a
        gdb.Value or a Python value that can be translated into a
        gdb.Value.


        If this function returns a Python None, frame arguments will
        not be printed.
        """
        pass

    def frame_locals (self):
        """ The local variables of the function in this frame.

        Arguments: None.

        Returns: An iterator that returns a tuple pairing of
        "argument", and "value".  The "argument" element can be either
        a gdb.Symbol or a string.  The "value" argument must be a
        gdb.Value or a Python value that can be translated into a
        gdb.Value.


        If this function returns a Python None, local variables will
        not be printed.
        """
        pass

    def frame (self):
        """ The gdb.Frame that this wrapper is wrapping.

        Arguments: None.

        Returns: The gdb.Frame that this wrapper is wrapping.
        """
        pass
