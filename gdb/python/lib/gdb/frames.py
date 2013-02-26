# Frame-filter commands.
# Copyright (C) 2012, 2013 Free Software Foundation, Inc.

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

"""Internal functions for working with frame-filters."""

import gdb
from gdb.FrameIterator import FrameIterator
from gdb.FrameWrapper import FrameWrapper
import itertools
import gdb.command.frame_filters as ffc
import collections

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
        all_filters = all_filters + objfile.frame_filters.values()
    cp = gdb.current_progspace()

    all_filters = all_filters + cp.frame_filters.values()
    all_filters = all_filters + gdb.frame_filters.values()

    sorted_frame_filters = sorted(all_filters, key = ffc._get_priority,
                                  reverse = True)

    sorted_frame_filters = filter(ffc._get_enabled,
                                  sorted_frame_filters)

    return sorted_frame_filters

def execute_frame_filters(frame, frame_low, frame_high):
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
    frame_iterator = itertools.imap(FrameWrapper, frame_iterator)

    for ff in sorted_list:
        frame_iterator = ff.filter(frame_iterator)

    # Slicing

    # Is this a relative offset, ie bt -2?
    if frame_low < 0:
        count = 0
        slice_length = abs(frame_low)
        sliced = collections.deque()

        for frame_item in frame_iterator:
            if count >= slice_length:
                sliced.popleft();
            count = count + 1
            sliced.append(frame_item)

        return iter(sliced)

    # -1 for frame_high means until the end of the stack frame, and
    # None means to the end of the iterator to islice.
    if frame_high == -1:
        frame_high = None
    else:
        # The end argument for islice is a count, not a position, so
        # add one as frames start as zero.
        frame_high = frame_high + 1;

    sliced = itertools.islice(frame_iterator, frame_low, frame_high)

    return sliced
