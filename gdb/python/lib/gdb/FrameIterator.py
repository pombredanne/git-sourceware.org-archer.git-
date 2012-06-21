import gdb
import itertools

class FrameIterator(object):
    """A frame iterator.  Iterates over gdb.Frames or objects that
    conform to that interface."""

    def __init__ (self, frame_obj):
        """Initialize a FrameIterator.  FRAME_OBJ is the starting
        frame."""

        super(FrameIterator, self).__init__()
        self.frame = frame_obj

    def __iter__ (self):
        return self

    def __getitem__(self,index):
        return next(itertools.islice(self.frame,index,index+1))

    def next (self):
        result = self.frame
        if result is None:
            raise StopIteration
        self.frame = result.older ()
        return result
