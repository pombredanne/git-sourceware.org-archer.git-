import gdb

class FrameFilter():

    name = "Default"
    priority = 100
    enabled = True

    def __init__(self, name, pri, enb):
        self.name = name
        self.priority  = pri
        self.enabled = enb

        gdb.frame_filters[name] = self

    def filter(iter):
        return iter
