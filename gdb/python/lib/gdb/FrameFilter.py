import gdb

class FrameFilter():
    
    name = "Default"
    priority = 100
    enabled = True

    def __init__(self, next_filter):
        self.next_filter = next_filter
        
    def filter(iter):
        return iter
