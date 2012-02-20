flevel = 0

class Main_filter:
    "Example main () filter"

    def __init__ (self, frame, what, level, args):
        self.frame = frame
        self.what = what
        self.lvl = level
        self.args = args
        

    def function (self):
        return str (self.frame.function())

    def level (self):
        global flevel
        rlevel = flevel
        flevel = flevel + 1
        return rlevel

    def address (self):
        return self.frame.pc()

    def filename (self):
        sal = self.frame.find_sal()
        if (sal):
            return sal.symtab.filename
        else:
            return "unknown"

    def line (self):
        sal = self.frame.find_sal()
        if (sal):
            return sal.line        
        else:
            return "<unknown line>"

def register_frame_filters (frame, what, level, args):

#    if (frame.name() == "main"):
        x = Main_filter (frame, what, level, args)
        return x
