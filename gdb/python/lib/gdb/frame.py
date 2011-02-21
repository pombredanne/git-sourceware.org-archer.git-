# Frame-related utilities.

# Copyright (C) 2008, 2009, 2010, 2011 Free Software Foundation, Inc.

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
import cStringIO

class FrameIterator(object):
    """An iterator that iterates over frames."""

    def __init__ (self, frame):
        "Initialize a FrameIterator.  FRAME is the starting frame."
        super(FrameIterator, self).__init__()
        self.frame = frame

    def __iter__ (self):
        return self

    def next (self):
        result = self.frame
        if result is None:
            raise StopIteration
        self.frame = result.older ()
        return result

class _SymWrapper(object):
    def __init__(self, frame, block, sym):
        super(_SymWrapper, self).__init__()
        self.frame = frame
        self.block = block
        if len(sym.linkage_name):
            nsym, is_field_of_this = gdb.lookup_symbol (sym.linkage_name, block)
            if nsym.addr_class != gdb.SYMBOL_LOC_REGISTER:
                sym = nsym
        self.sym = sym

    def name(self):
        return self.sym.print_name

    def value(self):
        try:
            return self.frame.read_var(self.sym)
        except RuntimeError, text:
            return text

class _BlockIterator(object):
    def __init__(self, frame, block, just_args):
        super(_BlockIterator, self).__init__()
        self.frame = frame
        self.block = block
        self.just_args = just_args
        self.iter = iter(self.block)

    def __iter__(self):
        return self

    def next(self):
        while True:
            try:
                result = self.iter.next()
                if result.is_argument == self.just_args:
                    return _SymWrapper(self.frame, self.block, result)
            except StopIteration:
                if self.block.function is not None:
                    raise StopIteration
                self.block = self.block.superblock
                self.iter = iter(self.block)

class FrameWrapper(object):
    """A wrapper for a gdb.Frame object that presents a simpler interface.
A FrameWrapper forwards most methods to the underlying Frame.
It omits a few methods, and adds some others.
Any object conforming to this interface may be returned by a frame filter."""

    def __init__(self, frame):
        super(FrameWrapper, self).__init__()
        self.frame = frame

    def name(self):
        name = self.frame.name()
        if name is None:
            name = '??'
        return name

    def type(self):
        return self.frame.type()

    def older(self):
        return self.frame.older()

    def arguments(self):
        try:
            block = self.frame.block()
            return _BlockIterator(self.frame, block, True)
        except RuntimeError:
            # It is ok if block() fails.
            return []

    def locals(self):
        try:
            block = self.frame.block()
            return _BlockIterator(self.frame, block, False)
        except RuntimeError:
            # It is ok if block() fails.
            return []

    def children(self):
        if hasattr(self.frame, 'children'):
            return self.frame.children()
        return []

    def file_and_line(self):
        sal = self.frame.find_sal()
        if sal.symtab and sal.symtab.filename:
            return (sal.symtab.filename, sal.line)
        return (None, None)

    def library(self):
        pc = self.frame.pc()
        return gdb.solib_name(pc)

def _print_symbol(stream, sym, sep):
    stream.write(sym.name())
    stream.write(sep)
    val = sym.value()
    if val is None:
        stream.write('???')
    else:
        stream.write(str(val))

def _print_args(frame, stream):
    stream.write(' (')
    first = True
    for arg in frame.arguments():
        if not first:
            stream.write(', ')
        first = False
        _print_symbol(stream, arg, '=')
    stream.write(')')

def _print_locals(frame, stream):
    for var in frame.locals():
        _print_symbol(stream, var, ' = ')
        stream.write('\n')

def print_frame(frame, stream, full = False, spaces = ''):
    stream.write(spaces)
    if frame.type() == gdb.DUMMY_FRAME:
        stream.write(" <function called from gdb>\n")
    elif frame.type() == gdb.SIGTRAMP_FRAME:
        stream.write(" <signal handler called>\n")
    elif frame.type() == gdb.ARCH_FRAME:
        stream.write(" <cross-architecture call>\n")
    else:
        stream.write(' ')
        stream.write(frame.name())
        _print_args(frame, stream)
        (filename, line) = frame.file_and_line()
        if filename is not None:
            stream.write('\n')
            stream.write(spaces)
            stream.write('    at ')
            stream.write(filename)
            stream.write(':')
            stream.write(str(line))
        else:
            lib = frame.library()
            if lib is not None:
                stream.write(' from ')
                stream.write(lib)
        stream.write('\n')
        if full:
            nstr = cStringIO.StringIO()
            _print_locals(frame, nstr)
            for line in nstr.getvalue().splitlines():
                stream.write(spaces)
                stream.write('        ')
                stream.write(line)
                stream.write('\n')
            nstr.close()
 
_frame_filters = {}

class FrameFilter(gdb.Parameter):
    def __init__(self, name):
        super(FrameFilter, self).__init__('backtrace filter ' + name,
                                          gdb.COMMAND_STACK,
                                          gdb.PARAM_ZINTEGER)
        self.name = name
        self.value = 1
        global _frame_filters
        _frame_filters[name] = self

    def filter(iter):
        return iter

def _get_value(elt):
    return elt.value

def _value_gt_0(elt):
    return elt.value > 0

def create_frame_filter(iter):
    global _frame_filters
    elts = filter(_value_gt_0, _frame_filters.values())
    # FIXME to make it stable, should also sort by name as secondary key
    elts.sort(key = _get_value, reverse = True)
    for filt in elts:
        iter = elts.filter(iter)
    return iter

def print_filters():
    global _frame_filters
    elts = _frame_filters.values()
    elts.sort(key = _get_value, reverse = True)
    if not len(elts):
        print 'No frame filters.'
    else:
        # It would be nice to print a short doc string here.
        print 'Priority\tName'
        for f in elts:
            if f.value > 0:
                print '%-8d\t%s' % (f.value, f.name)
            else:
                print 'Disabled\t%s' % f.name
