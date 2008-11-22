# Pretty-printers for libstc++.

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
import itertools

class StdPointerPrinter:
    "Print a smart pointer of some kind"

    def __init__ (self, typename, val):
        self.typename = typename
        self.val = val

    def to_string (self):
        return '%s (count %d) %s' % (self.typename, self.val['_M_refcount'],
                                     self.val['_M_ptr'])

class UniquePointerPrinter:
    "Print a unique_ptr"

    def __init__ (self, val):
        self.val = val

    def to_string (self):
        return self.val['_M_t']

class StdListPrinter:
    "Print a std::list"

    class _iterator:
        def __init__(self, nodetype, head):
            self.nodetype = nodetype
            self.base = head['_M_next']
            self.head = head.address()
            self.count = 0

        def __iter__(self):
            return self

        def next(self):
            if self.base == self.head:
                raise StopIteration
            elt = self.base.cast(self.nodetype).dereference()
            self.base = elt['_M_next']
            count = self.count
            self.count = self.count + 1
            return ('[%d]' % count, elt['_M_data'])

    def __init__(self, val):
        self.val = val

    def children(self):
        itype = self.val.type().template_argument(0)
        nodetype = gdb.Type('std::_List_node<%s>' % itype).pointer()
        return self._iterator(nodetype, self.val['_M_impl']['_M_node'])

    def to_string(self):
        if self.val['_M_impl']['_M_node'].address() == self.val['_M_impl']['_M_node']['_M_next']:
            return 'empty std::list'
        return 'std::list'

class StdSlistPrinter:
    "Print a __gnu_cxx::slist"

    class _iterator:
        def __init__(self, nodetype, head):
            self.nodetype = nodetype
            self.base = head['_M_head']['_M_next']
            self.count = 0

        def __iter__(self):
            return self

        def next(self):
            if self.base == 0:
                raise StopIteration
            elt = self.base.cast(self.nodetype).dereference()
            self.base = elt['_M_next']
            count = self.count
            self.count = self.count + 1
            return ('[%d]' % count, elt['_M_data'])

    def __init__(self, val):
        self.val = val

    def children(self):
        itype = self.val.type().template_argument(0)
        nodetype = gdb.Type('__gnu_cxx::_Slist_node<%s>' % itype).pointer()
        return self._iterator(nodetype, self.val)

    def to_string(self):
        if self.val['_M_head']['_M_next'] == 0:
            return 'empty __gnu_cxx::slist'
        return '__gnu_cxx::slist'

class StdVectorPrinter:
    "Print a std::vector"

    class _iterator:
        def __init__ (self, start, finish):
            self.item = start
            self.finish = finish
            self.count = 0

        def __iter__(self):
            return self

        def next(self):
            if self.item == self.finish:
                raise StopIteration
            count = self.count
            self.count = self.count + 1
            elt = self.item.dereference()
            self.item = self.item + 1
            return ('[%d]' % count, elt)

    def __init__(self, val):
        self.val = val

    def children(self):
        return self._iterator(self.val['_M_impl']['_M_start'],
                              self.val['_M_impl']['_M_finish'])

    def to_string(self):
        start = self.val['_M_impl']['_M_start']
        finish = self.val['_M_impl']['_M_finish']
        end = self.val['_M_impl']['_M_end_of_storage']
        return ('std::vector of length %d, capacity %d'
                % (int (finish - start), int (end - start)))

    def display_hint(self):
        return 'whatever'

class StdStackOrQueuePrinter:
    "Print a std::stack or std::queue"

    def __init__ (self, typename, val):
        self.typename = typename
        self.visualizer = gdb.get_default_visualizer(val['c'])

    def children (self):
        return self.visualizer.children()

    def to_string (self):
        return '%s wrapping: %s' % (self.typename,
                                    self.visualizer.to_string())

class RbtreeIterator:
    def __init__(self, rbtree):
        self.size = rbtree['_M_t']['_M_impl']['_M_node_count']
        self.node = rbtree['_M_t']['_M_impl']['_M_header']['_M_left']
        self.count = 0

    def __iter__(self):
        return self

    def __len__(self):
        return int (self.size)

    def next(self):
        if self.count == self.size:
            raise StopIteration
        result = self.node
        self.count = self.count + 1
        if self.count < self.size:
            # Compute the next node.
            node = self.node
            if node.dereference()['_M_right']:
                node = node.dereference()['_M_right']
                while node.dereference()['_M_left']:
                    node = node.dereference()['_M_left']
            else:
                parent = node.dereference()['_M_parent']
                while node == parent.dereference()['_M_right']:
                    node = parent
                    parent = parent.dereference()['_M_parent']
                if node.dereference()['_M_right'] != parent:
                    node = parent
            self.node = node
        return result

class StdMapPrinter:
    "Print a std::map or std::multimap"

    # Turn an RbtreeIterator into a pretty-print iterator.
    class _iter:
        def __init__(self, rbiter, type):
            self.rbiter = rbiter
            self.count = 0
            self.type = type

        def __iter__(self):
            return self

        def next(self):
            if self.count % 2 == 0:
                n = self.rbiter.next()
                n = n.cast(self.type).dereference()['_M_value_field']
                self.pair = n
                item = n['first']
            else:
                item = self.pair['second']
            result = ('[%d]' % self.count, item)
            self.count = self.count + 1
            return result

    def __init__ (self, typename, val):
        self.typename = typename
        self.val = val
        self.iter = RbtreeIterator (val)

    def to_string (self):
        return '%s with %d elements' % (self.typename, len (self.iter))

    def children (self):
        keytype = self.val.type().template_argument(0)
        valuetype = self.val.type().template_argument(1)
        nodetype = gdb.Type('std::_Rb_tree_node< std::pair< const %s, %s > >' % (keytype, valuetype))
        nodetype = nodetype.pointer()
        return self._iter (self.iter, nodetype)

    def display_hint (self):
        return 'map'

class StdSetPrinter:
    "Print a std::set or std::multiset"

    # Turn an RbtreeIterator into a pretty-print iterator.
    class _iter:
        def __init__(self, rbiter, type):
            self.rbiter = rbiter
            self.count = 0
            self.type = type

        def __iter__(self):
            return self

        def next(self):
            item = self.rbiter.next()
            item = item.cast(self.type).dereference()['_M_value_field']
            # FIXME: this is weird ... what to do?
            # Maybe a 'set' display hint?
            result = ('[%d]' % self.count, item)
            self.count = self.count + 1
            return result

    def __init__ (self, typename, val):
        self.typename = typename
        self.val = val
        self.iter = RbtreeIterator (val)

    def to_string (self):
        return '%s with %d elements' % (self.typename, len (self.iter))

    def children (self):
        keytype = self.val.type().template_argument(0)
        nodetype = gdb.Type('std::_Rb_tree_node< %s >' % keytype).pointer()
        return self._iter (self.iter, nodetype)

class StdBitsetPrinter:
    "Print a std::bitset"

    def __init__(self, val):
        self.val = val

    def to_string (self):
        # If template_argument handled values, we could print the
        # size.  Or we could use a regexp on the type.
        return 'std::bitset'

    def children (self):
        words = self.val['_M_w']
        wtype = words.type()
        tsize = wtype.target().sizeof()
        nwords = wtype.sizeof() / tsize
        result = []
        byte = 0
        while byte < nwords:
            w = words[byte]
            bit = 0
            while w != 0:
                if (w & 1) != 0:
                    # Another spot where we could use 'set'?
                    result.append(('[%d]' % (byte * tsize * 8 + bit), 1))
                bit = bit + 1
                w = w >> 1
            byte = byte + 1
        return result

class StdDequePrinter:
    "Print a std::deque"

    class _iter:
        def __init__(self, node, start, end, last, buffer_size):
            self.node = node
            self.p = start
            self.end = end
            self.last = last
            self.buffer_size = buffer_size
            self.count = 0

        def __iter__(self):
            return self

        def next(self):
            if self.p == self.last:
                raise StopIteration

            result = ('[%d]' % self.count, self.p.dereference())
            self.count = self.count + 1

            # Advance the 'cur' pointer.
            self.p = self.p + 1
            if self.p == self.end:
                # If we got to the end of this bucket, move to the
                # next bucket.
                self.node = self.node + 1
                self.p = self.node[0]
                self.end = self.p + self.buffer_size

            return result

    def __init__(self, val):
        self.val = val
        self.elttype = val.type().template_argument(0)
        size = self.elttype.sizeof ()
        if size < 512:
            self.buffer_size = int (512 / size)
        else:
            self.buffer_size = 1

    def to_string(self):
        start = self.val['_M_impl']['_M_start']
        end = self.val['_M_impl']['_M_finish']

        delta_n = end['_M_node'] - start['_M_node'] - 1
        delta_s = start['_M_last'] - start['_M_cur']
        delta_e = end['_M_cur'] - end['_M_first']

        size = self.buffer_size * delta_n + delta_s + delta_e

        return 'std::deque with %d elements' % long (size)

    def children(self):
        start = self.val['_M_impl']['_M_start']
        end = self.val['_M_impl']['_M_finish']
        return self._iter(start['_M_node'], start['_M_cur'], start['_M_last'],
                          end['_M_cur'], self.buffer_size)

class WideEncoding (gdb.Parameter):
    """The target wide character set is the encoding used for wchar_t."""

    set_doc = "Set the target wide character set."
    show_doc = "Show the target wide character set."

    # FIXME: needs a complete method -- but does Parameter support it?
    def __init__ (self):
        super (WideEncoding, self).__init__ ("target-wide-charset",
                                             gdb.COMMAND_SUPPORT,
                                             gdb.PARAM_STRING)
        # I think this is ok for most glibc locales.
        self.value = 'UTF-32'

target_wide_charset = WideEncoding()

class StdStringPrinter:
    "Print a std::basic_string of some kind"

    def __init__(self, encoding, val):
        self.encoding = encoding
        self.val = val

    def to_string(self):
        # Look up the target encoding as late as possible.
        encoding = self.encoding
        if encoding is None:
            encoding = gdb.get_parameter('target-charset')
        elif isinstance(encoding, WideEncoding):
            encoding = encoding.value
        return self.val['_M_dataplus']['_M_p'].string(encoding)

class Tr1HashtableIterator:
    def __init__ (self, hash):
        self.count = 0
        self.n_buckets = hash['_M_bucket_count']
        if self.n_buckets == 0:
            self.node = False
        else:
            self.bucket = hash['_M_buckets']
            self.node = self.bucket[0]
            self.update ()

    def __iter__ (self):
        return self

    def update (self):
        # If we advanced off the end of the chain, move to the next
        # bucket.
        while self.node == 0:
            self.bucket = self.bucket + 1
            self.node = self.bucket[0]
            self.count = self.count + 1
            # If we advanced off the end of the bucket array, then
            # we're done.
            if self.count == self.n_buckets:
                self.node = False

    def next (self):
        if not self.node:
            raise StopIteration
        result = self.node.dereference()['_M_v']
        self.node = self.node.dereference()['_M_next']
        self.update ()
        return result

class Tr1UnorderedSetPrinter:
    "Print a tr1::unordered_set"

    def __init__ (self, typename, val):
        self.typename = typename
        self.val = val

    def to_string (self):
        return '%s with %d elements' % (self.typename, self.val['_M_element_count'])

    @staticmethod
    def format_count (i):
        return '[%d]' % i

    def children (self):
        counter = itertools.imap (self.format_count, itertools.count())
        return itertools.izip (counter, Tr1HashtableIterator (self.val))

class Tr1UnorderedMapPrinter:
    "Print a tr1::unordered_map"

    def __init__ (self, typename, val):
        self.typename = typename
        self.val = val

    def to_string (self):
        return '%s with %d elements' % (self.typename, self.val['_M_element_count'])

    @staticmethod
    def flatten (list):
        for elt in list:
            for i in elt:
                yield i

    @staticmethod
    def format_one (elt):
        return (elt['first'], elt['second'])

    @staticmethod
    def format_count (i):
        return '[%d]' % i

    def children (self):
        counter = itertools.imap (self.format_count, itertools.count())
        # Map over the hash table and flatten the result.
        data = self.flatten (itertools.imap (self.format_one, Tr1HashtableIterator (self.val)))
        # Zip the two iterators together.
        return itertools.izip (counter, data)

    def display_hint (self):
        return 'map'

def register_libstdcxx_printers(obj):
    "Register libstdc++ pretty-printers with objfile Obj."

    if obj == None:
        obj = gdb

    # libstdc++ objects requiring pretty-printing.
    # In order from:
    # http://gcc.gnu.org/onlinedocs/libstdc++/latest-doxygen/a01847.html
    obj.pretty_printers['^std::basic_string<char,.*>$'] = lambda val: StdStringPrinter(None, val)
    obj.pretty_printers['^std::basic_string<wchar_t,.*>$'] = lambda val: StdStringPrinter(target_wide_charset, val)
    obj.pretty_printers['^std::basic_string<char16_t,.*>$'] = lambda val: StdStringPrinter('UTF-16', val)
    obj.pretty_printers['^std::basic_string<char32_t,.*>$'] = lambda val: StdStringPrinter('UTF-32', val)
    obj.pretty_printers['^std::bitset<.*>$'] = StdBitsetPrinter
    obj.pretty_printers['^std::deque<.*>$'] = StdDequePrinter
    obj.pretty_printers['^std::list<.*>$'] = StdListPrinter
    obj.pretty_printers['^std::map<.*>$'] = lambda val: StdMapPrinter("std::map", val)
    obj.pretty_printers['^std::multimap<.*>$'] = lambda val: StdMapPrinter("std::multimap", val)
    obj.pretty_printers['^std::multiset<.*>$'] = lambda val: StdSetPrinter("std::multiset", val)
    obj.pretty_printers['^std::priority_queue<.*>$'] = lambda val: StdStackOrQueuePrinter("std::priority_queue", val)
    obj.pretty_printers['^std::queue<.*>$'] = lambda val: StdStackOrQueuePrinter("std::queue", val)
    obj.pretty_printers['^std::set<.*>$'] = lambda val: StdSetPrinter("std::set", val)
    obj.pretty_printers['^std::stack<.*>$'] = lambda val: StdStackOrQueuePrinter("std::stack", val)
    obj.pretty_printers['^std::vector<.*>$'] = StdVectorPrinter
    # vector<bool>

    # C++0x stuff.
    # array - the default seems reasonable
    # smart_ptr?  seems to only be in boost right now
    obj.pretty_printers['^std::tr1::shared_ptr<.*>$'] = lambda val: StdPointerPrinter ('std::shared_ptr', val)
    obj.pretty_printers['^std::tr1::weak_ptr<.*>$'] = lambda val: StdPointerPrinter ('std::weak_ptr', val)
    obj.pretty_printers['^std::tr1::unique_ptr<.*>$'] = UniquePointerPrinter
    obj.pretty_printers['^std::tr1::unordered_map<.*>$'] = lambda val: Tr1UnorderedMapPrinter ('std::tr1::unordered_map', val)
    obj.pretty_printers['^std::tr1::unordered_set<.*>$'] = lambda val: Tr1UnorderedSetPrinter ('std::tr1::unordered_set', val)
    obj.pretty_printers['^std::tr1::unordered_multimap<.*>$'] = lambda val: Tr1UnorderedMapPrinter ('std::tr1::unordered_multimap', val)
    obj.pretty_printers['^std::tr1::unordered_multiset<.*>$'] = lambda val: Tr1UnorderedSetPrinter ('std::tr1::unordered_multiset', val)

    # Extensions.
    obj.pretty_printers['^__gnu_cxx::slist<.*>$'] = StdSlistPrinter

register_libstdcxx_printers (gdb.get_current_objfile())
