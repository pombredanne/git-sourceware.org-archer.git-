# Build the mappings
line_comps = {}
comp_lines = {}
num = part = 1
for line in open("result").xreadlines():
    line = line.rstrip()
    num += 1
    index = line.find("dc:0x")
    if index >= 0:
        assert part == 1
        continue
    if part == 1:
        part = 2
    comp = line.split()[-1]
    if not comp_lines.has_key(comp):
        comp_lines[comp] = []
    comp_lines[comp].append(num)
    assert not line_comps.has_key(num)
    line_comps[num] = comp

# Find the first complete loop
items = [(len(v), k, v) for k, v in comp_lines.items()]
items.sort()
items = [lines + [comp]
         for length, comp, lines in items
         if length == items[0][0] + 1]
items.sort()
start, limit = items[0][:2]
length = limit - start

# Check that the loop is looping
lines = line_comps.items()
lines.sort()
for line, actual in lines:
    loop, offset = divmod(line - start, length)
    expect = line_comps[start + offset]
    assert actual == expect

# Print the loop
for line in xrange(start, limit):
    print "%d:" % line, line_comps[line]

