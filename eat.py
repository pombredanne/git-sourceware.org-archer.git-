num = part = 1
print "TREE:"
for line in open("result").xreadlines():
    line = line.rstrip()
    num += 1
    index = line.find("dc:0x")
    if index >= 0:
        assert part == 1
        comp = line[index + 3:].split(None, 1)[0]
        print "%4d" % num, comp
        continue
    if part == 1:
        print
        print "FRAMES:"
        part = 2
    print "%4d" % num, line.split()[-1]
