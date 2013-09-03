class DemangleError(ValueError):
    def __init__(self, buf, maxlen = 56):
        if len(buf) > maxlen:
            buf = buf[:maxlen - 3] + "..."
        ValueError.__init__(self, buf)

class Demangler:
    def __init__(self, symbol, prefix="_Z"):
        assert symbol.startswith(prefix)
        self.indent = 0
        self.out_cont(1, symbol, 2, "<mangled-name>")

    def out_cont(self, indent, buffer, split, what):
        print "%-32s" % ("  " * self.indent + buffer[:split]), what
        self.indent += indent
        self.demangle(buffer[split:])

    def demangle(self, buf):
        if buf.startswith("St"):
            return self.out_cont(0, buf, 2, '"::std::"')
        limit = 0
        while buf[limit].isdigit():
            limit += 1
        if limit > 0:
            length = int(buf[:limit])
            start, limit = limit, limit + length
            return self.out_cont(0, buf, limit, '"%s"' % buf[start:limit])
        if buf[0] == "I":
            return self.out_cont(1, buf, 1, "<template-args>")
        if buf[0] == "R":
            return self.out_cont(1, buf, 1, "# reference to")
        if buf[0] == "N":
            return self.out_cont(1, buf, 1, "<nested-name>")
        raise DemangleError(buf)

demangle = Demangler

if __name__ == "__main__":
    demangle("_ZSt7forwardIRN1x14refobjiteratorINS0_3refINS0_4mime30mul"
             "tipart_section_processorObjIZ15get_body_parserIZZN14mime_"
             "processor21make_section_iteratorERKNS2_INS3_10sectionObjE"
             "NS0_10ptrrefBaseEEEbENKUlvE_clEvEUlSB_bE_ZZNS6_21make_sec"
             "tion_iteratorESB_bENKSC_clEvEUlSB_E0_ENS1_INS2_INS0_20out"
             "putrefiteratorObjIiEES8_EEEERKSsSB_OT_OT0_EUlmE_NS3_32mak"
             "e_multipart_default_discarderISP_EEEES8_EEEEEOT_RNSt16rem"
             "ove_referenceISW_E4typeE")
