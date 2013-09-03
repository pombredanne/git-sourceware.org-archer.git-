class DemangleError(ValueError):
    def __init__(self, buf, maxlen = 56):
        if len(buf) > maxlen:
            buf = buf[:maxlen - 3] + "..."
        ValueError.__init__(self, buf)

class Demangler:
    def __init__(self, symbol, prefix="_Z"):
        assert symbol.startswith(prefix)
        self.output_continue(symbol, 2, "(start of mangled symbol)")

    def output_continue(self, buffer, split, what):
        print buffer[:split], what
        self.demangle(buffer[split:])

    def demangle(self, buf):
        if buf.startswith("St"):
            return self.output_continue(buf, 2, "::std::")
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
