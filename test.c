#include <stdio.h>
#include <demangle.h>

const char *symbol = "_ZSt7forwardIRN1x14refobjiteratorINS0_3refINS0_" \
  "4mime30multipart_section_processorObjIZ15get_body_parserIZZN14mime" \
  "_processor21make_section_iteratorERKNS2_INS3_10sectionObjENS0_10pt" \
  "rrefBaseEEEbENKUlvE_clEvEUlSB_bE_ZZNS6_21make_section_iteratorESB_" \
  "bENKSC_clEvEUlSB_E0_ENS1_INS2_INS0_20outputrefiteratorObjIiEES8_EE" \
  "EERKSsSB_OT_OT0_EUlmE_NS3_32make_multipart_default_discarderISP_EE" \
  "EES8_EEEEEOT_RNSt16remove_referenceISW_E4typeE";

int
main (int argc, char *argv[])
{
  const char *demangled =
    cplus_demangle (symbol, DMGL_AUTO | DMGL_ANSI | DMGL_PARAMS);

  if (demangled == NULL)
    puts ("Demangler failed");
  else
    puts (demangled);
}
