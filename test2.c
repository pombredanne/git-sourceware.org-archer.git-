#include <stdio.h>
#include <demangle.h>

/* http://mentorembedded.github.io/cxx-abi/abi.html#mangling-compression */

const char *symbol = "_ZN1N1TIiiE2mfES0_IddE";

int
main (int argc, char *argv[])
{
  printf ("%s\n\n", symbol);
  puts (cplus_demangle (symbol, DMGL_AUTO | DMGL_ANSI | DMGL_PARAMS));
}
