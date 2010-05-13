#include "defs.h"

int
c_parse (void)
{
  return 0;
}

void
c_error (char *msg)
{
  char *lexptr = "unknown";
  error ("A %s in expression, near %s'.", (msg ? msg : "error"), lexptr);
}
