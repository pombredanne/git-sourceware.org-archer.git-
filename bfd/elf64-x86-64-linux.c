/* X86-64 specific support for ELF on GNU/Linux
   Copyright 2013 Free Software Foundation, Inc.

   This file is part of BFD, the Binary File Descriptor library.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston,
   MA 02110-1301, USA.  */

#define TARGET_LITTLE_SYM		    bfd_elf64_x86_64_linux_vec
#define TARGET_LITTLE_NAME		    "elf64-x86-64-linux"

#define elf_backend_write_core_note	    elf_x86_64_write_core_note

#include "elf64-x86-64.c"
#include "elf-linux-psinfo.h"

static char *
elf_x86_64_write_core_note (bfd *abfd, char *buf, int *bufsiz,
			    int note_type, ...)
{
  const struct elf_backend_data *bed = get_elf_backend_data (abfd);
  va_list ap;
  const struct elf_internal_prpsinfo *prpsinfo;
  long pid;
  int cursig;
  const void *gregs;

  switch (note_type)
    {
    default:
      return NULL;

    case NT_PRPSINFO:
      va_start (ap, note_type);
      prpsinfo = va_arg (ap, const struct elf_internal_prpsinfo *);
      va_end (ap);

      if (bed->s->elfclass == ELFCLASS32)
	{
	  struct elf_external_prpsinfo32 data32;

	  memset (&data32, 0, sizeof (data32));
	  PRPSINFO32_SWAP_FIELDS (abfd, prpsinfo, data32);

	  return elfcore_write_note (abfd, buf, bufsiz, "CORE", note_type,
				     &data32, sizeof (data32));
	}
      else
	{
	  struct elf_external_prpsinfo64 data64;

	  memset (&data64, 0, sizeof (data64));
	  PRPSINFO64_SWAP_FIELDS (abfd, prpsinfo, data64);

	  return elfcore_write_note (abfd, buf, bufsiz, "CORE", note_type,
				     &data64, sizeof (data64));
	}
      /* NOTREACHED */

    case NT_PRSTATUS:
#ifdef CORE_HEADER
      va_start (ap, note_type);
      pid = va_arg (ap, long);
      cursig = va_arg (ap, int);
      gregs = va_arg (ap, const void *);
      va_end (ap);

      if (bed->s->elfclass == ELFCLASS32)
	{
	  if (bed->elf_machine_code == EM_X86_64)
	    {
	      prstatusx32_t prstat;
	      memset (&prstat, 0, sizeof (prstat));
	      prstat.pr_pid = pid;
	      prstat.pr_cursig = cursig;
	      memcpy (&prstat.pr_reg, gregs, sizeof (prstat.pr_reg));
	      return elfcore_write_note (abfd, buf, bufsiz, "CORE", note_type,
					 &prstat, sizeof (prstat));
	    }
	  else
	    {
	      prstatus32_t prstat;
	      memset (&prstat, 0, sizeof (prstat));
	      prstat.pr_pid = pid;
	      prstat.pr_cursig = cursig;
	      memcpy (&prstat.pr_reg, gregs, sizeof (prstat.pr_reg));
	      return elfcore_write_note (abfd, buf, bufsiz, "CORE", note_type,
					 &prstat, sizeof (prstat));
	    }
	}
      else
	{
	  prstatus64_t prstat;
	  memset (&prstat, 0, sizeof (prstat));
	  prstat.pr_pid = pid;
	  prstat.pr_cursig = cursig;
	  memcpy (&prstat.pr_reg, gregs, sizeof (prstat.pr_reg));
	  return elfcore_write_note (abfd, buf, bufsiz, "CORE", note_type,
				     &prstat, sizeof (prstat));
	}
#else
      return NULL;
#endif /* CORE_HEADER */
    }
  /* NOTREACHED */
}
