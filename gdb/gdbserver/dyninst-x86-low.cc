/* Copyright (C) 2010-2014 Free Software Foundation, Inc.

   This file is part of GDB.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

extern "C"
{
#include "server.h"
#include "regdef.h"
#include "tdesc.h"
#include <stdarg.h>
#include <stdint.h>
#include <limits.h>
#include <sys/ptrace.h>
#include <sys/types.h>
}

#include <dyninst/PCProcess.h>
#include <dyninst/Event.h>
#include <map>
#include <string>
#include <vector>
#include <sstream>      // std::ostringstream
#include "dyninst-low.h"

static std::ostringstream dboss;

std::vector<int> dyninst_x86_gdb_regnum;

extern "C"
{
/* Defined in auto-generated file i386.c.  */
extern void init_registers_amd64_linux (void);
extern void init_registers_i386_linux (void);
extern const struct target_desc *tdesc_i386_linux;
extern const struct target_desc *tdesc_amd64_linux;
extern const struct target_desc *tdesc_i386;
extern const struct target_desc *tdesc_amd64;
}

static int use_xml;


/* Print a debug trace on standard output if debug_threads (--debug) is set.  */

#define DEBUG(args...) 			\
  if (debug_threads) {				\
      fprintf (stderr, "%s %s %s\n", "DEBUG(dyninst): ", __FUNCTION__, args); \
  }

static unsigned char *
register_data (struct regcache *regcache, int n, int fetch)
{
  return regcache->registers + regcache->tdesc->reg_defs[n].offset / 8;
}


string
canonicalize_reg (string &reg)
{
  int skip_arch = reg.find(":") + 2;
  string canon_reg = reg.substr(skip_arch);
//  if (canon_reg.substr(0,1) == "e")
//    canon_reg.replace(0,1,"r");
  return canon_reg;
}


void
dyninst_x86_reg_map_setup (RegisterPool regpool)
{
  for (int r = 0; r < dyninst_tdesc->num_registers; r++)
    {
      RegisterPool::iterator regidx = regpool.begin();
      for (; regidx != regpool.end(); regidx++)
	{
	  string reg = (*regidx).first.name();
	  string canon_reg = canonicalize_reg (reg);
	  if (strcmp (canon_reg.c_str(), dyninst_tdesc->reg_defs[r].name) != 0)
	    continue;
	  dyninst_x86_gdb_regnum.push_back ((*regidx).first.val());
	  break;
	}
    }
}


static void dump_registers (const char* whoami, RegisterPool regpool) __attribute__ ((unused));

static void dump_registers (const char*, RegisterPool) __attribute__ ((unused));

static void
dump_registers (const char* whoami, RegisterPool regpool)
{
  RegisterPool::iterator regidx;
  fprintf (stderr, "%s ", whoami);
  for (regidx = regpool.begin(); regidx != regpool.end(); regidx++)
    {
      MachRegister reg = (*regidx).first;
      MachRegisterVal regval = (*regidx).second;
      switch (reg.val())
      {
	case x86::ieax:
	case x86_64::irax:
	  fprintf (stderr, "rax=%#lx ", regval);break;
	case x86::iebx:
	case x86_64::irbx:
	  fprintf (stderr, "rbx=%#lx ", regval);break;
	case x86::iecx:
	case x86_64::ircx:
	  fprintf (stderr, "rcx=%#lx ", regval);break;
	case x86::iedx:
	case x86_64::irdx:
	  fprintf (stderr, "rdx=%#lx ", regval);break;
	case x86::iebp:
	case x86_64::irbp:
	  fprintf (stderr, "rbp=%#lx ", regval);break;
	case x86::iesp:
	case x86_64::irsp:
	  fprintf (stderr, "rsp=%#lx ", regval);break;
	case x86::iesi:
	case x86_64::irsi:
	  fprintf (stderr, "rsi=%#lx ", regval);break;
	case x86::iedi:
	case x86_64::irdi:
	  fprintf (stderr, "rdi=%#lx ", regval);break;
	case x86::ieip:
	case x86_64::irip:
	  fprintf (stderr, "rip=%#lx ", regval);break;
      }
    }
  fprintf (stderr, "\n");
}


/* The fill_function for the general-purpose register set.  */

static void
dyninst_x86_fill_gregset (struct regcache *regcache, RegisterPool regpool)
{
  for (int r = 0; r < dyninst_tdesc->num_registers; r++)
    regcache->register_status[r] = REG_VALID;

  if (debug_threads)
      dump_registers (__FUNCTION__, regpool);
  RegisterPool::iterator regidx = regpool.begin();
  for (; regidx != regpool.end(); regidx++)
    {
      int regn = 0;
      for (; regn < dyninst_tdesc->num_registers; regn++)
	if ((unsigned)regn < dyninst_x86_gdb_regnum.size() && (*regidx).first.val() == dyninst_x86_gdb_regnum[regn])
	  break;
      if (regn >= dyninst_tdesc->num_registers)
	continue;
      MachRegisterVal regval = (MachRegisterVal)((*regidx).second);

      regcache->register_status[regn] = REG_VALID;
      switch (register_size (regcache->tdesc, regn))
      {
	case 2:
	  {
	    u_int16_t *val = (u_int16_t*)register_data (regcache, regn, 0);
	    *val = regval;
	    break;
	  }
	case 4:
	  {
	    u_int32_t *val = (u_int32_t*)register_data (regcache, regn, 0);
	    *val = regval;
	    break;
	  }
	case 8:
	  {
	    u_int64_t *val = (u_int64_t*)register_data (regcache, regn, 0);
	    *val = regval;
	    break;
	  }
	case 10:
	  {
	    u_int64_t *val = (u_int64_t*)register_data (regcache, regn, 0);
	    *val = regval;
	    break;
	  }
	case 16:
	  {
	    u_int64_t *val = (u_int64_t*)register_data (regcache, regn, 0);
	    *val = regval;
	    break;
	  }
      }
    }
}


/* The store_function for the general-purpose register set.  */

static void
dyninst_x86_store_gregset (struct regcache *regcache, RegisterPool regpool)
{
  for (int r = 0; r < dyninst_tdesc->num_registers; r++)
    regcache->register_status[r] = REG_VALID;

  RegisterPool::iterator regidx = regpool.begin();
  for (; regidx != regpool.end(); regidx++)
    {
      int regn = 0;
      for (; regn < dyninst_tdesc->num_registers; regn++)
	if ((unsigned)regn < dyninst_x86_gdb_regnum.size() && (*regidx).first.val() == dyninst_x86_gdb_regnum[regn])
	  break;
      if (regn >= dyninst_tdesc->num_registers)
	continue;

      switch (register_size (regcache->tdesc, regn))
      {
	case 2:
	  {
	    u_int16_t *val = (u_int16_t*)register_data (regcache, regn, 0);
	    regpool[(*regidx).first] = (MachRegisterVal)*val;
	    break;
	  }
	case 4:
	  {
	    u_int32_t *val = (u_int32_t*)register_data (regcache, regn, 0);
	    regpool[(*regidx).first] = (MachRegisterVal)*val;
	    break;
	  }
	case 8:
	  {
	    u_int64_t *val = (u_int64_t*)register_data (regcache, regn, 0);
	    regpool[(*regidx).first] = (MachRegisterVal)*val;
	    break;
	  }
      }
    }
}


static void
dyninst_x86_fill_fpregset (struct regcache *regcache, RegisterPool regpool)
{
  // supported by dyninst
  //  rax rbp rbx rcx rdi rdx rip rsi rsp
  //  r8 - r15
  //  ds es fs gs cs ss flags orax fsbase gsbase

  // supported by dyninst but not in register pool (RegisterConversion-x86.C)
  //  xmm0 - xmm15
  //  TODO support xmm registers

  // not supported by dyninst
  //  fctrl fioff fiseg fooff fop foseg fstat ftag mxcsr
  //  st0 - st7
  DEBUG("floating point registers are not currently supported.\n");
}


static void
dyninst_x86_store_fpregset (struct regcache *regcache, RegisterPool regpool)
{
}


static CORE_ADDR
x86_get_pc (Thread::const_ptr thr)
{
  MachRegisterVal regval = 0;
  Architecture arch = thr->getProcess()->getArchitecture();

  if (arch == Arch_x86_64)
    {
      thr->getRegister(x86_64::rip, regval);
    }
  else if (arch == Arch_x86)
    {
      thr->getRegister(x86::eip, regval);
    }

  return regval;
}

static CORE_ADDR
x86_read_pc (struct regcache *regcache)
{
  int use_64bit;

  if (regcache == NULL)
    return (CORE_ADDR) 0;
  use_64bit = register_size (regcache->tdesc, 0) == 8;
  if (use_64bit)
    {
      unsigned long pc;
      collect_register_by_name (regcache, "rip", &pc);
      return (CORE_ADDR) pc;
    }
  else
    {
      unsigned int pc;
      collect_register_by_name (regcache, "eip", &pc);
      return (CORE_ADDR) pc;
    }
}

static void
x86_set_pc (struct regcache *regcache, CORE_ADDR pc)
{
  int use_64bit = register_size (regcache->tdesc, 0) == 8;

  if (use_64bit)
    {
      unsigned long newpc = pc;
      supply_register_by_name (regcache, "rip", &newpc);
    }
  else
    {
      unsigned int newpc = pc;
      supply_register_by_name (regcache, "eip", &newpc);
    }
}


static void
dyninst_linux_process_qsupported (const char *query)
{
  /* Return if gdb doesn't support XML.  If gdb sends "xmlRegisters="
     with "i386" in qSupported query, it supports dyninst XML target
     descriptions.  */
  use_xml = 0;
  if (query != NULL && strncmp (query, "xmlRegisters=", 13) == 0)
    {
      char *copy = xstrdup (query + 13);
      char *p;

      for (p = strtok (copy, ","); p != NULL; p = strtok (NULL, ","))
	{
	  if (strcmp (p, "i386") == 0)
	    {
	      use_xml = 1;
	      break;
	    }
	}

      free (copy);
    }

//  dyninst_linux_update_xmltarget ();
}


bool
x86_supports_range_stepping ()
{
  return true;
}


/* Implements the dyninst_target_ops.arch_setup routine.  */

static void
dyninst_x86_arch_setup (void)
{
  DEBUG("dyninst_x86_arch_setup\n");
  // TODO get at runtime, check xcr0_features

#ifdef __x86_64__
  init_registers_amd64_linux ();
  dyninst_tdesc = tdesc_amd64_linux;
#else
  init_registers_i386_linux ();
  dyninst_tdesc = tdesc_x32_linux;
#endif
}

/* Description of all the x86-dyninst register sets.  */

struct dyninst_regset_info dyninst_target_regsets[] = {
  /* General Purpose Registers.  */
  { dyninst_x86_fill_gregset, dyninst_x86_store_gregset},
  /* Floating Point Registers.  */
  { dyninst_x86_fill_fpregset, dyninst_x86_store_fpregset },
  /* End of list marker.  */
  { NULL, NULL }
};

/* The dyninst_target_ops vector for x86-dyninst.  */

static const unsigned char x86_breakpoint[] = { 0xCC };
#define x86_breakpoint_len 1

struct dyninst_target_ops the_low_target = {
  x86_breakpoint,
  x86_breakpoint_len,
  dyninst_x86_arch_setup,
  dyninst_linux_process_qsupported,
  x86_get_pc ,
  x86_read_pc,
  x86_set_pc,
  x86_supports_range_stepping,
  dyninst_x86_reg_map_setup,
  true, /* decr_pc_after_break */
};
