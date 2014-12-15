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


std::map<string,int> dyninst_x86_gdb_regnum;

extern "C"
{
/* Defined in auto-generated file i386.c.  */
extern void init_registers_amd64 (void);
extern void init_registers_i386 (void);
extern const struct target_desc *tdesc_i386_linux;
extern const struct target_desc *tdesc_amd64_linux;
extern const struct target_desc *tdesc_i386;
extern const struct target_desc *tdesc_amd64;
}

/* Print a debug trace on standard output if debug_threads (--debug) is set.  */

static void
dyninst_debug (const char *fmt, ...)
{
  va_list args;

  if (!debug_threads)
    return;

  va_start (args, fmt);
  fprintf (stderr, "DEBUG(dyninst): ");
  vfprintf (stderr, fmt, args);
  fprintf (stderr, "\n");
  va_end (args);
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


/* The fill_function for the general-purpose register set.  */

static void
dyninst_x86_fill_gregset (struct regcache *regcache, RegisterPool regpool)
{
  RegisterPool::iterator regidx = regpool.begin();
  for (; regidx != regpool.end(); regidx++)
    {
      string reg = (*regidx).first.name();
      string canon_reg = canonicalize_reg(reg);
      if (dyninst_x86_gdb_regnum.find(canon_reg) == dyninst_x86_gdb_regnum.end())
	continue;
      int regn = dyninst_x86_gdb_regnum[canon_reg];
      MachRegisterVal regval = (MachRegisterVal)((*regidx).second);
      const char *regstr;

      regcache->register_status[regn] = REG_VALID;
      switch (register_size (regcache->tdesc, regn))
      {
	case 2:
	  {
	    u_int16_t *val = (u_int16_t*)register_data (regcache, regn, 0);
	    regstr = reg.c_str();
	    dyninst_debug("dyninst_x86_fill_gregset %s/%d=%#lx", regstr, regn, regval);
	    *val = regval;
	    break;
	  }
	case 4:
	  {
	    u_int32_t *val = (u_int32_t*)register_data (regcache, regn, 0);
	    regstr = reg.c_str();
	    dyninst_debug("dyninst_x86_fill_gregset %s/%d=%#lx", regstr, regn, regval);
	    *val = regval;
	    break;
	  }
	case 8:
	  {
	    u_int64_t *val = (u_int64_t*)register_data (regcache, regn, 0);
	    regstr = reg.c_str();
	    dyninst_debug("dyninst_x86_fill_gregset %s/%d=%#lx", regstr, regn, regval);
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
  RegisterPool::iterator regidx = regpool.begin();
  for (; regidx != regpool.end(); regidx++)
    {
      string reg = (*regidx).first.name();
      string canon_reg = canonicalize_reg(reg);
      if (dyninst_x86_gdb_regnum.find(canon_reg) == dyninst_x86_gdb_regnum.end())
	continue;
      int regn = dyninst_x86_gdb_regnum[canon_reg];
      MachRegisterVal regval = (MachRegisterVal)((*regidx).second);
      const char *regstr = reg.c_str();

      regcache->register_status[regn] = REG_VALID;
      switch (register_size (regcache->tdesc, regn))
      {
	case 2:
	  {
	    u_int16_t *val = (u_int16_t*)register_data (regcache, regn, 0);
	    regpool[(*regidx).first] = (MachRegisterVal)val;
	    dyninst_debug("dyninst_x86_store_gregset %s/%d was %#lx now %#lx", regstr, regn, regval, *val);
	    break;
	  }
	case 4:
	  {
	    u_int32_t *val = (u_int32_t*)register_data (regcache, regn, 0);
	    regpool[(*regidx).first] = (MachRegisterVal)val;
	    dyninst_debug("dyninst_x86_store_gregset %s/%d was %#lx now %#lx", regstr, regn, regval, *val);
	    break;
	  }
	case 8:
	  {
	    u_int64_t *val = (u_int64_t*)register_data (regcache, regn, 0);
	    regpool[(*regidx).first] = (MachRegisterVal)val;
	    dyninst_debug("dyninst_x86_store_gregset %s/%d was %#lx now %#lx", regstr, regn, regval, *val);
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
  dyninst_debug("dyninst_x86_fill_fpregset: dyninst does not support floating registers.\n");
//  result = th->getRegister(MachRegister(x86_64::xmm0), rspval);
}


static void
dyninst_x86_store_fpregset (struct regcache *regcache, RegisterPool regpool)
{
  dyninst_debug("dyninst_x86_store_fpregset: dyninst does not support floating registers.\n", regcache);
}


static CORE_ADDR
x86_get_pc (struct regcache *regcache)
{
  int use_64bit = register_size (regcache->tdesc, 0) == 8;

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


bool
x86_supports_range_stepping ()
{
  return true;
}


/* Implements the dyninst_target_ops.arch_setup routine.  */

static void
dyninst_x86_arch_setup (void)
{
  dyninst_debug("dyninst_x86_arch_setup\n");
  // TODO get at runtime
#ifdef __x86_64__
  init_registers_amd64 ();
  dyninst_tdesc = tdesc_amd64;
#else
  init_registers_i386 ();
  dyninst_tdesc = tdesc_i386;
#endif
  //  init_registers_i386 ();
  //  dyninst_tdesc = tdesc_i386;
  for (int r = 0; r < dyninst_tdesc->num_registers; r++)
    {
      dyninst_x86_gdb_regnum[dyninst_tdesc->reg_defs[r].name] = r;
    }
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

struct dyninst_target_ops the_low_target = {
  dyninst_x86_arch_setup,
  x86_get_pc,
  x86_set_pc,
  x86_supports_range_stepping,
};
