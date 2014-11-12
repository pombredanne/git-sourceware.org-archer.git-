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
}
#include <dyninst/PCProcess.h>
#include <dyninst/Event.h>
#include <map>
#include <string>
#include <vector>

using namespace std;
using namespace __gnu_cxx;
using namespace Dyninst;
using namespace ProcControlAPI;

struct regcache;
struct target_desc;

/*  Some information relative to a given register set.   */

struct dyninst_regset_info
{
  /* Fill the buffer BUF from the contents of the given REGCACHE.  */
  void (*fill_function) (struct regcache *regcache, RegisterPool);
  /* Store the register value in BUF in the given REGCACHE.  */
  void (*store_function) (struct regcache *regcache, RegisterPool);
};

/* A list of regsets for the target being debugged, terminated by an entry
   where the size is negative.

   This list should be created by the target-specific code.  */

extern struct dyninst_regset_info dyninst_target_regsets[];


struct dyninst_target_ops
{
  /* Architecture-specific setup.  */
  void (*arch_setup) (void);
  CORE_ADDR (*get_pc) (struct regcache *regcache);
  void (*set_pc) (struct regcache *regcache, CORE_ADDR newpc);
  bool (*supports_range_stepping) ();
};

extern struct dyninst_target_ops the_low_target;


extern "C"
{
  extern const struct target_desc *dyninst_tdesc;
}
