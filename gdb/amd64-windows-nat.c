/* Copyright (C) 2008, 2009, 2010, 2011 Free Software Foundation, Inc.

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

#include "defs.h"
#include "gdbarch.h"
#include "observer.h"
#include "windows-nat.h"
#include "i386-nat.h"
#include <windows.h>

#define context_offset(x) (offsetof (CONTEXT, x))
static const int mappings[] =
{
  context_offset (Rax),
  context_offset (Rbx),
  context_offset (Rcx),
  context_offset (Rdx),
  context_offset (Rsi),
  context_offset (Rdi),
  context_offset (Rbp),
  context_offset (Rsp),
  context_offset (R8),
  context_offset (R9),
  context_offset (R10),
  context_offset (R11),
  context_offset (R12),
  context_offset (R13),
  context_offset (R14),
  context_offset (R15),
  context_offset (Rip),
  context_offset (EFlags),
  context_offset (SegCs),
  context_offset (SegSs),
  context_offset (SegDs),
  context_offset (SegEs),
  context_offset (SegFs),
  context_offset (SegGs),
  context_offset (FloatSave.FloatRegisters[0]),
  context_offset (FloatSave.FloatRegisters[1]),
  context_offset (FloatSave.FloatRegisters[2]),
  context_offset (FloatSave.FloatRegisters[3]),
  context_offset (FloatSave.FloatRegisters[4]),
  context_offset (FloatSave.FloatRegisters[5]),
  context_offset (FloatSave.FloatRegisters[6]),
  context_offset (FloatSave.FloatRegisters[7]),
  context_offset (FloatSave.ControlWord),
  context_offset (FloatSave.StatusWord),
  context_offset (FloatSave.TagWord),
  context_offset (FloatSave.ErrorSelector),
  context_offset (FloatSave.ErrorOffset),
  context_offset (FloatSave.DataSelector),
  context_offset (FloatSave.DataOffset),
  context_offset (FloatSave.ErrorSelector)
  /* XMM0-7 */ ,
  context_offset (Xmm0),
  context_offset (Xmm1),
  context_offset (Xmm2),
  context_offset (Xmm3),
  context_offset (Xmm4),
  context_offset (Xmm5),
  context_offset (Xmm6),
  context_offset (Xmm7),
  context_offset (Xmm8),
  context_offset (Xmm9),
  context_offset (Xmm10),
  context_offset (Xmm11),
  context_offset (Xmm12),
  context_offset (Xmm13),
  context_offset (Xmm14),
  context_offset (Xmm15),
  /* MXCSR */
  context_offset (FloatSave.MxCsr)
};
#undef context_offset

#ifndef CONTEXT_EXTENDED_REGSITERS
#define CONTEXT_EXTENDED_REGISTERS 0
#endif

#define CONTEXT_DEBUGGER_DR CONTEXT_FULL |CONTEXT_FLOATING_POINT \
	| CONTEXT_DEBUG_REGISTERS | CONTEXT_EXTENDED_REGISTERS

/* 32-bit version of CONTEXT structure.  */
#define SIZE_OF_80387_REGISTERS 80
#define CONTEXT32_i386 0x00010000
#define CONTEXT32_i486 0x00010000

#define CONTEXT32_CONTROL (CONTEXT32_i386 | 0x00000001L)
#define CONTEXT32_INTEGER (CONTEXT32_i386 | 0x00000002L)
#define CONTEXT32_SEGMENTS (CONTEXT32_i386 | 0x00000004L)
#define CONTEXT32_FLOATING_POINT (CONTEXT32_i386 | 0x00000008L)
#define CONTEXT32_DEBUG_REGISTERS (CONTEXT32_i386 | 0x00000010L)
#define CONTEXT32_EXTENDED_REGISTERS (CONTEXT32_i386 | 0x00000020L)

#define CONTEXT32_FULL (CONTEXT32_CONTROL | CONTEXT32_INTEGER \
			| CONTEXT32_SEGMENTS)

#define CONTEXT32_ALL (CONTEXT32_CONTROL | CONTEXT32_INTEGER \
		       | CONTEXT32_SEGMENTS | CONTEXT32_FLOATING_POINT \
		       | CONTEXT32_DEBUG_REGISTERS \
		       | CONTEXT32_EXTENDED_REGISTERS)
#define CONTEXT32_DEBUGGER_DR CONTEXT32_ALL


#define MAXIMUM_SUPPORTED_EXTENSION 512

    typedef struct _FLOATING_SAVE_AREA {
      DWORD ControlWord;
      DWORD StatusWord;
      DWORD TagWord;
      DWORD ErrorOffset;
      DWORD ErrorSelector;
      DWORD DataOffset;
      DWORD DataSelector;
      BYTE RegisterArea[SIZE_OF_80387_REGISTERS];
      DWORD Cr0NpxState;
    } FLOATING_SAVE_AREA;

    typedef FLOATING_SAVE_AREA *PFLOATING_SAVE_AREA;

    typedef struct _CONTEXT32 {
      DWORD ContextFlags;
      DWORD Dr0;
      DWORD Dr1;
      DWORD Dr2;
      DWORD Dr3;
      DWORD Dr6;
      DWORD Dr7;
      FLOATING_SAVE_AREA FloatSave;
      DWORD SegGs;
      DWORD SegFs;
      DWORD SegEs;
      DWORD SegDs;

      DWORD Edi;
      DWORD Esi;
      DWORD Ebx;
      DWORD Edx;
      DWORD Ecx;
      DWORD Eax;
      DWORD Ebp;
      DWORD Eip;
      DWORD SegCs;
      DWORD EFlags;
      DWORD Esp;
      DWORD SegSs;
      BYTE ExtendedRegisters[MAXIMUM_SUPPORTED_EXTENSION];
    } CONTEXT32;

    typedef CONTEXT32 *PCONTEXT32;


#ifndef _LDT_ENTRY_DEFINED
#define _LDT_ENTRY_DEFINED

    typedef struct _LDT_ENTRY {
      WORD LimitLow;
      WORD BaseLow;
      union {
	struct {
	  BYTE BaseMid;
	  BYTE Flags1;
	  BYTE Flags2;
	  BYTE BaseHi;
	} Bytes;
	struct {
	  DWORD BaseMid : 8;
	  DWORD Type : 5;
	  DWORD Dpl : 2;
	  DWORD Pres : 1;
	  DWORD LimitHi : 4;
	  DWORD Sys : 1;
	  DWORD Reserved_0 : 1;
	  DWORD Default_Big : 1;
	  DWORD Granularity : 1;
	  DWORD BaseHi : 8;
	} Bits;
      } HighWord;
    } LDT_ENTRY,*PLDT_ENTRY;
#endif /* _LDT_ENTRY_DEFINED */

#define context32_offset(x) ((int)(uintptr_t)&(((CONTEXT32 *)NULL)->x))
static const int mappings32[] =
{
  context32_offset (Eax),
  context32_offset (Ecx),
  context32_offset (Edx),
  context32_offset (Ebx),
  context32_offset (Esp),
  context32_offset (Ebp),
  context32_offset (Esi),
  context32_offset (Edi),
  context32_offset (Eip),
  context32_offset (EFlags),
  context32_offset (SegCs),
  context32_offset (SegSs),
  context32_offset (SegDs),
  context32_offset (SegEs),
  context32_offset (SegFs),
  context32_offset (SegGs),
  context32_offset (FloatSave.RegisterArea[0 * 10]),
  context32_offset (FloatSave.RegisterArea[1 * 10]),
  context32_offset (FloatSave.RegisterArea[2 * 10]),
  context32_offset (FloatSave.RegisterArea[3 * 10]),
  context32_offset (FloatSave.RegisterArea[4 * 10]),
  context32_offset (FloatSave.RegisterArea[5 * 10]),
  context32_offset (FloatSave.RegisterArea[6 * 10]),
  context32_offset (FloatSave.RegisterArea[7 * 10]),
  context32_offset (FloatSave.ControlWord),
  context32_offset (FloatSave.StatusWord),
  context32_offset (FloatSave.TagWord),
  context32_offset (FloatSave.ErrorSelector),
  context32_offset (FloatSave.ErrorOffset),
  context32_offset (FloatSave.DataSelector),
  context32_offset (FloatSave.DataOffset),
  context32_offset (FloatSave.ErrorSelector)
  /* XMM0-7 */ ,
  context32_offset (ExtendedRegisters[10*16]),
  context32_offset (ExtendedRegisters[11*16]),
  context32_offset (ExtendedRegisters[12*16]),
  context32_offset (ExtendedRegisters[13*16]),
  context32_offset (ExtendedRegisters[14*16]),
  context32_offset (ExtendedRegisters[15*16]),
  context32_offset (ExtendedRegisters[16*16]),
  context32_offset (ExtendedRegisters[17*16]),
  /* MXCSR */
  context32_offset (ExtendedRegisters[24])
};
#undef context32_offset

#define context32_offset(x) ((int)(uintptr_t)&(((CONTEXT *)NULL)->x))
static const int mappings32on64[] =
{
  context32_offset (Rax),
  context32_offset (Rcx),
  context32_offset (Rdx),
  context32_offset (Rbx),
  context32_offset (Rsp),
  context32_offset (Rbp),
  context32_offset (Rsi),
  context32_offset (Rdi),
  context32_offset (Rip),
  context32_offset (EFlags),
  context32_offset (SegCs),
  context32_offset (SegSs),
  context32_offset (SegDs),
  context32_offset (SegEs),
  context32_offset (SegFs),
  context32_offset (SegGs),
  context32_offset (FloatSave.FloatRegisters[0]),
  context32_offset (FloatSave.FloatRegisters[1]),
  context32_offset (FloatSave.FloatRegisters[2]),
  context32_offset (FloatSave.FloatRegisters[3]),
  context32_offset (FloatSave.FloatRegisters[4]),
  context32_offset (FloatSave.FloatRegisters[5]),
  context32_offset (FloatSave.FloatRegisters[6]),
  context32_offset (FloatSave.FloatRegisters[7]),
  context32_offset (FloatSave.ControlWord),
  context32_offset (FloatSave.StatusWord),
  context32_offset (FloatSave.TagWord),
  context32_offset (FloatSave.ErrorSelector),
  context32_offset (FloatSave.ErrorOffset),
  context32_offset (FloatSave.DataSelector),
  context32_offset (FloatSave.DataOffset),
  context32_offset (FloatSave.ErrorSelector)
  /* XMM0-7 */ ,
  context32_offset (Xmm0),
  context32_offset (Xmm1),
  context32_offset (Xmm2),
  context32_offset (Xmm3),
  context32_offset (Xmm4),
  context32_offset (Xmm5),
  context32_offset (Xmm6),
  context32_offset (Xmm7),
  context32_offset (Xmm8),
  context32_offset (Xmm9),
  context32_offset (Xmm10),
  context32_offset (Xmm11),
  context32_offset (Xmm12),
  context32_offset (Xmm13),
  context32_offset (Xmm14),
  context32_offset (Xmm15),
  /* MXCSR */
  context32_offset (FloatSave.MxCsr)
  };
#undef context32_offset

static DWORD WINAPI (*Win32SuspendThread) (HANDLE);
static BOOL WINAPI (*Win32GetThreadContext) (HANDLE, void *);
static BOOL WINAPI (*Win32SetThreadContext) (HANDLE, void *);
static BOOL WINAPI (*Win32GetThreadSelectorEntry) (HANDLE, DWORD, void *);
static DWORD WINAPI (*Win64SuspendThread) (HANDLE);
static BOOL WINAPI (*Win64GetThreadContext) (HANDLE, void *);
static BOOL WINAPI (*Win64SetThreadContext) (HANDLE, void *);
static BOOL WINAPI (*Win64GetThreadSelectorEntry) (HANDLE, DWORD, void *);

void
setup_i386_windows_nat (void)
{
  i386_set_debug_register_length (4);
  context_all_registers = CONTEXT32_DEBUGGER_DR;
  context_debug_registers_only = CONTEXT32_DEBUG_REGISTERS;
  if (Win32SuspendThread && Win32SetThreadContext && Win32GetThreadContext)
    {
      windows_set_context_register_offsets (mappings32);
      GdbSuspendThread = Win32SuspendThread;
      GdbGetThreadContext = Win32GetThreadContext;
      GdbSetThreadContext = Win32SetThreadContext;
      GdbGetThreadSelectorEntry = Win32GetThreadSelectorEntry;
    }
  else
    {
      windows_set_context_register_offsets (mappings32on64);
    }
}

void
setup_amd64_windows_nat (void)
{
  windows_set_context_register_offsets (mappings);
  i386_set_debug_register_length (8);
  context_all_registers = CONTEXT_DEBUGGER_DR;
  context_debug_registers_only = CONTEXT_DEBUG_REGISTERS;
  GdbSuspendThread = Win64SuspendThread;
  GdbGetThreadContext = Win64GetThreadContext;
  GdbSetThreadContext = Win64SetThreadContext;
  GdbGetThreadSelectorEntry = Win64GetThreadSelectorEntry;
}

static void
windows_notify_architecture_changed (struct gdbarch *gdbarch)
{
  const struct bfd_arch_info *bfd_arch_info;

  bfd_arch_info = gdbarch_bfd_arch_info (gdbarch);
  if (bfd_arch_info->bits_per_address == 64)
    {
      i386_dr_low.debug_register_length = 0;
      setup_amd64_windows_nat ();
    }
  else if (bfd_arch_info->bits_per_address == 32)
    {
      i386_dr_low.debug_register_length = 0;
      setup_i386_windows_nat ();
    }
  else
    error (_("Wrong architecture"));
}

void
_initialize_amd64_windows_nat (void)
{
  HMODULE h = LoadLibrary ("kernel32.dll");

  if (h != INVALID_HANDLE_VALUE)
    {
      Win64SuspendThread = (void *) GetProcAddress (h, "SuspendThread");
      Win64GetThreadContext = (void *) GetProcAddress (h, "GetThreadContext");
      Win64SetThreadContext = (void *) GetProcAddress (h, "SetThreadContext");
      Win64GetThreadSelectorEntry = (void *) GetProcAddress (h,
				      "GetThreadSelectorEntry");
      Win32SuspendThread = (void *) GetProcAddress (h, "Wow64SuspendThread");
      Win32GetThreadContext = (void *) GetProcAddress (h,
			        "Wow64GetThreadContext");
      Win32SetThreadContext = (void *) GetProcAddress (h,
			        "Wow64SetThreadContext");
      Win32GetThreadSelectorEntry = (void *) GetProcAddress (h,
				      "Wow64GetThreadSelectorEntry");
      Win32SuspendThread = (void *) GetProcAddress (h, "Wow64SuspendThread");
      CloseHandle (h);
    }
  setup_amd64_windows_nat ();
  observer_attach_architecture_changed (windows_notify_architecture_changed);
}

