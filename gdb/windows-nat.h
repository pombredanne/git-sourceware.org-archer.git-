/* Copyright 2008, 2009, 2010, 2011 Free Software Foundation, Inc.

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

#ifndef WINDOWS_NAT_H
#define WINDOWS_NAT_H
#include <windows.h>
extern void windows_set_context_register_offsets (const int *offsets);

extern DWORD context_all_registers;
extern DWORD context_debug_registers_only;

extern DWORD WINAPI (*GdbSuspendThread) (HANDLE);
extern BOOL WINAPI (*GdbGetThreadContext) (HANDLE, void *);
extern BOOL WINAPI (*GdbSetThreadContext) (HANDLE, void *);
extern BOOL WINAPI (*GdbGetThreadSelectorEntry) (HANDLE, DWORD, void *);

#endif

