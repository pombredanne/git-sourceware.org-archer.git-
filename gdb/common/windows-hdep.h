/* Copyright 2011 Free Software Foundation, Inc.

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

/* The primary purpose of this file is to separate out
   conversion between POSIX style and Windows NATIVE style
   path and path lists.
   For this purpose, generic replacement of the two cygwin function
   cygwin_conv_path and cygwin_conv_path_list are created.  */

#ifndef WINDOWS_HDEP_H
#define WINDOWS_HDEP_H

/* The following macros are redefined below depend on the use of wide
   chars or not.  */
#undef STARTUPINFO
#undef CreateProcess
#undef GetModuleFileNameEx
#undef GetSystemDirectory

#ifdef __CYGWIN__
#ifdef PATH_MAX
# define __PMAX	PATH_MAX
#else
# include <sys/param.h>
# define __PMAX MAXPATHLEN
#endif
/*  For new Cygwin 1.7.X versions, use WIDE version of Windows API functions
    by default, unless USE_ASCII_WINAPI is defined.
    For older Cygwin 1.5.X versionsd, or for MingwXX systems, default to use
    the Ansi versions.  */
#include <sys/cygwin.h>
#include <cygwin/version.h>
# if CYGWIN_VERSION_DLL_MAKE_COMBINED(CYGWIN_VERSION_API_MAJOR,CYGWIN_VERSION_API_MINOR) >= 181
#  define USE_NEW_CYGWIN_CONV
#   ifndef USE_ASCII_WINAPI
#     define USE_WIDE_WINAPI
#   endif /* not USE_ASCII_WINAPI  */
# else
/*  For older 1.5.X Cygwin versions, use cygwin_onv_to_XXX functions
    to emulate newer cygwin_conv_path function.  */
#  define USE_OLD_CYGWIN_CONV
#  define CW_SET_DOS_FILE_WARNING -1	/* no-op this for older Cygwin */
# endif
#else /* not __CYGWIN__ */
# define __PMAX	(MAX_PATH + 1)
# define USE_MINGW_CONV
# define NEED_SLEEP_SUBSTITUTE
#endif

#undef _GDB_T
#ifdef USE_WIDE_WINAPI
# define _GDB_T(text) L##text
# define WINDOWS_POSIX_TO_NATIVE WINDOWS_POSIX_TO_NATIVE_W
# define WINDOWS_NATIVE_TO_POSIX WINDOWS_NATIVE_W_TO_POSIX
  typedef wchar_t windows_buf_t;
# define STARTUPINFO STARTUPINFOW
# define CreateProcess CreateProcessW
# define GetSystemDirectory GetSystemDirectoryW
# define GetModuleFileNameEx_name "GetModuleFileNameExW"
# define bad_GetModuleFileNameEx bad_GetModuleFileNameExW
# define gdb_windows_strlen wstrlen
# define gdb_windows_strcat wcscat
/* Mingw has a special swprintf function without length argument.  */
# ifndef __CYGWIN__
#  define swprintf _snwprintf
#endif
#else /* not USE_WIDE_WINAPI */
# define _GDB_T(text) text
# define WINDOWS_POSIX_TO_NATIVE WINDOWS_POSIX_TO_NATIVE_A
# define WINDOWS_NATIVE_TO_POSIX WINDOWS_NATIVE_A_TO_POSIX
  typedef char windows_buf_t;
# define STARTUPINFO STARTUPINFOA
# define CreateProcess CreateProcessA
# define GetSystemDirectory GetSystemDirectoryA
# define GetModuleFileNameEx_name "GetModuleFileNameExA"
# define bad_GetModuleFileNameEx bad_GetModuleFileNameExA
# define gdb_windows_strlen strlen
# define gdb_windows_strcat strcat
#endif /* not USE_WIDE_WINAPI */


typedef enum
{
  WINDOWS_NATIVE_A_TO_POSIX = 1
  ,WINDOWS_POSIX_TO_NATIVE_A = 2
#ifdef USE_WIDE_WINAPI
  ,WINDOWS_NATIVE_W_TO_POSIX = 3
  ,WINDOWS_POSIX_TO_NATIVE_W = 4
#endif
  ,WINDOWS_NATIVE_TO_MSYS = 5
} conv_type;

/* GDB mapping to cygwin type cygwin_conv_path.
   If SIZE is zero, return required size (in number of characters)
   to store the converted string FROM.
   if SIZE is non-zero, convert FROM according to OPER value,
   respecting SIZE limit.
   Returns size of converted string in characters or
   -1 if there was an error.  */

extern int gdb_windows_conv_path (conv_type oper, const void *from,
				  void * to, int size);

/* Analogeous function for PATH style lists.  */

extern int gdb_windows_conv_path_list (conv_type oper, const void *from,
				       void * to, int size);

#ifdef NEED_SLEEP_SUBSTITUTE
extern int sleep (int);
#endif /* NEED_SLEEP_SUBSTITUTE */

#endif /* WINDOWS_HDEP_H */

