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
   cygwin_conv_path and cygwin_conv_path_list are created.

   The header also declares a substitute of sleep function
   if that function is not included in the system headers.  */

#ifndef WINDOWS_HDEP_H
#define WINDOWS_HDEP_H

/* The following macros are redefined below depending on the use of wide
   chars or not.  */
#undef STARTUPINFO
#undef CreateProcess
#undef GetModuleFileNameEx
#undef GetSystemDirectory
/* Macro _G(text) either expands to simply text or to L##text depending
   on use of wide chars or not.
   Although this is similar to _T macro inside winnt.h header,
   its value is set according to USE_WIDE_API macro, allowing to compile
   GDB with wide char use even on systems for which the default is to
   use ASCII chars.  */
#undef _G
/* Macro _G_SUFFIX(text) expands either to 'text "A"' or to 'text "W"'
   depending on the use of wide chars or not.  */
#undef _G_SUFFIX
/* Macro LPGSTR holds either LCSTR or LPWSTR.  */
#undef LPGSTR
/* Type win_buf_t is also set to char or wchar_t. */

/* The use of wide char for the above function is dependent of the definition
   of the macro USE_WIDE_WINAPI.

   For Cygwin compiler, the default is wide API for the newer cygwin
   version 1.7.X and above; and ascii API for older 1.5.X versions.  

   Otherwise mingw compiler is assumed, which defaults to use of ascii
   WINAPI for now (this may change later).

   Default can be overridden either by defining USE_WIDE_WINAPI
   to force use of wide API, or by defining USE_ASCII_WINAPI to prevent
   use of wide API.  */


/* CYGWIN part */
#ifdef __CYGWIN__
#ifdef PATH_MAX
# define __PMAX	PATH_MAX
#else
# include <sys/param.h>
# define __PMAX MAXPATHLEN
#endif
/* For new Cygwin 1.7.X versions, use WIDE version of Windows API functions
   by default, unless USE_ASCII_WINAPI is defined.
   For older Cygwin 1.5.X versionsd, or for MingwXX systems, default to use
   the Ansi versions.  */
#include <sys/cygwin.h>
#include <cygwin/version.h>
# if CYGWIN_VERSION_DLL_MAKE_COMBINED(CYGWIN_VERSION_API_MAJOR,CYGWIN_VERSION_API_MINOR) >= 181
/* Conditional set to use the newer cygwin_conv_path function.  */
#  define USE_NEW_CYGWIN_CONV
/* Set macro USE_WIDE_WINAPI by default, unless USE_ASCII_WINAPI is set
   to disable its use.  */
#   ifndef USE_ASCII_WINAPI
#     define USE_WIDE_WINAPI
#   endif /* not USE_ASCII_WINAPI  */
# else
/*  For older 1.5.X Cygwin versions, use cygwin_conv_to_XXX functions
    to emulate newer cygwin_conv_path function.  */
#  define USE_OLD_CYGWIN_CONV
#  define CW_SET_DOS_FILE_WARNING -1	/* no-op this for older Cygwin */
# endif
#else /* not __CYGWIN__ */
/* We assume that no __CYGWIN__ macro means that we are using a
   mingw compiler.  */
# define __PMAX	(MAX_PATH + 1)
# define USE_MINGW_CONV
# define NEED_SLEEP_SUBSTITUTE
#endif /* not __CYGWIN__ */


/* Using wide WINAPI.  */
#ifdef USE_WIDE_WINAPI
# define _G(text) L##text
# define _G_SUFFIX(text) (text "W")
# define LPGSTR LPWSTR
# define WINDOWS_POSIX_TO_NATIVE WINDOWS_POSIX_TO_NATIVE_W
# define WINDOWS_NATIVE_TO_POSIX WINDOWS_NATIVE_W_TO_POSIX
  typedef wchar_t win_buf_t;
# define STARTUPINFO STARTUPINFOW
# define CreateProcess CreateProcessW
# define GetSystemDirectory GetSystemDirectoryW
# define gdb_win_strlen wstrlen
# define gdb_win_strcat wcscat
/* Mingw has a special swprintf function without length argument.  */
# ifndef __CYGWIN__
#  define swprintf _snwprintf
#endif
#else /* not USE_WIDE_WINAPI */
/* Using ascii WINAPI.  */
# define _G(text) text
# define _G_SUFFIX(text) (text "A")
# define LPGSTR LPSTR
# define WINDOWS_POSIX_TO_NATIVE WINDOWS_POSIX_TO_NATIVE_A
# define WINDOWS_NATIVE_TO_POSIX WINDOWS_NATIVE_A_TO_POSIX
  typedef char win_buf_t;
# define STARTUPINFO STARTUPINFOA
# define CreateProcess CreateProcessA
# define GetSystemDirectory GetSystemDirectoryA
# define gdb_win_strlen strlen
# define gdb_win_strcat strcat
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

extern int gdb_win_conv_path (conv_type oper, const void *from,
			      void *to, int size);

/* Analogeous function for PATH style lists.  */

extern int gdb_win_conv_path_list (conv_type oper, const void *from,
				   void *to, int size);

#ifdef NEED_SLEEP_SUBSTITUTE
extern int sleep (int);
#endif /* NEED_SLEEP_SUBSTITUTE */

#endif /* WINDOWS_HDEP_H */

