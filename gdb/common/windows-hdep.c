

#include <winsock2.h>
#include <windows.h>
#include <sys/types.h>
#include <string.h>

#ifndef GDBSERVER
# include "defs.h"
# include "gdb_assert.h"
#else
# include "server.h"
#endif
#include "windows-hdep.h"

int
gdb_windows_conv_path (conv_type oper, const void *from,
		       void * to, int size)

{
#ifdef USE_MINGW_CONV
  if (size == 0)
    {
#ifdef USE_WIDE_WINAPI
      if (oper == WINDOWS_POSIX_TO_NATIVE_W)
	return MultiByteToWideChar (CP_ACP, 0, from, -1, to, 0);
      if (oper == WINDOWS_NATIVE_W_TO_POSIX)
	return WideCharToMultiByte (CP_ACP, 0, from, -1, to, 0, 0, 0);
      else
#endif /* USE_WIDE_WINAPI */
        return (strlen((const char *) from) + 1);
    }
  else
    {
      int len = strlen((const char *) from) + 1;
#ifdef USE_WIDE_WINAPI
      if (oper == WINDOWS_POSIX_TO_NATIVE_W)
	return MultiByteToWideChar (CP_ACP, 0, from, len, to, size);
      if (oper == WINDOWS_NATIVE_W_TO_POSIX)
	return WideCharToMultiByte (CP_ACP, 0, from, len, to, size, 0, 0);
      else
#endif /* USE_WIDE_WINAPI */
        {
	  if (size < len)
	    len = size;
	  strncpy ((char *) to, (const char *) from, len);
	  if (oper == WINDOWS_NATIVE_TO_MSYS)
	    {
	      char * p;
	      p = strchr (to, '\\');
	      while (p)
		{
		  *p = '/';
		  p = strchr (to, '\\');
		}
	      if (((char *) to)[1] == ':' && ((char *) to)[2] == '/')
		{
		  char drive = ((char *) to)[0];
		  ((char *) to)[0] = '/';
		  ((char *) to)[1] = drive;
		}

	    }
          /* For null termination.  */
	  if (len == size)
	    ((char *) to)[size] = '\0';
	}
      return len;
    }
#endif /* USE_MING_CONV */
#ifdef USE_NEW_CYGWIN_CONV
  int op;

  switch (oper)
    {
    case WINDOWS_NATIVE_W_TO_POSIX: 
      op = CCP_WIN_W_TO_POSIX;
      break;

    case WINDOWS_NATIVE_A_TO_POSIX:
    case WINDOWS_NATIVE_TO_MSYS:
      op = CCP_WIN_A_TO_POSIX;
      break;

    case WINDOWS_POSIX_TO_NATIVE_W:
      op = CCP_POSIX_TO_WIN_W;
      break;

    case WINDOWS_POSIX_TO_NATIVE_A:
      op = CCP_POSIX_TO_WIN_A;
      break;
    default:
      {
	int len = strlen((const char *) from) + 1;

	strncpy ((char *) to, (const char *) from, size);
	return len;
      }
    }

  return (cygwin_conv_path (op, from, to, size));
#endif /* USE_NEW_CYGWIN_CONV */
#ifdef USE_OLD_CYGWIN_CONV
  switch (oper)
  /* Those two macros:
     WINDOWS_NATIVE_W_TO_POSIX and WINDOWS_POSIX_TO_NATIVE_W
     are not defined for old Cygwin, and thus not handled here .  */
    {
    case WINDOWS_NATIVE_A_TO_POSIX:
    case WINDOWS_NATIVE_TO_MSYS:
      if (size == 0)
	return __PMAX;
      gdb_assert (size >= __PMAX);
      return cygwin_conv_to_full_posix_path (from, to);
      break;

    case WINDOWS_POSIX_TO_NATIVE_A:
      if (size == 0)
	return __PMAX;
      gdb_assert (size >= __PMAX);
      return cygwin_conv_to_win32_path (from, to);
      break;
    };
  return -1;
#endif /* USE_OLD_CYGWIN_CONV */
}

/* Analogeous function for PATH style lists.  */

int
gdb_windows_conv_path_list (conv_type oper, const void *from,
			    void * to, int size)

{
#ifdef USE_MINGW_CONV
#ifdef USE_WIDE_WINAPI
  switch (oper)
    {
    case WINDOWS_NATIVE_W_TO_POSIX:
    case WINDOWS_POSIX_TO_NATIVE_W:
      return -1;
    default:
      ;
    }
#endif /* USE_WIDE_WINAPI */
  if (size == 0)
    return (strlen((const char *) from) + 1);
  else
    {
      int len = strlen((const char *) from) + 1;
      if (len > size)
	len = size;
      strncpy ((char *) to, (const char *) from, len);
      /* For null termination.  */
      if (len == size)
	((char *) to)[size] = '\0';
      return len;
    }
#endif /* USE_MING_CONV */
#ifdef USE_NEW_CYGWIN_CONV
  int op;

  switch (oper)
    {
    case WINDOWS_NATIVE_W_TO_POSIX: 
      op = CCP_WIN_W_TO_POSIX;
      break;

    case WINDOWS_NATIVE_A_TO_POSIX:
    case WINDOWS_NATIVE_TO_MSYS:
      op = CCP_WIN_A_TO_POSIX;
      break;

    case WINDOWS_POSIX_TO_NATIVE_W:
      op = CCP_POSIX_TO_WIN_W;
      break;

    case WINDOWS_POSIX_TO_NATIVE_A:
      op = CCP_POSIX_TO_WIN_A;
      break;
    };

  return (cygwin_conv_path_list (op, from, to, size));
#endif /* USE_NEW_CYGWIN_CONV */
#ifdef USE_OLD_CYGWIN_CONV
  switch (oper)
  /* Those two macros:
     WINDOWS_NATIVE_W_TO_POSIX and WINDOWS_POSIX_TO_NATIVE_W
     are not defined for old Cygwin, and thus not handled here .  */
    {
    case WINDOWS_NATIVE_A_TO_POSIX:
    case WINDOWS_NATIVE_TO_MSYS:
      if (size == 0)
	return cygwin_win32_to_posix_path_list_buf_size (from);
      gdb_assert (size >= cygwin_win32_to_posix_path_list_buf_size (from));
      return cygwin_win32_to_posix_path_list (from, to);
      break;

    case WINDOWS_POSIX_TO_NATIVE_A:
      if (size == 0)
         return cygwin_posix_to_win32_path_list_buf_size (from);
      gdb_assert (size >= cygwin_posix_to_win32_path_list_buf_size (from));
      return cygwin_posix_to_win32_path_list (from, to);
      break;
    };
  return -1;
 #endif /* USE_OLD_CYGWIN_CONV */
}

#ifdef NEED_SLEEP_SUBSTITUTE
int
sleep (int t)
{
  /* Win32 API Sleep function accepts input time in milliseconds.  */
  Sleep (t * 1000);
  return t;
}
#endif /* NEED_SLEEP_SUBSTITUTE */
