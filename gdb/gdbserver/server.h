/* Common definitions for remote server for GDB.
   Copyright (C) 1993-2014 Free Software Foundation, Inc.

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

#ifndef SERVER_H
#define SERVER_H

#include "common-defs.h"

gdb_static_assert (sizeof (CORE_ADDR) >= sizeof (void *));

#ifdef __MINGW32CE__
#include "wincecompat.h"
#endif

#include "version.h"

#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif
/* On some systems such as MinGW, alloca is declared in malloc.h
   (there is no alloca.h).  */
#if HAVE_MALLOC_H
#include <malloc.h>
#endif

#if !HAVE_DECL_STRERROR
#ifndef strerror
extern char *strerror (int);	/* X3.159-1989  4.11.6.2 */
#endif
#endif

#if !HAVE_DECL_PERROR
#ifndef perror
extern void perror (const char *);
#endif
#endif

#if !HAVE_DECL_VASPRINTF
extern int vasprintf(char **strp, const char *fmt, va_list ap);
#endif
#if !HAVE_DECL_VSNPRINTF
int vsnprintf(char *str, size_t size, const char *format, va_list ap);
#endif

#ifdef IN_PROCESS_AGENT
#  define PROG "ipa"
#else
#  define PROG "gdbserver"
#endif

#include "buffer.h"
#include "xml-utils.h"
#include "regcache.h"
#include "gdb_signals.h"
#include "target.h"
#include "mem-break.h"
#include "gdbthread.h"
#include "inferiors.h"

/* Target-specific functions */

void initialize_low ();

/* Public variables in server.c */

extern int disable_packet_vCont;
extern int disable_packet_Tthread;
extern int disable_packet_qC;
extern int disable_packet_qfThreadInfo;


#if USE_WIN32API
#include <winsock2.h>
typedef SOCKET gdb_fildes_t;
#else
typedef int gdb_fildes_t;
#endif

#include "event-loop.h"

/* Functions from server.c.  */
extern int handle_serial_event (int err, gdb_client_data client_data);
extern int handle_target_event (int err, gdb_client_data client_data);

#include "remote-utils.h"

#include "utils.h"
#include "debug.h"

/* Maximum number of bytes to read/write at once.  The value here
   is chosen to fill up a packet (the headers account for the 32).  */
#define MAXBUFBYTES(N) (((N)-32)/2)

/* Buffer sizes for transferring memory, registers, etc.   Set to a constant
   value to accomodate multiple register formats.  This value must be at least
   as large as the largest register set supported by gdbserver.  */
#define PBUFSIZ 16384

/* Description of the remote protocol state for the currently
   connected target.  This is per-target state, and independent of the
   selected architecture.  */

struct server_state
{
  /* The thread set with an `Hc' packet.  `Hc' is deprecated in favor of
     `vCont'.  Note the multi-process extensions made `vCont' a
     requirement, so `Hc pPID.TID' is pretty much undefined.  So
     CONT_THREAD can be null_ptid for no `Hc' thread, minus_one_ptid for
     resuming all threads of the process (again, `Hc' isn't used for
     multi-process), or a specific thread ptid_t.

     We also set this when handling a single-thread `vCont' resume, as
     some places in the backends check it to know when (and for which
     thread) single-thread scheduler-locking is in effect.  */
  int attach_count;
  ptid_t cont_thread;
  // The thread set with an `Hg' packet.
  ptid_t general_thread;
  pid_t signal_pid;
  struct target_waitstatus last_status;
  ptid_t last_ptid;
  unsigned char *mem_buf;
  unsigned char readchar_buf[BUFSIZ];
  int readchar_bufcnt;
  unsigned char *readchar_bufp;
  // following were in inferior.c
  struct inferior_list all_processes;
  struct inferior_list all_threads;
  struct thread_info *current_thread;
};

typedef struct server_state server_state;

struct client_state
{
  char *executable;
  // file descriptor corresponding to this client
  gdb_fildes_t file_desc;
  char *own_buf;
  char *in_buf;
  char packet;
  char normalized_packet; // if own_buf == "OK"
  int pending;
  // following were in server.c
  // --once: Exit after the first connection has closed.
  int run_once;
  // --multi
  int multi_process;
  // QNonStop packet
  int non_stop;
  // QDisableRandomization packet
  int disable_randomization;
  int server_waiting;
  int extended_protocol;
  int response_needed;
  int exit_requested;
  int pass_signals[GDB_SIGNAL_LAST];
  int program_signals[GDB_SIGNAL_LAST];
  int program_signals_p;
  char **program_argv, **wrapper_argv;
  int packet_len;
  // following were in mem-break.c
  const unsigned char *breakpoint_data;
  int breakpoint_len;
  //
  server_state *ss;
  struct client_state *next;
};

typedef struct client_state client_state;

struct client_states
{
  client_state *first;
  client_state *current_cs;
  gdb_fildes_t current_fd;
};

client_state * get_client_state ();

client_state * set_client_state (gdb_fildes_t);

void delete_client_state (gdb_fildes_t fd);

#endif /* SERVER_H */
