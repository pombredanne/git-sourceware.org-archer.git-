/* Common definitions for remote server for GDB.
   Copyright (C) 1993-2015 Free Software Foundation, Inc.

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

extern ptid_t cont_thread;
extern ptid_t general_thread;

extern int server_waiting;
extern int pass_signals[];
extern int program_signals[];
extern int program_signals_p;

extern int disable_packet_vCont;
extern int disable_packet_Tthread;
extern int disable_packet_qC;
extern int disable_packet_qfThreadInfo;

extern int run_once;
extern int multi_process;
extern int report_fork_events;
extern int report_vfork_events;
extern int non_stop;
extern int extended_protocol;

/* True if the "swbreak+" feature is active.  In that case, GDB wants
   us to report whether a trap is explained by a software breakpoint
   and for the server to handle PC adjustment if necessary on this
   target.  Only enabled if the target supports it.  */
extern int swbreak_feature;

/* True if the "hwbreak+" feature is active.  In that case, GDB wants
   us to report whether a trap is explained by a hardware breakpoint.
   Only enabled if the target supports it.  */
extern int hwbreak_feature;

extern int disable_randomization;

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

/* Get rid of the currently pending stop replies that match PTID.  */
extern void discard_queued_stop_replies (ptid_t ptid);

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

#endif /* SERVER_H */

/* Description of the remote protocol state for the currently
   connected target.  This is per-target state, and independent of the
   selected architecture.  */

struct server_state
{
  /* From server.c */
  /* The thread set with an `Hc' packet.  `Hc' is deprecated in favor of
     `vCont'.  Note the multi-process extensions made `vCont' a
     requirement, so `Hc pPID.TID' is pretty much undefined.  So
     CONT_THREAD can be null_ptid for no `Hc' thread, minus_one_ptid for
     resuming all threads of the process (again, `Hc' isn't used for
     multi-process), or a specific thread ptid_t.  */
  ptid_t cont_thread;
  /* The thread set with an `Hg' packet.  */
  ptid_t general_thread;
  /* The PID of the originally created or attached inferior.  Used to
     send signals to the process when GDB sends us an asynchronous interrupt
     (user hitting Control-C in the client), and to wait for the child to exit
     when no longer debugging it.  */

  unsigned long signal_pid;
  /* Last status reported to GDB.  */
  struct target_waitstatus last_status;
  ptid_t last_ptid;
  unsigned char *mem_buf;

  /* from remote-utils.c */
  /* Internal buffer used by readchar.
     These are global to readchar because reschedule_remote needs to be
     able to tell whether the buffer is empty.  */
  unsigned char readchar_buf[BUFSIZ];
  int readchar_bufcnt;
  unsigned char *readchar_bufp;
  /* from inferiors.c */
  struct inferior_list all_processes;
  struct inferior_list all_threads;
  struct thread_info *current_thread;
};

typedef struct server_state server_state;

struct client_state
{
  /* From server.c */
  int server_waiting;

  int extended_protocol;
  int response_needed;
  int exit_requested;

  /* --once: Exit after the first connection has closed.  */
  int run_once;

  int multi_process;
  int report_fork_events;
  int report_vfork_events;
  int non_stop;
  int swbreak_feature;
  int hwbreak_feature;

  /* Whether we should attempt to disable the operating system's address
     space randomization feature before starting an inferior.  */
  int disable_randomization;

  char **program_argv, **wrapper_argv;

  int pass_signals[GDB_SIGNAL_LAST];
  int program_signals[GDB_SIGNAL_LAST];
  int program_signals_p;
  char *own_buffer;
  server_state *ss;
};

typedef struct client_state client_state;

struct client_states
{
  client_state *first;
  client_state *current_cs;
  gdb_fildes_t current_fd;
};

client_state * get_client_state ();

#define cont_thread	(get_client_state()->ss->cont_thread)
#define general_thread	(get_client_state()->ss->general_thread)
#define signal_pid	(get_client_state()->ss->signal_pid)
#define last_status	(get_client_state()->ss->last_status)
#define last_ptid	(get_client_state()->ss->last_ptid)
#define mem_buf		(get_client_state()->ss->mem_buf)
#define readchar_buf	(get_client_state()->ss->readchar_buf)
#define readchar_bufcnt	(get_client_state()->ss->readchar_bufcnt)
#define readchar_bufp	(get_client_state()->ss->readchar_bufp)
#define all_processes  	(get_client_state()->ss->all_processes)
#define all_threads	(get_client_state()->ss->all_threads)
#define current_thread   (get_client_state()->ss->current_thread)
#define server_waiting	(get_client_state()->server_waiting)
#define extended_protocol	(get_client_state()->extended_protocol)
#define response_needed	(get_client_state()->response_needed)
#define exit_requested	(get_client_state()->exit_requested)
#define run_once	(get_client_state()->run_once)
#define multi_process	(get_client_state()->multi_process)
#define report_fork_events	(get_client_state()->report_fork_events)
#define report_vfork_events	(get_client_state()->report_vfork_events)
#define non_stop	(get_client_state()->non_stop)
#define swbreak_feature	(get_client_state()->swbreak_feature)
#define hwbreak_feature	(get_client_state()->hwbreak_feature)
#define disable_randomization	(get_client_state()->disable_randomization)
#define program_argv	(get_client_state()->program_argv)
#define wrapper_argv	(get_client_state()->wrapper_argv)
#define pass_signals	(get_client_state()->pass_signals)
#define program_signals	(get_client_state()->program_signals)
#define program_signals_p	(get_client_state()->program_signals_p)
#define own_buffer	(get_client_state()->own_buffer)
