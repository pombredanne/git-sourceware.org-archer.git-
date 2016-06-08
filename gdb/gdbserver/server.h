/* Common definitions for remote server for GDB.
   Copyright (C) 1993-2016 Free Software Foundation, Inc.

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
extern int notify_clients (char *buffer);

/* Get rid of the currently pending stop replies that match PTID.  */
extern void discard_queued_stop_replies (ptid_t ptid);

#include "remote-utils.h"

#include "utils.h"
#include "debug.h"
#include "gdb_vecs.h"

/* Maximum number of bytes to read/write at once.  The value here
   is chosen to fill up a packet (the headers account for the 32).  */
#define MAXBUFBYTES(N) (((N)-32)/2)

/* Buffer sizes for transferring memory, registers, etc.   Set to a constant
   value to accomodate multiple register formats.  This value must be at least
   as large as the largest register set supported by gdbserver.  */
#define PBUFSIZ 16384

/* Definition for an unknown syscall, used basically in error-cases.  */
#define UNKNOWN_SYSCALL (-1)

/* Definition for any syscall, used for unfiltered syscall reporting.  */
#define ANY_SYSCALL (-2)

#endif /* SERVER_H */

/* Description of the remote protocol state for the currently
   connected target.  This is per-target state, and independent of the
   selected architecture. */

struct server_state
{
  int attach_count_;
  /* From server.c */
  /* The thread set with an `Hc' packet.  `Hc' is deprecated in favor of
     `vCont'.  Note the multi-process extensions made `vCont' a
     requirement, so `Hc pPID.TID' is pretty much undefined.  So
     CONT_THREAD can be null_ptid for no `Hc' thread, minus_one_ptid for
     resuming all threads of the process (again, `Hc' isn't used for
     multi-process), or a specific thread ptid_t.  */
  ptid_t cont_thread_;
  /* The thread set with an `Hg' packet.  */
  ptid_t general_thread_;
  /* The PID of the originally created or attached inferior.  Used to
     send signals to the process when GDB sends us an asynchronous interrupt
     (user hitting Control-C in the client), and to wait for the child to exit
     when no longer debugging it.  */

  unsigned long signal_pid_;
  /* Last status reported to GDB.  */
  struct target_waitstatus last_status_;
  /* Was last status an exit status? (sticky if yes) */
  int last_status_exited;
  ptid_t last_ptid_;
  unsigned char *mem_buf_;

  /* from remote-utils.c */
  /* Internal buffer used by readchar.
     These are global to readchar because reschedule_remote needs to be
     able to tell whether the buffer is empty.  */
  unsigned char readchar_buf_[BUFSIZ];
  int readchar_bufcnt_;
  unsigned char *readchar_bufp_;
  /* from inferiors.c */
  struct inferior_list all_processes_;
  struct inferior_list all_threads_;
  struct thread_info *current_thread_;
};

typedef struct server_state server_state;

struct client_breakpoint
{
  CORE_ADDR addr;
  struct client_breakpoint *next;
};

enum packet_types { other_packet, vContc, vConts, vContt, vRun, vAttach };
typedef enum packet_types packet_types;

enum exit_types { no_exit, have_exit, sent_exit };
typedef enum exit_types exit_types;


struct client_state
{
  gdb_fildes_t file_desc;
  int attached_to_client;
  int packet_type;
  int last_packet_type;
  int pending;
  int nonstop_pending;
  int catch_syscalls;
  ptid_t last_cont_ptid;
  struct target_waitstatus last_cont_waitstatus;

  /* From server.c */
  int server_waiting_;

  int extended_protocol_;
  int response_needed_;
  int exit_requested_;

  /* --once: Exit after the first connection has closed.  */
  int run_once_;

  int multi_process_;
  int report_fork_events_;
  int report_vfork_events_;
  int report_exec_events_;
  int report_thread_events_;
  /* Whether to report TARGET_WAITKING_NO_RESUMED events.  */
  int report_no_resumed_;
  int non_stop_;
  /* True if the "swbreak+" feature is active.  In that case, GDB wants
     us to report whether a trap is explained by a software breakpoint
     and for the server to handle PC adjustment if necessary on this
     target.  Only enabled if the target supports it.  */
  int swbreak_feature_;
  /* True if the "hwbreak+" feature is active.  In that case, GDB wants
     us to report whether a trap is explained by a hardware breakpoint.
     Only enabled if the target supports it.  */
  int hwbreak_feature_;

  /* True if the "vContSupported" feature is active.  In that case, GDB
     wants us to report whether single step is supported in the reply to
     "vCont?" packet.  */
  int vCont_supported_;

  /* Whether we should attempt to disable the operating system's address
     space randomization feature before starting an inferior.  */
  int disable_randomization_;

  char **program_argv_;
  char **wrapper_argv_;
  int packet_length_;

  int pass_signals_[GDB_SIGNAL_LAST];
  int program_signals_[GDB_SIGNAL_LAST];
  int program_signals_p_;
  char *in_buffer_;
  char *own_buffer_;

  /* from remote-utils.c */
  int remote_debug_;
  /* If true, then we tell GDB to use noack mode by default.  */
  int noack_mode_;
  int transport_is_reliable_;

  struct client_breakpoint *client_breakpoints;
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
int have_multiple_clients();
void delete_client_state (gdb_fildes_t fd);


#define attach_count	(get_client_state()->ss->attach_count_)
#define cont_thread	(get_client_state()->ss->cont_thread_)
#define general_thread	(get_client_state()->ss->general_thread_)
#define signal_pid	(get_client_state()->ss->signal_pid_)
#define last_status	(get_client_state()->ss->last_status_)
#define last_ptid	(get_client_state()->ss->last_ptid_)
#define mem_buf		(get_client_state()->ss->mem_buf_)
#define readchar_buf	(get_client_state()->ss->readchar_buf_)
#define readchar_bufcnt	(get_client_state()->ss->readchar_bufcnt_)
#define readchar_bufp	(get_client_state()->ss->readchar_bufp_)
#define all_processes  	(get_client_state()->ss->all_processes_)
#define all_threads	(get_client_state()->ss->all_threads_)
#define current_thread   (get_client_state()->ss->current_thread_)
#define server_waiting	(get_client_state()->server_waiting_)
#define extended_protocol	(get_client_state()->extended_protocol_)
#define response_needed	(get_client_state()->response_needed_)
#define exit_requested	(get_client_state()->exit_requested_)
#define run_once	(get_client_state()->run_once_)
#define multi_process	(get_client_state()->multi_process_)
#define report_fork_events	(get_client_state()->report_fork_events_)
#define report_vfork_events	(get_client_state()->report_vfork_events_)
#define report_exec_events     (get_client_state()->report_exec_events_)
#define report_thread_events   (get_client_state()->report_thread_events_)
#define report_no_resumed      (get_client_state()->report_no_resumed_)
#define non_stop	(get_client_state()->non_stop_)
#define swbreak_feature	(get_client_state()->swbreak_feature_)
#define hwbreak_feature	(get_client_state()->hwbreak_feature_)
#define vCont_supported        (get_client_state()->vCont_supported_)
#define disable_randomization	(get_client_state()->disable_randomization_)
#define program_argv	(get_client_state()->program_argv_)
#define wrapper_argv	(get_client_state()->wrapper_argv_)
#define packet_length	(get_client_state()->packet_length_)
#define pass_signals	(get_client_state()->pass_signals_)
#define program_signals	(get_client_state()->program_signals_)
#define program_signals_p	(get_client_state()->program_signals_p_)
#define in_buffer	(get_client_state()->in_buffer_)
#define own_buffer	(get_client_state()->own_buffer_)
#define remote_debug	(get_client_state()->remote_debug_)
#define noack_mode	(get_client_state()->noack_mode_)
#define transport_is_reliable (get_client_state()->transport_is_reliable_)
