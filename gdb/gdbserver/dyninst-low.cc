/* Copyright (C) 2009-2014 Free Software Foundation, Inc.

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
#include "target.h"

#include <limits.h>
#include <stdlib.h>
#include <sys/ptrace.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include "gdb_wait.h"
#include <signal.h>
#include <errno.h>
#include <sys/select.h>
}

#include <dyninst/PCProcess.h>
#include <dyninst/Event.h>
#include <dyninst/ProcessSet.h>
#include <map>
#include <string>
#include <vector>
#include "dyninst-low.h"
#include <iostream>
#include <sstream>

#define DEBUG(func,cerrstr,args...) 			\
  if (debug_threads) {					\
     cerr << "DEBUG(dyninst): " << func << ' ';		\
     args;						\
     cerr << cerrstr << '\n';				\
  }

using namespace std;
using namespace __gnu_cxx;
using namespace Dyninst;
using namespace ProcControlAPI;


typedef std::vector<Breakpoint::ptr> BreakpointSet;
BreakpointSet dyninst_bpset;
ProcessSet::ptr dyninst_procset;
Thread::const_ptr NULL_Thread = Thread::const_ptr();


/* Are there pending callbacks for handleEvents */

bool
have_callbacks ()
{
  fd_set set;
  struct timeval timeout;

  /* Initialize the file descriptor set. */

  FD_ZERO (&set);
  FD_SET (evNotify()->getFD(), &set);

  /* Initialize the timeout data structure. */
  timeout.tv_sec = 1;
  timeout.tv_usec = 0;

  int result = TEMP_FAILURE_RETRY (select (FD_SETSIZE,
				     &set, NULL, NULL,
				     &timeout));
  // 0 if timeout, 1 if input available, -1 if error.
  if (result <= 0)
    return false;
  else
    return true;
}


void pid_to_string (Event::const_ptr ev);
void pid_to_string (ptid_t ptid);


class EventSet
{
private:
  /* The list of events that handleEvents has encountered */
  std::vector<Event::const_ptr> current_events;
public:
  EventSet() { NULL_Event = Event::const_ptr(); }
  Event::const_ptr NULL_Event;
  void insert (Event::const_ptr ev) { current_events.push_back(ev); }
  void erase (Event::const_ptr ev)
    {
      std::vector<Event::const_ptr>::iterator it;
      for (it = current_events.begin() ;
	  it != current_events.end(); ++it)
	{
	  Event::const_ptr event = *it;
	  if (event == ev)
	    {
	      current_events.erase(it);
	      break;
	    }
	}
    }

  /* Get the event for a given PTID */

  Event::const_ptr get (ptid_t ptid)
  {
    DEBUG("events.get",(!have_callbacks() ? "(no callbacks)" : ""),pid_to_string (ptid));
    Event::const_ptr event;
    bool result;

    result = Process::handleEvents(true);
    if (!result)
      return NULL_Event;
    DEBUG("events.get","after handleEvents");

    std::vector<Event::const_ptr>::reverse_iterator rit;
    for (rit = current_events.rbegin() ;
	rit != current_events.rend(); ++rit)
      {

	event = *rit;
	if (event->getProcess()->getPid() == ptid.pid)
	  {
	    DEBUG("events.get","returning event " << event->name());
	    return event;
	  }
      }
    DEBUG("events.get","returning null event");
    return NULL_Event;
  }

  bool is_stopped (Event::const_ptr event)
  {
    if (event == NULL)
      return false;
    else
      return event->getEventType().code() == EventType::Stop;
   }

  bool is_exit (Event::const_ptr event)
  {
    if (event != NULL)
      return event->getEventType().code() == EventType::Exit;
    else return false;
  }


  bool is_breakpoint(Event::const_ptr event)
  {
    if (event != NULL)
      return event->getEventType().code() == EventType::Breakpoint;
    else return false;
  }


  bool is_singlestep(Event::const_ptr event)
  {
    if (event != NULL)
      return event->getEventType().code() == EventType::SingleStep;
    else return false;
  }

  bool is_signal (Event::const_ptr event)
  {
    if (event != NULL)
	return event->getEventType().code() == EventType::Signal;
      else return false;
  }

  bool is_threadcreate (Event::const_ptr event)
  {
    if (event != NULL)
      return event->getEventType().code() == EventType::ThreadCreate;
    else return false;
  }

  bool is_threaddestroy(Event::const_ptr event)
  {
    if (event != NULL)
      return event->getEventType().code() == EventType::ThreadDestroy;
    else return false;
  }

  void dump ()
  {
    std::vector<Event::const_ptr>::iterator it;
    for (it = current_events.begin() ;
	it != current_events.end(); ++it)
      {
	Event::const_ptr event = *it;
	if (event == NULL)
	  continue;
	DEBUG("dump",event->name(), pid_to_string (event));
      }

  }
} events;


void
pid_to_string (Dyninst::PID pid, Dyninst::LWP lwp, Dyninst::THR_ID tid)
{
  if (pid == -1)
    cerr << "pid=NULL";
  else
    {
      cerr << "pid=" << pid;
      if (pid != lwp && lwp > 0)
	cerr << " lwp=" << lwp;
      if (tid > 0)
	cerr << " thr=" << tid;
    }
  cerr << ' ';
}


/* Pretty print a pid/lwp/tid tuple */

void
pid_to_string (ptid_t ptid)
{
  pid_to_string (ptid.pid, ptid.lwp, ptid.tid);
}

void
pid_to_string (Thread::const_ptr thr)
{
  long tid;
  if (thr == NULL)
    return;
  if (thr->haveUserThreadInfo())
    tid = thr->getTID();
  else
    tid = -1;

  pid_to_string (thr->getProcess()->getPid(), thr->getLWP(), tid);
}

void
pid_to_string (Event::const_ptr ev)
{
  if (ev == NULL)
    return;
  pid_to_string (ev->getThread());
}


int using_threads = 1;

extern "C"
{
  const struct target_desc *dyninst_tdesc;
}

/* Per-process private data.  */

struct process_info_private
{
  /* The PTID obtained from the last wait performed on this process.
     Initialized to null_ptid until the first wait is performed.  */
  ptid_t last_wait_event_ptid;
  Process::ptr process;
} process_info_private;

struct thread_info_private
{
  Thread::const_ptr thread;
  Event::const_ptr event;
  CORE_ADDR step_range_start;
  CORE_ADDR step_range_end;
};


/* Add a PROCESS PID, that is possibly being ATTACHED */

static struct process_info *
dyninst_add_process (int pid, int attached, Process::ptr process)
{
  struct process_info *proc;

  proc = add_process (pid, attached);
  proc->tdesc = dyninst_tdesc;
  proc->piprivate = new struct process_info_private;
  proc->piprivate->process = process;
  return proc;
}

/* Add a THREAD to enable mapping a gdbserver thread to a dyninst Thread */

void
dyninst_add_thread(int pid, Thread::const_ptr thread)
{
  Dyninst::LWP lwp;
  if (thread != NULL)
    {
      struct thread_info_private *tip = new struct thread_info_private;
      tip->thread = thread;
      lwp = thread->getLWP();
     current_thread =  add_thread (ptid_build (pid, lwp, 0), tip);
    }
  else
    {
      current_thread = add_thread (ptid_build (pid, pid, 0), NULL);
    }
}


/* Get the dyninst Thread corresponding to the current thread */

static Thread::const_ptr
dyninst_get_inferior_thread()
{
  DEBUG("dyninst_get_inferior_thread", "current_thread=" << current_thread);
  struct thread_info_private *tip = (struct thread_info_private*)(current_thread->target_data);
  DEBUG("dyninst_get_inferior_thread", "", pid_to_string (tip->thread));
  if (!tip)
    error ("No inferior thread");
  return tip->thread;
}


/* Generic dyninst registerEventCallback handler */

Process::cb_ret_t
signal_handler(Event::const_ptr ev)
{
  events.insert(ev);
  DEBUG("signal_handler", ev->name());
  return Process::cbDefault;
}


/* Handle a dyninst SingleStep event */

Process::cb_ret_t
singlestep_handler(Event::const_ptr ev)
{
  events.insert(ev);
  DEBUG("signal_handler", ev->name());
  ev->getThread()->setSingleStepMode(false);
  return Process::cbThreadStop;
}


void
myregisterCB(EventType et, Process::cb_func_t f)
{
  bool result = Process::registerEventCallback(et, f);
  if (!result) {
    cout << "Error registering thread callback " << et.name() << '\n';
  }
}


/* Create a process for PROGRAM and its ALLARGS */

static int
dyninst_create_inferior (char *program, char **allargs)
{
  vector<string> args;
  // Create a new target process
  string exec = program;
  int argc = sizeof(allargs) / sizeof(char*);
  for (int i=1; i<argc; i++)
    {
      string s = string(allargs[i]);
      args[i] = s;
    }


  Process::ptr dyninst_process = Process::createProcess(exec, args);
  if (dyninst_process == Process::ptr())
    error ("No such file: %s\n", exec.c_str());
  dyninst_procset->insert(dyninst_process);

  myregisterCB(EventType::Bootstrap, signal_handler);
  myregisterCB(EventType::Breakpoint, signal_handler);
  myregisterCB(EventType::Crash, signal_handler);
  myregisterCB(EventType::Exec, signal_handler);
  myregisterCB(EventType::Exit, signal_handler);
  myregisterCB(EventType::Fork, signal_handler);
//  myregisterCB(EventType::Library, signal_handler);
  myregisterCB(EventType::RPC, signal_handler);
  myregisterCB(EventType::Signal, signal_handler);
  myregisterCB(EventType::SingleStep, singlestep_handler);
  myregisterCB(EventType::Stop, signal_handler);
  myregisterCB(EventType::Terminate, signal_handler);
  myregisterCB(EventType::ThreadCreate, signal_handler);
  myregisterCB(EventType::ThreadDestroy, signal_handler);
  myregisterCB(EventType::UserThreadCreate, signal_handler);


  DEBUG("dyninst_create_inferior", "created process " << dyninst_process->getPid() << program);

  pid_t pid = dyninst_process->getPid();
  dyninst_add_process (pid, 0, dyninst_process);

  ThreadPool::iterator thidx;
  for (thidx = dyninst_process->threads().begin();
      thidx != dyninst_process->threads().end(); thidx++)
    {
      DEBUG("dyninst_create_inferior", "created thread " << (*thidx)->getTID() << ' ' << (*thidx)->getLWP());
      Thread::ptr th = *thidx;
      dyninst_add_thread (pid, th);
    }

  dyninst_process->stopProc();
  return dyninst_process->getPid();
}

/* Attach to a running process */

static int
dyninst_attach (unsigned long pid)
{
  static Process::ptr dyninst_proc = Process::attachProcess(pid);

  DEBUG("dyninst_attach", "pid=" << pid);
  myregisterCB(EventType::Bootstrap, signal_handler);
  myregisterCB(EventType::Breakpoint, signal_handler);
  myregisterCB(EventType::Crash, signal_handler);
  myregisterCB(EventType::Exec, signal_handler);
  myregisterCB(EventType::Exit, signal_handler);
  myregisterCB(EventType::Fork, signal_handler);
  myregisterCB(EventType::Library, signal_handler);
  myregisterCB(EventType::RPC, signal_handler);
  myregisterCB(EventType::Signal, signal_handler);
  myregisterCB(EventType::SingleStep, signal_handler);
  myregisterCB(EventType::Stop, signal_handler);
  myregisterCB(EventType::Terminate, signal_handler);
  myregisterCB(EventType::ThreadCreate, signal_handler);
  myregisterCB(EventType::ThreadDestroy, signal_handler);
  myregisterCB(EventType::UserThreadCreate, signal_handler);


  if (dyninst_proc == Process::ptr())
    error ("Cannot attach to process %lu: %s (%d)\n", pid,
	   dyninst_proc->getLastErrorMsg(), dyninst_proc->getLastError());

  dyninst_add_process (pid, 1, dyninst_proc);
  ThreadPool::iterator thidx;
  for (thidx = dyninst_proc->threads().begin();
      thidx != dyninst_proc->threads().end(); thidx++)
    {
      DEBUG("dyninst_attach", "created thread " << (*thidx)->getTID() << ' ' << (*thidx)->getLWP());
      Thread::const_ptr th = *thidx;
      dyninst_add_thread (pid, th);
    }

  return 0;
}

/* Continue a thread described by RESUME_INFO */

static void
dyninst_resume (struct thread_resume *resume_info, size_t n)
{
  /* FIXME: Assume for now that n == 1.  */
  ptid_t ptid = resume_info[0].thread;

  if (ptid_equal (ptid, minus_one_ptid))
    ptid = thread_to_gdb_id (current_thread);

  Dyninst::PID pid = ptid_get_pid (ptid);
  ProcessSet::iterator procset_it = dyninst_procset->find(pid);
  if (procset_it == dyninst_procset->end())
    error ("Cannot resume process %lu\n", (long unsigned)pid);

  Process::ptr dyninst_process = *procset_it;

  DEBUG("dyninst_resume", "", pid_to_string (ptid));
  DEBUG("dyninst_resume", "found proc " << dyninst_process->getPid());
  ThreadPool::iterator thidx;
  Thread::ptr th;

  if (dyninst_process->isTerminated())
    return;
  for (thidx = dyninst_process->threads().begin();
      thidx != dyninst_process->threads().end(); thidx++)
    {
      th = *thidx;
      if (th->getTID() == ptid.tid)
	{
	  th = *thidx;
	  break;
	}
    }

  // resume_continue, resume_step, resume_stop
  struct thread_info_private *tip = (struct thread_info_private*)(current_thread->target_data);
  tip->step_range_start = resume_info->step_range_start;
  tip->step_range_end = resume_info->step_range_end;
  struct regcache *regcache = get_thread_regcache (current_thread, 1);
  CORE_ADDR pc = (*the_low_target.get_pc) (regcache);
  DEBUG("dyninst_resume", "range " << resume_info->step_range_start << "/" << resume_info->step_range_end << " pc=" << pc << "kind=" << resume_info[0].kind);
  if (resume_info[0].kind == resume_step)
    {
      DEBUG("dyninst_resume", "in step mode", pid_to_string (th));
      th->setSingleStepMode(true);
    }
  else
    th->setSingleStepMode(false);

  regcache_invalidate ();

  DEBUG("dyninst_resume","before continue");
  bool result = dyninst_process->continueProc();
  if (! result)
    cout << "Cannot continueProc " << dyninst_process->getLastErrorMsg() << '\n';
}

/* Handle a vCont packet */

static void
dyninst_continue (ptid_t ptid)
{
  struct thread_resume resume_info;

  DEBUG("dyninst_continue","", pid_to_string (ptid));
  
  resume_info.thread = ptid;
  resume_info.kind = resume_continue;
  resume_info.sig = 0;

  dyninst_resume (&resume_info, 1);
}


/* True if LWP is stopped in its stepping range.  */

bool
in_step_range ()
{
  struct thread_info_private *tip = (struct thread_info_private*)(current_thread->target_data);
  struct regcache *regcache = get_thread_regcache (current_thread, 1);
  CORE_ADDR pc = (*the_low_target.get_pc) (regcache);
  DEBUG("in_step_range", "range " << tip->step_range_start << "/" << tip->step_range_end << " pc=" << pc << " returning" << (pc >= tip->step_range_start && pc < tip->step_range_end));

  return (pc >= tip->step_range_start && pc < tip->step_range_end);
}


/* Wait for a thread PTID having a target STATUS */

static ptid_t
dyninst_wait_1 (ptid_t ptid, struct target_waitstatus *status, int options)
{
  Dyninst::PID pid = 0;
  ptid_t new_ptid;


  DEBUG("dyninst_wait_1", "", pid_to_string (ptid));
  if (ptid_equal (ptid, minus_one_ptid))
    {
      pid = ptid_get_pid (thread_to_gdb_id (current_thread));
      new_ptid = ptid_build (pid, pid, 0);
    }
  else
    {
      pid = ptid_get_pid(ptid);
      new_ptid = ptid;
    }

  ProcessSet::iterator procset_it = dyninst_procset->find(pid);
  Process::ptr dyninst_process = *procset_it;


  Event::const_ptr event;

  while (true)		// wait events loop
    {
      if ((event = events.get(new_ptid)))
	{
	  struct process_info *pi;
	  pi = find_process_pid(pid);
	  if (pi)
	    pi->piprivate->last_wait_event_ptid = new_ptid;
	}
      if (event == NULL)
	event = events.get(new_ptid);

      events.dump();


      if (events.is_threadcreate(event))
	{
	  Thread::const_ptr thr;
//	  if (event != events.NULL_Event)
	  if (event != NULL)
	    thr = event->getThread();
	  else
	    thr = NULL_Thread;
	  DEBUG("dyninst_wait_1", "New thread: ", pid_to_string (ptid));
	  dyninst_add_thread (pid, thr);
	}
      else if (events.is_exit(event))
	{
	  EventExit::const_ptr exit_ev = event->getEventExit();
	  status->kind = TARGET_WAITKIND_EXITED;
	  status->value.integer = exit_ev->getExitCode();
	  DEBUG("dyninst_wait_1", "process exited with status: " << status->value.integer);
	}
      else if (events.is_breakpoint(event) || events.is_singlestep(event))
	{
	  EventBreakpoint::const_ptr breakpoint_ev = event->getEventBreakpoint();
	  if (breakpoint_ev)
	    {
	      std::vector<Breakpoint::const_ptr> bps;
	      breakpoint_ev->getBreakpoints(bps);
	      std::vector<Breakpoint::const_ptr>::iterator it;
	      for (it = bps.begin(); it != bps.end(); it++)
		{
		  Breakpoint::const_ptr bp = *it;
		  DEBUG("dyninst_wait_1", "breakpoint " << bp->getToAddress());
		}
	    }
	  events.erase(event);
	  status->kind = TARGET_WAITKIND_STOPPED;
	  // EventStop doesn't have a signal member
	  status->value.integer = gdb_signal_from_host (SIGTRAP);
	  DEBUG("dyninst_wait_1", "process trapped: ");
	}
      else if (events.is_signal(event))
	{
	  status->kind = TARGET_WAITKIND_SIGNALLED;
	  EventSignal::const_ptr signal_ev = event->getEventSignal();
	  DEBUG("dyninst_wait_1", signal_ev->name());
	  status->value.integer = signal_ev->getSignal();
	  DEBUG("dyninst_wait_1", "process signalled with signal: " << status->value.integer);
	}
      else if (events.is_threadcreate(event))
	{
	  dyninst_continue (new_ptid);
	  continue;		// wait events loop
	}
      else if (events.is_threaddestroy(event))
	{
	  remove_thread (find_thread_ptid (new_ptid));
	  dyninst_continue (new_ptid);
	  continue;		// wait events loop
	}
      else
	{
	  status->kind = TARGET_WAITKIND_STOPPED;
	  status->value.integer = gdb_signal_from_host (0);
//	  DEBUG("dyninst_wait_1", "process received: " << (event != events.NULL_Event ? event->name() : ""));
	  DEBUG("dyninst_wait_1", "process received: " << (event != NULL ? event->name() : ""));
	}
      break;
    }

  DEBUG("dyninst_wait_1 returning", "", pid_to_string (ptid));
  return new_ptid;
}


/*  Wait for a thread PTID */

static ptid_t
dyninst_wait (ptid_t ptid, struct target_waitstatus *status, int options)
{
  ptid_t new_ptid;

  DEBUG("dyninst_wait","", pid_to_string (ptid));
  new_ptid = dyninst_wait_1 (ptid, status, options);

  DEBUG("dyninst_wait", "kind=" << status->kind, pid_to_string (new_ptid));
  return new_ptid;
}

/* Kill a PID */

static int
dyninst_kill (Dyninst::PID pid)
{
  struct process_info *process;
  ProcessSet::iterator procset_it = dyninst_procset->find(pid);
  Process::ptr dyninst_process = *procset_it;

  DEBUG("dyninst_kill", "pid=" <<  pid);
  process = find_process_pid (pid);
  if (process == NULL)
    return -1;

  dyninst_process->stopProc();
  dyninst_procset->erase(dyninst_process);
  the_target->mourn (process);
  return 0;
}

/* Detach a PID */

static int
dyninst_detach (Dyninst::PID pid)
{
  struct process_info *process;
  ProcessSet::iterator procset_it = dyninst_procset->find(pid);
  Process::ptr dyninst_process = *procset_it;

  DEBUG("dyninst_detach", "pid=" << pid);
  process = find_process_pid (pid);
  if (process == NULL)
    return -1;

  dyninst_procset->erase(dyninst_process);
  the_target->mourn (process);
  return 0;
}

/* Remove a PROC */

static void
dyninst_mourn (struct process_info *proc)
{
  DEBUG("dyninst_mourn", "proc=" << proc);
  proc->piprivate = NULL;
  clear_inferiors ();
}


static void
dyninst_join (Dyninst::PID pid)
{
}


static int
dyninst_thread_alive (ptid_t ptid)
{
  /* The list of threads is updated at the end of each wait, so it
     should be up to date.  No need to re-fetch it.  */
  DEBUG("dyninst_thread_alive", "", pid_to_string (ptid));
  return (find_thread_ptid (ptid) != NULL);
}

/* Fetch the current thread's registers into REGCACHE */

static void
dyninst_fetch_registers (struct regcache *regcache, int regno)
{
  Thread::const_ptr th = dyninst_get_inferior_thread();
  DEBUG("dyninst_fetch_registers", " regno=" << regno, pid_to_string (th));

  struct dyninst_regset_info *regset = dyninst_target_regsets;

  RegisterPool regpool;
  th->getAllRegisters(regpool);

  regset->fill_function (regcache, regpool);
}

/* Store the current threads registers from REGCACHE */

static void
dyninst_store_registers (struct regcache *regcache, int regno)
{
  Thread::const_ptr th = dyninst_get_inferior_thread();
  DEBUG("dyninst_store_registers", " regno=" << regno, pid_to_string (th));

  struct dyninst_regset_info *regset = dyninst_target_regsets;

  RegisterPool regpool;
  th->getAllRegisters(regpool);

  regset->store_function (regcache, regpool);
  th->setAllRegisters(regpool);
}

static CORE_ADDR
dyninst_read_pc (struct regcache *regcache)
{
  if (the_low_target.get_pc == NULL)
    return 0;

  return (*the_low_target.get_pc) (regcache);
}

static void
dyninst_write_pc (struct regcache *regcache, CORE_ADDR pc)
{
  gdb_assert (the_low_target.set_pc != NULL);

  (*the_low_target.set_pc) (regcache, pc);
}

/* Copy LEN bytes from inferior's memory starting at MEMADDR
   to debugger memory starting at MYADDR.  */

static int
dyninst_read_memory (CORE_ADDR memaddr, unsigned char *myaddr, int len)
{
  DEBUG("dyninst_read_memory", "memaddr" <<  memaddr << " myaddr=" << (void*)myaddr);
  Thread::const_ptr th = dyninst_get_inferior_thread();
  Process::const_ptr dyninst_process = th->getProcess();

  bool result = dyninst_process->readMemory((void*)myaddr, (Dyninst::Address)memaddr, len);
  if (debug_threads)
    {
      /* Dump up to four bytes.  */
      unsigned int val = * (unsigned int *) myaddr;
      if (len == 1)
	val = val & 0xff;
      else if (len == 2)
	val = val & 0xffff;
      else if (len == 3)
	val = val & 0xffffff;
    }

  if (!result)
    return 1;
  else
    return 0;
}

/* Copy LEN bytes of data from debugger memory at MYADDR to inferior's
   memory at MEMADDR.  On failure returns the value of errno. */


static int
dyninst_write_memory (CORE_ADDR memaddr, const unsigned char *myaddr, int len)
{
  DEBUG("dyninst_write_memory", "memaddr=" << memaddr << " myaddr=" << (void*)myaddr);
  Thread::const_ptr th = dyninst_get_inferior_thread();
  Process::const_ptr proc = th->getProcess();
  bool result = proc->writeMemory((Dyninst::Address)memaddr, (void*)myaddr, len);
  if (debug_threads)
    {
      /* Dump up to four bytes.  */
      unsigned int val = * (unsigned int *) myaddr;
      if (len == 1)
	val = val & 0xff;
      else if (len == 2)
	val = val & 0xffff;
      else if (len == 3)
	val = val & 0xffffff;
    }

  if (!result)
    return 1;
  else
    return 0;
}

/* Kill the current process */

static void
dyninst_request_interrupt (void)
{
  Dyninst::PID pid = ptid_get_pid (thread_to_gdb_id (current_thread));
  ProcessSet::iterator procset_it = dyninst_procset->find(pid);
  Process::ptr dyninst_process = *procset_it;

  dyninst_process->terminate();
}


/* Breakpoint/Watchpoint support.  */

static int
dyninst_supports_z_point_type (char z_type)
{
  switch (z_type)
    {
    case Z_PACKET_SW_BP:
    case Z_PACKET_HW_BP:
    case Z_PACKET_WRITE_WP:
    case Z_PACKET_ACCESS_WP:
      return 1;
    default:
      return 0;
    }
}


/* Set a breakpoint BP of TYPE at ADDR for LEN bytes */

static int
dyninst_insert_point (enum raw_bkpt_type type, CORE_ADDR addr, int len,
		      struct raw_breakpoint *bp)
{
  Thread::const_ptr th = dyninst_get_inferior_thread();
  Process::const_ptr dyninst_process = th->getProcess();

  DEBUG("dyninst_insert_point", " addr=" << addr, pid_to_string(th));
  Breakpoint::ptr brp = Breakpoint::newBreakpoint();
  brp->setData(bp);
  bool result = dyninst_process->addBreakpoint(addr, brp);
  dyninst_bpset.push_back(brp);
  return result == false;
}

/* Remove a breakpoint BP of TYPE at ADDR for LEN bytes */

static int
dyninst_remove_point (enum raw_bkpt_type type, CORE_ADDR addr, int len,
		      struct raw_breakpoint *rbp)
{
  Thread::const_ptr th = dyninst_get_inferior_thread();
  DEBUG("dyninst_remove_point", " addr=" << addr, pid_to_string (th) );
  Process::const_ptr dyninst_process = th->getProcess();

  std::vector<Breakpoint::ptr>::iterator it;
  bool result;
  for (it = dyninst_bpset.begin(); it != dyninst_bpset.end(); it++)
    {
      Breakpoint::ptr bp = *it;
      if ((struct raw_breakpoint*)bp->getData() == rbp)
	{
	  result = dyninst_process->rmBreakpoint(addr, bp);
	  break;
	}
    }
  dyninst_bpset.erase(it);

  return result == false;
}


static int
dyninst_supports_range_stepping (void)
{
  if (*the_low_target.supports_range_stepping == NULL)
    return 0;

  return (*the_low_target.supports_range_stepping) ();
}


/* The Dyninst target_ops vector.  */

static struct target_ops dyninst_target_ops = {
  dyninst_create_inferior,
  dyninst_attach,
  dyninst_kill,
  dyninst_detach,
  dyninst_mourn,
  dyninst_join,
  dyninst_thread_alive,
  dyninst_resume,
  dyninst_wait,
  dyninst_fetch_registers,
  dyninst_store_registers,
  NULL,  /* prepare_to_access_memory */
  NULL,  /* done_accessing_memory */
  dyninst_read_memory,
  dyninst_write_memory,
  NULL,  /* look_up_symbols */
  dyninst_request_interrupt,
  NULL,  /* read_auxv */
  dyninst_supports_z_point_type,
  dyninst_insert_point,  /* insert_point */
  dyninst_remove_point,  /* remove_point */
  NULL,  // stopped_by_watchpoint
  NULL,  // stopped_data_address
  NULL,  // read_offsets
  NULL,  // get_tls_address
  NULL,  // qxfer_spu
  NULL,  // hostio_last_error
  NULL,  // qxfer_osdata
  NULL,  // qxfer_siginfo
  NULL,  // supports_non_stop
  NULL,  // async
  NULL,  // start_non_stop
  NULL,  // supports_multi_process
  NULL,  // handle_monitor_command
  NULL,  // common_core_of_thread
  NULL,  // read_loadmap
  NULL,  // process_qsupported
  NULL,  // supports_tracepoints
  dyninst_read_pc,
  dyninst_write_pc,
  NULL,  // thread_stopped
  NULL,
  NULL,  // pause_all
  NULL,  // unpause_all
  NULL,  // cancel_breakpoints
  NULL,  // stabilize_threads
  NULL,  // install_fast_tracepoint_jump_pad
  NULL,  // emit_ops
  NULL,  // supports_disable_randomization
  NULL,  // get_min_fast_tracepoint_insn_len
  NULL,  // qxfer_libraries_svr4
  NULL,  // supports_agent
  NULL,
  NULL,
  NULL,
  NULL,
  dyninst_supports_range_stepping
};

void
initialize_low (void)
{

  cerr << showbase // show the 0x prefix
      << internal // fill between the prefix and the number
      << hex;
  dyninst_procset = ProcessSet::newProcessSet();
  set_target_ops (&dyninst_target_ops);
  the_low_target.arch_setup ();
}
