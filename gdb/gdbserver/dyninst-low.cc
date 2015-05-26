/* Copyright (C) 2009-2015 Free Software Foundation, Inc.

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
#include "dll.h"

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
#include <sys/stat.h>
#include <fcntl.h>
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
#include <typeinfo>

#define DEBUG(cerrstr,args...) 			\
  if (debug_threads) {					\
     cerr << "DEBUG(dyninst): " << __FUNCTION__ << ' ';	\
     args;						\
     cerr << cerrstr << '\n';				\
  }


#define DYNERRMSG(errstr,args...)						\
  fprintf (stderr, "%s %s " errstr "\n", __FUNCTION__, dyninst_process->getLastErrorMsg(), ##args);
#define DYNERR(errstr,args...)						\
  fprintf (stderr, "%s " errstr "\n", __FUNCTION__, ##args);
#define DEBUG_ENTER() if (debug_threads) debug_enter()
#define DEBUG_EXIT()  if (debug_threads) debug_exit()


using namespace std;
using namespace __gnu_cxx;
using namespace Dyninst;
using namespace ProcControlAPI;


typedef std::vector<Breakpoint::ptr> BreakpointSet;
BreakpointSet dyninst_bpset;
ProcessSet::ptr dyninst_procset;
typedef std::vector<Library::ptr> LibrarySet;
LibrarySet dyninst_libset;
Thread::const_ptr NULL_Thread = Thread::const_ptr();


void
dump_procset ()
{
  ProcessSet::iterator it;
  ThreadPool::const_iterator thidx;
  for (it = dyninst_procset->begin(); it != dyninst_procset->end(); it++)
    {
      Process::const_ptr proc = *it;
      cout << __FUNCTION__ << ' ' << proc->getPid() << ' ' << hex;
      for (thidx = proc->threads().begin();
	  thidx != proc->threads().end(); thidx++)
	{
	  Thread::const_ptr thr = *thidx;
	  cout << thr->getLWP() << ' ' << the_low_target.get_pc (thr) << ' ';
	}
      cout << '\n';
    }
}

/* Are there pending callbacks for handleEvents */

bool have_callbacks () __attribute__ ((unused));

bool
have_callbacks ()
{
  fd_set set;
  struct timeval timeout;
  /* Initialize the file descriptor set. */

  FD_ZERO (&set);
  FD_SET (evNotify()->getFD(), &set);

  /* Initialize the timeout data structure. */
  timeout.tv_sec = 0;
  timeout.tv_usec = 250;

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


bool operator<(const ptid_t & l, const ptid_t & r )
{
  if (l.pid < r.pid)
    return true;
  if (l.tid < r.tid)
    return true;
  if (l.lwp < r.lwp)
    return true;
  return false;
}

bool operator==(const ptid_t & l, const ptid_t & r )
{
  if (l.pid == r.pid && l.tid == r.tid && l.lwp == r.lwp)
    return true;
  return false;
}

class EventSet
{
private:
  /* The list of events that handleEvents has encountered */
  std::vector<std::pair<ptid_t, Event::const_ptr> > current_events;
public:
  EventSet() { NULL_Event = Event::const_ptr(); }
  Event::const_ptr NULL_Event;
  void insert (Event::const_ptr ev)
  {
    ptid_t ptid = ptid_build (ev->getProcess()->getPid(), ev->getThread()->getLWP(), (ev->getThread()->haveUserThreadInfo() ? ev->getThread()->getTID() : -1));
    current_events.push_back(make_pair (ptid, ev));
    if (debug_threads)
      {
	dump("after insert ");
	dump_procset ();
      }
  }
  void erase (Event::const_ptr ev)
  {
    if (ev != NULL_Event)
      DEBUG(ev->name());
    std::vector<std::pair<ptid_t,Event::const_ptr> >::iterator it;
    for (it = current_events.begin() ;
	it != current_events.end(); ++it)
      {
	Event::const_ptr event = it->second;
	if (event == ev)
	  {
	    current_events.erase(it);
	    break;
	  }
      }
    if (debug_threads)
      dump("after erase ");
  }

  /* Get the event for a given PTID */

  Event::const_ptr get (ptid_t ptid)
  {
    Event::const_ptr event;
    int attempt = 1;

    do
      {
	std::vector<std::pair<ptid_t,Event::const_ptr> >::iterator it;
	for (it = current_events.begin() ;
	    it != current_events.end(); ++it)
	  {
	    ptid_t ceptid = it->first;
	    event = it->second;
	    if (ptid.pid == -1
		|| (ceptid.pid == ptid.pid))
//	        || (ceptid.pid == ptid.pid
//		    && ceptid.lwp == ptid.lwp))
	      {
		CORE_ADDR pc = (the_low_target.get_pc) (event->getThread());
		DEBUG(event->name() << ' ' << ceptid.pid << '/' << ceptid.lwp << "pc=" << pc);
		return event;
	      }
	  }
	if (! Process::handleEvents(true))
	  return NULL_Event;
	DEBUG("after handleEvents",pid_to_string(ptid));
	attempt += 1;
      }
    while (attempt <= 2);

    DEBUG("returning null event");
    return NULL_Event;
  }

  bool is_stopped (Event::const_ptr event)
  {
    if (event != NULL)
      return event->getEventType().code() == EventType::Stop;
    else return false;
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

  Thread::const_ptr is_threadcreate (Event::const_ptr event)
  {
    Thread::const_ptr thr;
    if (event != NULL)
      {
	if (event->getEventType().code() == EventType::LWPCreate)
	  {
	    EventNewLWP::const_ptr newlwp_ev = event->getEventNewLWP();
	    if (event != NULL)
	      DEBUG(event->getEventType().code() << " LWPCreate");
	    return newlwp_ev->getNewThread();
	  }
	else if (event->getEventType().code() == EventType::UserThreadCreate)
	  {
	    EventNewUserThread::const_ptr newlwp_ev = event->getEventNewUserThread();
	    if (event != NULL)
	      DEBUG(event->getEventType().code() << " UserThreadCreate");
	    return newlwp_ev->getNewThread();
	  }
	else if (event->getEventType().code() == EventType::ThreadCreate)
	  {
	    EventNewUserThread::const_ptr newlwp_ev = event->getEventNewUserThread();
	    if (event != NULL)
	      DEBUG(event->getEventType().code() << " ThreadCreate");
	    return newlwp_ev->getNewThread();
	  }
      }
    return NULL_Thread;
  }

  Thread::const_ptr is_threaddestroy (Event::const_ptr event)
  {
    Thread::const_ptr thr;
    if (event != NULL)
      {
	if (event->getEventType().code() == EventType::LWPDestroy)
	  {
	    EventLWPDestroy::const_ptr lwpd_ev = event->getEventLWPDestroy();
	    if (event != NULL)
	      DEBUG(event->getEventType().code() << " LWPDestroy");
	    return lwpd_ev->getThread();
	  }
	else if (event->getEventType().code() == EventType::UserThreadDestroy)
	  {
	    EventUserThreadDestroy::const_ptr utd_ev = event->getEventUserThreadDestroy();
	    if (event != NULL)
	      DEBUG(event->getEventType().code() << " UserThreadDestroy");
	    return utd_ev->getThread();
	  }
	else if (event->getEventType().code() == EventType::ThreadDestroy)
	  {
	    EventThreadDestroy::const_ptr td_ev = event->getEventThreadDestroy();
	    if (event != NULL)
	      DEBUG(event->getEventType().code() << " ThreadDestroy");
	    return td_ev->getThread();
	  }
      }
    return NULL_Thread;
  }

  void dump (string comment)
  {
    int idx = 1;
    std::vector<std::pair<ptid_t,Event::const_ptr> >::iterator it;
    for (it = current_events.begin() ;
	it != current_events.end(); ++it)
      {
	Event::const_ptr event = it->second;
	if (event == NULL)
	  continue;
	DEBUG(dec << idx << hex << ' ' << comment << event->name(), pid_to_string (event));
	idx += 1;
      }

  }
} events;


void
pid_to_string (Dyninst::PID pid, Dyninst::LWP lwp, Dyninst::THR_ID tid, CORE_ADDR pc)
{
  if (pid == -1)
    cerr << "pid=*";
  else
    {
      cerr << "pid=" << pid;
      if (lwp < 0)
	cerr << " lwp=*";
      else
	cerr << " lwp=" << lwp;
      if (tid > 0)
	cerr << " thr=" << tid;

      string source = "";
      if (pc == 0)
	{
	  struct thread_info *ti = find_thread_ptid (ptid_build (pid, lwp, tid));
	  if (ti == NULL)
	    ti = find_thread_ptid (ptid_build (pid, lwp, 0));
	  if (ti != NULL)
	    {
	      struct regcache *regcache = get_thread_regcache (ti, 0);
	      pc = (*the_low_target.read_pc) (regcache);
	      source = " cached";
	    }
	}
      cerr << source << " pc=" << pc;
    }
  cerr << ' ';
}


static Thread::const_ptr dyninst_get_thread(ptid_t ptid);

/* Pretty print a pid/lwp/tid tuple */

void
pid_to_string (ptid_t ptid)
{
  CORE_ADDR pc;
  Thread::const_ptr thr = dyninst_get_thread(ptid);
  if (thr != NULL_Thread)
    pc = (the_low_target.get_pc) (thr);
  else
    pc = 0;
  pid_to_string (ptid.pid, ptid.lwp, ptid.tid, pc);
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

  CORE_ADDR pc = (the_low_target.get_pc) (thr);

  pid_to_string (thr->getProcess()->getPid(), thr->getLWP(), tid, pc);
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


static Thread::const_ptr
dyninst_get_thread(ptid_t ptid)
{
  struct thread_info *ti = find_thread_ptid (ptid);
  if (ti != NULL)
    return ((struct thread_info_private*)ti->target_data)->thread;
  else
    return NULL_Thread;
}


/* Add a PROCESS PID, that is possibly being ATTACHED */

static struct process_info *
dyninst_add_process (int pid, int attached, Process::ptr process)
{
  struct process_info *proc;

  proc = add_process (pid, attached);
  proc->tdesc = dyninst_tdesc;
  proc->priv = new struct process_info_private;
  proc->priv->process = process;
  return proc;
}

/* Add a THREAD to enable mapping a gdbserver thread to a dyninst Thread */

void
dyninst_add_thread(int pid, Thread::const_ptr thread)
{
  Dyninst::LWP lwp;
  client_state *cs = get_client_state ();

  if (thread != NULL)
    {
      struct thread_info_private *tip = new struct thread_info_private;
      tip->thread = thread;
      lwp = thread->getLWP();
      cs->ss->current_thread =  add_thread (ptid_build (pid, lwp, 0), tip);
    }
  else
    {
      cs->ss->current_thread = add_thread (ptid_build (pid, pid, 0), NULL);
    }
}


void
dyninst_remove_thread(Thread::const_ptr thread)
{
  if (thread != NULL)
    {
      ptid_t ptid = ptid_build (thread->getProcess()->getPid(), thread->getLWP(), 0);
      struct thread_info *ti = find_thread_ptid (ptid);
      if (ti != NULL)
	{
	  delete (struct thread_info_private*)ti->target_data;
	  ti->target_data = NULL;
	  remove_thread (ti);
	}
    }
}


/* Get the dyninst Thread corresponding to the current thread */

static Thread::const_ptr
dyninst_get_inferior_thread()
{
  client_state *cs = get_client_state ();

  struct thread_info_private *tip = (struct thread_info_private*)(cs->ss->current_thread->target_data);
  if (!tip)
    DYNERR ("No inferior thread");
  return tip->thread;
}


/* Generic dyninst registerEventCallback handler */

Process::cb_ret_t
signal_handler(Event::const_ptr ev)
{
  DEBUG(ev->name());
  if (ev->name() != "post-LWPDestroy")
    events.insert(ev);
  return Process::cbDefault;
}


/* Handle a dyninst Breakpoint event */

Process::cb_ret_t
breakpoint_handler(Event::const_ptr ev)
{
  EventBreakpoint::const_ptr bp_ev = ev->getEventBreakpoint();
  DEBUG('@' << bp_ev->getAddress() << " lwp=" << bp_ev->getThread()->getLWP());
  events.insert(ev);
  return Process::cbDefault;
}


/* Handle a dyninst SingleStep event */

Process::cb_ret_t
singlestep_handler(Event::const_ptr ev)
{
  DEBUG(ev->name());
  events.insert(ev);
  ev->getThread()->setSingleStepMode(false);
  return Process::cbThreadStop;
}


// Handle a dyninst LWPDestroy event

Process::cb_ret_t
lwpdestroy_handler(Event::const_ptr ev)
{
  Process::cb_ret_t cbret (Process::cbProcStop, Process::cbProcStop);
  DEBUG(ev->name());
  if (ev->name() != "post-LWPDestroy")
    events.insert(ev);
  return cbret;
}


/* Handle a dyninst Stop event */

Process::cb_ret_t
stop_handler(Event::const_ptr ev)
{
  DEBUG(ev->name() << " stopped="  << ev->getThread()->isStopped() << " running=" << ev->getThread()->isRunning(), pid_to_string (ev->getThread()));
  events.insert(ev);
  return Process::cbDefault;
}


/* Handle a dyninst Library event */

Process::cb_ret_t
library_handler(Event::const_ptr ev)
{
  EventLibrary::const_ptr lib_ev = ev->getEventLibrary();

  for (set<Library::ptr>::const_iterator i = lib_ev->libsAdded().begin(); i != lib_ev->libsAdded().end(); i++)
    {
      Library::ptr lib = *i;
      DEBUG("added library " << lib->getAbsoluteName() << ' ' << lib->getDynamicAddress() << ' ' << lib->getLoadAddress() << endl);
    }

  return Process::cbDefault;
}


void
myregisterCB(EventType et, Process::cb_func_t f)
{
  if (! Process::registerEventCallback(et, f))
    cout << "Error registering thread callback " << et.name() << '\n';
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
    DYNERRMSG ("No such file: %s\n", exec.c_str());
  dyninst_procset->insert(dyninst_process);

  myregisterCB(EventType::Bootstrap, signal_handler);
  myregisterCB(EventType::Breakpoint, breakpoint_handler);
  myregisterCB(EventType::Crash, signal_handler);
  myregisterCB(EventType::Exec, signal_handler);
  myregisterCB(EventType::Exit, signal_handler);
  myregisterCB(EventType::Fork, signal_handler);
  myregisterCB(EventType::LWPCreate, signal_handler);
  myregisterCB(EventType::LWPDestroy, lwpdestroy_handler);
  myregisterCB(EventType::Library, library_handler);
  myregisterCB(EventType::RPC, signal_handler);
  myregisterCB(EventType::Signal, signal_handler);
  myregisterCB(EventType::SingleStep, singlestep_handler);
  myregisterCB(EventType::Stop, stop_handler);
  myregisterCB(EventType::Terminate, signal_handler);
  // ThreadCreate, UserThreadCreate handled via LWPCreate
  // ThreadDestroy, UserThreadDestroy handled via LWPDestroy


  DEBUG("created process " << dyninst_process->getPid() << program);

  pid_t pid = dyninst_process->getPid();
  dyninst_add_process (pid, 0, dyninst_process);

  ThreadPool::iterator thidx;
  for (thidx = dyninst_process->threads().begin();
      thidx != dyninst_process->threads().end(); thidx++)
    {
      bool reg_map_setup = false;
      RegisterPool regpool;
      DEBUG("created thread " << (*thidx)->getTID() << ' ' << (*thidx)->getLWP());
      Thread::ptr th = *thidx;
      dyninst_add_thread (pid, th);
      if (! reg_map_setup)
	{
	  th->getAllRegisters (regpool);
	  (*the_low_target.reg_map_setup)(regpool);
	  reg_map_setup = true;
	}
    }

  if (! dyninst_process->stopProc())
    DYNERRMSG ("Cannot stop process %ld", (long int)pid);
  return dyninst_process->getPid();
}

/* Attach to a running process */

static int
dyninst_attach (unsigned long pid)
{
  ProcessSet::iterator procset_it = dyninst_procset->find(pid);
  if (procset_it != dyninst_procset->end())
    DEBUG("already attached to pid " << pid);
  Process::ptr dyninst_process = Process::attachProcess(pid);
  if (dyninst_process == Process::ptr())
    DYNERRMSG ("Cannot attach to process %ld", pid);

  dyninst_procset->insert(dyninst_process);

  DEBUG("pid=" << pid);
  myregisterCB(EventType::Bootstrap, signal_handler);
  myregisterCB(EventType::Breakpoint, breakpoint_handler);
  myregisterCB(EventType::Crash, signal_handler);
  myregisterCB(EventType::Exec, signal_handler);
  myregisterCB(EventType::Exit, signal_handler);
  myregisterCB(EventType::Fork, signal_handler);
  myregisterCB(EventType::LWPCreate, signal_handler);
  myregisterCB(EventType::LWPDestroy, signal_handler);
  myregisterCB(EventType::Library, library_handler);
  myregisterCB(EventType::RPC, signal_handler);
  myregisterCB(EventType::Signal, signal_handler);
  myregisterCB(EventType::SingleStep, singlestep_handler);
  myregisterCB(EventType::Stop, stop_handler);
  myregisterCB(EventType::Terminate, signal_handler);
  // ThreadCreate, UserThreadCreate handled via LWPCreate
  // ThreadDestroy, UserThreadDestroy handled via LWPDestroy


  if (dyninst_process == Process::ptr())
    DYNERRMSG ("Cannot attach to process %lu", pid);

  dyninst_add_process (pid, 1, dyninst_process);
  ThreadPool::iterator thidx;
  for (thidx = dyninst_process->threads().begin();
      thidx != dyninst_process->threads().end(); thidx++)
    {
      bool reg_map_setup = false;
      RegisterPool regpool;
      DEBUG("created thread " << (*thidx)->getTID() << ' ' << (*thidx)->getLWP());
      Thread::const_ptr th = *thidx;
      dyninst_add_thread (pid, th);
      if (! reg_map_setup)
	{
	  th->getAllRegisters (regpool);
	  (*the_low_target.reg_map_setup)(regpool);
	  reg_map_setup = true;
	}
    }

  LibraryPool::iterator libidx;
  for (libidx = dyninst_process->libraries().begin(); libidx != dyninst_process->libraries().end(); libidx++)
    {
      Library::ptr lib = *libidx;
      DEBUG("added library " << lib->getName() << ' ' << lib->getDynamicAddress() << ' ' << lib->getLoadAddress() << endl);
    }

  return 0;
}

/* Continue a thread described by RESUME_INFO */

static void
dyninst_resume (struct thread_resume *resume_info, size_t n)
{
  client_state *cs = get_client_state ();


  DEBUG_ENTER ();

  for (int i = 0; i < (int)n; i++)
    {
      ptid_t ptid = resume_info[i].thread;

      if (ptid_equal(ptid, minus_one_ptid))
	ptid = thread_to_gdb_id (cs->ss->current_thread);

      Dyninst::PID pid = ptid_get_pid (ptid);
      ProcessSet::iterator procset_it = dyninst_procset->find(pid);
      if (procset_it == dyninst_procset->end())
	DYNERR ("Cannot resume process %lu\n", (long unsigned)pid);

      Process::ptr dyninst_process = *procset_it;

      ThreadPool::iterator thidx;
      Thread::ptr th;

      if (dyninst_process->isTerminated())
	{
	  DEBUG_EXIT ();
	  return;
	}

      regcache_invalidate ();

      if (resume_info[i].thread == minus_one_ptid
	  || ptid_get_lwp (resume_info[i].thread) == -1
	  || ptid_is_pid (resume_info[i].thread))
	{
	  DEBUG("before continue pid=" << pid);
	  if (! dyninst_process->continueProc())
	    DEBUG("Unable to continueProc " << dyninst_process->getLastErrorMsg());
	  DEBUG_EXIT ();
	  return;
	}

      for (thidx = dyninst_process->threads().begin();
	   thidx != dyninst_process->threads().end(); thidx++)
	{
	  th = *thidx;
	  if (th->getLWP() == ptid.lwp)
	    {
	      // resume_continue, resume_step, resume_stop
	      struct thread_info *ti = find_thread_ptid (ptid);
	      struct thread_info_private *tip = (struct thread_info_private*)(ti->target_data);
	      tip->step_range_start = resume_info->step_range_start;
	      tip->step_range_end = resume_info->step_range_end;
	      struct regcache *regcache = get_thread_regcache (ti, 1);
	      CORE_ADDR pc = (*the_low_target.read_pc) (regcache);
	      DEBUG("range " << resume_info->step_range_start << "/" << resume_info->step_range_end << " pc=" << pc << " kind=" << resume_info[i].kind);

	      switch (resume_info[i].kind)
		{
		case resume_step:
		    MachRegisterVal result;
		    th->getRegister(MachRegister(x86_64::rip), result);
		    DEBUG("in step mode @" << result, pid_to_string (th));
		    if (! th->setSingleStepMode(true))
		      DYNERRMSG("Unable to setSingleStepMode");
		case resume_continue:
		    regcache_invalidate_thread (ti);
		    if (! th->continueThread())
		      DYNERRMSG("Unable to continueThread");
		    break;
		case resume_stop:
		  if (! th->stopThread())
		    DYNERRMSG("Unable to stopThread");
		}
	    }
	}
    }
  DEBUG_EXIT ();
}


/* Handle a vCont packet */

static void dyninst_continue (ptid_t) __attribute__ ((unused));

static void
dyninst_continue (ptid_t ptid)
{
  struct thread_resume resume_info;

  DEBUG("", pid_to_string (ptid));
  
  resume_info.thread = ptid;
  resume_info.kind = resume_continue;
  resume_info.sig = 0;

  dyninst_resume (&resume_info, 1);
}


/* True if LWP is stopped in its stepping range.  */

bool
in_step_range ()
{
  client_state *cs = get_client_state ();

  struct thread_info_private *tip = (struct thread_info_private*)(cs->ss->current_thread->target_data);
  struct regcache *regcache = get_thread_regcache (cs->ss->current_thread, 1);
  CORE_ADDR pc = (*the_low_target.read_pc) (regcache);

  return (pc >= tip->step_range_start && pc < tip->step_range_end);
}


/* Wait for a thread PTID having a target STATUS */

static ptid_t
dyninst_wait_1 (ptid_t ptid, struct target_waitstatus *status, int options)
{
  ptid_t new_ptid = ptid;
  Dyninst::PID pid = ptid.pid;
  bool thread_created = false;
  client_state *cs = get_client_state ();

  Event::const_ptr event;

  DEBUG_ENTER ();
  while (true)		// wait events loop
    {
      if ((event = events.get(new_ptid)))
	{
	  struct process_info *pi;
	  pid = (ptid.pid != -1) ? ptid.pid : event->getProcess()->getPid();
	  new_ptid = ptid_build (pid, pid, 0);
	  pi = find_process_pid(pid);
	  if (pi && pi->priv)
	    pi->priv->last_wait_event_ptid = new_ptid;
	}
      if (event == NULL)
	{
	  event = events.get(new_ptid);
//	  DEBUG("", pid_to_string (new_ptid));
	}
      else
	DEBUG("", pid_to_string (event));


      Thread::const_ptr new_thr;
      if ((new_thr = events.is_threadcreate(event)) != NULL_Thread)
	{
	  DEBUG("threadcreate pid=" << pid << " New thread: " << event->getThread()->getLWP() << ' ' << ((new_thr != NULL_Thread) ? new_thr->getLWP() : 0), pid_to_string (new_ptid));
	  dyninst_add_thread (new_thr->getProcess()->getPid(), new_thr);
	  events.erase(event);
	  thread_created = true;
	  continue;		// wait events loop
	}
      // LWPCreate pre-LWPDestroy post-LWPDestroy
      else if ((new_thr = events.is_threaddestroy(event)) != NULL_Thread)
	{
	  DEBUG("threaddestroy pid=" << pid << " New thread: " << event->getThread()->getLWP() << ' ' << ((new_thr != NULL_Thread) ? new_thr->getLWP() : 0), pid_to_string (new_ptid));
	  struct thread_info* ti = find_thread_ptid (ptid_build (new_thr->getProcess()->getPid(), new_thr->getLWP(), 0));
	  if (ti)
	    remove_thread (ti);
	  events.erase(event);
	  continue;		// wait events loopNULL
	}
      else if (events.is_exit(event))
	{
	  EventExit::const_ptr exit_ev = event->getEventExit();
	  events.erase(event);
	  status->kind = TARGET_WAITKIND_EXITED;
	  status->value.integer = exit_ev->getExitCode();
	  DEBUG("exit process with status: " << status->value.integer);
	}
      else if (events.is_stopped(event))
	{
	  events.erase(event);
	  status->kind = TARGET_WAITKIND_STOPPED;
	  status->value.integer = SIGSTOP;
	  DEBUG("stop process with status: " << status->value.integer, pid_to_string(event));
	  if (thread_created)
	    {
	      Thread::const_ptr thr = event->getThread();
	      thread_created = false;
	    }
	  continue;
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
		  DEBUG("breakpoint " << bp->getToAddress());
		}
	    }
	  events.erase(event);
	  status->kind = TARGET_WAITKIND_STOPPED;
	  // EventStop doesn't have a signal member
	  status->value.integer = gdb_signal_from_host (SIGTRAP);
	  DEBUG("returning ", pid_to_string (new_ptid));
	  cs->ss->current_thread = find_thread_ptid (new_ptid);
	  DEBUG_EXIT ();
	  return ptid_build (event->getProcess()->getPid(), event->getThread()->getLWP(), 0);
	}
      else if (events.is_signal(event))
	{
	  events.erase(event);
	  DEBUG("erase");
	  status->kind = TARGET_WAITKIND_SIGNALLED;
	  EventSignal::const_ptr signal_ev = event->getEventSignal();
	  DEBUG("signal" << signal_ev->name());
	  status->value.integer = signal_ev->getSignal();
	  DEBUG("process signalled with signal: " << status->value.integer);
	}
      else
	{
	  events.erase(event);
	  status->kind = TARGET_WAITKIND_STOPPED;
	  status->value.integer = gdb_signal_from_host (0);
	  DEBUG("process received: " << (event != NULL ? event->name() : ""));
	}
      break;
    }

  DEBUG("Returning", pid_to_string (ptid_of(cs->ss->current_thread)));
  DEBUG_EXIT ();
  return ptid_of (cs->ss->current_thread);
}


/*  Wait for a thread PTID */

static ptid_t
dyninst_wait (ptid_t ptid, struct target_waitstatus *status, int options)
{
  ptid_t new_ptid;

  DEBUG("", pid_to_string (ptid));
  new_ptid = dyninst_wait_1 (ptid, status, options);

  DEBUG("kind=" << status->kind, pid_to_string (new_ptid));
  return new_ptid;
}

/* Kill a PID */

static int
dyninst_kill (Dyninst::PID pid)
{
  struct process_info *process;
  ProcessSet::iterator procset_it = dyninst_procset->find(pid);
  if (procset_it == dyninst_procset->end())
    return 1;
  Process::ptr dyninst_process = *procset_it;

  process = find_process_pid (pid);
  if (process == NULL)
    return -1;

  if (! dyninst_process->stopProc())
    DYNERRMSG ("Unable to stop process %ld", (long)pid);
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
  clear_inferiors ();
// TODO
  ThreadPool::iterator thidx;

  Process::ptr dyninst_process;
  if (false && proc->priv)
    {
      dyninst_process = ((struct process_info_private*)(proc->priv))->process;
      for (thidx = dyninst_process->threads().begin();
	  thidx != dyninst_process->threads().end(); thidx++)
	{
	  Thread::const_ptr th = *thidx;
	  dyninst_remove_thread (th);
	}
    }
  delete proc->priv;
  proc->priv = NULL;
}


static void
dyninst_join (Dyninst::PID pid)
{
  Event::const_ptr event;

  while (true)
    {
      ptid_t new_ptid = ptid_build (pid, pid, 0);
      ProcessSet::iterator procset_it = dyninst_procset->find(pid);
      Process::ptr dyninst_process = *procset_it;
      if (! dyninst_process->hasRunningThread())
	break;
      if ((event = events.get(new_ptid)))
	{
	  if (events.is_exit(event))
	    {
	      EventExit::const_ptr exit_ev = event->getEventExit();
	      DEBUG("process exited with status: " << exit_ev->getExitCode());
	      break;
	    }
	}
    }

}


static int
dyninst_thread_alive (ptid_t ptid)
{
  /* The list of threads is updated at the end of each wait, so it
     should be up to date.  No need to re-fetch it.  */
  return (find_thread_ptid (ptid) != NULL);
}

/* Fetch the current thread's registers into REGCACHE */


RegisterPool regpool;

static void
dyninst_fetch_registers (struct regcache *regcache, int regno)
{
  Thread::const_ptr thr = dyninst_get_inferior_thread();
  Thread::ptr th = boost::const_pointer_cast<Thread>(thr);
  Process::const_ptr dyninst_process = thr->getProcess();
  Process::ptr dyninst_proc = boost::const_pointer_cast<Process>(dyninst_process);

  struct dyninst_regset_info *regset = dyninst_target_regsets;

  enum {cont_none, cont_thread, cont_proc} howto_continue;

  howto_continue = cont_none;
  if (! th->isStopped())
    {
      if (! dyninst_proc->stopProc())
	{
	  DYNERRMSG ("Unable to stop process %ld", (long)th->getProcess()->getPid());
	}
      else
	howto_continue = cont_proc;
    }

  if (! th->getAllRegisters(regpool))
    {
      DEBUG("getAllRegisters " << getLastErrorMsg());
    }

  switch (howto_continue)
    {
    case (cont_thread):
      th->continueThread();
      break;
    case (cont_proc):
      dyninst_proc->continueProc();
      break;
    case (cont_none):
	;
    }

  regset->fill_function (regcache, regpool);
}

/* Store the current threads registers from REGCACHE */

static void
dyninst_store_registers (struct regcache *regcache, int regno)
{
  Thread::const_ptr thr = dyninst_get_inferior_thread();
  Thread::ptr th = boost::const_pointer_cast<Thread>(thr);
  Process::const_ptr dyninst_process = thr->getProcess();
  Process::ptr dyninst_proc = boost::const_pointer_cast<Process>(dyninst_process);

  DEBUG("regno=" << regno, pid_to_string (th));

  struct dyninst_regset_info *regset = dyninst_target_regsets;

  enum {cont_none, cont_thread, cont_proc} howto_continue;

  howto_continue = cont_none;
  if (! th->isStopped())
    {
      if (! dyninst_proc->stopProc())
	{
	  DYNERRMSG ("Unable to stop process %ld", (long)th->getProcess()->getPid());
	}
      else
	howto_continue = cont_proc;
    }

  if (! th->getAllRegisters(regpool))
    {
      DEBUG(getLastErrorMsg(), pid_to_string (th));
    }

  regset->store_function (regcache, regpool);
  if (! th->setAllRegisters(regpool))
    {
      DEBUG(getLastErrorMsg(), pid_to_string (th));
    }

  switch (howto_continue)
    {
    case (cont_thread):
      th->continueThread();
      break;
    case (cont_proc):
      dyninst_proc->continueProc();
      break;
    case (cont_none):
	;
    }
}


static void
dyninst_process_qsupported (const char *query)
{
  if (the_low_target.process_qsupported != NULL)
    the_low_target.process_qsupported (query);
}


static CORE_ADDR dyninst_get_pc (Thread::ptr) __attribute__ ((unused));

static CORE_ADDR
dyninst_get_pc (Thread::ptr thr)
{
  CORE_ADDR pc = (*the_low_target.get_pc) (thr);
  return pc;
}

static CORE_ADDR
dyninst_read_pc (struct regcache *regcache)
{
  if (the_low_target.read_pc == NULL)
    return 0;

  CORE_ADDR pc = (*the_low_target.read_pc) (regcache);
  DEBUG("pc is " << pc);
  return pc;
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
  DEBUG("memaddr=" <<  memaddr << " myaddr=" << (void*)myaddr);
  Thread::const_ptr th = dyninst_get_inferior_thread();
  Process::const_ptr dyninst_process = th->getProcess();
  bool result;

  if (!(result = dyninst_process->readMemory((void*)myaddr, (Dyninst::Address)memaddr, len)))
    DYNERRMSG("Unable to read memory at %#lx", (long unsigned)memaddr);

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

  return result == false;
}

/* Copy LEN bytes of data from debugger memory at MYADDR to inferior's
   memory at MEMADDR.  On failure returns the value of errno. */


static int
dyninst_write_memory (CORE_ADDR memaddr, const unsigned char *myaddr, int len)
{
  DEBUG("memaddr=" << memaddr << " myaddr=" << (void*)myaddr);
  Thread::const_ptr th = dyninst_get_inferior_thread();
  Process::const_ptr dyninst_process = th->getProcess();
  bool result;

  if (! (result = dyninst_process->writeMemory((Dyninst::Address)memaddr, (void*)myaddr, len)))
    DYNERRMSG("Unable to write memory at %#lx", (long unsigned)memaddr);
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

  return result == false;
}

/* Kill the current process */

static void
dyninst_request_interrupt (void)
{
  client_state *cs = get_client_state ();

  Dyninst::PID pid = ptid_get_pid (thread_to_gdb_id (cs->ss->current_thread));
  ProcessSet::iterator procset_it = dyninst_procset->find(pid);
  Process::ptr dyninst_process = *procset_it;

  if (! dyninst_process->terminate())
    DYNERRMSG ("Unable to interrupt process %ld", (long)pid);
}


/* Copy LEN bytes from inferior's auxiliary vector starting at OFFSET
   to debugger memory starting at MYADDR.  */

static int
dyninst_read_auxv (CORE_ADDR offset, unsigned char *myaddr, unsigned int len)
{
  char filename[PATH_MAX];
  int fd, n;
  client_state *cs = get_client_state ();
  int pid = lwpid_of (cs->ss->current_thread);

  xsnprintf (filename, sizeof filename, "/proc/%d/auxv", pid);

  fd = open (filename, O_RDONLY);
  if (fd < 0)
    return -1;

  if (offset != (CORE_ADDR) 0
      && lseek (fd, (off_t) offset, SEEK_SET) != (off_t) offset)
    n = -1;
  else
    n = read (fd, myaddr, len);

  close (fd);

  return n;
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
  Thread::const_ptr thr = dyninst_get_inferior_thread();
  Thread::ptr th = boost::const_pointer_cast<Thread>(thr);
  Process::const_ptr dyninst_process = th->getProcess();
  Process::ptr dyninst_proc = boost::const_pointer_cast<Process>(dyninst_process);
  bool result;
  enum {cont_none, cont_thread, cont_proc} howto_continue;

  Breakpoint::ptr brp = Breakpoint::newBreakpoint();
  DEBUG(" addr=" << addr << ' ' << brp->getToAddress(), pid_to_string(th));
  brp->setData(bp);

  howto_continue = cont_none;
  if (! dyninst_proc->allThreadsStopped())
    {
      if (! dyninst_proc->stopProc())
	{
	  DYNERRMSG ("Unable to stop process %ld", (long)th->getProcess()->getPid());
	}
      else
	howto_continue = cont_proc;
    }

  if (! (result = dyninst_process->addBreakpoint(addr, brp)))
    DYNERRMSG ("Unable insert breakpoint at %#lx", (long)addr);
  dyninst_bpset.push_back(brp);

  switch (howto_continue)
    {
    case (cont_thread):
      th->continueThread();
      break;
    case (cont_proc):
      dyninst_proc->continueProc();
      break;
    case (cont_none):
	;
    }

  return 0;
}

/* Remove a breakpoint BP of TYPE at ADDR for LEN bytes */

static int
dyninst_remove_point (enum raw_bkpt_type type, CORE_ADDR addr, int len,
		      struct raw_breakpoint *rbp)
{
  Thread::const_ptr thr = dyninst_get_inferior_thread();
  Thread::ptr th = boost::const_pointer_cast<Thread>(thr);
  Process::const_ptr dyninst_process = thr->getProcess();
  Process::ptr dyninst_proc = boost::const_pointer_cast<Process>(dyninst_process);

  enum {cont_none, cont_thread, cont_proc} howto_continue;

  howto_continue = cont_none;
  if (! dyninst_proc->allThreadsStopped())
    {
      if (! dyninst_proc->stopProc())
	{
	  DYNERRMSG ("Unable to stop process %ld", (long)th->getProcess()->getPid());
	}
      else
	howto_continue = cont_proc;
    }

  std::vector<Breakpoint::ptr>::iterator it;
  bool result = false;

  for (it = dyninst_bpset.begin(); it != dyninst_bpset.end(); it++)
    {
      Breakpoint::ptr bp = *it;
      if ((struct raw_breakpoint*)bp->getData() == rbp)
	{
	  DEBUG(" addr=" << addr << ' ' << bp->getToAddress(), pid_to_string(th));
	  if (! (result = dyninst_process->rmBreakpoint(addr, bp)))
	    DYNERRMSG ("Unable remove breakpoint at %#lx", (long)addr);
	  break;
	}
    }

  dyninst_bpset.erase(it);

  switch (howto_continue)
    {
    case (cont_thread):
      break;
    case (cont_proc):
      break;
    case (cont_none):
	;
    }

  return 0;
}


static int
dyninst_supports_non_stop (void)
{
  return 1;
}


static int
dyninst_supports_multi_process (void)
{
  return 1;
}


static int
dyninst_supports_range_stepping (void)
{
  if (*the_low_target.supports_range_stepping == NULL)
    return 0;

  return (*the_low_target.supports_range_stepping) ();
}


static int
dyninst_qxfer_libraries_svr4 (const char *annex, unsigned char *readbuf,
			    unsigned const char *writebuf,
			    CORE_ADDR offset, int len)
{
  // <library-list-svr4 version="1.0" main-lm="0x320cc21168">
  // <library name="/lib64/libc.so.6" lm="0x7ff37530a700" l_addr="0x0" l_ld="0x320d1b6b40"/>
  // <library name="/lib64/ld-linux-x86-64.so.2" lm="0x320cc20998" l_addr="0x0" l_ld="0x320cc1fe10"/>
  // </library-list-svr4>

  Thread::const_ptr th = dyninst_get_inferior_thread();
  Process::const_ptr dyninst_proc = th->getProcess();

  string document = "<library-list-svr4 version=\"1.0\">";

  LibraryPool::const_iterator libidx;
  bool first = true;
  for (libidx = dyninst_proc->libraries().begin(); libidx != dyninst_proc->libraries().end(); libidx++)
    {
      Library::const_ptr lib = *libidx;
      if (first)
	{
	  first = false;
	  continue;
	}
      DEBUG("added library " << lib->getName() << ' ' << lib->getDynamicAddress() << ' ' << lib->getLoadAddress() << endl);
      std::ostringstream oss;

      oss << "<library name=\"" << lib->getAbsoluteName()
	  << "\" lm=\"0x0\" "
	  << "l_addr=\"0x0\" "
	  << "l_ld=\"" << hex <<  lib->getDynamicAddress() << "\"/>";
      document += oss.str();
    }
  document += "</library-list-svr4>";
  document.copy((char*)readbuf, document.length());
  readbuf[document.length()]='\0';

  return document.length();
}


// Return non-zero if HEADER is a 64-bit ELF file. (from linux-low)

#if 0
static int
elf_64_header_p (const Elf64_Ehdr *header, unsigned int *machine)
{
  if (header->e_ident[EI_MAG0] == ELFMAG0
      && header->e_ident[EI_MAG1] == ELFMAG1
      && header->e_ident[EI_MAG2] == ELFMAG2
      && header->e_ident[EI_MAG3] == ELFMAG3)
    {
      *machine = header->e_machine;
      return header->e_ident[EI_CLASS] == ELFCLASS64;

    }
  *machine = EM_NONE;
  return -1;
}

// Return non-zero if FILE is a 64-bit ELF file,
// zero if the file is not a 64-bit ELF file,
// and -1 if the file is not accessible or doesn't exist.

static int
elf_64_file_p (const char *file, unsigned int *machine)
{
  Elf64_Ehdr header;
  int fd;

  fd = open (file, O_RDONLY);
  if (fd < 0)
    return -1;

  if (read (fd, &header, sizeof (header)) != sizeof (header))
    {
      close (fd);
      return 0;
    }
  close (fd);

  return elf_64_header_p (&header, machine);
}
#endif


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
  dyninst_read_auxv,
  dyninst_supports_z_point_type,
  dyninst_insert_point,  /* insert_point */
  dyninst_remove_point,  /* remove_point */
  NULL,  // stopped_by_sw_breakpoint
  NULL,  // supports_stopped_by_sw_breakpoint
  NULL,  // stopped_by_hw_breakpoint
  NULL,  // supports_stopped_by_hw_breakpoint
  NULL,  // supports_conditional_breakpoints
  NULL,  // stopped_by_watchpoint
  NULL,  // stopped_data_address
  NULL,  // read_offsets
  NULL,  // get_tls_address
  NULL,  // qxfer_spu
  NULL,  // hostio_last_error
  NULL,  // qxfer_osdata
  NULL,  // qxfer_siginfo
  dyninst_supports_non_stop,
  NULL,  // async
  NULL,  // start_non_stop
  dyninst_supports_multi_process,
  NULL,  // supports_fork_events
  NULL,  // supports_vfork_events
  NULL,  // handle_new_gdb_connection
  NULL,  // handle_monitor_command
  NULL,  // core_of_thread
  NULL,  // read_loadmap
  dyninst_process_qsupported,  // process_qsupported
  NULL,  // supports_tracepoints
  dyninst_read_pc,
  dyninst_write_pc,
  NULL,  // thread_stopped
  NULL,  // get_tib_address
  NULL,  // pause_all
  NULL,  // unpause_all
  NULL,  // stabilize_threads
  NULL,  // install_fast_tracepoint_jump_pad
  NULL,  // emit_ops
  NULL,  // supports_disable_randomization
  NULL,  // get_min_fast_tracepoint_insn_len
  dyninst_qxfer_libraries_svr4,  // qxfer_libraries_svr4
  NULL,  // supports_agent
  NULL,  // supports_btrace
  NULL,  // enable_btrace
  NULL,  // disable_btrace
  NULL,  // read_btrace
  NULL,  // read_btrace_conf
  dyninst_supports_range_stepping,
  NULL   // pid_to_exec_file
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
