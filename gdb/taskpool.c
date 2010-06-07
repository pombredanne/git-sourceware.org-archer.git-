/* Task pool implementation.

   Copyright (C) 2009, 2010 Free Software Foundation, Inc.

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
#include "exceptions.h"
#include "vec.h"
#include "taskpool.h"

#ifdef HAVE_PTHREAD_H
#include <pthread.h>
#endif
#include <time.h>
#include <sys/time.h>
#include <signal.h>

/* To port the threading code to a new host, you must define types and
   functions for the taskpool implementation to use.  You can use the
   pthreads code as a guide.
  
   We use pthread_create as a sentinel for all the pthread
   functionality.  */
#ifdef HAVE_PTHREAD_CREATE

/* The type of a mutex.  */
typedef pthread_mutex_t gdb_mutex;

/* The type of a condition variable.  */
typedef pthread_cond_t gdb_condition;

/* Initialize a mutex.  */
static inline void
gdb_mutex_init (gdb_mutex *m)
{
  pthread_mutex_init (m, NULL);
}

/* Destroy a mutex.  */
static inline void
gdb_mutex_destroy (gdb_mutex *m)
{
  pthread_mutex_destroy (m);
}

/* Acquire a mutex.  */
static inline void
gdb_mutex_lock (gdb_mutex *m)
{
  pthread_mutex_lock (m);
}

/* Try for N_SECONDS seconds to acquire the lock M.  If the lock is
   acquired, return 0.  Otherwise, return nonzero.  */
static inline int
gdb_mutex_timed_lock (gdb_mutex *m, unsigned int n_seconds)
{
  struct timespec ts;
  struct timeval tv;

  gettimeofday (&tv, NULL);
  ts.tv_sec = tv.tv_sec + n_seconds;
  ts.tv_nsec = tv.tv_usec * 1000;

  return pthread_mutex_timedlock (m, &ts);
}

/* Release a mutex.  */
static inline void
gdb_mutex_unlock (gdb_mutex *m)
{
  pthread_mutex_unlock (m);
}

/* Initialize the condition variable C.  */
static inline void
gdb_cond_init (gdb_condition *c)
{
  pthread_cond_init (c, NULL);
}

/* Wait on the condition variable C.  The mutex M is associated with
   C; it will already have been acquired by the caller.  Wait for up
   to N_SECONDS seconds; return 0 on success, nonzero if the condition
   was never triggered.  */
static inline int
gdb_cond_timed_wait (gdb_condition *c, gdb_mutex *m, unsigned int n_seconds)
{
  struct timespec ts;
  struct timeval tv;

  gettimeofday (&tv, NULL);
  ts.tv_sec = tv.tv_sec + n_seconds;
  ts.tv_nsec = tv.tv_usec * 1000;

  return pthread_cond_timedwait (c, m, &ts);
}

/* Signal the condition variable.  */
static inline void
gdb_cond_signal (gdb_condition *c)
{
  pthread_cond_signal (c);
}

/* Start a new thread.  A new thread should be run in the background
   -- it should not prevent gdb from exiting.  Also, care must be
   taken to arrange for signals to be delivered to gdb's main thread.
   FUNC is called in the new thread, with DATUM as its argument.  */
static inline void
gdb_create_new_thread (void *(func) (void *), void *datum)
{
  pthread_attr_t attr;
  pthread_t tem;
  sigset_t block, old;

  /* Temporarily block every signal.  This is used to prevent the
     worker threads from accepting any signal that must be
     delivered to the main thread.  */
  sigfillset (&block);
  pthread_sigmask (SIG_BLOCK, &block, &old);

  pthread_attr_init (&attr);
  pthread_attr_setdetachstate (&attr, PTHREAD_CREATE_DETACHED);

  pthread_create (&tem, &attr, func, datum);

  pthread_attr_destroy (&attr);
  pthread_sigmask (SIG_SETMASK, &old, NULL);
}

/* Return the number of processors on this system, or -1 if this is
   unknown.  If this function returns 0, gdb assumes that no threads
   are available at all.  */
static inline int
gdb_n_processors (void)
{
#ifdef _SC_NPROCESSORS_ONLN
  int n = (int) sysconf (_SC_NPROCESSORS_ONLN);
  if (n == 0)
    n = -1;
  return n;
#else
  return -1;
#endif
}

#else /* HAVE_PTHREAD_CREATE */

/* Threadless implementation.  */

typedef int gdb_mutex;
typedef int gdb_condition;

static inline void
gdb_mutex_init (gdb_mutex *m)
{
  *m = 0;
}

static inline void
gdb_mutex_destroy (gdb_mutex *m)
{
}

static inline void
gdb_mutex_lock (gdb_mutex *m)
{
}

static inline int
gdb_mutex_timed_lock (gdb_mutex *m, unsigned int n_seconds)
{
  return 0;
}

static inline void
gdb_mutex_unlock (gdb_mutex *m)
{
}

static inline void
gdb_cond_init (gdb_condition *c)
{
  *c = 0;
}

static inline int
gdb_cond_timed_wait (gdb_condition *c, gdb_mutex *m, unsigned int n_seconds)
{
  return 1;
}

static inline void
gdb_cond_signal (gdb_condition *c)
{
}

static inline void
gdb_create_new_thread (void *(func) (void *), void *datum)
{
  internal_error (__FILE__, __LINE__,
		  _("should not be possible to start a new thread"));
}

static inline int
gdb_n_processors (void)
{
  return 0;
}

#endif /* HAVE_PTHREAD_CREATE */



typedef struct task *task_p;

DEF_VEC_P (task_p);

/* A task pool.  This holds a number of threads, plus a task queue.  */
struct task_pool
{
  /* The task queue.  */
  VEC (task_p) *queue;

  /* The queue lock.  This must be held when modifying any element of
     the task pool.  */
  gdb_mutex lock;

  /* The queue condition.  This is signaled when a new task is pushed
     on the queue.  */
  gdb_condition condition;

  /* The number of worker threads present active.  */
  int worker_count;
  /* The number of worker threads waiting for a new task.  */
  int waiters;
  /* The maximum number of worker threads.  */
  int max_workers;
};

/* A single task in a task pool.  */
struct task
{
  /* The task's message.  */
  char *message;

  /* The task's priority.  */
  unsigned long priority;

  /* The function that implements this task.  It either returns a
     result, or throws an exception.  */
  void *(*function) (void *arg);
  /* The user's data that is passed to the function.  If this needs
     some form of destruction, then FUNCTION is responsible for
     destroying it.  */
  void *user_data;

  /* If zero, the task has not been run.  If greater than zero, the
     task completed successfully, and the result is in RESULT.  If
     less than zero, then the task failed and the exception is in
     EXCEPTION.  */
  int completed;
  /* The task's result.  */
  void *result;
  /* Any exception that was thrown.  */
  struct gdb_exception exception;

  /* The task lock.  This must be acquired before modifying any field
     of this task.  */
  gdb_mutex lock;
};

/* Acquire the task lock.  If MAIN_THREAD is false, just lock.  If
   MAIN_THREAD is true, print the task message if the task is taking
   "too long".  Return 1 if the message was printed, 0 otherwise.  */
static int
acquire_task_lock (struct task *task, int main_thread)
{
  if (!main_thread)
    {
      gdb_mutex_lock (&task->lock);
      return 0;
    }
  else
    {
      /* If the task returns in less than 1 second, just go on.  */
      if (! gdb_mutex_timed_lock (&task->lock, 1))
	return 0;

      /* Print the message.  */
      puts_unfiltered (task->message);
      gdb_flush (gdb_stdout);
      gdb_mutex_lock (&task->lock);

      return 1;
    }
}

/* Run TASK and set its result fields accordingly.  Returns true if
   the task should now be destroyed, false otherwise.  If MAIN_THREAD
   is true, possibly print the message to the user.  */
static int
run_task (struct task *task, int main_thread)
{
  int result, message_printed;

  message_printed = acquire_task_lock (task, main_thread);
  if (!task->completed)
    {
      volatile struct gdb_exception except;

      /* If we are running the task in the main thread, print the
	 message unconditionally.  */
      if (main_thread)
	{
	  gdb_assert (!message_printed);

	  puts_unfiltered (task->message);
	  gdb_flush (gdb_stdout);
	  message_printed = 1;
	}

      TRY_CATCH (except, RETURN_MASK_ALL)
	{
	  task->result = task->function (task->user_data);
	}
      if (except.reason < 0)
	{
	  task->completed = -1;
	  task->exception = except;
	  task->exception.message = xstrdup (except.message);
	}
      else
	task->completed = 1;
      result = 0;
    }
  else
    result = 1;

  if (message_printed)
    {
      puts_unfiltered ("done\n");
      gdb_flush (gdb_stdout);
    }

  gdb_mutex_unlock (&task->lock);
  return result;
}

/* Destroy a task.  */
static void
free_task (struct task *task)
{
  gdb_mutex_destroy (&task->lock);
  xfree (task->message);
  xfree (task);
}

/* The body of a worker thread.  It takes a task from the list, runs
   it, and repeats.  */
static void *
worker_thread (void *p)
{
  struct task_pool *pool = p;
  while (1)
    {
      int r;
      struct task *job;

      gdb_mutex_lock (&pool->lock);

      r = 0;
      while (VEC_empty (task_p, pool->queue) && r == 0)
	{
	  ++pool->waiters;
	  /* Wait up to 15 seconds for a new job.  If we have a
	     spurious wakeup we will wait the whole time again; this
	     doesn't seem very important.  */
	  r = gdb_cond_timed_wait (&pool->condition, &pool->lock, 15);
	  --pool->waiters;
	}

      if (VEC_empty (task_p, pool->queue))
	{
	  job = NULL;
	  --pool->worker_count;
	}
      else
	job = VEC_pop (task_p, pool->queue);

      gdb_mutex_unlock (&pool->lock);

      if (!job)
	{
	  /* We timed out waiting for a job, so exit.  */
	  break;
	}

      if (run_task (job, 0))
	free_task (job);
    }

  clear_exception_cache ();

  /* The result is ignored.  */
  return NULL;
}

/* A qsort comparison function that compares task priorities.  */
static int
compare_priorities (const void *a, const void *b)
{
  const struct task * const *ta = a;
  const struct task * const *tb = b;
  return ((*ta)->priority < (*tb)->priority ? -1
	  : ((*ta)->priority > (*tb)->priority ? 1 : 0));
}

/* Add a task to the task pool POOL, and return it.  */
struct task *
create_task (struct task_pool *pool, char *message,
	     unsigned long priority,
	     void *(*function) (void *), void *user_data)
{
  struct task *result;

  result = xmalloc (sizeof (struct task));

  result->message = message;
  result->priority = priority;
  result->function = function;
  result->user_data = user_data;
  result->completed = 0;
  result->result = NULL;
  gdb_mutex_init (&result->lock);

  /* Acquire the pool lock while operating on the pool.  */
  gdb_mutex_lock (&pool->lock);

  if (pool->max_workers == 0)
    {
      /* No threads, so run the task right away.  We pretend that this
	 is not the main thread, because we don't want the message to
	 be printed.  */
      run_task (result, 0);
    }
  else
    {	
      /* Push the task and then sort by priority.  */
      VEC_safe_push (task_p, pool->queue, result);
      qsort (VEC_address (task_p, pool->queue),
	     VEC_length (task_p, pool->queue),
	     sizeof (task_p),
	     compare_priorities);

      /* If a thread is already waiting for a job, or if we are
	 already running the maximum number of worker threads, then we
	 just notify a waiting thread.  Otherwise, we start a new
	 thread.  */
      if (pool->waiters == 0 && pool->worker_count < pool->max_workers)
	{
	  gdb_create_new_thread (worker_thread, pool);
	  ++pool->worker_count;
	}
      else
	gdb_cond_signal (&pool->condition);
    }

  gdb_mutex_unlock (&pool->lock);

  return result;
}

/* Return TASK's answer, or throw an exception.  */
void *
get_task_answer (struct task *task)
{
  struct task *iter;
  int i, destroy;
  void *result;

  destroy = run_task (task, 1);

  if (task->completed < 0)
    {
      struct gdb_exception exc = task->exception;
      if (destroy)
	free_task (task);
      /* We do this in a funny way to avoid leaking the message.  */
      make_cleanup (xfree, (char *) exc.message);
      throw_error (exc.error, "%s", exc.message);
    }

  result = task->result;
  if (destroy)
    free_task (task);
  return result;
}

void
cancel_task (struct task *task)
{
  int destroy;

  gdb_mutex_lock (&task->lock);
  destroy = task->completed;
  if (!task->completed)
    task->completed = 1;
  gdb_mutex_unlock (&task->lock);
  if (destroy)
    free_task (task);
}

/* Create a new task pool.  */
struct task_pool *
create_task_pool (int max_workers)
{
  struct task_pool *result;

  result = xmalloc (sizeof (struct task_pool));
  memset (result, 0, sizeof (struct task_pool));

  gdb_mutex_init (&result->lock);
  gdb_cond_init (&result->condition);

  if (max_workers == -1)
    {
      max_workers = gdb_n_processors ();
      if (max_workers < 0)
	max_workers = 5;
      else if (max_workers > 0)
	max_workers *= 2;
    }
  result->max_workers = max_workers;

  return result;
}
