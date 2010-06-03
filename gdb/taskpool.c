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

#include <pthread.h>
#include <time.h>
#include <sys/time.h>
#include <signal.h>

typedef struct task *task_p;

DEF_VEC_P (task_p);

/* A task pool.  This holds a number of threads, plus a task queue.  */
struct task_pool
{
  /* The task queue.  */
  VEC (task_p) *queue;

  /* The queue lock.  This must be held when modifying any element of
     the task pool.  */
  pthread_mutex_t lock;

  /* The queue condition.  This is signaled when a new task is pushed
     on the queue.  */
  pthread_cond_t condition;

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
  pthread_mutex_t lock;
};

/* Acquire the task lock.  If MAIN_THREAD is false, just lock.  If
   MAIN_THREAD is true, print the task message if the task is taking
   "too long".  Return 1 if the message was printed, 0 otherwise.  */
static int
acquire_task_lock (struct task *task, int main_thread)
{
  if (!main_thread)
    {
      pthread_mutex_lock (&task->lock);
      return 0;
    }
  else
    {
      struct timespec ts;
      struct timeval tv;

      /* If the task returns in less than 1 second, just go on.  */
      gettimeofday (&tv, NULL);
      ts.tv_sec = tv.tv_sec + 1;
      ts.tv_nsec = tv.tv_usec * 1000;
      if (! pthread_mutex_timedlock (&task->lock, &ts))
	return 0;

      /* Print the message.  */
      puts_unfiltered (task->message);
      gdb_flush (gdb_stdout);
      pthread_mutex_lock (&task->lock);

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

  pthread_mutex_unlock (&task->lock);
  return result;
}

/* Destroy a task.  */
static void
free_task (struct task *task)
{
  pthread_mutex_destroy (&task->lock);
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
      struct timespec ts;
      struct timeval tv;
      int r;
      struct task *job;

      pthread_mutex_lock (&pool->lock);

      /* Wait up to 15 seconds for a new job.  */
      gettimeofday (&tv, NULL);
      ts.tv_sec = tv.tv_sec + 15;
      ts.tv_nsec = tv.tv_usec * 1000;

      r = 0;
      while (VEC_empty (task_p, pool->queue) && r == 0)
	{
	  ++pool->waiters;
	  r = pthread_cond_timedwait (&pool->condition, &pool->lock, &ts);
	  --pool->waiters;
	}

      if (VEC_empty (task_p, pool->queue))
	{
	  job = NULL;
	  --pool->worker_count;
	}
      else
	job = VEC_pop (task_p, pool->queue);

      pthread_mutex_unlock (&pool->lock);

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
  pthread_mutex_init (&result->lock, NULL);

  /* Acquire the pool lock while operating on the pool.  */
  pthread_mutex_lock (&pool->lock);

  /* Push the task and then sort by priority.  */
  VEC_safe_push (task_p, pool->queue, result);
  qsort (VEC_address (task_p, pool->queue),
	 VEC_length (task_p, pool->queue),
	 sizeof (task_p),
	 compare_priorities);

  /* If a thread is already waiting for a job, or if we are already
     running the maximum number of worker threads, then we just notify
     a waiting thread.  Otherwise, we start a new thread.  */
  if (pool->waiters == 0 && pool->worker_count < pool->max_workers)
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

      pthread_create (&tem, &attr, worker_thread, pool);
      ++pool->worker_count;

      pthread_attr_destroy (&attr);
      pthread_sigmask (SIG_SETMASK, &old, NULL);
    }
  else
    pthread_cond_signal (&pool->condition);

  pthread_mutex_unlock (&pool->lock);

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

  pthread_mutex_lock (&task->lock);
  destroy = task->completed;
  if (!task->completed)
    task->completed = 1;
  pthread_mutex_unlock (&task->lock);
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

  pthread_mutex_init (&result->lock, NULL);
  pthread_cond_init (&result->condition, NULL);

  if (max_workers == -1)
    {
#ifdef _SC_NPROCESSORS_ONLN
      max_workers = (int) sysconf (_SC_NPROCESSORS_ONLN);
#endif
      if (max_workers <= 0)
	max_workers = 5;
      else
	max_workers *= 2;
    }
  result->max_workers = max_workers;

  return result;
}
