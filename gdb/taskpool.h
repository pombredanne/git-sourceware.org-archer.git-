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

#ifndef TASKPOOL_H
#define TASKPOOL_H

struct task_pool;
struct task;

/* The GDB task pool provides a simple way to hide work in the
   background.

   A task pool manages a number of threads, with the maximum number
   set at the time the pool is created.  It holds a queue of tasks,
   sorted by priority.  A worker thread simply loops, pulling the next
   task off the queue and running it.

   There is currently no way to destroy a task pool.

   The threads associated with a task pool will exit if no jobs are
   available for a certain amount of time.  They will be restarted as
   needed.

   Task pools are designed to work even if threads are not available
   on the host.  See get_task_answer.
   
   If MAX_WORKERS is -1, then create_task_pool tries to choose a
   number based on the number of available processors.  */
struct task_pool *create_task_pool (int max_workers);
   
/* A task is simply a user-provided function with some user-provided
   data.  When run it can either throw an exception, or return a
   result.

   A task has an associated message for the user.  This must be
   translated by the caller.  It should end with "...".  It is only
   printed if the task pool determines that the user is waiting for
   the task to complete.  When the task completes, "done" is printed
   after the message.  The message should be xmalloc()d.

   A task has a priority, also provided by the user.  Priorities must
   be comparable within a given pool but are not otherwise meaningful
   to the pool code.

   Every task is associated with a pool.

   It is up to the task's creator to ensure that the task is in fact
   thread-safe.  Be warned!  In GDB this can be tricky due to all the
   lurking global variables.

   A task is not destroyed until either cancel_task or get_task_answer
   has been called on it.  */
struct task *create_task (struct task_pool *pool,
			  char *message,
			  unsigned long priority,
			  void *(*function) (void *),
			  void *user_data);

/* get_task_answer returns the result of a task.  This may only be
   called once per task, and cannot be called if cancel_task has
   already been called.

   If the task's function returned an answer, this will return it.
   Otherwise, if the task's function threw an exception, this function
   will throw the same exception.

   If the task was not yet processed by some worker thread, it will be
   processed immediately in the thread that calls get_task_answer.
   This also enables task pools to work even when threads are
   unavailable.  */
void *get_task_answer (struct task *task);

/* Cancel a task.  This may only be called once per task, and cannot
   be called if get_task_answer has already been called.
   
   If the task is already running, this waits until the task has
   completed.  */
void cancel_task (struct task *task);

#endif /* TASKPOOL_H */
