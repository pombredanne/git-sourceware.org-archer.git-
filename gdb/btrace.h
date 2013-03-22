/* Branch trace support for GDB, the GNU debugger.

   Copyright (C) 2013 Free Software Foundation, Inc.

   Contributed by Intel Corp. <markus.t.metzger@intel.com>.

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

#ifndef BTRACE_H
#define BTRACE_H

/* Branch tracing (btrace) is a per-thread control-flow execution trace of the
   inferior.  For presentation purposes, the branch trace is represented as a
   list of sequential control-flow blocks, one such list per thread.  */

#include "btrace-common.h"

struct thread_info;
struct btrace_function;

/* A branch trace instruction.

   This represents a single instruction in a branch trace.  */
struct btrace_insn
{
  /* The address of this instruction.  */
  CORE_ADDR pc;
};

/* A vector of branch trace instructions.  */
typedef struct btrace_insn btrace_insn_s;
DEF_VEC_O (btrace_insn_s);

/* A doubly-linked list of branch trace function segments.  */
struct btrace_func_link
{
  struct btrace_function *prev;
  struct btrace_function *next;
};

/* Flags for btrace function segments.  */
enum btrace_function_flag
{
  /* The 'up' link interpretation.
     If set, it points to the function segment we returned to.
     If clear, it points to the function segment we called from.  */
  bfun_up_links_to_ret = (1 << 0),

  /* The 'up' link points to a tail call.  This obviously only makes sense
     if bfun_up_links_to_ret is clear.  */
  bfun_up_links_to_tailcall = (1 << 1)
};

/* A branch trace function segment.

   This represents a function segment in a branch trace, i.e. a consecutive
   number of instructions belonging to the same function.  */
struct btrace_function
{
  /* The full and minimal symbol for the function.  One of them may be NULL.  */
  struct minimal_symbol *msym;
  struct symbol *sym;

  /* The previous and next segment belonging to the same function.  */
  struct btrace_func_link segment;

  /* The previous and next function in control flow order.  */
  struct btrace_func_link flow;

  /* The directly preceding function segment in a (fake) call stack.  */
  struct btrace_function *up;

  /* The instructions in this function segment.  */
  VEC (btrace_insn_s) *insn;

  /* The instruction number offset for the first instruction in this
     function segment.  */
  unsigned int insn_offset;

  /* The function number.  */
  unsigned int number;

  /* The function level.  */
  int level;

  /* The source line range of this function segment (both inclusive).  */
  int lbegin, lend;

  /* A bit-vector of btrace_function_flag.  */
  unsigned int flags;
};

/* A branch trace instruction iterator.  */
struct btrace_insn_iterator
{
  /* The branch trace function segment containing the instruction.  */
  struct btrace_function *function;

  /* The index into the function segment's instruction vector.  */
  unsigned int index;
};

/* Branch trace iteration state for "record instruction-history".  */
struct btrace_insn_history
{
  /* The branch trace instruction range from begin (inclusive) to
     end (exclusive) that has been covered last time.  */
  struct btrace_insn_iterator begin;
  struct btrace_insn_iterator end;
};

/* Branch trace iteration state for "record function-call-history".  */
struct btrace_call_history
{
  /* The branch trace function range from begin (inclusive) to end (exclusive)
     that has been covered last time.  */
  struct btrace_function *begin;
  struct btrace_function *end;
};

/* Branch trace information per thread.

   This represents the branch trace configuration as well as the entry point
   into the branch trace data.  For the latter, it also contains the index into
   an array of branch trace blocks used for iterating though the branch trace
   blocks of a thread.  */
struct btrace_thread_info
{
  /* The target branch trace information for this thread.

     This contains the branch trace configuration as well as any
     target-specific information necessary for implementing branch tracing on
     the underlying architecture.  */
  struct btrace_target_info *target;

  /* The current branch trace for this thread.  */
  struct btrace_function *begin;
  struct btrace_function *end;

  /* The function level offset.  When added to each function's level,
     this normalizes the function levels such that the smallest level
     becomes zero.  */
  int level;

  /* The instruction history iterator.  */
  struct btrace_insn_history *insn_history;

  /* The function call history iterator.  */
  struct btrace_call_history *call_history;
};

/* Enable branch tracing for a thread.  */
extern void btrace_enable (struct thread_info *tp);

/* Disable branch tracing for a thread.
   This will also delete the current branch trace data.  */
extern void btrace_disable (struct thread_info *);

/* Disable branch tracing for a thread during teardown.
   This is similar to btrace_disable, except that it will use
   target_teardown_btrace instead of target_disable_btrace.  */
extern void btrace_teardown (struct thread_info *);

/* Fetch the branch trace for a single thread.  */
extern void btrace_fetch (struct thread_info *);

/* Clear the branch trace for a single thread.  */
extern void btrace_clear (struct thread_info *);

/* Clear the branch trace for all threads when an object file goes away.  */
extern void btrace_free_objfile (struct objfile *);

/* Parse a branch trace xml document into a block vector.  */
extern VEC (btrace_block_s) *parse_xml_btrace (const char*);

/* Dereference a branch trace instruction iterator.  Return a pointer to the
   instruction the iterator points to or NULL if the interator does not point
   to a valid instruction.  */
extern const struct btrace_insn *
btrace_insn_get (const struct btrace_insn_iterator *);

/* Return the instruction number for a branch trace iterator.  Returns zero
   if the iterator does not point to a valid instruction.  */
extern unsigned int btrace_insn_number (const struct btrace_insn_iterator *);

/* Initialize a branch trace instruction iterator to point to the begin/end of
   the branch trace.  Throws an error if there is no branch trace.  */
extern void btrace_insn_begin (struct btrace_insn_iterator *,
			       struct btrace_thread_info *);
extern void btrace_insn_end (struct btrace_insn_iterator *,
			     struct btrace_thread_info *);

/* Increment/decrement a branch trace instruction iterator.  Return the number
   of instructions by which the instruction iterator has been advanced.
   Returns zero, if the operation failed.  */
extern unsigned int btrace_insn_next (struct btrace_insn_iterator *,
				      unsigned int stride);
extern unsigned int btrace_insn_prev (struct btrace_insn_iterator *,
				      unsigned int stride);

/* Compare two branch trace instruction iterators.
   Return a negative number if LHS < RHS.
   Return zero if LHS == RHS.
   Return a positive number if LHS > RHS.  */
extern int btrace_insn_cmp (const struct btrace_insn_iterator *lhs,
			    const struct btrace_insn_iterator *rhs);

/* Find an instruction in the function branch trace by its number.
   If the instruction is found, initialize the branch trace instruction
   iterator to point to this instruction and return 1.
   Return 0, otherwise.  */
extern int btrace_find_insn_by_number (struct btrace_insn_iterator *,
				       const struct btrace_thread_info *,
				       unsigned int number);

/* Find a function in the function branch trace by its number.
   Return a pointer to that function or NULL if no such function is found.  */
extern struct btrace_function *
btrace_find_function_by_number (const struct btrace_thread_info *,
				unsigned int number);

/* Set the branch trace instruction history to [BEGIN; END).  */
extern void btrace_set_insn_history (struct btrace_thread_info *,
				     struct btrace_insn_iterator *begin,
				     struct btrace_insn_iterator *end);

/* Set the branch trace function call history to [BEGIN; END).  */
extern void btrace_set_call_history (struct btrace_thread_info *,
				     struct btrace_function *begin,
				     struct btrace_function *end);

#endif /* BTRACE_H */
