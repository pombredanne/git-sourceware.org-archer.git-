/* itset.c - Inferior/Thread sets.
   Copyright (C) 2010 Free Software Foundation, Inc.

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
#include "itset.h"
#include "vec.h"
#include "bfd.h"
#include "inferior.h"
#include "progspace.h"
#include "gdb_string.h"

#include <ctype.h>


/* Forward declaration of the base class.  */

struct itset_base;

/* An element of an I/T set is a class with some virtual methods,
   defined here.  */

struct itset_vtable
{
  /* Return true if the element contains the inferior.  The element
     and the inferior are passed as arguments.  */

  int (*contains_inferior) (struct itset_base *elt, struct inferior *inf);

  /* Destroy the contents of this element.  If the element does not
     require any special cleanup, this can be NULL.  This should not
     free the element itself; that is done by the caller.  */

  void (*destroy) (struct itset_base *);
};

/* The base class of all I/T set elements.  */

struct itset_base
{
  const struct itset_vtable *vtable;
};

typedef struct itset_base *itset_base_ptr;
DEF_VEC_P (itset_base_ptr);

struct itset
{
  /* The original specification of the set.  */
  const char *spec;

  /* The elements making up the set.  */
  VEC (itset_base_ptr) *elements;
};



/* A helper function that returns true if the inferior INF is
   contained by the set ELEMENTS.  */

static int
set_contains_inferior (VEC (itset_base_ptr) *elements, struct inferior *inf)
{
  int ix;
  struct itset_base *elt;

  for (ix = 0; VEC_iterate (itset_base_ptr, elements, ix, elt); ++ix)
    {
      if (elt->vtable->contains_inferior (elt, inf))
	return 1;
    }

  return 0;
}

/* A helper function to destroy all the elements in the set ELEMENTS.
   This also destroys ELEMENTS itself.  */

static void
set_free (VEC (itset_base_ptr) *elements)
{
  int ix;
  struct itset_base *elt;

  for (ix = 0; VEC_iterate (itset_base_ptr, elements, ix, elt); ++ix)
    {
      if (elt->vtable->destroy)
	elt->vtable->destroy (elt);
      xfree (elt);
    }

  VEC_free (itset_base_ptr, elements);
}



/* An I/T set element representing all inferiors using a certain
   executable.  */

struct itset_exec
{
  struct itset_base base;

  /* The name of the executable.  */
  const char *name;
};

/* Implementation of `contains_inferior' method.  */

static int
exec_contains_inferior (struct itset_base *base, struct inferior *inf)
{
  struct itset_exec *exec = (struct itset_exec *) base;

  /* FIXME: smarter compare */
  return strcmp (exec->name, bfd_get_filename (inf->pspace->ebfd)) == 0;
}

/* Implementation of `destroy' method.  */

static void
exec_destroy (struct itset_base *base)
{
  struct itset_exec *exec = (struct itset_exec *) base;

  xfree ((char *) exec->name);
}

static const struct itset_vtable exec_vtable =
{
  exec_contains_inferior,
  exec_destroy
};

/* Create a new `exec' I/T set element.  */

static struct itset_base *
create_exec_itset (const char *arg)
{
  struct itset_exec *exec;

  exec = XNEW (struct itset_exec);
  exec->base.vtable = &exec_vtable;
  exec->name = arg;

  return (struct itset_base *) exec;
}



/* The value representing any inferior or thread.  */

#define WILDCARD -1

/* An I/T set element representing a range of inferiors.  */

struct itset_range
{
  struct itset_base base;

  /* The first and last inferiors in this range.  If INF_FIRST is
     WILDCARD, then INF_LAST is unused.  */
  int inf_first, inf_last;

  /* The thread number used by this range.  If WILDCARD, then this
     matches any thread.  */
  int thread;
};

/* Implementation of `contains_inferior' method.  */

static int
range_contains_inferior (struct itset_base *base, struct inferior *inf)
{
  struct itset_range *range = (struct itset_range *) base;

  if (range->inf_first == WILDCARD
      || (inf->num >= range->inf_first && inf->num <= range->inf_last))
    return 1;
  return 0;
}

static const struct itset_vtable range_vtable =
{
  range_contains_inferior,
  NULL
};

/* Create a new `range' I/T set element.  */

static struct itset_base *
create_range_itset (int inf_first, int inf_last, int thread)
{
  struct itset_range *range;

  range = XNEW (struct itset_range);
  range->base.vtable = &range_vtable;
  range->inf_first = inf_first;
  range->inf_last = inf_last;
  range->thread = thread;

  return (struct itset_base *) range;
}



/* Implementation of `contains_inferior' method.  */

static int
current_contains_inferior (struct itset_base *base, struct inferior *inf)
{
  return current_inferior () == inf;
}

static const struct itset_vtable current_vtable =
{
  current_contains_inferior,
  NULL
};

/* Create a new I/T set element representing just the current
   inferior.  */

static struct itset_base *
create_current_itset (void)
{
  struct itset_base *base;

  base = XNEW (struct itset_base);
  base->vtable = &current_vtable;

  return base;
}



/* An I/T set element representing a static list of inferiors.  */

struct itset_static
{
  struct itset_base base;

  /* The inferiors.  */
  VEC (int) *elements;
};

/* Helper function to compare two ints.  */

static int
static_lessthan (const int a, const int b)
{
  return b < a;
}

/* Implementation of `contains_inferior' method.  */

static int
static_contains_inferior (struct itset_base *base, struct inferior *inf)
{
  struct itset_static *st = (struct itset_static *) base;
  int idx;

  idx = VEC_lower_bound (int, st->elements, inf->num, static_lessthan);
  if (idx < VEC_length (int, st->elements)
      && VEC_index (int, st->elements, idx) == inf->num)
    return 1;
  return 0;
}

/* Implementation of `destroy' method.  */

static void
static_free (struct itset_base *base)
{
  struct itset_static *st = (struct itset_static *) base;

  VEC_free (int, st->elements);
}

static const struct itset_vtable static_vtable =
{  
  static_contains_inferior,
  static_free
};

/* Helper struct used to pass data through iterate_over_inferiors.  */

struct iter_data
{
  /* The I/T set we are constructing.  */

  struct itset_static *st;

  /* The elements of the original (dynamic) I/T set.  */

  VEC (itset_base_ptr) *elements;
};

/* A callback for iterate_over_inferiors that adds an inferior to the
   result set, if it is in the source set.  */

static int
check_one_inferior (struct inferior *inf, void *datum)
{
  struct iter_data *id = datum;

  if (set_contains_inferior (id->elements, inf))
    VEC_safe_push (int, id->st->elements, inf->num);

  /* Keep going.  */
  return 0;
}

/* Create a new static I/T set from the list of elements.  */

static struct itset_base *
create_static_itset (VEC (itset_base_ptr) *elements)
{
  struct itset_static *st;
  struct iter_data datum;

  st = XNEW (struct itset_static);
  st->base.vtable = &static_vtable;
  st->elements = NULL;

  datum.st = st;
  datum.elements = elements;

  iterate_over_inferiors (check_one_inferior, &datum);

  if (VEC_length (int, st->elements) > 1)
    {
      qsort (VEC_address (int, st->elements),
	     VEC_length (int, st->elements),
	     sizeof (int), compare_positive_ints);

    }

  return (struct itset_base *) st;
}



/* Helper function to skip whitespace.  Returns an updated pointer
   into the text.  */

static const char *
skip_whitespace (const char *text)
{
  while (isspace (*text))
    ++text;
  return text;
}

/* Parse an I/T set range.  A range has the form F[:L][.T], where F is
   the starting inferior, L is the ending inferior, and T is the
   thread.  Updates RESULT with the new I/T set elements, and returns
   an updated pointer into the spec.  Throws an exception on
   error.  */

static const char *
parse_range (struct itset *result, const char *text)
{
  int inf_first, inf_last, thread;
  struct itset_base *elt;

  if (*text == '*')
    {
      inf_first = WILDCARD;
      inf_last = WILDCARD;
      ++text;
    }
  else
    {
      inf_first = strtol (text, (char **) &text, 10);
      if (*text == ':')
	{
	  ++text;
	  inf_last = strtol (text, (char **) &text, 10);
	}
      else
	inf_last = inf_first;
    }

  if (*text == '.')
    {
      ++text;
      if (*text == '*')
	thread = WILDCARD;
      else
	thread = strtol (text, (char **) &text, 10);
    }

  elt = create_range_itset (inf_first, inf_last, thread);
  VEC_safe_push (itset_base_ptr, result->elements, elt);

  return text;
}

/* Parse a named I/T set.  Currently the only named sets which are
   recognized are `exec (NAME)', and `current'.  Updates RESULT with
   the new I/T set elements, and returns an updated pointer into the
   spec.  Throws an exception on error.  */

static const char *
parse_named (struct itset *result, const char *text)
{
  const char *name = text;
  struct itset_base *elt;

  for (text = name + 1; isalnum (*text) || *text == '_'; ++text)
    ;

  if (strncmp ("current", name, text - name) == 0)
    elt = create_current_itset ();
  else if (strncmp ("exec", name, text - name) == 0)
    {
      const char *tem;
      char *arg;

      text = skip_whitespace (text);
      if (*text != '(')
	error ("'(' expected in I/T set after `exec'");
      text = skip_whitespace (text + 1);
      tem = strchr (text, ')');
      if (!tem)
	error ("no closing ')' in I/T set for `exec'");
      arg = xmalloc (tem - text + 1);
      memcpy (arg, text, tem - text);
      arg[tem - text] = '\0';
      text = tem + 1;
      elt = create_exec_itset (arg);
    }
  else
    {
      char *tem = alloca (text - name + 1);

      memcpy (tem, text, text - name);
      tem[text - name] = '\0';
      error (_("unknown named I/T set: `%s'"), tem);
    }

  VEC_safe_push (itset_base_ptr, result->elements, elt);

  return text;
}

/* A cleanup function that calls itset_free.  */

static void
itset_cleanup (void *d)
{
  itset_free (d);
}

/* Parse an I/T set specification and return a new I/T set.  Throws an
   exception on error.  */

struct itset *
itset_create (const char *spec)
{
  int is_static = 0;
  struct itset *result;
  struct cleanup *cleanups;

  if (*spec != '[')
    error ("I/T set must start with `['");

  result = XNEW (struct itset);
  result->spec = xstrdup (spec);
  result->elements = NULL;

  cleanups = make_cleanup (itset_cleanup, result);

  /* Skip the '['.  */
  ++spec;
  spec = skip_whitespace (spec);

  if (*spec == '!')
    {
      is_static = 1;
      ++spec;
    }

  while (1)
    {
      spec = skip_whitespace (spec);

      if (isdigit (*spec) || *spec == '*')
	spec = parse_range (result, spec);
      else if (isalpha (*spec))
	spec = parse_named (result, spec);
      else
	error ("invalid I/T syntax at `%s'", spec);

      spec = skip_whitespace (spec);
      if (*spec == ',')
	++spec;
      else
	break;
    }

  if (*spec != ']')
    error ("I/T set must end with `]'");

  if (is_static)
    {
      struct itset_base *st = create_static_itset (result->elements);

      set_free (result->elements);
      VEC_safe_push (itset_base_ptr, result->elements, st);
    }

  return result;
}

/* Return 1 if SET contains INF, 0 otherwise.  */

int
itset_contains_inferior (struct itset *set, struct inferior *inf)
{
  return set_contains_inferior (set->elements, inf);
}

/* Destroy SET.  */

void
itset_free (struct itset *set)
{
  set_free (set->elements);
  xfree ((char *) set->spec);
  xfree (set);
}
