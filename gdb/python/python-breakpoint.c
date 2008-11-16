/* Python interface to breakpoints

   Copyright (C) 2008 Free Software Foundation, Inc.

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
#include "value.h"
#include "exceptions.h"
#include "python-internal.h"
#include "charset.h"
#include "breakpoint.h"
#include "gdbcmd.h"
#include "gdbthread.h"
#include "observer.h"


/* From breakpoint.c.  */
extern struct breakpoint *breakpoint_chain;


typedef struct breakpoint_object breakpoint_object;

static PyTypeObject breakpoint_object_type;

/* A dynamically allocated vector of breakpoint objects.  Each
   breakpoint has a number.  A breakpoint is valid if its slot in this
   vector is non-null.  When a breakpoint is deleted, we drop our
   reference to it and zero its slot; this is how we let the Python
   object have a lifetime which is independent from that of the gdb
   breakpoint.  */
static breakpoint_object **bppy_breakpoints;

/* Number of slots in bppy_breakpoints.  */
static int bppy_slots;

/* Number of live breakpoints.  */
static int bppy_live;

/* Variables used to pass information between the Breakpoint
   constructor and the breakpoint-created hook function.  */
static breakpoint_object *bppy_pending_object;

struct breakpoint_object
{
  PyObject_HEAD

  /* The breakpoint number according to gdb.  */
  int number;

  /* The gdb breakpoint object, or NULL if the breakpoint has been
     deleted.  */
  struct breakpoint *bp;
};

/* Evaluate to true if the breakpoint NUM is valid, false otherwise.  */
#define BPPY_VALID_P(Num)			\
    ((Num) >= 0					\
     && (Num) < bppy_slots			\
     && bppy_breakpoints[Num] != NULL)

/* Require that BREAKPOINT be a valid breakpoint ID; throw a Python
   exception if it is invalid.  */
#define BPPY_REQUIRE_VALID(Breakpoint)					\
    do {								\
      if (! BPPY_VALID_P ((Breakpoint)->number))			\
	return PyErr_Format (PyExc_RuntimeError, "breakpoint %d is invalid", \
			     (Breakpoint)->number);			\
    } while (0)

/* Python function which checks the validity of a breakpoint object.  */
static PyObject *
bppy_is_valid (PyObject *self, PyObject *args)
{
  if (((breakpoint_object *) self)->bp)
    Py_RETURN_TRUE;
  Py_RETURN_FALSE;
}

/* Python function to test whether or not the breakpoint is enabled.  */
static PyObject *
bppy_is_enabled (PyObject *self, PyObject *args)
{
  if (! ((breakpoint_object *) self)->bp)
    Py_RETURN_FALSE;
  /* Not clear what we really want here.  */
  if (((breakpoint_object *) self)->bp->enable_state == bp_enabled)
    Py_RETURN_TRUE;
  Py_RETURN_FALSE;
}

/* Python function to test whether or not the breakpoint is silent.  */
static PyObject *
bppy_is_silent (PyObject *self, PyObject *args)
{
  BPPY_REQUIRE_VALID ((breakpoint_object *) self);
  if (((breakpoint_object *) self)->bp->silent)
    Py_RETURN_TRUE;
  Py_RETURN_FALSE;
}

/* Python function to set the enabled state of a breakpoint.  */
static PyObject *
bppy_set_enabled (PyObject *self, PyObject *newvalue)
{
  breakpoint_object *self_bp = (breakpoint_object *) self;

  BPPY_REQUIRE_VALID (self_bp);
  if (! PyBool_Check (newvalue))
    return PyErr_Format (PyExc_RuntimeError, "argument must be boolean");

  if (newvalue == Py_True)
    enable_breakpoint (self_bp->bp);
  else
    disable_breakpoint (self_bp->bp);

  Py_RETURN_NONE;
}

/* Python function to set the 'silent' state of a breakpoint.  */
static PyObject *
bppy_set_silent (PyObject *self, PyObject *newvalue)
{
  breakpoint_object *self_bp = (breakpoint_object *) self;

  BPPY_REQUIRE_VALID (self_bp);
  if (! PyBool_Check (newvalue))
    return PyErr_Format (PyExc_RuntimeError, "argument must be boolean");

  self_bp->bp->silent = (newvalue == Py_True);

  Py_RETURN_NONE;
}

/* Python function to set the thread of a breakpoint.  */
static PyObject *
bppy_set_thread (PyObject *self, PyObject *newvalue)
{
  breakpoint_object *self_bp = (breakpoint_object *) self;
  int id;

  BPPY_REQUIRE_VALID (self_bp);
  if (PyInt_Check (newvalue))
    {
      id = (int) PyInt_AsLong (newvalue);
      if (! valid_thread_id (id))
	return PyErr_Format (PyExc_RuntimeError, "invalid thread id");
    }
  else if (newvalue == Py_None)
    id = -1;
  else
    return PyErr_Format (PyExc_RuntimeError, "invalid thread id");

  self_bp->bp->thread = id;

  Py_RETURN_NONE;
}

/* Python function to set the ignore count of a breakpoint.  */
static PyObject *
bppy_set_ignore_count (PyObject *self, PyObject *newvalue)
{
  breakpoint_object *self_bp = (breakpoint_object *) self;
  long value;

  BPPY_REQUIRE_VALID (self_bp);
  if (! PyInt_Check (newvalue))
    return PyErr_Format (PyExc_RuntimeError, "argument must be integer");

  value = PyInt_AsLong (newvalue);
  if (value < 0)
    value = 0;
  set_ignore_count (self_bp->number, (int) value, 0);

  Py_RETURN_NONE;
}

/* Python function to clear the hit count of a breakpoint.  */
static PyObject *
bppy_clear_hit_count (PyObject *self, PyObject *newvalue)
{
  breakpoint_object *self_bp = (breakpoint_object *) self;
  long value;

  BPPY_REQUIRE_VALID (self_bp);

  self_bp->bp->hit_count = 0;

  Py_RETURN_NONE;
}

/* Python function to get the location of a breakpoint.  */
static PyObject *
bppy_get_location (PyObject *self, PyObject *args)
{
  char *str;

  BPPY_REQUIRE_VALID ((breakpoint_object *) self);
  str = ((breakpoint_object *) self)->bp->addr_string;
  /* FIXME: watchpoints?  tracepoints?  */
  if (! str)
    str = "";
  return PyString_Decode (str, strlen (str), host_charset (), NULL);
}

/* Python function to get the condition expression of a breakpoint.  */
static PyObject *
bppy_get_condition (PyObject *self, PyObject *args)
{
  char *str;
  BPPY_REQUIRE_VALID ((breakpoint_object *) self);

  str = ((breakpoint_object *) self)->bp->cond_string;
  if (! str)
    Py_RETURN_NONE;
  return PyString_Decode (str, strlen (str), host_charset (), NULL);
}

/* Python function to get the commands attached to a breakpoint.  */
static PyObject *
bppy_get_commands (PyObject *self, PyObject *args)
{
  breakpoint_object *self_bp = (breakpoint_object *) self;
  long length;
  volatile struct gdb_exception except;
  struct ui_file *string_file;
  struct cleanup *chain;
  PyObject *result;
  char *cmdstr;

  BPPY_REQUIRE_VALID (self_bp);

  if (! self_bp->bp->commands)
    Py_RETURN_NONE;

  string_file = mem_fileopen ();
  chain = make_cleanup_ui_file_delete (string_file);

  TRY_CATCH (except, RETURN_MASK_ALL)
    {
      /* FIXME: this can fail.  Maybe we need to be making a new
	 ui_out object here?  */
      ui_out_redirect (uiout, string_file);
      print_command_lines (uiout, self_bp->bp->commands, 0);
      ui_out_redirect (uiout, NULL);
    }
  cmdstr = ui_file_xstrdup (string_file, &length);
  GDB_PY_HANDLE_EXCEPTION (except);

  result = PyString_Decode (cmdstr, strlen (cmdstr), host_charset (), NULL);
  do_cleanups (chain);
  xfree (cmdstr);
  return result;
}

/* Python function to get the breakpoint's number.  */
static PyObject *
bppy_get_number (PyObject *self, PyObject *args)
{
  breakpoint_object *self_bp = (breakpoint_object *) self;

  BPPY_REQUIRE_VALID (self_bp);

  return PyInt_FromLong (self_bp->number);
}

/* Python function to get the breakpoint's thread ID.  */
static PyObject *
bppy_get_thread (PyObject *self, PyObject *args)
{
  breakpoint_object *self_bp = (breakpoint_object *) self;

  BPPY_REQUIRE_VALID (self_bp);

  if (self_bp->bp->thread == -1)
    Py_RETURN_NONE;
  return PyInt_FromLong (self_bp->bp->thread);
}

/* Python function to get the breakpoint's hit count.  */
static PyObject *
bppy_get_hit_count (PyObject *self, PyObject *args)
{
  breakpoint_object *self_bp = (breakpoint_object *) self;

  BPPY_REQUIRE_VALID (self_bp);

  return PyInt_FromLong (self_bp->bp->hit_count);
}

/* Python function to get the breakpoint's ignore count.  */
static PyObject *
bppy_get_ignore_count (PyObject *self, PyObject *args)
{
  breakpoint_object *self_bp = (breakpoint_object *) self;

  BPPY_REQUIRE_VALID (self_bp);

  return PyInt_FromLong (self_bp->bp->ignore_count);
}

/* Python function to create a new breakpoint.  */
static PyObject *
bppy_new (PyTypeObject *subtype, PyObject *args, PyObject *kwargs)
{
  PyObject *result;
  char *spec;
  volatile struct gdb_exception except;

  /* FIXME: allow condition, thread, temporary, ... ? */
  if (! PyArg_ParseTuple (args, "s", &spec))
    return NULL;
  result = subtype->tp_alloc (subtype, 0);
  if (! result)
    return NULL;
  bppy_pending_object = (breakpoint_object *) result;
  bppy_pending_object->number = -1;
  bppy_pending_object->bp = NULL;

  TRY_CATCH (except, RETURN_MASK_ALL)
    {
      set_breakpoint (spec, NULL, 0, 0, -1, 0, AUTO_BOOLEAN_TRUE);
    }
  if (except.reason < 0)
    {
      subtype->tp_free (result);
      return PyErr_Format (except.reason == RETURN_QUIT
			     ? PyExc_KeyboardInterrupt : PyExc_RuntimeError,
			     "%s", except.message);
    }

  BPPY_REQUIRE_VALID ((breakpoint_object *) result);
  return result;
}



/* Static function to return a tuple holding all breakpoints.  */

PyObject *
gdbpy_get_breakpoints (PyObject *self, PyObject *args)
{
  PyObject *result;

  if (bppy_live == 0)
    Py_RETURN_NONE;

  result = PyTuple_New (bppy_live);
  if (result)
    {
      int i, out = 0;
      for (i = 0; out < bppy_live; ++i)
	{
	  if (! bppy_breakpoints[i])
	    continue;
	  Py_INCREF (bppy_breakpoints[i]);
	  PyTuple_SetItem (result, out, (PyObject *) bppy_breakpoints[i]);
	  ++out;
	}
    }
  return result;
}



/* Event callback functions.  */

/* Callback that is used when a breakpoint is created.  This function
   will create a new Python breakpoint object.  */
static void
gdbpy_breakpoint_created (int num)
{
  breakpoint_object *newbp;
  struct breakpoint *bp;
  PyGILState_STATE state;

  if (num < 0)
    return;

  for (bp = breakpoint_chain; bp; bp = bp->next)
    if (bp->number == num)
      break;
  if (! bp)
    return;

  if (num >= bppy_slots)
    {
      int old = bppy_slots;
      bppy_slots = bppy_slots * 2 + 10;
      bppy_breakpoints
	= (breakpoint_object **) xrealloc (bppy_breakpoints,
					   (bppy_slots
					    * sizeof (breakpoint_object *)));
      memset (&bppy_breakpoints[old], 0,
	      (bppy_slots - old) * sizeof (PyObject *));
    }

  ++bppy_live;

  state = PyGILState_Ensure ();

  if (bppy_pending_object)
    {
      newbp = bppy_pending_object;
      bppy_pending_object = NULL;
    }
  else
    newbp = PyObject_New (breakpoint_object, &breakpoint_object_type);
  if (newbp)
    {
      PyObject *hookfn;

      newbp->number = num;
      newbp->bp = bp;
      bppy_breakpoints[num] = newbp;

      hookfn = gdbpy_get_hook_function ("new_breakpoint");
      if (hookfn)
	{
	  PyObject *result;
	  result = PyObject_CallFunctionObjArgs (hookfn, newbp, NULL);
	  if (result)
	    {
	      Py_DECREF (result);
	    }
	  Py_DECREF (hookfn);
	}
    }

  /* Just ignore errors here.  */
  PyErr_Clear ();

  PyGILState_Release (state);
}

/* Callback that is used when a breakpoint is deleted.  This will
   invalidate the corresponding Python object.  */
static void
gdbpy_breakpoint_deleted (int num)
{
  PyGILState_STATE state;

  state = PyGILState_Ensure ();
  if (BPPY_VALID_P (num))
    {
      bppy_breakpoints[num]->bp = NULL;
      Py_DECREF (bppy_breakpoints[num]);
      bppy_breakpoints[num] = NULL;
      --bppy_live;
    }
  PyGILState_Release (state);
}



/* Initialize the Python breakpoint code.  */
void
gdbpy_initialize_breakpoints (void)
{
  breakpoint_object_type.tp_new = bppy_new;
  if (PyType_Ready (&breakpoint_object_type) < 0)
    return;

  Py_INCREF (&breakpoint_object_type);
  PyModule_AddObject (gdb_module, "Breakpoint",
		      (PyObject *) &breakpoint_object_type);

  observer_attach_breakpoint_created (gdbpy_breakpoint_created);
  observer_attach_breakpoint_deleted (gdbpy_breakpoint_deleted);
}



static PyMethodDef breakpoint_object_methods[] =
{
  { "is_valid", bppy_is_valid, METH_NOARGS,
    "Return true if this breakpoint is valid, false if not." },
  { "is_enabled", bppy_is_enabled, METH_NOARGS,
    "Return true if this breakpoint is enabled, false if disabled." },
  { "is_silent", bppy_is_silent, METH_NOARGS,
    "Return true if this breakpoint is silent, false if verbose." },

  { "set_enabled", bppy_set_enabled, METH_O,
    "Enable or disable this breakpoint" },
  { "set_silent", bppy_set_silent, METH_O,
    "Make this breakpoint quiet or verbose" },
  { "set_thread", bppy_set_thread, METH_O,
    "Set the thread ID for this breakpoint.\n\
If the argument is a thread ID, make this a thread-specific breakpoint.\n\
If the argument is None, make this breakpoint not thread-specific.\n\
Any other argument is an error." },
  { "set_ignore_count", bppy_set_ignore_count, METH_O,
    "Set the ignore count for this breakpoint" },
  { "clear_hit_count", bppy_set_ignore_count, METH_NOARGS,
    "Set the hit count for this breakpoint to zero" },

  { "get_location", bppy_get_location, METH_NOARGS,
    "Return the location of this breakpoint, as specified by the user"},
  { "get_condition", bppy_get_condition, METH_NOARGS,
    "Return the condition of this breakpoint, as specified by the user.\n\
Returns None if no condition set."},
  { "get_commands", bppy_get_commands, METH_NOARGS,
    "Return the commands of this breakpoint, as specified by the user"},
  { "get_number", bppy_get_number, METH_NOARGS,
    "Return this breakpoint's number." },
  { "get_thread", bppy_get_thread, METH_NOARGS,
    "For a thread-specific breakpoint, return the thread ID.\n\
Otherwise, return None." },
  { "get_hit_count", bppy_get_hit_count, METH_NOARGS,
    "Return the number of times this breakpoint has been hit." },
  { "get_ignore_count", bppy_get_ignore_count, METH_NOARGS,
    "Return the number of times this breakpoint should be automatically continued." },

  { 0 }
};

static PyTypeObject breakpoint_object_type =
{
  PyObject_HEAD_INIT (NULL)
  0,				  /*ob_size*/
  "gdb.Breakpoint",		  /*tp_name*/
  sizeof (breakpoint_object),	  /*tp_basicsize*/
  0,				  /*tp_itemsize*/
  0,				  /*tp_dealloc*/
  0,				  /*tp_print*/
  0,				  /*tp_getattr*/
  0,				  /*tp_setattr*/
  0,				  /*tp_compare*/
  0,				  /*tp_repr*/
  0,				  /*tp_as_number*/
  0,				  /*tp_as_sequence*/
  0,				  /*tp_as_mapping*/
  0,				  /*tp_hash */
  0,				  /*tp_call*/
  0,				  /*tp_str*/
  0,				  /*tp_getattro*/
  0,				  /*tp_setattro*/
  0,				  /*tp_as_buffer*/
  Py_TPFLAGS_DEFAULT,		  /*tp_flags*/
  "GDB breakpoint object",	  /* tp_doc */
  0,				  /* tp_traverse */
  0,				  /* tp_clear */
  0,				  /* tp_richcompare */
  0,				  /* tp_weaklistoffset */
  0,				  /* tp_iter */
  0,				  /* tp_iternext */
  breakpoint_object_methods	  /* tp_methods */
};
