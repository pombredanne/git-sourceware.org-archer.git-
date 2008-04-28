/* gdb commands implemented in Python

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
#include "gdbcmd.h"
#include "cli/cli-decode.h"
#include "completer.h"

/* Struct representing built-in completion types.  */
struct cmdpy_completer
{
  /* Python symbol name.  */
  char *name;
  /* Completion function.  */
  char **(*completer) (struct cmd_list_element *, char *, char *);
};

static struct cmdpy_completer completers[] =
{
  { "COMPLETE_NONE", noop_completer },
  { "COMPLETE_FILENAME", filename_completer },
  { "COMPLETE_LOCATION", location_completer },
  { "COMPLETE_COMMAND", command_completer },
  { "COMPLETE_SYMBOL", make_symbol_completion_list_fn },
};

#define N_COMPLETERS (sizeof (completers) / sizeof (completers[0]))

/* A gdb command.  For the time being only ordinary commands (not
   set/show commands) are allowed.  */
struct cmdpy_object
{
  PyObject_HEAD

  /* The corresponding gdb command object, or NULL if the command is
     no longer installed.  */
  /* It isn't clear if we will ever care about this.  */
  struct cmd_list_element *command;
};

typedef struct cmdpy_object cmdpy_object;

static PyObject *cmdpy_dont_repeat (PyObject *self, PyObject *args);

static PyMethodDef cmdpy_object_methods[] =
{
  { "dont_repeat", cmdpy_dont_repeat, METH_NOARGS,
    "Prevent command repetition when user enters empty line." },

  { 0 }
};

static PyTypeObject cmdpy_object_type =
{
  PyObject_HEAD_INIT (NULL)
  0,				  /*ob_size*/
  "gdb.Command",		  /*tp_name*/
  sizeof (cmdpy_object),	  /*tp_basicsize*/
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
  Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE, /*tp_flags*/
  "GDB command object",		  /* tp_doc */
  0,				  /* tp_traverse */
  0,				  /* tp_clear */
  0,				  /* tp_richcompare */
  0,				  /* tp_weaklistoffset */
  0,				  /* tp_iter */
  0,				  /* tp_iternext */
  cmdpy_object_methods		  /* tp_methods */
};



/* Python function which wraps dont_repeat.  */
static PyObject *
cmdpy_dont_repeat (PyObject *self, PyObject *args)
{
  dont_repeat ();
  Py_RETURN_NONE;
}




/* Called if the gdb cmd_list_element is destroyed.  */
static void
cmdpy_destroyer (struct cmd_list_element *self, void *context)
{
  /* Release our hold on the command object.  */
  cmdpy_object *cmd = (cmdpy_object *) context;
  cmd->command = NULL;
  Py_DECREF (cmd);

  /* We allocated the name and doc string.  */
  xfree (self->name);
  xfree (self->doc);
}

/* Called by gdb to invoke the command.  */
static void
cmdpy_function (struct cmd_list_element *command, char *args, int from_tty)
{
  cmdpy_object *obj = (cmdpy_object *) get_cmd_context (command);
  PyObject *method, *argobj, *ttyobj, *result;

  if (! obj)
    error ("Invalid invocation of Python command object");
  if (! PyObject_HasAttrString ((PyObject *) obj, "invoke"))
    error ("Python command object missing 'invoke' method");

  method = PyString_FromString ("invoke");
  if (! args)
    {
      argobj = Py_None;
      Py_INCREF (argobj);
    }
  else
    {
      argobj = PyString_FromString (args);
      if (! argobj)
	error ("Couldn't convert arguments to Python string");
    }
  ttyobj = from_tty ? Py_True : Py_False;
  Py_INCREF (ttyobj);
  result = PyObject_CallMethodObjArgs ((PyObject *) obj, method, argobj,
				       ttyobj, NULL);
  Py_DECREF (method);
  Py_DECREF (argobj);
  Py_DECREF (ttyobj);
  if (! result)
    {
      PyObject *ptype, *pvalue, *ptraceback;
      char *s, *str;

      PyErr_Fetch (&ptype, &pvalue, &ptraceback);

      if (pvalue && PyString_Check (pvalue))
	{
	  /* Make a temporary copy of the string data.  */
	  char *s = PyString_AsString (pvalue);
	  char *copy = alloca (strlen (s) + 1);
	  strcpy (copy, s);

	  PyErr_Restore (ptype, pvalue, ptraceback);
	  gdbpy_print_stack ();
	  error ("Error occurred in Python command: %s", copy);
	}
      else
	{
	  PyErr_Restore (ptype, pvalue, ptraceback);
	  gdbpy_print_stack ();
	  error ("Error occurred in Python command");
	}
    }
  Py_DECREF (result);
}

/* Called by gdb for command completion.  */
static char **
cmdpy_completer (struct cmd_list_element *command, char *text, char *word)
{
  cmdpy_object *obj = (cmdpy_object *) get_cmd_context (command);
  PyObject *method, *textobj, *wordobj, *resultobj;
  char **result;

  if (! obj)
    error ("Invalid invocation of Python command object");
  if (! PyObject_HasAttrString ((PyObject *) obj, "complete"))
    {
      /* If there is no complete method, don't error -- instead, just
	 say that there are no completions.  */
      return NULL;
    }

  method = PyString_FromString ("complete");
  textobj = PyString_FromString (text);
  if (! textobj)
    error ("could not convert argument to Python string");
  wordobj = PyString_FromString (word);
  if (! wordobj)
    error ("could not convert argument to Python string");

  resultobj = PyObject_CallMethodObjArgs ((PyObject *) obj, method, textobj,
					  wordobj, NULL);
  Py_DECREF (method);
  Py_DECREF (textobj);
  Py_DECREF (wordobj);
  if (! resultobj)
    {
      /* Just swallow errors here.  */
      PyErr_Clear ();
      return NULL;
    }

  result = NULL;
  if (PySequence_Check (resultobj))
    {
      Py_ssize_t i, len = PySequence_Size (resultobj);
      Py_ssize_t out;
      if (len < 0)
	goto done;

      result = (char **) xmalloc ((len + 1) * sizeof (char *));
      for (i = out = 0; i < len; ++i)
	{
	  char *s;
	  int l;
	  PyObject *elt = PySequence_GetItem (resultobj, i);
	  if (elt == NULL || ! PyString_Check (elt))
	    {
	      /* Skip problem elements.  */
	      PyErr_Clear ();
	      continue;
	    }
	  s = PyString_AsString (elt);
	  result[out] = xstrdup (s);
	  ++out;
	}
      result[out] = NULL;
    }
  else if (PyInt_Check (resultobj))
    {
      /* User code may also return one of the completion constants,
	 thus requesting that sort of completion.  */
      long value = PyInt_AsLong (resultobj);
      if (value >= 0 && value < (long) N_COMPLETERS)
	result = completers[value].completer (command, text, word);
    }

 done:

  Py_DECREF (resultobj);
  return result;
}

/* Object initializer; sets up gdb-side structures for command.

   Use: __init__(NAME, CMDCLASS, [COMPLETERCLASS]).

   NAME is the name of the command.  Currently only one-word commands
   are supported.

   CMDCLASS is the kind of command.  It should be one of the COMMAND_*
   constants defined in the gdb module.

   COMPLETERCLASS is the kind of completer.  If not given, the
   "complete" method will be used.  Otherwise, it should be one of the
   COMPLETE_* constants defined in the gdb module.

   The documentation for the command is taken from the doc string for
   the python class.
   
*/
static int
cmdpy_init (PyObject *self, PyObject *args, PyObject *kwds)
{
  cmdpy_object *obj = (cmdpy_object *) self;
  char *name;
  int cmdtype;
  int completetype = -1;
  char *docstring = NULL;
  volatile struct gdb_exception except;

  if (obj->command)
    {
      /* Note: this is apparently not documented in Python.  We return
	 0 for success, -1 for failure.  */
      PyErr_Format (PyExc_RuntimeError, "Command object already initialized");
      return -1;
    }

  if (! PyArg_ParseTuple (args, "si|i", &name, &cmdtype, &completetype))
    return -1;

  if (cmdtype != no_class && cmdtype != class_run
      && cmdtype != class_vars && cmdtype != class_stack
      && cmdtype != class_files && cmdtype != class_support
      && cmdtype != class_info && cmdtype != class_breakpoint
      && cmdtype != class_trace && cmdtype != class_obscure
      && cmdtype != class_maintenance)
    {
      PyErr_Format (PyExc_RuntimeError, "invalid command class argument");
      return -1;
    }

  if (completetype < -1 || completetype >= (int) N_COMPLETERS)
    {
      PyErr_Format (PyExc_RuntimeError, "invalid completion type argument");
      return -1;
    }

  if (PyObject_HasAttrString (self, "__doc__"))
    {
      PyObject *ds_obj = PyObject_GetAttrString (self, "__doc__");
      if (ds_obj && PyString_Check (ds_obj))
	docstring = xstrdup (PyString_AsString (ds_obj));
    }
  if (! docstring)
    docstring = xstrdup ("This command is not documented.");

  TRY_CATCH (except, RETURN_MASK_ALL)
    {
      /* It would be nice to support multi-word commands here, but it
	 is a bit tricky given how gdb command data structures seem to
	 work.  */
      struct cmd_list_element *cmd = add_cmd (xstrdup (name),
					      (enum command_class) cmdtype,
					      NULL,
					      docstring,
					      &cmdlist);
      /* There appears to be no API to set this.  */
      cmd->func = cmdpy_function;
      cmd->destroyer = cmdpy_destroyer;

      obj->command = cmd;
      Py_INCREF (self);
      set_cmd_context (cmd, self);
      set_cmd_completer (cmd, ((completetype == -1) ? cmdpy_completer
			       : completers[completetype].completer));
    }
  if (except.reason < 0)
    {
      PyErr_Format (except.reason == RETURN_QUIT
		    ? PyExc_KeyboardInterrupt : PyExc_RuntimeError,
		    "%s", except.message);
      return -1;
    }
  return 0;
}



/* Initialize the 'commands' code.  */
void
gdbpy_initialize_commands (void)
{
  int i;

  cmdpy_object_type.tp_new = PyType_GenericNew;
  cmdpy_object_type.tp_init = cmdpy_init;
  if (PyType_Ready (&cmdpy_object_type) < 0)
    return;

  /* Note: alias and user seem to be special; pseudo appears to be
     unused, and there is no reason to expose tui or xdb, I think.  */
  if (PyModule_AddIntConstant (gdb_module, "COMMAND_NONE", no_class) < 0
      || PyModule_AddIntConstant (gdb_module, "COMMAND_RUN", class_run) < 0
      || PyModule_AddIntConstant (gdb_module, "COMMAND_VARS", class_vars) < 0
      || PyModule_AddIntConstant (gdb_module, "COMMAND_STACK", class_stack) < 0
      || PyModule_AddIntConstant (gdb_module, "COMMAND_FILES", class_files) < 0
      || PyModule_AddIntConstant (gdb_module, "COMMAND_SUPPORT",
				  class_support) < 0
      || PyModule_AddIntConstant (gdb_module, "COMMAND_INFO", class_info) < 0
      || PyModule_AddIntConstant (gdb_module, "COMMAND_BREAKPOINT",
				  class_breakpoint) < 0
      || PyModule_AddIntConstant (gdb_module, "COMMAND_TRACE", class_trace) < 0
      || PyModule_AddIntConstant (gdb_module, "COMMAND_OBSCURE",
				  class_obscure) < 0
      || PyModule_AddIntConstant (gdb_module, "COMMAND_MAINTENANCE",
				  class_maintenance) < 0)
    return;

  for (i = 0; i < N_COMPLETERS; ++i)
    {
      if (PyModule_AddIntConstant (gdb_module, completers[i].name, i) < 0)
	return;
    }

  Py_INCREF (&cmdpy_object_type);
  PyModule_AddObject (gdb_module, "Command",
		      (PyObject *) &cmdpy_object_type);
}
