/* General python/gdb code

   Copyright (C) 2008, 2009 Free Software Foundation, Inc.

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
#include "command.h"
#include "ui-out.h"
#include "cli/cli-script.h"
#include "gdbcmd.h"
#include "objfiles.h"
#include "observer.h"
#include "gdb_regex.h"
#include "language.h"
#include "valprint.h"
#include "event-loop.h"

#include <ctype.h>

/* True if we should print the stack when catching a Python error,
   false otherwise.  */
static int gdbpy_should_print_stack = 1;

/* This is true if we should auto-load python code when an objfile is
   opened, false otherwise.  */
static int gdbpy_auto_load = 1;

#ifdef HAVE_PYTHON

#include "python.h"
#include "libiberty.h"
#include "cli/cli-decode.h"
#include "charset.h"
#include "top.h"
#include "solib.h"
#include "exceptions.h"
#include "python-internal.h"
#include "linespec.h"
#include "symtab.h"
#include "source.h"
#include "version.h"
#include "inferior.h"
#include "gdbthread.h"
#include "target.h"
#include "gdbthread.h"
#include "event-top.h"

static PyMethodDef GdbMethods[];

PyObject *gdb_module;

/* Some string constants we may wish to use.  */
PyObject *gdbpy_to_string_cst;
PyObject *gdbpy_children_cst;
PyObject *gdbpy_display_hint_cst;
PyObject *gdbpy_doc_cst;

/* Given a command_line, return a command string suitable for passing
   to Python.  Lines in the string are separated by newlines.  The
   return value is allocated using xmalloc and the caller is
   responsible for freeing it.  */

static char *
compute_python_string (struct command_line *l)
{
  struct command_line *iter;
  char *script = NULL;
  int size = 0;
  int here;

  for (iter = l; iter; iter = iter->next)
    size += strlen (iter->line) + 1;

  script = xmalloc (size + 1);
  here = 0;
  for (iter = l; iter; iter = iter->next)
    {
      int len = strlen (iter->line);
      strcpy (&script[here], iter->line);
      here += len;
      script[here++] = '\n';
    }
  script[here] = '\0';
  return script;
}

/* Take a command line structure representing a 'python' command, and
   evaluate its body using the Python interpreter.  */

void
eval_python_from_control_command (struct command_line *cmd)
{
  int ret;
  char *script;
  struct cleanup *cleanup;
  PyGILState_STATE state;

  if (cmd->body_count != 1)
    error (_("Invalid \"python\" block structure."));

  state = PyGILState_Ensure ();
  cleanup = make_cleanup_py_restore_gil (&state);

  script = compute_python_string (cmd->body_list[0]);
  ret = PyRun_SimpleString (script);
  xfree (script);
  if (ret)
    {
      gdbpy_print_stack ();
      error (_("Error while executing Python code."));
    }

  do_cleanups (cleanup);
}

/* Implementation of the gdb "python" command.  */

static void
python_command (char *arg, int from_tty)
{
  struct cleanup *cleanup;
  PyGILState_STATE state;

  state = PyGILState_Ensure ();
  cleanup = make_cleanup_py_restore_gil (&state);

  while (arg && *arg && isspace (*arg))
    ++arg;
  if (arg && *arg)
    {
      if (PyRun_SimpleString (arg))
	{
	  gdbpy_print_stack ();
	  error (_("Error while executing Python code."));
	}
    }
  else
    {
      struct command_line *l = get_command_line (python_control, "");
      make_cleanup_free_command_lines (&l);
      execute_control_command_untraced (l);
    }

  do_cleanups (cleanup);
}



/* Transform a gdb parameters's value into a Python value.  May return
   NULL (and set a Python exception) on error.  Helper function for
   get_parameter.  */

PyObject *
gdbpy_parameter_value (enum var_types type, void *var)
{
  switch (type)
    {
    case var_string:
    case var_string_noescape:
    case var_optional_filename:
    case var_filename:
    case var_enum:
      {
	char *str = * (char **) var;
	if (! str)
	  str = "";
	return PyString_Decode (str, strlen (str), host_charset (), NULL);
      }

    case var_boolean:
      {
	if (* (int *) var)
	  Py_RETURN_TRUE;
	else
	  Py_RETURN_FALSE;
      }

    case var_auto_boolean:
      {
	enum auto_boolean ab = * (enum auto_boolean *) var;
	if (ab == AUTO_BOOLEAN_TRUE)
	  Py_RETURN_TRUE;
	else if (ab == AUTO_BOOLEAN_FALSE)
	  Py_RETURN_FALSE;
	else
	  Py_RETURN_NONE;
      }

    case var_integer:
      if ((* (int *) var) == INT_MAX)
	Py_RETURN_NONE;
      /* Fall through.  */
    case var_zinteger:
      return PyLong_FromLong (* (int *) var);

    case var_uinteger:
      {
	unsigned int val = * (unsigned int *) var;
	if (val == UINT_MAX)
	  Py_RETURN_NONE;
	return PyLong_FromUnsignedLong (val);
      }
    }

  return PyErr_Format (PyExc_RuntimeError, "programmer error: unhandled type");
}

/* A Python function which returns a gdb parameter's value as a Python
   value.  */

static PyObject *
gdbpy_parameter (PyObject *self, PyObject *args)
{
  struct cmd_list_element *alias, *prefix, *cmd;
  char *arg, *newarg;
  int found = -1;
  volatile struct gdb_exception except;

  if (! PyArg_ParseTuple (args, "s", &arg))
    return NULL;

  newarg = concat ("show ", arg, (char *) NULL);

  TRY_CATCH (except, RETURN_MASK_ALL)
    {
      found = lookup_cmd_composition (newarg, &alias, &prefix, &cmd);
    }
  xfree (newarg);
  GDB_PY_HANDLE_EXCEPTION (except);
  if (!found)
    return PyErr_Format (PyExc_RuntimeError,
			 "could not find parameter `%s'", arg);

  if (! cmd->var)
    return PyErr_Format (PyExc_RuntimeError, "`%s' is not a parameter", arg);
  return gdbpy_parameter_value (cmd->var_type, cmd->var);
}

/* A Python function which evaluates a string using the gdb CLI.  */

static PyObject *
execute_gdb_command (PyObject *self, PyObject *args)
{
  struct cmd_list_element *alias, *prefix, *cmd;
  char *arg, *newarg;
  PyObject *from_tty_obj = NULL;
  int from_tty;
  int cmp;
  volatile struct gdb_exception except;

  if (! PyArg_ParseTuple (args, "s|O!", &arg, &PyBool_Type, &from_tty_obj))
    return NULL;

  from_tty = 0;
  if (from_tty_obj)
    {
      cmp = PyObject_IsTrue (from_tty_obj);
      if (cmp < 0)
	  return NULL;
      from_tty = cmp;
    }

  TRY_CATCH (except, RETURN_MASK_ALL)
    {
      execute_command (arg, from_tty);
    }
  GDB_PY_HANDLE_EXCEPTION (except);

  /* Do any commands attached to breakpoint we stopped at.  */
  bpstat_do_actions ();

  Py_RETURN_NONE;
}

/* Implementation of gdb.solib_address (Long) -> String.
   Returns the name of the shared library holding a given address, or None.  */

static PyObject *
gdbpy_solib_address (PyObject *self, PyObject *args)
{
  unsigned long long pc;
  char *soname;
  PyObject *str_obj;

  if (!PyArg_ParseTuple (args, "K", &pc))
    return NULL;

  soname = solib_name_from_address (pc);
  if (soname)
    str_obj = PyString_Decode (soname, strlen (soname), host_charset (), NULL);
  else
    {
      str_obj = Py_None;
      Py_INCREF (Py_None);
    }

  return str_obj;
}

/* A Python function which is a wrapper for decode_line_1.  */

static PyObject *
gdbpy_decode_line (PyObject *self, PyObject *args)
{
  struct symtabs_and_lines sals = { NULL, 0 }; /* Initialize to appease gcc.  */
  struct symtab_and_line sal;
  char *arg = NULL;
  int free_sals = 0, i;
  PyObject *result = NULL;
  volatile struct gdb_exception except;

  if (! PyArg_ParseTuple (args, "|s", &arg))
    return NULL;

  TRY_CATCH (except, RETURN_MASK_ALL)
    {
      if (arg)
	{
	  char *copy;

	  arg = strdup (arg);
	  copy = arg;

	  sals = decode_line_1 (&copy, 0, 0, 0, 0, 0);
	  free_sals = 1;
	}
      else
	{
	  set_default_source_symtab_and_line ();
	  sal = get_current_source_symtab_and_line ();
	  sals.sals = &sal;
	  sals.nelts = 1;
	}
    }
  if (arg)
    xfree (arg);

  if (except.reason < 0)
    {
      if (free_sals)
	xfree (sals.sals);
      /* We know this will always throw.  */
      GDB_PY_HANDLE_EXCEPTION (except);
    }

  if (sals.nelts)
    {
      result = PyTuple_New (sals.nelts);
      for (i = 0; i < sals.nelts; ++i)
	{
	  PyObject *obj;
	  char *str;

	  obj = symtab_and_line_to_sal_object (sals.sals[i]);
	  if (! obj)
	    {
	      Py_DECREF (result);
	      result = NULL;
	      break;
	    }

	  PyTuple_SetItem (result, i, obj);
	}
    }

  if (free_sals)
    xfree (sals.sals);

  if (result)
    return result;
  Py_RETURN_NONE;
}

/* Parse a string and evaluate it as an expression.  */
static PyObject *
gdbpy_parse_and_eval (PyObject *self, PyObject *args)
{
  char *expr_str;
  struct value *result = NULL;
  volatile struct gdb_exception except;

  if (!PyArg_ParseTuple (args, "s", &expr_str))
    return NULL;

  TRY_CATCH (except, RETURN_MASK_ALL)
    {
      result = parse_and_eval (expr_str);
    }
  GDB_PY_HANDLE_EXCEPTION (except);

  return value_to_value_object (result);
}



/* Posting and handling events.  */

/* A single event.  */
struct gdbpy_event
{
  /* The Python event.  This is just a callable object.  */
  PyObject *event;
  /* The next event.  */
  struct gdbpy_event *next;
};

/* All pending events.  */
static struct gdbpy_event *gdbpy_event_list;
/* The final link of the event list.  */
static struct gdbpy_event **gdbpy_event_list_end;

/* We use a file handler, and not an async handler, so that we can
   wake up the main thread even when it is blocked in poll().  */
static int gdbpy_event_fds[2];

/* The file handler callback.  This reads from the internal pipe, and
   then processes the Python event queue.  This will always be run in
   the main gdb thread.  */
static void
gdbpy_run_events (int err, gdb_client_data ignore)
{
  PyGILState_STATE state;
  char buffer[100];
  int r;

  state = PyGILState_Ensure ();

  /* Just read whatever is available on the fd.  It is relatively
     harmless if there are any bytes left over.  */
  r = read (gdbpy_event_fds[0], buffer, sizeof (buffer));

  while (gdbpy_event_list)
    {
      /* Dispatching the event might push a new element onto the event
	 loop, so we update here "atomically enough".  */
      struct gdbpy_event *item = gdbpy_event_list;
      gdbpy_event_list = gdbpy_event_list->next;
      if (gdbpy_event_list == NULL)
	gdbpy_event_list_end = &gdbpy_event_list;

      /* Ignore errors.  */
      PyObject_CallObject (item->event, NULL);

      Py_DECREF (item->event);
      xfree (item);
    }

  PyGILState_Release (state);
}

/* Submit an event to the gdb thread.  */
static PyObject *
gdbpy_post_event (PyObject *self, PyObject *args)
{
  struct gdbpy_event *event;
  PyObject *func;
  int wakeup;

  if (!PyArg_ParseTuple (args, "O", &func))
    return NULL;

  if (!PyCallable_Check (func))
    {
      PyErr_SetString (PyExc_RuntimeError, "Posted event is not callable");
      return NULL;
    }

  Py_INCREF (func);

  /* From here until the end of the function, we have the GIL, so we
     can operate on our global data structures without worrying.  */
  wakeup = gdbpy_event_list == NULL;

  event = XNEW (struct gdbpy_event);
  event->event = func;
  event->next = NULL;
  *gdbpy_event_list_end = event;
  gdbpy_event_list_end = &event->next;

  /* Wake up gdb when needed.  */
  if (wakeup)
    {
      char c = 'q';		/* Anything. */
      if (write (gdbpy_event_fds[1], &c, 1) != 1)
        return PyErr_SetFromErrno (PyExc_IOError);
    }

  Py_RETURN_NONE;
}

/* Initialize the Python event handler.  */
static void
gdbpy_initialize_events (void)
{
  if (!pipe (gdbpy_event_fds))
    {
      gdbpy_event_list_end = &gdbpy_event_list;
      add_file_handler (gdbpy_event_fds[0], gdbpy_run_events, NULL);
    }
}



/* Printing.  */

/* A python function to write a single string using gdb's filtered
   output stream.  */
static PyObject *
gdbpy_write (PyObject *self, PyObject *args)
{
  char *arg;
  if (! PyArg_ParseTuple (args, "s", &arg))
    return NULL;
  printf_filtered ("%s", arg);
  Py_RETURN_NONE;
}

/* A python function to flush gdb's filtered output stream.  */
static PyObject *
gdbpy_flush (PyObject *self, PyObject *args)
{
  gdb_flush (gdb_stdout);
  Py_RETURN_NONE;
}

/* Print a python exception trace, or print nothing and clear the
   python exception, depending on gdbpy_should_print_stack.  Only call
   this if a python exception is set.  */
void
gdbpy_print_stack (void)
{
  if (gdbpy_should_print_stack)
    PyErr_Print ();
  else
    PyErr_Clear ();
}



/* Script interface.  */

/* True if 'gdb -P' was used, false otherwise.  */
static int running_python_script;

/* True if we are currently in a call to 'gdb.cli', false otherwise.  */
static int in_cli;

/* Enter the command loop.  */

static PyObject *
gdbpy_cli (PyObject *unused1, PyObject *unused2)
{
  if (! running_python_script || in_cli)
    return PyErr_Format (PyExc_RuntimeError, "cannot invoke CLI recursively");

  in_cli = 1;
  cli_command_loop ();
  in_cli = 0;

  Py_RETURN_NONE;
}

/* Set up the Python argument vector and evaluate a script.  This is
   used to implement 'gdb -P'.  */

void
run_python_script (int argc, char **argv)
{
  FILE *input;
  PyGILState_STATE state;

  /* We never free this, since we plan to exit at the end.  */
  state = PyGILState_Ensure ();

  running_python_script = 1;
  PySys_SetArgv (argc - 1, argv + 1);
  input = fopen (argv[0], "r");
  if (! input)
    {
      fprintf (stderr, "could not open %s: %s\n", argv[0], strerror (errno));
      exit (1);
    }
  PyRun_SimpleFile (input, argv[0]);
  fclose (input);
  exit (0);
}

void
source_python_script (FILE *stream, char *file)
{
  PyGILState_STATE state;

  state = PyGILState_Ensure ();

  PyRun_SimpleFile (stream, file);

  fclose (stream);
  PyGILState_Release (state);
}



/* The "current" objfile.  This is set when gdb detects that a new
   objfile has been loaded.  It is only set for the duration of a call
   to gdbpy_new_objfile; it is NULL at other times.  */
static struct objfile *gdbpy_current_objfile;

/* The file name we attempt to read.  */
#define GDBPY_AUTO_FILENAME "-gdb.py"

/* This is a new_objfile observer callback which loads python code
   based on the path to the objfile.  */
static void
gdbpy_new_objfile (struct objfile *objfile)
{
  char *realname;
  char *filename, *debugfile;
  int len;
  FILE *input;
  PyGILState_STATE state;
  struct cleanup *cleanups;

  if (!gdbpy_auto_load || !objfile || !objfile->name)
    return;

  state = PyGILState_Ensure ();

  gdbpy_current_objfile = objfile;

  realname = gdb_realpath (objfile->name);
  len = strlen (realname);
  filename = xmalloc (len + sizeof (GDBPY_AUTO_FILENAME));
  memcpy (filename, realname, len);
  strcpy (filename + len, GDBPY_AUTO_FILENAME);

  input = fopen (filename, "r");
  debugfile = filename;

  cleanups = make_cleanup (xfree, filename);
  make_cleanup (xfree, realname);

  if (!input && debug_file_directory)
    {
      /* Also try the same file in the separate debug info directory.  */
      debugfile = xmalloc (strlen (filename)
			   + strlen (debug_file_directory) + 1);
      strcpy (debugfile, debug_file_directory);
      /* FILENAME is absolute, so we don't need a "/" here.  */
      strcat (debugfile, filename);

      make_cleanup (xfree, debugfile);
      input = fopen (debugfile, "r");
    }

  if (!input && gdb_datadir)
    {
      /* Also try the same file in a subdirectory of gdb's data
	 directory.  */
      debugfile = xmalloc (strlen (gdb_datadir) + strlen (filename)
			   + strlen ("/auto-load") + 1);
      strcpy (debugfile, gdb_datadir);
      strcat (debugfile, "/auto-load");
      /* FILENAME is absolute, so we don't need a "/" here.  */
      strcat (debugfile, filename);

      make_cleanup (xfree, debugfile);
      input = fopen (debugfile, "r");
    }

  if (input)
    {
      /* We don't want to throw an exception here -- but the user
	 would like to know that something went wrong.  */
      if (PyRun_SimpleFile (input, debugfile))
	gdbpy_print_stack ();
      fclose (input);
    }

  do_cleanups (cleanups);
  gdbpy_current_objfile = NULL;

  PyGILState_Release (state);
}

/* Return the current Objfile, or None if there isn't one.  */
static PyObject *
gdbpy_get_current_objfile (PyObject *unused1, PyObject *unused2)
{
  PyObject *result;

  if (! gdbpy_current_objfile)
    Py_RETURN_NONE;

  result = objfile_to_objfile_object (gdbpy_current_objfile);
  if (result)
    Py_INCREF (result);
  return result;
}

/* Return a sequence holding all the Objfiles.  */
static PyObject *
gdbpy_objfiles (PyObject *unused1, PyObject *unused2)
{
  struct objfile *objf;
  PyObject *list;

  list = PyList_New (0);
  if (!list)
    return NULL;

  ALL_OBJFILES (objf)
  {
    PyObject *item = objfile_to_objfile_object (objf);
    if (!item || PyList_Append (list, item) == -1)
      {
	Py_DECREF (list);
	return NULL;
      }
  }

  return list;
}

#else /* HAVE_PYTHON */

/* Dummy implementation of the gdb "python" command.  */

static void
python_command (char *arg, int from_tty)
{
  while (arg && *arg && isspace (*arg))
    ++arg;
  if (arg && *arg)
    error (_("Python scripting is not supported in this copy of GDB."));
  else
    {
      struct command_line *l = get_command_line (python_control, "");
      struct cleanup *cleanups = make_cleanup_free_command_lines (&l);
      execute_control_command_untraced (l);
      do_cleanups (cleanups);
    }
}

void
eval_python_from_control_command (struct command_line *cmd)
{
  error (_("Python scripting is not supported in this copy of GDB."));
}

void
source_python_script (FILE *stream)
{
  fclose (stream);
  error (_("Python scripting is not supported in this copy of GDB."));
}

#endif /* HAVE_PYTHON */



/* Lists for 'maint set python' commands.  */

static struct cmd_list_element *set_python_list;
static struct cmd_list_element *show_python_list;

/* Function for use by 'maint set python' prefix command.  */

static void
set_python (char *args, int from_tty)
{
  help_list (set_python_list, "maintenance set python ", -1, gdb_stdout);
}

/* Function for use by 'maint show python' prefix command.  */

static void
show_python (char *args, int from_tty)
{
  cmd_show_list (show_python_list, from_tty, "");
}

/* Initialize the Python code.  */

/* Provide a prototype to silence -Wmissing-prototypes.  */
extern initialize_file_ftype _initialize_python;

void
_initialize_python (void)
{
  add_com ("python", class_obscure, python_command,
#ifdef HAVE_PYTHON
	   _("\
Evaluate a Python command.\n\
\n\
The command can be given as an argument, for instance:\n\
\n\
    python print 23\n\
\n\
If no argument is given, the following lines are read and used\n\
as the Python commands.  Type a line containing \"end\" to indicate\n\
the end of the command.")
#else /* HAVE_PYTHON */
	   _("\
Evaluate a Python command.\n\
\n\
Python scripting is not supported in this copy of GDB.\n\
This command is only a placeholder.")
#endif /* HAVE_PYTHON */
	   );

  add_prefix_cmd ("python", no_class, show_python,
		  _("Prefix command for python maintenance settings."),
		  &show_python_list, "maintenance show python ", 0,
		  &maintenance_show_cmdlist);
  add_prefix_cmd ("python", no_class, set_python,
		  _("Prefix command for python maintenance settings."),
		  &set_python_list, "maintenance set python ", 0,
		  &maintenance_set_cmdlist);

  add_setshow_boolean_cmd ("print-stack", class_maintenance,
			   &gdbpy_should_print_stack, _("\
Enable or disable printing of Python stack dump on error."), _("\
Show whether Python stack will be printed on error."), _("\
Enables or disables printing of Python stack traces."),
			   NULL, NULL,
			   &set_python_list,
			   &show_python_list);

  add_setshow_boolean_cmd ("auto-load", class_maintenance,
			   &gdbpy_auto_load, _("\
Enable or disable auto-loading of Python code when an object is opened."), _("\
Show whether Python code will be auto-loaded when an object is opened."), _("\
Enables or disables auto-loading of Python code when an object is opened."),
			   NULL, NULL,
			   &set_python_list,
			   &show_python_list);

#ifdef HAVE_PYTHON
  Py_Initialize ();
  PyEval_InitThreads ();

  gdb_module = Py_InitModule ("gdb", GdbMethods);

  /* The casts to (char*) are for python 2.4.  */
  PyModule_AddStringConstant (gdb_module, "VERSION", (char*) version);
  PyModule_AddStringConstant (gdb_module, "HOST_CONFIG", (char*) host_name);
  PyModule_AddStringConstant (gdb_module, "TARGET_CONFIG", (char*) target_name);
#ifdef PYTHONDIR
  PyModule_AddStringConstant (gdb_module, "pythondir", PYTHONDIR);
#else
  if (gdb_datadir)
    PyModule_AddStringConstant (gdb_module, "datadir", gdb_datadir);
#endif

  gdbpy_initialize_values ();
  gdbpy_initialize_breakpoints ();
  gdbpy_initialize_frames ();
  gdbpy_initialize_symtabs ();
  gdbpy_initialize_commands ();
  gdbpy_initialize_symbols ();
  gdbpy_initialize_blocks ();
  gdbpy_initialize_functions ();
  gdbpy_initialize_types ();
  gdbpy_initialize_parameters ();
  gdbpy_initialize_objfile ();
  gdbpy_initialize_thread ();
  gdbpy_initialize_inferior ();
  gdbpy_initialize_events ();

  PyRun_SimpleString ("import gdb");
  PyRun_SimpleString ("gdb.pretty_printers = []");

  observer_attach_new_objfile (gdbpy_new_objfile);

  gdbpy_to_string_cst = PyString_FromString ("to_string");
  gdbpy_children_cst = PyString_FromString ("children");
  gdbpy_display_hint_cst = PyString_FromString ("display_hint");
  gdbpy_doc_cst = PyString_FromString ("__doc__");

  /* Create a couple objects which are used for Python's stdout and
     stderr.  */
  PyRun_SimpleString ("\
import sys\n\
class GdbOutputFile:\n\
  def close(self):\n\
    # Do nothing.\n\
    return None\n\
\n\
  def isatty(self):\n\
    return False\n\
\n\
  def write(self, s):\n\
    gdb.write(s)\n\
\n\
  def writelines(self, iterable):\n\
    for line in iterable:\n\
      self.write(line)\n\
\n\
  def flush(self):\n\
    gdb.flush()\n\
\n\
sys.stderr = GdbOutputFile()\n\
sys.stdout = GdbOutputFile()\n\
if hasattr (gdb, 'datadir'):\n\
  gdb.pythondir = gdb.datadir + '/python'\n\
if hasattr (gdb, 'pythondir'):\n\
  sys.path.insert(0, gdb.pythondir)\n\
  gdb.__path__ = [gdb.pythondir + '/gdb']\n\
  from os.path import exists\n\
  ipy = gdb.pythondir + '/gdb/__init__.py'\n\
  if exists (ipy):\n\
    execfile (ipy)\n\
");

  /* Release the GIL while gdb runs.  */
  PyThreadState_Swap (NULL);
  PyEval_ReleaseLock ();

#endif /* HAVE_PYTHON */
}



#if HAVE_PYTHON

static PyMethodDef GdbMethods[] =
{
  { "history", gdbpy_history, METH_VARARGS,
    "Get a value from history" },
  { "execute", execute_gdb_command, METH_VARARGS,
    "Execute a gdb command" },
  { "cli", gdbpy_cli, METH_NOARGS,
    "Enter the gdb CLI" },
  { "parameter", gdbpy_parameter, METH_VARARGS,
    "Return a gdb parameter's value" },

  { "breakpoints", gdbpy_breakpoints, METH_NOARGS,
    "Return a tuple of all breakpoint objects" },

  { "default_visualizer", gdbpy_default_visualizer, METH_VARARGS,
    "Find the default visualizer for a Value." },

  { "current_objfile", gdbpy_get_current_objfile, METH_NOARGS,
    "Return the current Objfile being loaded, or None." },
  { "objfiles", gdbpy_objfiles, METH_NOARGS,
    "Return a sequence of all loaded objfiles." },

  { "selected_frame", gdbpy_selected_frame, METH_NOARGS,
    "selected_frame () -> gdb.Frame.\n\
Return the selected frame object." },
  { "frame_stop_reason_string", gdbpy_frame_stop_reason_string, METH_VARARGS,
    "stop_reason_string (Integer) -> String.\n\
Return a string explaining unwind stop reason." },

  { "lookup_type", (PyCFunction) gdbpy_lookup_type,
    METH_VARARGS | METH_KEYWORDS,
    "lookup_type (name [, block]) -> type\n\
Return a Type corresponding to the given name." },

  { "lookup_symbol", (PyCFunction) gdbpy_lookup_symbol,
    METH_VARARGS | METH_KEYWORDS,
    "lookup_symbol (name [, block] [, domain]) -> (symbol, is_field_of_this)\n\
Return a tuple with the symbol corresponding to the given name (or None) and\n\
a boolean indicating if name is a field of the current implied argument\n\
`this' (when the current language is object-oriented)." },
  { "solib_address", gdbpy_solib_address, METH_VARARGS,
    "solib_address (Long) -> String.\n\
Return the name of the shared library holding a given address, or None." },

  { "block_for_pc", gdbpy_block_for_pc, METH_VARARGS,
    "Return the block containing the given pc value, or None." },

  { "decode_line", gdbpy_decode_line, METH_VARARGS,
    "Decode a string argument the way that 'break' or 'edit' does.\n\
Return a tuple holding the file name (or None) and line number (or None).\n\
Note: may later change to return an object." },

  { "selected_thread", gdbpy_selected_thread, METH_NOARGS,
    "selected_thread () -> gdb.InferiorThread.\n\
Return the selected thread object." },
  { "inferiors", gdbpy_inferiors, METH_NOARGS,
    "inferiors () -> (gdb.Inferior, ...).\n\
Return a tuple containing all inferiors." },

  { "parse_and_eval", gdbpy_parse_and_eval, METH_VARARGS,
    "Parse a string as an expression, evaluate it, and return the result." },

  { "post_event", gdbpy_post_event, METH_VARARGS,
    "Post an event into gdb's event loop." },

  { "write", gdbpy_write, METH_VARARGS,
    "Write a string using gdb's filtered stream." },
  { "flush", gdbpy_flush, METH_NOARGS,
    "Flush gdb's filtered stdout stream." },

  {NULL, NULL, 0, NULL}
};

#endif /* HAVE_PYTHON */
