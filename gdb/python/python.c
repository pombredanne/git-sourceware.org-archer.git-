/* General python/gdb code

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
#include "command.h"
#include "ui-out.h"
#include "cli/cli-script.h"
#include "gdbcmd.h"
#include "objfiles.h"
#include "observer.h"
#include "gdb_regex.h"
#include "language.h"

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


PyObject *gdb_module;

/* Some string constants we may wish to use.  */
PyObject *gdbpy_to_string_cst;
PyObject *gdbpy_children_cst;

static PyObject *get_parameter (PyObject *, PyObject *);
static PyObject *execute_gdb_command (PyObject *, PyObject *);
static PyObject *gdbpy_solib_address (PyObject *, PyObject *);
static PyObject *gdbpy_decode_line (PyObject *, PyObject *);
static PyObject *gdbpy_find_pc_function (PyObject *, PyObject *);
static PyObject *gdbpy_get_threads (PyObject *, PyObject *);
static PyObject *gdbpy_get_current_thread (PyObject *, PyObject *);
static PyObject *gdbpy_switch_to_thread (PyObject *, PyObject *);
static PyObject *gdbpy_write (PyObject *, PyObject *);
static PyObject *gdbpy_flush (PyObject *, PyObject *);
static PyObject *gdbpy_cli (PyObject *, PyObject *);
static PyObject *gdbpy_get_default_visualizer (PyObject *, PyObject *);

static PyMethodDef GdbMethods[] =
{
  { "get_value_from_history", gdbpy_get_value_from_history, METH_VARARGS,
    "Get a value from history" },
  { "execute", execute_gdb_command, METH_VARARGS,
    "Execute a gdb command" },
  { "cli", gdbpy_cli, METH_NOARGS,
    "Enter the gdb CLI" },
  { "get_parameter", get_parameter, METH_VARARGS,
    "Return a gdb parameter's value" },

  { "get_breakpoints", gdbpy_get_breakpoints, METH_NOARGS,
    "Return a tuple of all breakpoint objects" },

  { "get_default_visualizer", gdbpy_get_default_visualizer, METH_VARARGS,
    "Find the default visualizer for a Value." },

  { "get_frames", gdbpy_get_frames, METH_NOARGS,
    "Return a tuple of all frame objects" },
  { "get_current_frame", gdbpy_get_current_frame, METH_NOARGS,
    "Return the current frame object" },
  { "get_selected_frame", gdbpy_get_selected_frame, METH_NOARGS,
    "Return the selected frame object" },
  { "frame_stop_reason_string", gdbpy_frame_stop_reason_string,
    METH_VARARGS, "Return a string explaining unwind stop reason" },

  { "lookup_symbol", gdbpy_lookup_symbol, METH_VARARGS,
    "Return the symbol corresponding to the given name, or None." },
  { "solib_address", gdbpy_solib_address, METH_VARARGS,
    "Return shared library holding a given address, or None." },

  { "find_pc_function", gdbpy_find_pc_function, METH_VARARGS,
    "Return the function containing the given pc value, or None." },

  { "decode_line", gdbpy_decode_line, METH_VARARGS,
    "Decode a string argument the way that 'break' or 'edit' does.\n\
Return a tuple holding the file name (or None) and line number (or None).\n\
Note: may later change to return an object." },

  { "get_threads", gdbpy_get_threads, METH_NOARGS,
    "Return a tuple holding all the valid thread IDs." },
  { "get_current_thread", gdbpy_get_current_thread, METH_NOARGS,
    "Return the thread ID of the current thread." },
  { "switch_to_thread", gdbpy_switch_to_thread, METH_VARARGS,
    "Switch to a thread, given the thread ID." },

  { "write", gdbpy_write, METH_VARARGS,
    "Write a string using gdb's filtered stream." },
  { "flush", gdbpy_flush, METH_NOARGS,
    "Flush gdb's filtered stdout stream." },

  {NULL, NULL, 0, NULL}
};

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
  char *script;

  if (cmd->body_count != 1)
    error (_("Invalid \"python\" block structure."));

  script = compute_python_string (cmd->body_list[0]);
  PyRun_SimpleString (script);
  xfree (script);
  if (PyErr_Occurred ())
    {
      gdbpy_print_stack ();
      error (_("error while executing Python code"));
    }
}

/* Implementation of the gdb "python" command.  */

static void
python_command (char *arg, int from_tty)
{
  while (arg && *arg && isspace (*arg))
    ++arg;
  if (arg && *arg)
    {
      PyRun_SimpleString (arg);
      if (PyErr_Occurred ())
	{
	  gdbpy_print_stack ();
	  error (_("error while executing Python code"));
	}
    }
  else
    {
      struct command_line *l = get_command_line (python_control, "");
      struct cleanup *cleanups = make_cleanup_free_command_lines (&l);
      execute_control_command_untraced (l);
      do_cleanups (cleanups);
    }
}



/* Transform a gdb parameters's value into a Python value.  May return
   NULL (and set a Python exception) on error.  Helper function for
   get_parameter.  */

static PyObject *
parameter_to_python (struct cmd_list_element *cmd)
{
  switch (cmd->var_type)
    {
    case var_string:
    case var_string_noescape:
    case var_optional_filename:
    case var_filename:
    case var_enum:
      {
	char *str = * (char **) cmd->var;
	if (! str)
	  str = "";
	return PyString_Decode (str, strlen (str), host_charset (), NULL);
      }

    case var_boolean:
      {
	if (* (int *) cmd->var)
	  Py_RETURN_TRUE;
	else
	  Py_RETURN_FALSE;
      }

    case var_auto_boolean:
      {
	enum auto_boolean ab = * (enum auto_boolean *) cmd->var;
	if (ab == AUTO_BOOLEAN_TRUE)
	  Py_RETURN_TRUE;
	else if (ab == AUTO_BOOLEAN_FALSE)
	  Py_RETURN_FALSE;
	else
	  Py_RETURN_NONE;
      }

    case var_integer:
      if ((* (int *) cmd->var) == INT_MAX)
	Py_RETURN_NONE;
      /* Fall through.  */
    case var_zinteger:
      return PyLong_FromLong (* (int *) cmd->var);

    case var_uinteger:
      {
	unsigned int val = * (unsigned int *) cmd->var;
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
get_parameter (PyObject *self, PyObject *args)
{
  struct cmd_list_element *alias, *prefix, *cmd;
  char *arg, *newarg;
  volatile struct gdb_exception except;

  if (! PyArg_ParseTuple (args, "s", &arg))
    return NULL;

  newarg = concat ("show ", arg, (char *) NULL);

  TRY_CATCH (except, RETURN_MASK_ALL)
    {
      if (! lookup_cmd_composition (newarg, &alias, &prefix, &cmd))
	{
	  xfree (newarg);
	  return PyErr_Format (PyExc_RuntimeError,
			       "could not find parameter `%s'", arg);
	}
    }
  xfree (newarg);
  GDB_PY_HANDLE_EXCEPTION (except);

  if (! cmd->var)
    return PyErr_Format (PyExc_RuntimeError, "`%s' is not a parameter", arg);
  return parameter_to_python (cmd);
}

/* A Python function which evaluates a string using the gdb CLI.  */

static PyObject *
execute_gdb_command (PyObject *self, PyObject *args)
{
  struct cmd_list_element *alias, *prefix, *cmd;
  char *arg, *newarg;
  volatile struct gdb_exception except;

  if (! PyArg_ParseTuple (args, "s", &arg))
    return NULL;

  TRY_CATCH (except, RETURN_MASK_ALL)
    {
      execute_command (arg, 0);
    }
  GDB_PY_HANDLE_EXCEPTION (except);

  /* Do any commands attached to breakpoint we stopped at.  */
  bpstat_do_actions ();

  Py_RETURN_NONE;
}

static PyObject *
gdbpy_solib_address (PyObject *self, PyObject *args)
{
  unsigned long long pc;
  char *soname;
  PyObject *str_obj;

  if (!PyArg_ParseTuple (args, "K", &pc))
    return NULL;

  soname = solib_address (pc);
  if (soname)
    str_obj = PyString_Decode (soname, strlen (soname), host_charset (), NULL);
  else
    {
      str_obj = Py_None;
      Py_INCREF (Py_None);
    }

  return str_obj;
}

static PyObject *
gdbpy_find_pc_function (PyObject *self, PyObject *args)
{
  unsigned long long pc;
  struct symbol *sym;
  PyObject *sym_obj;

  if (!PyArg_ParseTuple (args, "K", &pc))
    return NULL;

  sym = find_pc_function (pc);
  if (sym)
    return symbol_to_symbol_object (sym);

  Py_RETURN_NONE;
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



/* Threads.  */

/* Callback function for use with iterate_over_threads.  This function
   just counts the number of threads.  */

static int
count_callback (struct thread_info *info, void *user_data)
{
  int *count = (int *) user_data;
  ++*count;
  return 0;
}

/* Structure for storing some state when iterating over threads.  */

struct set_thread_info
{
  PyObject *tuple;
  int index;
};

/* Callback function for use with iterate_over_threads.  This function
   stores the thread ID into a Python tuple.  */

static int
update_tuple_callback (struct thread_info *info, void *user_data)
{
  struct set_thread_info *tinfo = (struct set_thread_info *) user_data;
  PyTuple_SetItem (tinfo->tuple, tinfo->index, PyInt_FromLong (info->num));
  ++tinfo->index;
  return 0;
}

/* Python function which yields a tuple holding all valid thread IDs.  */

static PyObject *
gdbpy_get_threads (PyObject *unused1, PyObject *unused2)
{
  int thread_count = 0;
  struct set_thread_info info;
  PyObject *result;

  prune_threads ();
  target_find_new_threads ();

  iterate_over_threads (count_callback, &thread_count);

  if (!thread_count)
    Py_RETURN_NONE;

  result = PyTuple_New (thread_count);
  info.tuple = result;
  info.index = 0;
  iterate_over_threads (update_tuple_callback, &info);
  return result;
}

/* Python function that returns the current thread's ID.  */

static PyObject *
gdbpy_get_current_thread (PyObject *unused1, PyObject *unused2)
{
  if (PIDGET (inferior_ptid) == 0)
    Py_RETURN_NONE;
  return PyInt_FromLong (pid_to_thread_id (inferior_ptid));
}

/* Python function for switching to a given thread.  */

static PyObject *
gdbpy_switch_to_thread (PyObject *self, PyObject *args)
{
  int id;
  if (! PyArg_ParseTuple (args, "i", &id))
    return NULL;
  if (! valid_thread_id (id))
    return PyErr_Format (PyExc_RuntimeError, "invalid thread id");
  switch_to_thread (thread_id_to_pid (id));
  Py_RETURN_NONE;
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



/* The file name we attempt to read.  */
#define GDBPY_AUTO_FILENAME ".gdb.py"

/* This is a new_objfile observer callback which loads python code
   based on the path to the objfile.  */
static void
gdbpy_new_objfile (struct objfile *objfile)
{
  char *p;
  char *filename;
  int len;
  FILE *input;

  if (!gdbpy_auto_load || !objfile || !objfile->name)
    return;

  p = (char *) lbasename (objfile->name);
  len = p - objfile->name;
  filename = xmalloc (len + sizeof (GDBPY_AUTO_FILENAME));
  memcpy (filename, objfile->name, len);
  strcpy (filename + len, GDBPY_AUTO_FILENAME);

  input = fopen (filename, "r");

  if (input)
    {
      /* We don't want to throw an exception here -- but the user
	 would like to know that something went wrong.  */
      if (PyRun_SimpleFile (input, filename))
	gdbpy_print_stack ();
      fclose (input);
    }

  xfree (filename);
}



/* Return a string representing TYPE.  */
static char *
get_type (struct type *type)
{
  struct cleanup *old_chain;
  struct ui_file *stb;
  char *thetype;
  long length;

  stb = mem_fileopen ();
  old_chain = make_cleanup_ui_file_delete (stb);

  CHECK_TYPEDEF (type);

  type_print (type, "", stb, -1);

  thetype = ui_file_xstrdup (stb, &length);
  do_cleanups (old_chain);
  return thetype;
}

/* Find the pretty-printing function for TYPE.  If no pretty-printer
   exists, return NULL.  If one exists, return a borrowed reference.
   If a printer is found, *DICTP is set to a reference to the
   dictionary object; it must be derefed by the caller.  DICT_NAME is
   the name of the dictionary to search for types.  */
static PyObject *
find_pretty_printer (struct type *type, PyObject **dictp, char *dict_name)
{
  PyObject *dict, *key, *func, *found = NULL;
  Py_ssize_t iter;
  char *type_name = NULL;
  volatile struct gdb_exception except;

  /* Fetch the pretty printer dictionary.  */
  if (! PyObject_HasAttrString (gdb_module, dict_name))
    return NULL;
  dict = PyObject_GetAttrString (gdb_module, dict_name);
  if (! dict)
    return NULL;
  if (! PyDict_Check (dict) || ! PyDict_Size (dict))
    {
      Py_DECREF (dict);
      return NULL;
    }

  /* Get the name of the type.  */
  TRY_CATCH (except, RETURN_MASK_ALL)
    {
      type_name = get_type (type);
    }
  if (except.reason < 0)
    {
      Py_DECREF (dict);
      return NULL;
    }

  /* See if the type matches a pretty-printer regexp.  */
  iter = 0;
  while (! found && PyDict_Next (dict, &iter, &key, &func))
    {
      char *rx_str;

      if (! PyString_Check (key))
	continue;
      rx_str = PyString_AsString (key);
      if (re_comp (rx_str) == NULL && re_exec (type_name) == 1)
	found = func;
    }

  xfree (type_name);

  if (found)
    *dictp = dict;
  else
    Py_DECREF (dict);

  return found;
}

/* Pretty-print a single value, VALUE, using the printer function
   FUNC.  If the function returns a string, an xmalloc()d copy is
   returned.  Otherwise, if the function returns a value, a *OUT_VALUE
   is set to the value, and NULL is returned.  On error, *OUT_VALUE is
   set to NULL and NULL is returned.  If CHILDREN is true, we may also
   try to call an object's "children" method and format the output
   accordingly.  */
static char *
pretty_print_one_value (PyObject *func, struct value *value,
			struct value **out_value, int children)
{
  char *output = NULL;
  volatile struct gdb_exception except;

  TRY_CATCH (except, RETURN_MASK_ALL)
    {
      PyObject *val_obj, *result;

      /* FIXME: memory management here.  Why are values so
	 funny?  */
      value = value_copy (value);

      val_obj = value_to_value_object (value);

      /* The function might be an MI-style class with a to_string
	 method, or it might be an ordinary function.  FIXME: should
	 also check for the 'children' method here.  */
      if (PyObject_HasAttr (func, gdbpy_to_string_cst))
	result = PyObject_CallMethodObjArgs (func, gdbpy_to_string_cst,
					     val_obj, NULL);
      else if (children
	       && PyObject_HasAttr (func, gdbpy_children_cst)
	       && PyObject_HasAttrString (gdb_module, "_format_children"))
	{
	  PyObject *fmt = PyObject_GetAttrString (gdb_module,
						  "_format_children");
	  result = PyObject_CallFunctionObjArgs (fmt, func, val_obj, NULL);
	  Py_DECREF (fmt);
	}
      else
	result = PyObject_CallFunctionObjArgs (func, val_obj, NULL);
      if (result)
	{
	  if (PyString_Check (result))
	    output = xstrdup (PyString_AsString (result));
	  else
	    *out_value = convert_value_from_python (result);
	  Py_DECREF (result);
	}
      else
	gdbpy_print_stack ();

      Py_DECREF (val_obj);
    }

  return output;
}

/* Try to pretty-print VALUE.  Return an xmalloc()d string
   representation of the value.  If the result is NULL, and *OUT_VALUE
   is set, then *OUT_VALUE is a value which should be printed in place
   of VALUE.  *OUT_VALUE is not passed back to the pretty-printer.
   Returns NULL and sets *OUT_VALUE to NULL on error or if no
   pretty-printer was available.  */
char *
apply_pretty_printer (struct value *value, struct value **out_value)
{
  PyObject *dict, *func;
  char *output;

  *out_value = NULL;

  func = find_pretty_printer (value_type (value), &dict, "cli_pretty_printers");
  if (! func)
    return NULL;

  output = pretty_print_one_value (func, value, out_value, 1);

  Py_DECREF (dict);

  return output;
}

/* Like apply_pretty_printer, but called from the 'val' (and not
   'value') printing code.  Arguments are as for val_print.  Returns
   an xmalloc()d pretty-printed string if a pretty-printer was found
   and was successful.  */
char *
apply_val_pretty_printer (struct type *type, const gdb_byte *valaddr,
			  int embedded_offset, CORE_ADDR address,
			  struct ui_file *stream, int format,
			  int deref_ref, int recurse,
			  enum val_prettyprint pretty,
			  const struct language_defn *language)
{
  PyObject *dict, *func;
  struct value *value, *replacement = NULL;
  char *output;

  func = find_pretty_printer (type, &dict, "cli_pretty_printers");
  if (! func)
    return NULL;

  value = value_from_contents (type, valaddr, embedded_offset, address);
  output = pretty_print_one_value (func, value, &replacement, 1);

  Py_DECREF (dict);

  if (output)
    return output;

  if (! replacement)
    return NULL;

  language->la_val_print (value_type (replacement),
			  value_contents_all (replacement),
			  value_embedded_offset (replacement),
			  VALUE_ADDRESS (replacement),
			  stream, format, deref_ref, recurse,
			  pretty);

  return xstrdup ("");
}

/* Apply a pretty-printer for the varobj code.  PRINTER_OBJ is the
   print object.  It must have a 'to_string' method (but this is
   checked by varobj, not here) which accepts a gdb.Value and returns
   a string.  This returns an xmalloc()d string, or NULL on error.  */
char *
apply_varobj_pretty_printer (PyObject *printer_obj, struct value *value)
{
  struct value *out_value = NULL;
  char *result = pretty_print_one_value (printer_obj, value, &out_value, 0);

  if (!result && out_value)
    {
      struct ui_file *stb;
      struct cleanup *old_chain;
      long dummy;

      stb = mem_fileopen ();
      old_chain = make_cleanup_ui_file_delete (stb);

      common_val_print (value, stb, 0, 1, 0, 0, current_language);
      result = ui_file_xstrdup (stb, &dummy);

      do_cleanups (old_chain);
    }

  return result;
}

/* Find a pretty-printer object for the varobj module.  Returns a
   borrowed reference to the object if successful; returns NULL if
   not.  TYPE is the type of the varobj for which a printer should be
   returned.  */
PyObject *
gdbpy_get_varobj_pretty_printer (struct type *type)
{
  PyObject *dict = NULL;
  PyObject *printer = find_pretty_printer (type, &dict, "mi_pretty_printers");
  if (dict)
    {
      Py_DECREF (dict);
    }
  return printer;
}

/* A Python function which wraps gdbpy_get_varobj_pretty_printer and
   instantiates the resulting class.  This accepts a Value argument
   and returns a pretty printer instance, or None.  This function is
   useful as an argument to the MI command -var-set-visualizer.  */
static PyObject *
gdbpy_get_default_visualizer (PyObject *self, PyObject *args)
{
  PyObject *val_obj;
  PyObject *result;
  struct value *value;

  if (! PyArg_ParseTuple (args, "O", &val_obj))
    return NULL;
  value = value_object_to_value (val_obj);
  if (! value)
    {
      PyErr_SetString (PyExc_RuntimeError, "argument must be a gdb.Value");
      return NULL;
    }

  result = gdbpy_get_varobj_pretty_printer (value_type (value));
  if (result)
    {
      /* Instantiate it.  */
      result = PyObject_CallFunctionObjArgs (result, NULL);
    }

  if (! result)
    {
      PyErr_Clear ();
      result = Py_None;
    }

  Py_INCREF (result);
  return result;
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

char *
apply_pretty_printer (struct value *ignore, struct value **out)
{
  *out = NULL;
  return NULL;
}

char *
apply_val_pretty_printer (struct type *type, const gdb_byte *valaddr,
			  int embedded_offset, CORE_ADDR address,
			  struct ui_ifle *stream, int format,
			  int deref_ref, int recurse,
			  enum val_prettyprint pretty,
			  const language_defn *language)
{
  return NULL;
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

  gdb_module = Py_InitModule ("gdb", GdbMethods);

  /* The casts to (char*) are for python 2.4.  */
  PyModule_AddStringConstant (gdb_module, "VERSION", (char*) version);
  PyModule_AddStringConstant (gdb_module, "HOST_CONFIG", (char*) host_name);
  PyModule_AddStringConstant (gdb_module, "TARGET_CONFIG", (char*) target_name);

  gdbpy_initialize_values ();
  gdbpy_initialize_breakpoints ();
  gdbpy_initialize_frames ();
  gdbpy_initialize_symtabs ();
  gdbpy_initialize_commands ();
  gdbpy_initialize_symbols ();
  gdbpy_initialize_blocks ();
  gdbpy_initialize_functions ();
  gdbpy_initialize_types ();

  PyRun_SimpleString ("import gdb");
  PyRun_SimpleString ("gdb.cli_pretty_printers = {}");
  PyRun_SimpleString ("gdb.mi_pretty_printers = {}");

  observer_attach_new_objfile (gdbpy_new_objfile);

  gdbpy_to_string_cst = PyString_FromString ("to_string");
  gdbpy_children_cst = PyString_FromString ("children");

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
");

  PyRun_SimpleString ("\
def _format_children(obj, val):\n\
  result = []\n\
  if hasattr(obj, 'header'):\n\
    result.append(obj.header(val))\n\
  max = gdb.get_parameter('print elements')\n\
  i = 0\n\
  for elt in obj.children(val):\n\
    (name, val) = elt\n\
    result.append('%s = %s' % (name, str(val)))\n\
    i = i + 1\n\
    if max == 0 or i == max:\n\
      break\n\
  return '\\n'.join(result)\n\
\n\
gdb._format_children = _format_children\n\
");
#endif /* HAVE_PYTHON */
}
