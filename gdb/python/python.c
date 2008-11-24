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
#include "valprint.h"

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
  volatile struct gdb_exception except;

  if (! PyArg_ParseTuple (args, "s|O!", &arg, &PyBool_Type, &from_tty_obj))
    return NULL;

  from_tty = 0;
  if (from_tty_obj)
    from_tty = from_tty_obj == Py_True;

  TRY_CATCH (except, RETURN_MASK_ALL)
    {
      execute_command (arg, from_tty);
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
  char *filename;
  int len;
  FILE *input;
  PyGILState_STATE state;

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

  if (input)
    {
      /* We don't want to throw an exception here -- but the user
	 would like to know that something went wrong.  */
      if (PyRun_SimpleFile (input, filename))
	gdbpy_print_stack ();
      fclose (input);
    }

  xfree (realname);
  xfree (filename);
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
gdbpy_get_objfiles (PyObject *unused1, PyObject *unused2)
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

/* Helper function for find_pretty_printer which iterates over a
   dictionary and tries to find a match.  */
static PyObject *
search_pp_dictionary (PyObject *dict, char *type_name)
{
  Py_ssize_t iter;
  PyObject *key, *func, *found = NULL;

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

  return found;
}

/* Find the pretty-printing constructor function for TYPE.  If no
   pretty-printer exists, return NULL.  If one exists, return a new
   reference.  */
static PyObject *
find_pretty_printer (struct type *type)
{
  PyObject *dict, *found = NULL;
  char *type_name = NULL;
  struct objfile *obj;
  volatile struct gdb_exception except;

  /* Get the name of the type.  */
  TRY_CATCH (except, RETURN_MASK_ALL)
    {
      /* If we have a reference, use the referenced type.  */
      if (TYPE_CODE (type) == TYPE_CODE_REF)
	type = TYPE_TARGET_TYPE (type);
      /* Strip off any qualifiers from the type.  */
      type = make_cv_type (0, 0, type, NULL);
      type_name = get_type (type);
    }
  if (except.reason < 0)
    return NULL;

  /* Look at the pretty-printer dictionary for each objfile.  */
  ALL_OBJFILES (obj)
  {
    PyObject *objf = objfile_to_objfile_object (obj);
    if (!objf)
      continue;

    dict = objfpy_get_printers (objf, NULL);
    found = search_pp_dictionary (dict, type_name);
    if (found)
      goto done;

    Py_DECREF (dict);
  }

  /* Fetch the global pretty printer dictionary.  */
  dict = NULL;
  if (! PyObject_HasAttrString (gdb_module, "pretty_printers"))
    goto done;
  dict = PyObject_GetAttrString (gdb_module, "pretty_printers");
  if (! dict)
    goto done;
  if (! PyDict_Check (dict) || ! PyDict_Size (dict))
    goto done;

  found = search_pp_dictionary (dict, type_name);

 done:
  xfree (type_name);

  if (found)
    Py_INCREF (found);
  Py_XDECREF (dict);

  return found;
}

/* Pretty-print a single value, via the printer object PRINTER.  If
   the function returns a string, an xmalloc()d copy is returned.
   Otherwise, if the function returns a value, a *OUT_VALUE is set to
   the value, and NULL is returned.  On error, *OUT_VALUE is set to
   NULL and NULL is returned.  */
static char *
pretty_print_one_value (PyObject *printer, struct value **out_value)
{
  char *output = NULL;
  volatile struct gdb_exception except;

  TRY_CATCH (except, RETURN_MASK_ALL)
    {
      PyObject *result;

      result = PyObject_CallMethodObjArgs (printer, gdbpy_to_string_cst, NULL);
      if (result)
	{
	  if (gdbpy_is_string (result))
	    output = python_string_to_host_string (result);
	  else if (PyObject_TypeCheck (result, &value_object_type))
	    {
	      /* If we just call convert_value_from_python for this
		 type, we won't know who owns the result.  For this
		 one case we need to copy the resulting value.  */
	      struct value *v = value_object_to_value (result);
	      *out_value = value_copy (v);
	    }
	  else
	    *out_value = convert_value_from_python (result);
	  Py_DECREF (result);
	}
      else
	gdbpy_print_stack ();
    }

  return output;
}

/* Instantiate a pretty-printer given a constructor, CONS, and a
   value, VAL.  Return NULL on error.  */
PyObject *
gdbpy_instantiate_printer (PyObject *cons, struct value *value)
{
  PyObject *val_obj = NULL, *result;
  volatile struct gdb_exception except;

  TRY_CATCH (except, RETURN_MASK_ALL)
    {
      /* FIXME: memory management here.  Why are values so
	 funny?  */
      value = value_copy (value);
      val_obj = value_to_value_object (value);
    }
  GDB_PY_HANDLE_EXCEPTION (except);

  result = PyObject_CallFunctionObjArgs (cons, val_obj, NULL);
  Py_DECREF (val_obj);
  return result;
}

/* Return the display hint for the object printer, PRINTER.  Return
   NULL if there is no display_hint method, or if the method did not
   return a string.  On error, print stack trace and return NULL.  On
   success, return an xmalloc()d string.  */
char *
gdbpy_get_display_hint (PyObject *printer)
{
  PyObject *hint;
  char *result = NULL;

  if (! PyObject_HasAttr (printer, gdbpy_display_hint_cst))
    return NULL;

  hint = PyObject_CallMethodObjArgs (printer, gdbpy_display_hint_cst, NULL);
  if (gdbpy_is_string (hint))
    result = python_string_to_host_string (hint);
  if (hint)
    Py_DECREF (hint);
  else
    gdbpy_print_stack ();

  return result;
}

/* Helper for apply_val_pretty_printer which calls to_string and
   formats the result.  */
static void
print_string_repr (PyObject *printer, struct ui_file *stream, int recurse,
		   const struct value_print_options *options,
		   const struct language_defn *language)
{
  char *output;
  struct value *replacement = NULL;

  output = pretty_print_one_value (printer, &replacement);
  if (output)
    {
      fputs_filtered (output, stream);
      xfree (output);
    }
  else if (replacement)
    common_val_print (replacement, stream, recurse, options, language);
  else
    gdbpy_print_stack ();
}

/* Helper for apply_val_pretty_printer that formats children of the
   printer, if any exist.  */
static void
print_children (PyObject *printer, struct ui_file *stream, int recurse,
		const struct value_print_options *options,
		const struct language_defn *language)
{
  char *hint;
  int is_map = 0, i;
  PyObject *children, *iter;
  struct cleanup *cleanups;

  if (! PyObject_HasAttr (printer, gdbpy_children_cst))
    return;

  /* If we are printing a map, we want some special formatting.  */
  hint = gdbpy_get_display_hint (printer);
  if (hint)
    {
      is_map = ! strcmp (hint, "map");
      xfree (hint);
    }

  children = PyObject_CallMethodObjArgs (printer, gdbpy_children_cst,
					 NULL);
  if (! children)
    {
      gdbpy_print_stack ();
      return;
    }

  cleanups = make_cleanup_py_decref (children);

  iter = PyObject_GetIter (children);
  if (!iter)
    {
      gdbpy_print_stack ();
      goto done;
    }
  make_cleanup_py_decref (iter);

  for (i = 0; ; ++i)
    {
      PyObject *py_v, *item = PyIter_Next (iter);
      char *name;
      struct cleanup *inner_cleanup;

      if (! item)
	break;

      if (! PyArg_ParseTuple (item, "sO", &name, &py_v))
	{
	  gdbpy_print_stack ();
	  Py_DECREF (item);
	  continue;
	}
      inner_cleanup = make_cleanup_py_decref (item);

      if (i == 0)
	fputs_filtered (" = {", stream);
      else if (! is_map || i % 2 == 0)
	fputs_filtered (options->pretty ? "," : ", ", stream);

      if (options->pretty && (! is_map || i % 2 == 0))
	{
	  fputs_filtered ("\n", stream);
	  print_spaces_filtered (2 + 2 * recurse, stream);
	}

      if (is_map && i % 2 == 0)
	fputs_filtered ("[", stream);
      else if (! is_map)
	{
	  fputs_filtered (name, stream);
	  fputs_filtered (" = ", stream);
	}

      if (gdbpy_is_string (py_v))
	{
	  char *text = python_string_to_host_string (py_v);
	  if (! text)
	    gdbpy_print_stack ();
	  else
	    {
	      fputs_filtered (text, stream);
	      xfree (text);
	    }
	}
      else
	{
	  struct value *value = convert_value_from_python (py_v);
	  common_val_print (value, stream, recurse + 1, options, language);
	}

      if (is_map && i % 2 == 0)
	fputs_filtered ("] = ", stream);
      else if (! options->pretty)
	wrap_here (n_spaces (2 + 2 * recurse));

      do_cleanups (inner_cleanup);
    }

  if (i)
    {
      if (options->pretty)
	{
	  fputs_filtered ("\n", stream);
	  print_spaces_filtered (2 * recurse, stream);
	}
      fputs_filtered ("}", stream);
    }

 done:
  do_cleanups (cleanups);
}

int
apply_val_pretty_printer (struct type *type, const gdb_byte *valaddr,
			  int embedded_offset, CORE_ADDR address,
			  struct ui_file *stream, int recurse,
			  const struct value_print_options *options,
			  const struct language_defn *language)
{
  PyObject *func, *printer;
  struct value *value;
  char *hint;
  struct cleanup *cleanups;
  int result = 0;
  PyGILState_STATE state;

  state = PyGILState_Ensure ();
  cleanups = make_cleanup_py_restore_gil (&state);

  /* Find the constructor.  */
  func = find_pretty_printer (type);
  if (! func)
    goto done;

  /* Instantiate the printer.  */
  value = value_from_contents_and_address (type, valaddr, embedded_offset,
					   address);
  printer = gdbpy_instantiate_printer (func, value);
  Py_DECREF (func);

  if (!printer)
    {
      gdbpy_print_stack ();
      goto done;
    }

  make_cleanup_py_decref (printer);
  if (printer != Py_None)
    {
      print_string_repr (printer, stream, recurse, options, language);
      print_children (printer, stream, recurse, options, language);

      result = 1;
    }

 done:
  do_cleanups (cleanups);
  return result;
}

/* Apply a pretty-printer for the varobj code.  PRINTER_OBJ is the
   print object.  It must have a 'to_string' method (but this is
   checked by varobj, not here) which accepts takes no arguments and
   returns a string.  This function returns an xmalloc()d string if
   the printer returns a string.  The printer may return a replacement
   value instead; in this case *REPLACEMENT is set to the replacement
   value, and this function returns NULL.  On error, *REPLACEMENT is
   set to NULL and this function also returns NULL.  */
char *
apply_varobj_pretty_printer (PyObject *printer_obj, struct value *value,
			     struct value **replacement)
{
  char *result;
  PyGILState_STATE state = PyGILState_Ensure ();

  *replacement = NULL;
  result = pretty_print_one_value (printer_obj, replacement);
  PyGILState_Release (state);

  return result;
}

/* Find a pretty-printer object for the varobj module.  Returns a new
   reference to the object if successful; returns NULL if not.  TYPE
   is the type of the varobj for which a printer should be
   returned.  */
PyObject *
gdbpy_get_varobj_pretty_printer (struct type *type)
{
  return find_pretty_printer (type);
}

/* A Python function which wraps find_pretty_printer and instantiates
   the resulting class.  This accepts a Value argument and returns a
   pretty printer instance, or None.  This function is useful as an
   argument to the MI command -var-set-visualizer.  */
static PyObject *
gdbpy_get_default_visualizer (PyObject *self, PyObject *args)
{
  PyObject *val_obj;
  PyObject *cons, *printer = NULL;
  struct value *value;

  if (! PyArg_ParseTuple (args, "O", &val_obj))
    return NULL;
  value = value_object_to_value (val_obj);
  if (! value)
    {
      PyErr_SetString (PyExc_TypeError, "argument must be a gdb.Value");
      return NULL;
    }

  cons = find_pretty_printer (value_type (value));
  if (cons)
    {
      /* While it is a bit lame to pass value here and make a new
	 Value, it is probably better to share the instantiation
	 code.  */
      printer = gdbpy_instantiate_printer (cons, value);
      Py_DECREF (cons);
    }

  if (!printer)
    {
      PyErr_Clear ();
      printer = Py_None;
      Py_INCREF (printer);
    }

  return printer;
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

int
apply_val_pretty_printer (struct type *type, const gdb_byte *valaddr,
			  int embedded_offset, CORE_ADDR address,
			  struct ui_ifle *stream, int format,
			  int deref_ref, int recurse,
			  enum val_prettyprint pretty,
			  const language_defn *language)
{
  return 0;
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
  if (gdb_datadir)
    PyModule_AddStringConstant (gdb_module, "datadir", gdb_datadir);

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

  PyRun_SimpleString ("import gdb");
  PyRun_SimpleString ("gdb.pretty_printers = {}");

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
  gdb.pythonlibdir = gdb.datadir + '/python'\n\
  sys.path.insert(0, gdb.pythonlibdir)\n\
  gdb.__path__ = [gdb.pythonlibdir + '/gdb']\n\
  from os.path import exists\n\
  ipy = gdb.pythonlibdir + '/gdb/__init__.py'\n\
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

  { "get_current_objfile", gdbpy_get_current_objfile, METH_NOARGS,
    "Return the current Objfile being loaded, or None." },
  { "get_objfiles", gdbpy_get_objfiles, METH_NOARGS,
    "Return a sequence of all loaded objfiles." },

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

  { "get_block_for_pc", gdbpy_get_block_for_pc, METH_VARARGS,
    "Return the block containing the given pc value, or None." },

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

#endif /* HAVE_PYTHON */
