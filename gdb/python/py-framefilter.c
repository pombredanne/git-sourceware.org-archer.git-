/* Python frame filters

   Copyright (C) 2012 Free Software Foundation, Inc.

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
#include "objfiles.h"
#include "symtab.h"
#include "language.h"
#include "exceptions.h"
#include "arch-utils.h"
#include "language.h"
#include "python.h"
#include "ui-out.h"
#include "valprint.h"
#include "annotate.h"

#ifdef HAVE_PYTHON
#include "python-internal.h"

/* Helper function for find_frame_filter which iterates over a list,
   calls each function and inspects output.  This will return a filter
   object if one of the filters registers interest in FRAME.  If no
   frame filter is found, it will return None.  On error, it will set
   the Python error and return NULL.  */

static PyObject *
search_frame_filter_list (PyObject *list, PyObject *frame,
			  PyObject *level, PyObject *what,
			  PyObject *args)

{
  Py_ssize_t list_size, list_index;
  PyObject *function;
  PyObject *filter = NULL;
  
  list_size = PyList_Size (list);
  for (list_index = 0; list_index < list_size; list_index++)
    {
      function = PyList_GetItem (list, list_index);
      if (! function)
	return NULL;

      /* Skip if disabled.  */
      if (PyObject_HasAttr (function, gdbpy_enabled_cst))
	{
	  PyObject *attr = PyObject_GetAttr (function, gdbpy_enabled_cst);
	  int cmp;

	  if (!attr)
	    return NULL;
	  cmp = PyObject_IsTrue (attr);

	  Py_DECREF (attr);
	  if (cmp == -1)
	    return NULL;

	  if (!cmp)
	    continue;
	}

      filter = PyObject_CallFunctionObjArgs (function, frame, level,
					     what, args, NULL);

      if (! filter)
	return NULL;
      else if (filter != Py_None)
	return filter;

      Py_DECREF (filter);
    }

  Py_RETURN_NONE;
}


/* Subroutine of find_frame_filter to simplify it.  Look for a frame
   filter for FRAME in the gdb module.  The result is NULL if there's
   an error and the search should be terminated.  The result is
   Py_None, if no frame filter is found.  Otherwise the result is the
   frame filter function, suitably inc-ref'd.  */

static PyObject *
find_frame_filter_from_gdb (PyObject *frame, PyObject *level,
			    PyObject *what, PyObject *args)
{
  PyObject *filter_list;
  PyObject *function;

  /* Fetch the global frame filter list.  */
  if (! PyObject_HasAttrString (gdb_module, "frame_filters"))
    Py_RETURN_NONE;

  filter_list = PyObject_GetAttrString (gdb_module, "frame_filters");
  if (filter_list == NULL || ! PyList_Check (filter_list))
    {
      Py_XDECREF (filter_list);
      Py_RETURN_NONE;
    }

  function = search_frame_filter_list (filter_list, frame, level,
				       what, args);
  Py_XDECREF (filter_list);
  return function;
}
/* Subroutine of find_frame_filter to simplify it.  Look for a frame
   filter for FRAME in all objfiles.  The result is NULL if there's an
   error and the search should be terminated.  The result is Py_None
   if no frame filter found.  Otherwise the result is the
   pretty-printer function. */

static PyObject *
find_frame_filter_from_objfiles (PyObject *frame, PyObject *level,
				 PyObject *what, PyObject *args)
{
  PyObject *filter_list;
  PyObject *function;
  struct objfile *obj;

  ALL_OBJFILES (obj)
  {
    PyObject *objf = objfile_to_objfile_object (obj);
    if (!objf)
      {
	/* Ignore the error and continue.  */
	PyErr_Clear ();
	continue;
      }

    filter_list = objfpy_get_frame_filters (objf, NULL);

    function = search_frame_filter_list (filter_list, frame, level,
					 what, args);

    Py_XDECREF (filter_list);

    /* If there is an error in any objfile list, abort the search and exit.  */
    if (! function)
      return NULL;

    if (function != Py_None)
      return function;

    Py_DECREF (function);
  }

  Py_RETURN_NONE;
}

/* Subroutine of find_pretty_printer to simplify it.  Look for a frame
   filter for FRAME in the current program space.  The result is NULL
   if there's an error and the search should be terminated.  The
   result is Py_None, no frame filter was found.  Otherwise the result
   is the pretty-printer function.  */

static PyObject *
find_frame_filter_from_progspace (PyObject *frame, PyObject *level,
				  PyObject *what, PyObject *args)
{
  PyObject *filter_list;
  PyObject *function;
  PyObject *obj = pspace_to_pspace_object (current_program_space);

  if (!obj)
    return NULL;
  filter_list = pspy_get_frame_filters (obj, NULL);

  function = search_frame_filter_list (filter_list, frame, level,
				       what, args);
  Py_XDECREF (filter_list);
  return function;
}


/* Find the frame filter constructor function for FRAME.  If no
   frame filter exists, return None.  If one exists, return a new
   reference.  On error, set the Python error and return NULL.  */

static PyObject *
find_frame_filter (PyObject *frame, int print_level,
		   enum print_what print_what, int print_args)

{
  PyObject *function = NULL;
  PyObject *py_print_level = NULL;
  PyObject *py_print_args = NULL;
  PyObject *py_print_what = NULL;

  /* Convert the parameters to Python objects.  If any of them fail,
     abort.  */
  py_print_level = PyBool_FromLong (print_level);
  if (!py_print_level)
    goto exit_func;

  py_print_args = PyBool_FromLong (print_args);
  if (!py_print_args)
    goto exit_func;

  py_print_what = PyLong_FromLong (print_what);
  if (!py_print_what)
    goto exit_func;

  /* Look at the frame filter list for each objfile
     in the current program-space.  */
  function = find_frame_filter_from_objfiles (frame,
					      py_print_level,
					      py_print_what,
					      py_print_args);

  if (function == NULL || function != Py_None)
    goto exit_func;
  Py_DECREF (function);

  /* Look at the frame filter list for the current program-space.  */
  function = find_frame_filter_from_progspace (frame,
					       py_print_level,
					       py_print_what,
					       py_print_args);

  if (function == NULL || function != Py_None)
    goto exit_func;
  Py_DECREF (function);

  /* Look at the frame filter list in the gdb module.  */
  function = find_frame_filter_from_gdb (frame,
					 py_print_level,
					 py_print_what,
					 py_print_args);
 exit_func:

  Py_XDECREF (py_print_level);
  Py_XDECREF (py_print_what);
  Py_XDECREF (py_print_args);

  return function;
}

static int
py_print_locals (PyObject *filter,
		 struct value_print_options opts)
{
  int indent = 4;
  struct ui_stream *stb;
  struct cleanup *old_chain = make_cleanup (null_cleanup, NULL);

  if (PyObject_HasAttrString (filter, "frame_locals"))
    {
      PyObject *result = PyObject_CallMethod (filter, "frame_locals", NULL);
      volatile struct gdb_exception except;
      const struct language_defn *language;

      if (result)
	{
	  Py_ssize_t size, list_index;

	  make_cleanup_py_decref (result);

	  if (! PyList_Check (result))
	    {
	      PyErr_SetString (PyExc_RuntimeError,
			       _("frame_locals must return a Python list."));
	      goto locals_error;
	    }

	  size = PyList_Size (result);

	  if (size > 0)
	    {
	      for (list_index = 0; list_index < size; list_index++)
		{
		  PyObject *sym_tuple, *sym, *value;
		  char *sym_name;
		  struct value *val;
		  struct symbol *symbol;

		  sym_tuple = PyList_GetItem (result, list_index);
		  if (! sym_tuple)
		    goto locals_error;

		  if (! PyTuple_Check (sym_tuple)
		      && PyTuple_Size (sym_tuple) != 2)
		    {
		      PyErr_SetString (PyExc_RuntimeError,
				       _("frame_locals list must contain a Python tuple."));
		      goto locals_error;
		    }

		  /* Each element in the locals arguments list should be a
		     tuple containing two elements.  The local name,
		     which can be a string or a gdb.Symbol, and the
		     value.  */

		  /* Name.  */
		  sym = PyTuple_GetItem (sym_tuple, 0);
		  if (! sym)
		      goto locals_error;

		  /* Value.  */
		  value = PyTuple_GetItem (sym_tuple, 1);
		  if (! value)
		    goto locals_error;

		  /* For arg name, the user can return a symbol or a
		     string.  */
		  if (PyString_Check (sym))
		    {
		      sym_name = python_string_to_host_string (sym);
		      language = current_language;
		      if (! sym_name)
			goto locals_error;
		    }
		  else
		    {
		      symbol = symbol_object_to_symbol (sym);
		      sym_name = xstrdup (SYMBOL_PRINT_NAME (symbol));

		      if (language_mode == language_mode_auto)
			language = language_def (SYMBOL_LANGUAGE (symbol));
		      else
			language = current_language;
		    }

		  fprintf_filtered (gdb_stdout, "%s%s = ",
				    n_spaces (2 * indent), sym_name);

		  xfree (sym_name);
		  val = value_object_to_value (value);
		  if (! val)
		    {
		      PyErr_SetString (PyExc_RuntimeError,
				       _("Invalid value in frame."));

		      goto locals_error;
		    }

		  TRY_CATCH (except, RETURN_MASK_ERROR)
		    {
		      opts.deref_ref = 1;
		      common_val_print (val, gdb_stdout, indent, &opts, language);
		    }
		  if (except.reason < 0)
		    {
		      PyErr_SetString (PyExc_RuntimeError,
				       except.message);
		      goto locals_error;
		    }
		  fprintf_filtered (gdb_stdout, "\n");

		  gdb_flush (gdb_stdout);
		}
	    }
	}
      else
	goto locals_error;
    }

  do_cleanups (old_chain);
  return 1;

 locals_error:
  do_cleanups (old_chain);
  return 0;
}

static int
py_print_args (PyObject *filter,
	    struct ui_out *out,
	    struct value_print_options opts,
	    const char *print_args_type)
{
  struct cleanup *old_chain = make_cleanup (null_cleanup, NULL);
  PyObject *result = NULL;
  struct ui_stream *stb = NULL;

  /* Frame arguments.  */
  annotate_frame_args ();
  ui_out_text (out, " (");

  if (PyObject_HasAttrString (filter, "frame_args"))
    {
      PyObject *result = PyObject_CallMethod (filter, "frame_args", NULL);
      volatile struct gdb_exception except;
      const struct language_defn *language;

      result = PyObject_CallMethod (filter, "frame_args", NULL);

      if (result)
	{
	  Py_ssize_t size, list_index;

	  make_cleanup_py_decref (result);
	  stb = ui_out_stream_new (out);
	  make_cleanup_ui_out_stream_delete (stb);

	  if (! PyList_Check (result))
	    {
	      PyErr_SetString (PyExc_RuntimeError,
			       _("frame_args must return a Python list."));
	      goto args_error;
	    }

	  size = PyList_Size (result);

	  if (size > 0)
	    {
	      for (list_index = 0; list_index < size; list_index++)
		{
		  PyObject *sym_tuple, *sym, *value;
		  const char *sym_name;
		  struct value *val;
		  struct symbol *symbol;

		  sym_tuple = PyList_GetItem (result, list_index);
		  if (! sym_tuple)
		    goto args_error;

		  if (! PyTuple_Check (sym_tuple)
		      && PyTuple_Size (sym_tuple) != 2)
		    {
		      PyErr_SetString (PyExc_RuntimeError,
				       _("frame_arg list must contain a Python tuple."));
		      goto args_error;
		    }

		  /* Each element in the frame arguments list should be a
		     tuple containing two elements.  The argument name,
		     which can be a string or a gdb.Symbol, and the
		     value.  */

		  /* Name.  */
		  sym = PyTuple_GetItem (sym_tuple, 0);
		  if (! sym)
		      goto args_error;

		  /* Value.  */
		  value = PyTuple_GetItem (sym_tuple, 1);
		  if (! value)
		    goto args_error;

		  /* For arg name, the user can return a symbol or a
		     string.  */
		  if (PyString_Check (sym))
		    {
		      sym_name = PyString_AsString (sym);
		      language = current_language;
		      if (! sym_name)
			goto args_error;
		    }
		  else
		    {
		      symbol = symbol_object_to_symbol (sym);
		      sym_name = SYMBOL_PRINT_NAME (symbol);
		      if (language_mode == language_mode_auto)
			language = language_def (SYMBOL_LANGUAGE (symbol));
		      else
			language = current_language;
		    }

		  annotate_arg_begin ();
		  ui_out_field_string (out, "name", sym_name);
		  ui_out_text (out, "=");

		  val = value_object_to_value (value);
		  if (! val)
		    {
		      PyErr_SetString (PyExc_RuntimeError,
				       _("Invalid value in frame."));
		      goto args_error;
		    }

		  annotate_arg_value (value_type (val));

		  opts.deref_ref = 1;

		  /* True in "summary" mode, false otherwise.  */
		  opts.summary = !strcmp (print_args_type, "scalars");

		  TRY_CATCH (except, RETURN_MASK_ALL)
		    {
		      common_val_print (val, stb->stream, 2, &opts, language);
		    }
		  if (except.reason > 0)
		    {
		      PyErr_SetString (PyExc_RuntimeError,
				       except.message);
		      goto args_error;
		    }

		  ui_out_field_stream (out, "value", stb);

		  if (size != 1 && list_index < size-1)
		    ui_out_text (out, ", ");
		  annotate_arg_end ();
		}
	    }
	}
      else
	goto args_error;
    }
  ui_out_text (out, ")");

  do_cleanups (old_chain);
  return 1;

 args_error:
  do_cleanups (old_chain);
  return 0;
}

static int
py_print_frame (PyObject *filter,
	     int print_level,
	     enum print_what print_what,
	     int print_args,
	     const char *print_args_type,
	     struct ui_out *out,
	     struct value_print_options opts,
	     struct frame_info *frame)

{
  int level = 0;
  CORE_ADDR address = 0;
  struct gdbarch *gdbarch = get_frame_arch (frame);
  char *func = NULL;
  char *filename = NULL;
  int line = 0;
  volatile struct gdb_exception except;

  /* First check to see if this frame is to be omitted.  */
  if (PyObject_HasAttrString (filter, "omit"))
    {
      PyObject *result = PyObject_CallMethod (filter, "omit", NULL);

      if (result)
	{
	  int omit = 0;

	  if (! PyBool_Check (result))
	    {
	      Py_DECREF (result);
	      PyErr_SetString (PyExc_RuntimeError,
			       _("'omit' must return type boolean."));
	      goto error;
	    }

	  omit = PyObject_IsTrue (result);

	  Py_DECREF (result);

	  if (omit == -1)
	    goto error;
	  else if (omit)
	    return 1;
	}
      else
	goto error;
    }

  /* Print frame level.  */
  if (print_level)
    {
      if (PyObject_HasAttrString (filter, "level"))
	{
	  PyObject *result = PyObject_CallMethod (filter, "level", "i",
						  frame_relative_level (frame),
						  NULL);

	  if (result)
	    {
	      level = PyLong_AsLong (result);
	      Py_DECREF (result);
	    }
	  else
	    goto error;
	}
      else
	level = frame_relative_level (frame);
    }

  /* Print frame address.  */
  if (PyObject_HasAttrString (filter, "address"))
    {
      PyObject *result = PyObject_CallMethod (filter, "address", NULL);

      if (result)
	{
	  address = PyLong_AsLong (result);
	  Py_DECREF (result);
	}
      else
	goto error;
    }
  else
    address = 0;

  annotate_frame_begin (print_level ? level : 0,
			gdbarch, address);

  ui_out_text (out, "#");
  ui_out_field_fmt_int (out, 2, ui_left, "level",
			level);
  if (opts.addressprint)
    {
      annotate_frame_address ();
      ui_out_field_core_addr (out, "addr", gdbarch, address);
      annotate_frame_address_end ();
    }
  ui_out_text (out, " in ");

  /* Print frame function.  */
  if (PyObject_HasAttrString (filter, "function"))
    {
      PyObject *result = PyObject_CallMethod (filter, "function", NULL);

      if (result)
	{
	  char *dup = PyString_AsString (result);
	  if (! dup)
	    {
	      Py_DECREF (result);
	      goto error;
	    }

	  func = xstrdup (dup);

	  Py_DECREF (result);
	}
      else
	goto error;
    }
  else
    func = xstrdup ("<unknown>");

  annotate_frame_function_name ();
  ui_out_field_string (out, "func", func);

  /* Frame arguments.  */
  if (print_args)
    {
      if (! py_print_args (filter, out, opts, print_args_type))
	goto error;
    }

  if (PyObject_HasAttrString (filter, "filename"))
    {
      PyObject *result = PyObject_CallMethod (filter, "filename", NULL);

      if (result)
	{
	  char *dup = PyString_AsString (result);

	  if (! dup)
	    {
	      Py_DECREF (result);
	      goto error;
	    }

	  filename  = xstrdup (dup);

	  Py_DECREF (result);
	}
      else
	goto error;
    }
  else
    func = xstrdup("<unknown function>");

  annotate_frame_source_begin ();
  ui_out_wrap_hint (out, "   ");
  ui_out_text (out, " at ");
  annotate_frame_source_file ();
  ui_out_field_string (out, "file", filename);
  annotate_frame_source_file_end ();

  if (PyObject_HasAttrString (filter, "line"))
    {
      PyObject *result = PyObject_CallMethod (filter, "line", NULL);

      if (result)
	{
	  line  = PyLong_AsLong (result);
	  Py_DECREF (result);
	}
      else
	goto error;
    }
  else
    line = 0;

  ui_out_text (out, ":");
  annotate_frame_source_line ();
  ui_out_field_int (out, "line", line);
  annotate_frame_source_end ();

  ui_out_text (out, "\n");
  annotate_frame_end ();

  if (PyObject_HasAttrString (filter, "elide"))
    {
      PyObject *result = PyObject_CallMethod (filter, "elide", NULL);

      if (result)
	{
	  struct frame_info *restart = NULL;
	  struct frame_info *current = NULL;
	  /* Fix result here.  */
	  TRY_CATCH (except, RETURN_MASK_ALL)
	    {
	      restart = frame_object_to_frame_info (result);
	    }
	  if (except.reason > 0)
	    {
	      Py_DECREF (result);
	      PyErr_SetString (PyExc_RuntimeError,
			       except.message);
	      goto error;
	    }
	  if (! restart)
	    {
	      Py_DECREF (result);
	      PyErr_SetString (PyExc_RuntimeError,
			       _("Cannot locate frame to continue backtrace."));
	      goto error;
	    }
	  if (restart != frame)
	    {
	      current=get_prev_frame (frame);
	      while (current!=restart)
		{
		  set_frame_print_elide (current);
		  current = get_prev_frame (current);
		}
	    }
	}
      else
	goto error;
    }

  xfree (func);
  xfree (filename);
  return 1;

 error:
  xfree (func);
  xfree (filename);
  return 0;
}

int
apply_frame_filter (struct frame_info *frame, int print_level,
		    enum print_what print_what, int print_args,
		    const char *print_args_type, 
		    struct ui_out *out, int print_frame,
		    int print_locals)
{
  struct gdbarch *gdbarch = get_frame_arch (frame);
  struct cleanup *cleanups;
  PyObject *frame_obj, *filter;
  int result = 0;
  int print_result = 0;
  struct value_print_options opts;
  int success = 0;

  cleanups = ensure_python_env (gdbarch, current_language);

  frame_obj = frame_info_to_frame_object (frame);
  if (! frame_obj)
    goto done;

  /* Find the constructor.  */
  filter = find_frame_filter (frame_obj, print_level, print_what, print_args);
  Py_DECREF (frame_obj);

  make_cleanup_py_decref (filter);
  if (! filter || filter == Py_None)
    goto done;

  get_user_print_options (&opts);
  if (print_frame)
    {
      success =  py_print_frame (filter, print_level, print_what,
				 print_args, print_args_type, out, opts,
				 frame);
      if (success == 0 && PyErr_Occurred ())
	gdbpy_print_stack ();
    }
  
  if (print_locals)
    {
      success = py_print_locals (filter, opts);
      if (success == 0 && PyErr_Occurred ())
	gdbpy_print_stack ();
    }

 done:
  do_cleanups (cleanups);
  return success;
}

#else /* HAVE_PYTHON */
int
apply_frame_filter (struct frame_info *frame, int print_level,
		    enum print_what print_what, int print_args,
		    const char *print_args_type,
		    struct ui_out *out, int print_frame,
		    int print_locals)
{
  return NULL;
}

#endif /* HAVE_PYTHON */
