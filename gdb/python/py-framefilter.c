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
#include "exceptions.h"
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
print_frame (PyObject *filter, int print_level,
	     enum print_what print_what, int print_args,
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

  if (print_level)
    {
      if (PyObject_HasAttrString (filter, "level"))
	{
	  PyObject *result = PyObject_CallMethod (filter, "level", NULL);

	  if (result)
	    {
	      level = PyLong_AsLong (result);
	      Py_DECREF (result);
	    }
	}
      else
	level = frame_relative_level (frame);
    }

  if (PyObject_HasAttrString (filter, "address"))
    {
      PyObject *result = PyObject_CallMethod (filter, "address", NULL);

      if (result)
	{
	  address = PyLong_AsLong (result);
	  Py_DECREF (result);
	}
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

  if (PyObject_HasAttrString (filter, "function"))
    {
      PyObject *result = PyObject_CallMethod (filter, "function", NULL);

      if (result)
	{
	  char *dup = PyString_AsString (result);
	  if (dup)
	    func  = xstrdup (dup);
	  else
	    return 0;
	  Py_DECREF (result);
	}
    }
  else
    func = xstrdup("<unknown>");

  annotate_frame_function_name ();
  ui_out_field_string (out, "func", func);

  if (PyObject_HasAttrString (filter, "filename"))
    {
      PyObject *result = PyObject_CallMethod (filter, "filename", NULL);

      if (result)
	{
	  char *dup = PyString_AsString (result);
	  if (dup)
	    filename  = xstrdup (dup);
	  else
	    return 0;
	  Py_DECREF (result);
	}
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
    }
  else
    line = 0;

  ui_out_text (out, ":");
  annotate_frame_source_line ();
  ui_out_field_int (out, "line", line);
  annotate_frame_source_end ();

  ui_out_text (out, "\n");
  annotate_frame_end ();

  xfree (func);
  xfree (filename);

  return 1;
}

int
apply_frame_filter (struct frame_info *frame, int print_level,
		    enum print_what print_what, int print_args,
		    struct ui_out *out)
{
  struct gdbarch *gdbarch = get_frame_arch (frame);
  struct cleanup *cleanups;
  PyObject *frame_obj, *filter;
  int result = 0;
  int print_result = 0;
  struct value_print_options opts;

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
  print_result = print_frame (filter, print_level, print_what,
			      print_args, out, opts, frame);
  
  if (print_result)
    result = 1;
  else
    gdbpy_print_stack ();

 done:
  do_cleanups (cleanups);
  return result;
}

#else /* HAVE_PYTHON */

int
apply_frame_filter (struct frame_info *frame, int print_level,
		    enum print_what print_what, int print_args,
		    struct ui_out *out)
{
  return 0;
}

#endif /* HAVE_PYTHON */
