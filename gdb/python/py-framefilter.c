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

static int
py_print_locals (PyObject *filter,
		 struct value_print_options opts)
{
  int indent = 4;
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
  struct ui_file *stb;

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
	  stb = mem_fileopen ();
	  make_cleanup_ui_file_delete (stb);

	  if (! PyList_Check (result))
	    {
	      PyErr_SetString (PyExc_RuntimeError,
			       _("frame_args must return a Python list."));
	      goto args_error;
	    }

	  size = PyList_Size (result);

	  for (list_index = 0; list_index < size; list_index++)
	    {
	      PyObject *sym_tuple, *sym, *value, *pvalue;
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
	      
	      val = convert_value_from_python (value);
	      //	      val = value_object_to_value (pvalue);
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
		  common_val_print (val, stb, 2, &opts, language);
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

static CORE_ADDR
get_address (PyObject *filter, int *error)
{
  PyObject *result = NULL;
  CORE_ADDR address = 0;
  
  *error = 0;
  if (PyObject_HasAttrString (filter, "address"))
      result = PyObject_CallMethod (filter, "address", NULL);
  else
    {
      if (PyObject_HasAttrString (filter, "pc"))
	  result = PyObject_CallMethod (filter, "pc", NULL);
      else
	return 0;
    }
      
  if (result)
    {
      address = PyLong_AsLong (result);
      Py_DECREF (result);
    }
  else
    *error = 1;
  
  return address;
}

static int
py_print_frame (PyObject *filter,
		int print_level,
		enum print_what print_what,
		int print_args,
		const char *print_args_type,
		struct ui_out *out,
		struct value_print_options opts)
{
  int level = 0;
  CORE_ADDR address = 0;
  struct gdbarch *gdbarch = NULL;
  char *func = NULL;
  char *filename = NULL;
  int line = 0;
  struct frame_info *frame = NULL;
  volatile struct gdb_exception except;
  int error = 0;
  
  /* Get the frame.  */
  if (PyObject_HasAttrString (filter, "inferior_frame"))
    {
      PyObject *result = PyObject_CallMethod (filter, "inferior_frame", NULL);

      if (! result)
	goto error;
      if (result == Py_None)
	gdbarch = target_gdbarch;
      else
	{
	  frame = frame_object_to_frame_info (result);
	  if (! frame)
	    {
	      Py_DECREF (result);
	      PyErr_SetString (PyExc_RuntimeError,
			       _("'inferior_frame' should return None, " \
				 "or a gdb.Frame."));
	      goto error;
	    }
	  gdbarch = get_frame_arch (frame);
	}
      Py_DECREF (result);
    }
  else
    gdbarch = target_gdbarch;
  
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

  if (PyObject_HasAttrString (filter, "elide"))
    {
      PyObject *result = PyObject_CallMethod (filter, "elide", NULL);

      if (result)
	{
	  int elide = 0;

	  if (! PyBool_Check (result))
	    {
	      Py_DECREF (result);
	      PyErr_SetString (PyExc_RuntimeError,
			       _("'elide' must return type boolean."));
	      goto error;
	    }

	  elide = PyObject_IsTrue (result);

	  Py_DECREF (result);

	  if (elide == -1)
	    goto error;
	  else if (elide)
	    ui_out_spaces (out, 4);
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
  
  
  address = get_address (filter, &error);
  if (error)
    goto error;

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
		    const char *print_args_type,  struct ui_out *out,
		    int print_locals, int count)
{
  struct gdbarch *gdbarch = get_frame_arch (frame);
  struct cleanup *cleanups;
  int result = 0;
  int print_result = 0;
  struct value_print_options opts;
  int success = 0;
  PyObject *module;
  PyObject *sort_func;
  PyObject *iterable;
  PyObject *frame_obj;

  cleanups = ensure_python_env (gdbarch, current_language);

  module = PyImport_ImportModule ("gdb.command.frame_filters");
  if (! module)
    goto done;
  
  frame_obj = frame_info_to_frame_object (frame);
  if (! frame_obj)
    goto done;

  sort_func = PyObject_GetAttrString (module, "invoke");
  if (!sort_func)
    {
      Py_DECREF (module);
      Py_DECREF (frame_obj);
      goto done;
    }
  
  iterable = PyObject_CallFunctionObjArgs (sort_func, frame_obj, NULL);
  Py_DECREF (module);
  Py_DECREF (sort_func);
  Py_DECREF (frame_obj);
  
  if (!iterable)
    goto done;
  
  get_user_print_options (&opts);

  make_cleanup_py_decref (iterable);

  /* Is it an iterator */
  if PyIter_Check (iterable)
    {
      PyObject *iterator = PyObject_GetIter (iterable);
      PyObject *item;

      if (iterator == NULL)
	goto done;

      while ((item = PyIter_Next (iterator)))
	{
	  success =  py_print_frame (item, print_level, print_what,
				     print_args, print_args_type, out,
				     opts);
	  if (success == 0 && PyErr_Occurred ())
	    gdbpy_print_stack ();

	  if (print_locals)
	    {
	      success = py_print_locals (item, opts);
	      if (success == 0 && PyErr_Occurred ())
		gdbpy_print_stack ();
	    }
	  Py_DECREF (item);
	}
      Py_DECREF (iterator);
    }
  else
    {
      Py_DECREF (iterable);
      error (_("Frame filter must support iteration protocol"));
    }

 done:
  if (PyErr_Occurred ())
    gdbpy_print_stack ();
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
