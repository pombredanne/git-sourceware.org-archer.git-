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
#include "hashtab.h"

#ifdef HAVE_PYTHON
#include "python-internal.h"

static int
extract_sym_and_value (PyObject *obj, char **name,
		       struct value **value,
		       const struct language_defn **language)
{
  if (PyObject_HasAttrString (obj, "symbol"))
    {
      PyObject *result = PyObject_CallMethod (obj, "symbol", NULL);
      if (! result)
	return 0;

      /* For 'symbol' callback, the function can return a symbol or a
	 string.  */
      if (PyString_Check (result))
	{
	  *name = python_string_to_host_string (result);
	  Py_DECREF (result);

	  if (! name)
	      return 0;
	  *language = current_language;
	}
      else
	{
	  struct symbol *symbol = symbol_object_to_symbol (result);

	  Py_DECREF (result);
	  if (! symbol)
	    {
	      PyErr_SetString (PyExc_RuntimeError,
			       _("Unexpected value.  Expecting a " \
				 "gdb.Symbol or a Python string."));
	      return 0;
	    }

	  *name = xstrdup (SYMBOL_PRINT_NAME (symbol));

	  if (language_mode == language_mode_auto)
	    *language = language_def (SYMBOL_LANGUAGE (symbol));
	  else
	    *language = current_language;
	}
    }
  else
    {
      PyErr_SetString (PyExc_RuntimeError,
		       _("Mandatory function 'symbol' not " \
			 "implemented."));
      return 0;
    }

  if (PyObject_HasAttrString (obj, "value"))
    {
      PyObject *result = PyObject_CallMethod (obj, "value", NULL);

      if (! result)
	{
	  xfree (*name);
	  return 0;
	}

      *value = convert_value_from_python (result);

      Py_DECREF (result);
      if (! *value)
	{
	  xfree (*name);
	  return 0;
	}
    }
  else
    {
      PyErr_SetString (PyExc_RuntimeError,
		       _("Mandatory function 'value' not " \
			 "implemented."));
      return 0;
    }
  return 1;
}

static int
py_print_locals (PyObject *filter,
		 struct value_print_options opts)
{
  int indent = 4;
  struct cleanup *old_chain = make_cleanup (null_cleanup, NULL);

  if (PyObject_HasAttrString (filter, "frame_locals"))
    {
      PyObject *result = PyObject_CallMethod (filter, "frame_locals",
					      NULL);

      if (result)
	{
	  make_cleanup_py_decref (result);

	  if (result != Py_None)
	    {
	      if (! PyIter_Check (result))
		{
		  PyErr_SetString (PyExc_RuntimeError,
				   _("'frame_locals' function must " \
				     "return  an iterator."));
		  goto locals_error;
		}
	      else
		{
		  PyObject *iterator = PyObject_GetIter (result);
		  PyObject *item;

		  if (! iterator)
		    goto locals_error;

		  while ((item = PyIter_Next (iterator)))
		    {
		      const struct language_defn *language;
		      char *sym_name;
		      struct value *val;
		      int value_success = 0;
		      volatile struct gdb_exception except;

		      if (! item)
			goto locals_error;


		      value_success = extract_sym_and_value (item, &sym_name,
							     &val,
							     &language);
		      Py_DECREF (item);

		      if (! value_success)
			{
			  Py_DECREF (iterator);
			  goto locals_error;
			}

		      fprintf_filtered (gdb_stdout, "%s%s = ",
					n_spaces (2 * indent),
					sym_name);

		      xfree (sym_name);

		      TRY_CATCH (except, RETURN_MASK_ERROR)
			{
			  opts.deref_ref = 1;
			  common_val_print (val, gdb_stdout,
					    indent, &opts,
					    language);
			}
		      if (except.reason < 0)
			{
			  PyErr_SetString (PyExc_RuntimeError,
					   except.message);
			  Py_DECREF (iterator);
			  goto locals_error;
			}
		      fprintf_filtered (gdb_stdout, "\n");
		      gdb_flush (gdb_stdout);
		    }
		}
	    }
	}
    }
  else
    goto locals_error;

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
      PyObject *result;
      volatile struct gdb_exception except;
      const struct language_defn *language;

      result = PyObject_CallMethod (filter, "frame_args", NULL);
      if (result)
	{
	  make_cleanup_py_decref (result);
	  stb = mem_fileopen ();
	  make_cleanup_ui_file_delete (stb);

	  if (result != Py_None)
	    {
	      if (! PyIter_Check (result))
		{
		  PyErr_SetString (PyExc_RuntimeError,
				   _("'frame_args' function must " \
				     "return  an iterator."));
		  goto args_error;
		}
	      else
		{
		  PyObject *iterator = PyObject_GetIter (result);
		  PyObject *item;
		  int first = 0;
		  if (! iterator)
		    goto args_error;

		  item = PyIter_Next (iterator);
		  if (! item && PyErr_Occurred ())
		    goto args_error;

		  while (item)
		    {
		      const struct language_defn *language;
		      char *sym_name;
		      struct value *val;
		      int value_success = 0;
		      volatile struct gdb_exception except;

		      value_success = extract_sym_and_value (item,
							     &sym_name,
							     &val,
							     &language);
		      Py_DECREF (item);

		      if (! value_success)
			{
			  Py_DECREF (iterator);
			  goto args_error;
			}

		      annotate_arg_begin ();
		      ui_out_field_string (out, "name", sym_name);
		      ui_out_text (out, "=");
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
			  Py_DECREF (iterator);
			  goto args_error;
			}

		      ui_out_field_stream (out, "value", stb);

		      /* Collect the next item from the iterator.  If
			 this is the last item, we do not print the
			 ",".  */
		      item = PyIter_Next (iterator);
		      if (item)
			ui_out_text (out, ", ");
		      else
			if (PyErr_Occurred ())
			  {
			    Py_DECREF (iterator);
			    goto args_error;
			  }

		      annotate_arg_end ();
		    }
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

/* Hash function for the printed frame hash.  */

static hashval_t
hash_printed_frame_entry (const void *data)
{
  const struct frame_info *frame = data;

  return htab_hash_pointer (frame);
}

/* Equality function for the printed hash.  */

static int
eq_printed_frame_entry (const void *a, const void *b)
{
  const struct frame_info *ea = a;
  const struct frame_info *eb = b;

  return ea == eb;
}


static int
py_print_frame (PyObject *filter,
		int print_level,
		enum print_what print_what,
		int print_args,
		const char *print_args_type,
		int print_locals,
		struct ui_out *out,
		struct value_print_options opts,
		int indent,
		htab_t levels_printed)
{
  int has_addr = 0;
  CORE_ADDR address = 0;
  struct gdbarch *gdbarch = NULL;
  struct frame_info *frame = NULL;

  /* Get the underlying frame.  */
  if (PyObject_HasAttrString (filter, "inferior_frame"))
    {
      PyObject *result = PyObject_CallMethod (filter, "inferior_frame", NULL);

      if (! result)
	goto error;
      frame = frame_object_to_frame_info (result);
      if (! frame)
	{
	  Py_DECREF (result);
	  goto error;
	}

      gdbarch = get_frame_arch (frame);
      Py_DECREF (result);
    }
  else
    {
      PyErr_SetString (PyExc_RuntimeError,
		       _("'inferior_frame' API must be implemented."));
      goto error;
    }

  /* Elided frames are also printed with this function (recursively)
     and are printed with indention.  */
  if (indent > 0)
    ui_out_spaces (out, indent);

  /* The address is required for frame annotations, and also for
     address printing.  */
  if (PyObject_HasAttrString (filter, "address"))
    {
      PyObject *paddr = PyObject_CallMethod (filter, "address", NULL);
      if (paddr)
	{
	  if (paddr != Py_None)
	    {
	      has_addr = 1;
	      address = PyLong_AsLong (paddr);
	    }
	  Py_DECREF (paddr);
	}
      else
	goto error;
    }

  /* Print frame level.  */
  if (print_level)
    {
      struct frame_info **slot;
      int level;

      slot = (struct frame_info **) htab_find_slot (levels_printed,
						    frame, INSERT);
      level = frame_relative_level (frame);

      /* Check if this frame has already been printed (there are cases
	 where elided synthetic dummy-frames have to 'borrow' the frame
	 architecture from the eliding frame.  If that is the case, do
	 not print 'level', but print spaces.  */
      if (*slot != NULL && (*slot) == frame)
	{
	  char buffer[10];
	  sprintf (buffer, "%d", level);
	  ui_out_spaces (out, strlen (buffer) + 2);
	}
      else
	{
	  *slot = frame;
	  annotate_frame_begin (print_level ? level : 0,
				gdbarch, address);
	  ui_out_text (out, "#");
	  ui_out_field_fmt_int (out, 2, ui_left, "level",
				level);
	}
    }

  /* Print address to the address field.  If no is provided address,
     printing nothing.  */
  if  (opts.addressprint && has_addr)
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
	  if (result != Py_None)
	    {
	      char *func = NULL;
	      char *dup = PyString_AsString (result);

	      if (! dup)
		{
		  Py_DECREF (result);
		  goto error;
		}

	      func = xstrdup (dup);
	      annotate_frame_function_name ();
	      ui_out_field_string (out, "func", func);
	      xfree (func);

	    }
	  Py_DECREF (result);
	}
      else
	goto error;
    }

  /* Frame arguments.  */
  if (print_args)
    {
      if (! py_print_args (filter, out, opts, print_args_type))
	goto error;
    }

  annotate_frame_source_begin ();

  if (PyObject_HasAttrString (filter, "filename"))
    {
      PyObject *result = PyObject_CallMethod (filter, "filename",
					      NULL);
      if (result)
	{
	  if (result != Py_None)
	    {
	      char *filename = NULL;
	      char *dup = PyString_AsString (result);

	      if (! dup)
		{
		  Py_DECREF (result);
		  goto error;
		}

	      filename  = xstrdup (dup);
	      ui_out_wrap_hint (out, "   ");
	      ui_out_text (out, " at ");
	      annotate_frame_source_file ();
	      ui_out_field_string (out, "file", filename);
	      annotate_frame_source_file_end ();
	      xfree (filename);
	    }
	  Py_DECREF (result);
	}
      else
	goto error;
    }

  if (PyObject_HasAttrString (filter, "line"))
    {
      PyObject *result = PyObject_CallMethod (filter, "line", NULL);
      int line;

      if (result)
	{
	  if (result != Py_None)
	    {
	      line  = PyLong_AsLong (result);
	      ui_out_text (out, ":");
	      annotate_frame_source_line ();
	      ui_out_field_int (out, "line", line);
	    }
	  Py_DECREF (result);
	}
      else
	goto error;
    }

  /* For MI we need to deal with the children population of elided
     frames, so if MI output detected do not send newline.  */
  if (! ui_out_is_mi_like_p (out))
    {
      if (has_addr)
	annotate_frame_end ();
      ui_out_text (out, "\n");
    }

  if (print_locals)
    {
      int success = py_print_locals (filter, opts);
      if (success == 0 && PyErr_Occurred ())
	goto error;
    }

  /* Finally recursively print elided frames, if any.  */
  if (PyObject_HasAttrString (filter, "elided"))
    {
      PyObject *result = PyObject_CallMethod (filter, "elided", NULL);

      if (! result)
	goto error;
      if (result != Py_None)
	{
	  if (! PyIter_Check (result))
	    {
	      PyErr_SetString (PyExc_RuntimeError,
			       _("'elided' function must return an iterator."));
	      Py_DECREF (result);
	      goto error;
	    }
	  else
	    {
	      PyObject *iterator = PyObject_GetIter (result);
	      PyObject *item;
	      struct cleanup *cleanup_stack;

	      if (iterator == NULL)
		goto error;

	      if (ui_out_is_mi_like_p (out))
		{
		  cleanup_stack = make_cleanup_ui_out_list_begin_end (out, "children");
		}
	      else
		{
		  cleanup_stack = make_cleanup (null_cleanup, NULL);
		  indent = indent + 4;
		}

	      while ((item = PyIter_Next (iterator)))
		{
		  int success =  py_print_frame (item, print_level, print_what,
						 print_args, print_args_type,
						 print_locals, out,
						 opts, indent, levels_printed);
		  if (success == 0 && PyErr_Occurred ())
		    {
		      do_cleanups (cleanup_stack);
		      goto error;
		    }
		}

	      do_cleanups (cleanup_stack);
	    }
	}
    }

  return PY_BT_COMPLETED;

 error:
  return PY_BT_ERROR;
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

  if (iterable == Py_None)
    {
      Py_DECREF (iterable);
      return 2;
    }

  get_user_print_options (&opts);

  make_cleanup_py_decref (iterable);

  /* Is it an iterator */
  if PyIter_Check (iterable)
    {
      PyObject *iterator = PyObject_GetIter (iterable);
      PyObject *item;
      htab_t levels_printed;

      if (iterator == NULL)
	goto done;

      levels_printed = htab_create (20,
				    hash_printed_frame_entry,
				    eq_printed_frame_entry,
				    NULL);

      while ((item = PyIter_Next (iterator)) && count--)
	{
	  success =  py_print_frame (item, print_level, print_what,
				     print_args, print_args_type,
				     print_locals, out, opts, 0,
				     levels_printed);
	  if (success == PY_BT_ERROR && PyErr_Occurred ())
	    {
	      gdbpy_print_stack ();
	      /* FIXME:  Should we try to continue to print other
		 frames when we encounter an error?  */
	      break;
	    }
	  Py_DECREF (item);
	}

      htab_delete (levels_printed);
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
  return 2;
}

#endif /* HAVE_PYTHON */
