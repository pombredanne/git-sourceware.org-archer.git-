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
#include "python.h"
#include "ui-out.h"
#include "valprint.h"
#include "annotate.h"
#include "hashtab.h"
#include "mi/mi-cmds.h"
#include "demangle.h"
#include "mi/mi-cmds.h"

#ifdef HAVE_PYTHON
#include "python-internal.h"


/* Helper function to extract a symbol, name and language definition
   from a Python object that conforms to the "Symbol Value" interface.
   OBJ is the Python object to extract the values from.  **NAME is a
   pass-through argument where the name of the symbol will be written.
   **NAME is allocated in this function, but the caller is responsible
   for clean up.  **SYM is a pass-through argument where the symbol
   will be written.  In the case of the API returning a string, this
   will be set to NULL.  **LANGUAGE is also a pass-through argument
   denoting the language attributed to the Symbol.  In the case of
   **SYM being NULL, this will be set to the current language.
   Returns 0 on error with the appropriate Python exception set, and 1
   on success.  */

static int
extract_sym (PyObject *obj, char **name, struct symbol **sym,
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

	  if (! *name)
	    return 0;
	  *language = current_language;
	  *sym = NULL;
	}
      else
	{
	  /* This type checks 'result' during the conversion so we
	     just call it unconditionally and check the return.  */
	  *sym = symbol_object_to_symbol (result);

	  Py_DECREF (result);

	  if (! *sym)
	    {
	      PyErr_SetString (PyExc_RuntimeError,
			       _("Unexpected value.  Expecting a " \
				 "gdb.Symbol or a Python string."));
	      return 0;
	    }

	  /* Duplicate the symbol name, so the caller has consistency
	     in garbage collection.  */
	  *name = xstrdup (SYMBOL_PRINT_NAME (*sym));

	  if (language_mode == language_mode_auto)
	    *language = language_def (SYMBOL_LANGUAGE (*sym));
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

  return 1;
}

/* Helper function to extract a value from an object that conforms to
   the "Symbol Value" interface.  OBJ is the Python object to extract
   the value from.  **VALUE is a pass-through argument where the value
   will be written.  If the object does not have the value attribute,
   or provides the Python None for a value, **VALUE will be set to
   NULL and this function will return as successful.  Returns 0 on
   error with the appropriate Python exception set, and 1 on
   success.  */

static int
extract_value (PyObject *obj, struct value **value)
{
  if (PyObject_HasAttrString (obj, "value"))
    {
      PyObject *vresult = PyObject_CallMethod (obj, "value", NULL);

      if (! vresult)
	return 0;

      /* The Python code has returned 'None' for a value, so we set
	 value to NULL.  This flags that GDB should read the
	 value.  */
      if (vresult == Py_None)
	{
	  Py_DECREF (vresult);
	  *value = NULL;
	  return 1;
	}
      else
	{
	  *value = convert_value_from_python (vresult);
	  Py_DECREF (vresult);

	  if (*value == NULL)
	    return 0;

	  return 1;
	}
    }
  else
    *value = NULL;

  return 1;
}

static int
mi_should_print (struct symbol *sym, const char *type)
{
  int print_me = 0;

  switch (SYMBOL_CLASS (sym))
    {
    default:
    case LOC_UNDEF:	/* catches errors        */
    case LOC_CONST:	/* constant              */
    case LOC_TYPEDEF:	/* local typedef         */
    case LOC_LABEL:	/* local label           */
    case LOC_BLOCK:	/* local function        */
    case LOC_CONST_BYTES:	/* loc. byte seq.        */
    case LOC_UNRESOLVED:	/* unresolved static     */
    case LOC_OPTIMIZED_OUT:	/* optimized out         */
      print_me = 0;
      break;

    case LOC_ARG:	/* argument              */
    case LOC_REF_ARG:	/* reference arg         */
    case LOC_REGPARM_ADDR:	/* indirect register arg */
    case LOC_LOCAL:	/* stack local           */
    case LOC_STATIC:	/* static                */
    case LOC_REGISTER:	/* register              */
    case LOC_COMPUTED:	/* computed location     */
      if (strcmp (type, "all"))
	print_me = 1;
      else if (strcmp (type, "locals"))
	print_me = !SYMBOL_IS_ARGUMENT (sym);
      else
	print_me = SYMBOL_IS_ARGUMENT (sym);
    }
  return print_me;
}

/* Helper function which outputs a type name to a "type" field in a
   stream.  OUT is the ui-out structure the type name will be output
   too, and VAL is the value that the type will be extracted from.
   Returns 0 on error, with any GDB exceptions converted to a Python
   exception.  */

static int
py_print_type (struct ui_out *out, struct value *val)
{
  volatile struct gdb_exception except;

  TRY_CATCH (except, RETURN_MASK_ALL)
    {
      struct type *type;
      struct ui_file *stb;
      struct cleanup *cleanup;

      stb = mem_fileopen ();
      cleanup = make_cleanup_ui_file_delete (stb);
      type = check_typedef (value_type (val));
      type_print (value_type (val), "", stb, -1);
      ui_out_field_stream (out, "type", stb);
      do_cleanups (cleanup);
    }
  if (except.reason > 0)
    {
      PyErr_SetString (PyExc_RuntimeError,
		       except.message);
      return 0;
    }
  return 1;
}

/* Helper function which outputs a value name to value field in a
   stream.  OUT is the ui-out structure the value will be output too,
   VAL is the value that will be printed, OPTS contains the value
   printing options, MI_PRINT_TYPE is the value delimiter for MI
   output and LANGUAGE is the language_defn that the value will be
   printed with.  If the output is detected to be non-MI,
   MI_PRINT_TYPE is ignored.  Returns 0 on error, with any GDB
   exceptions converted to a Python exception.  */

static int
py_print_value (struct ui_out *out, struct value *val,
		struct value_print_options opts,
		int mi_print_type,
		const struct language_defn *language)
{
  int should_print = 0;

  /* MI disallows different value types against different options the
     client passes, so test type against option.  For CLI print all
     values.  */
  if (ui_out_is_mi_like_p (out))
    {
      struct type *type;

      type = check_typedef (value_type (val));
      if (mi_print_type == PRINT_ALL_VALUES)
	should_print = 1;
      else if (mi_print_type == PRINT_SIMPLE_VALUES
	       && TYPE_CODE (type) != TYPE_CODE_ARRAY
	       && TYPE_CODE (type) != TYPE_CODE_STRUCT
	       && TYPE_CODE (type) != TYPE_CODE_UNION)
	should_print = 1;
    }
  else
    should_print = 1;

  if (should_print)
    {
      volatile struct gdb_exception except;

      TRY_CATCH (except, RETURN_MASK_ALL)
	{
	  struct ui_file *stb;
	  struct cleanup *cleanup;

	  stb = mem_fileopen ();
	  cleanup = make_cleanup_ui_file_delete (stb);
	  common_val_print (val, stb, 0, &opts, language);
	  ui_out_field_stream (out, "value", stb);
	  do_cleanups (cleanup);
	}
      if (except.reason > 0)
	{
	  PyErr_SetString (PyExc_RuntimeError,
			   except.message);
	  return 0;
	}
    }

  return 1;
}

/* Helper function to call a Python method and extract an iterator
   from the result, error checking for Python exception and returns
   that are not iterators.  FILTER is the Python object to call, and
   FUNC is the name of the method.  Returns a PyObject, or NULL on
   error with the appropriate exception set.  This function can return
   an iterator, or None.  */

static PyObject *
get_py_iter_from_func (PyObject *filter, char *func)
{
  if (PyObject_HasAttrString (filter, func))
    {
      PyObject *result = PyObject_CallMethod (filter, func, NULL);

      if (result)
	{
	  if (result != Py_None)
	    {
	      if (! PyIter_Check (result))
		{
		  PyErr_Format (PyExc_RuntimeError,
				_(" %s function must " \
				  "return an iterator."), func);
		  Py_DECREF (result);
		  return NULL;
		}
	      else
		{
		  PyObject *iterator = PyObject_GetIter (result);

		  Py_DECREF (result);

		  if (! iterator)
		    return NULL;
		  else
		    return iterator;
		}
	    }
	}
      else
	return NULL;
    }

    Py_RETURN_NONE;
}

/*  Helper function to output a single frame argument and value to an
    output stream.  This function will account for entry values if the
    FV parameter is populated, the frame argument has entry values
    associated with them, and the appropriate "set entry-value"
    options are set.  Will output in CLI or MI like format depending
    on the type of output stream detected.  OUT is the output stream,
    SYM_NAME is the name of the symbol.  If SYM_NAME is populated then
    it must have an accompanying value in the parameter FV.  FA is a
    frame argument structure.  If FA is populated, both SYM_NAME and
    FV are ignored.  OPTS contains the value printing options,
    MI_PRINT_TYPE is an enumerator to the value types that will be
    printed if the output is MI.  PRINT_MI_ARGS indicates whether to
    output the ARGS="1" field in MI output.  */
static int
py_print_single_arg (struct ui_out *out,
		     char *sym_name,
		     struct frame_arg *fa,
		     struct value *fv,
		     struct value_print_options opts,
		     int mi_print_type,
		     int print_mi_args_flag,
		     const char *cli_print_frame_args_type,
		     const struct language_defn *language)
{
  struct value *val;
  struct cleanup *inner_cleanup =
    make_cleanup (null_cleanup, NULL);

  if (fa)
    {
      language = language_def (SYMBOL_LANGUAGE (fa->sym));
      val = fa->val;
    }
  else
    val = fv;

  /*  MI has varying rules for tuples, but generally if there is only
      one element in each item in the list, do not start a tuple.  */
  if (print_mi_args_flag || mi_print_type != PRINT_NO_VALUES)
    {
      inner_cleanup =
	make_cleanup_ui_out_tuple_begin_end (out,
					     NULL);
    }

  annotate_arg_begin ();

  /* If frame argument is populated, check for entry-values and the
     entry value options.  */
  if (fa)
    {
      struct ui_file *stb;

      stb = mem_fileopen ();

      fprintf_symbol_filtered (stb, SYMBOL_PRINT_NAME (fa->sym),
			       SYMBOL_LANGUAGE (fa->sym),
			       DMGL_PARAMS | DMGL_ANSI);
      if (fa->entry_kind == print_entry_values_compact)
	{
	  fputs_filtered ("=", stb);

	  fprintf_symbol_filtered (stb, SYMBOL_PRINT_NAME (fa->sym),
				   SYMBOL_LANGUAGE (fa->sym),
				   DMGL_PARAMS | DMGL_ANSI);
	}
      if (fa->entry_kind == print_entry_values_only
	  || fa->entry_kind == print_entry_values_compact)
	{
	  fputs_filtered ("@entry", stb);
	}
      ui_out_field_stream (out, "name", stb);
      ui_file_delete (stb);
    }
  else
    /* Otherwise, just output the name.  */
    ui_out_field_string (out, "name", sym_name);

  annotate_arg_name_end ();

  if (! ui_out_is_mi_like_p (out))
    ui_out_text (out, "=");

  if (print_mi_args_flag)
    ui_out_field_int (out, "arg", 1);

  /* For MI print the type.  */
  if (ui_out_is_mi_like_p (out)
      && mi_print_type == PRINT_SIMPLE_VALUES)
    {
      if (! py_print_type (out, val))
	goto error;
    }

  annotate_arg_value (value_type (val));

    /* If the output is to the CLI, and the user option set print
     frame-arguments is set to none, just output "...".  */
  if (! ui_out_is_mi_like_p (out)
      && ! strcmp (cli_print_frame_args_type, "none"))

    {
      ui_out_field_string (out, "value", "...");
    }
  else
    {
      /* If CLI, and the first if condition above not true always
	 print values.  For MI do not print values if the enumerator
	 is PRINT_NO_VALUES.  */
      if (! ui_out_is_mi_like_p (out)
	  || (ui_out_is_mi_like_p (out)
	      && mi_print_type != PRINT_NO_VALUES))
	{
	  if (! py_print_value (out, val, opts, mi_print_type, language))
	    goto error;
	}
    }

  do_cleanups (inner_cleanup);

  return 1;

 error:
  do_cleanups (inner_cleanup);
  return 0;
}

/* Helper function to loop over frame arguments provided by the
   "frame_arguments" Python API.  Elements in the iterator must
   conform to the "Symbol Value" interface.  ITER is the Python
   iterator object, OUT is the output stream, MI_PRINT_TYPE is an
   enumerator to the value types that will be printed if the output is
   MI, PRINT_MI_ARGS indicates whether to output the ARGS="1" field in
   MI output, CLI_PRINT_FRAME_ARGS_TYPE is an enumerator of the user
   set option for frame argument printing output, and FRAME is the
   backing frame.  If (all) the frame argument values are provided via
   the "value" API call, FRAME is not needed.  */

static int
enumerate_args (PyObject *iter,
		struct ui_out *out,
		int mi_print_type,
		int print_mi_args_flag,
		const char *cli_print_frame_args_type,
		struct frame_info *frame)
{
  PyObject *item;
  struct value_print_options opts;

  get_user_print_options (&opts);

  if (! ui_out_is_mi_like_p (out))
    {
      /* True in "summary" mode, false otherwise.  */
      opts.summary = !strcmp (cli_print_frame_args_type, "scalars");
    }

  opts.deref_ref = 1;


  annotate_frame_args ();

  /*  Collect the first argument outside of the loop, so output of
      commas in the argument output is correct.  At the end of the
      loop block collect another item from the iterator, and, if it is
      not null emit a comma.  */
  item = PyIter_Next (iter);
  if (! item && PyErr_Occurred ())
    goto error;

  while (item)
    {
      const struct language_defn *language;
      char *sym_name;
      struct symbol *sym;
      struct value *val;
      int success = 0;
      volatile struct gdb_exception except;

      success = extract_sym (item, &sym_name, &sym, &language);
      if (! success)
	{
	  Py_DECREF (item);
	  goto error;
	}

      success = extract_value (item, &val);
      if (! success)
	{
	  xfree (sym_name);
	  Py_DECREF (item);
	  goto error;
	}

      Py_DECREF (item);
      item = NULL;

      if (sym && ui_out_is_mi_like_p (out) && ! mi_should_print (sym, "args"))
	continue;

      /* If the object did not provide a value, read it using
	 read_frame_args and account for entry values, if any.  */
      if (! val)
	{
	  struct frame_arg arg, entryarg;

	  /* If there is no value, and also no symbol, set error and
	     exit.  */
	  if (! sym)
	    {
	      PyErr_SetString (PyExc_RuntimeError,
			       _("No symbol or value provided."));
	      xfree (sym_name);
	      goto error;
	    }

	  TRY_CATCH (except, RETURN_MASK_ALL)
	    {
	      read_frame_arg (sym, frame, &arg, &entryarg);
	    }
	  if (except.reason > 0)
	    {
	      xfree (sym_name);
	      PyErr_SetString (PyExc_RuntimeError,
			       except.message);
	      goto error;
	    }

	  /* The object has not provided a value, so this is a frame
	     argument to be read by GDB.  In this case we have to
	     account for entry-values.  */

	  if (arg.entry_kind != print_entry_values_only)
	    py_print_single_arg (out, NULL, &arg, NULL, opts,
				 mi_print_type,
				 print_mi_args_flag,
				 cli_print_frame_args_type, NULL);

	  if (entryarg.entry_kind != print_entry_values_no)
	    {
	      if (arg.entry_kind != print_entry_values_only)
		{
		  ui_out_text (out, ", ");
		  ui_out_wrap_hint (out, "    ");
		}

	      py_print_single_arg (out, NULL, &entryarg, NULL, opts,
				   mi_print_type,
				   print_mi_args_flag,
				   cli_print_frame_args_type, NULL);
	    }

	  xfree (arg.error);
	  xfree (entryarg.error);
	}
      else
	{
	  /* If the object has provided a value, we just print that.  */
	  if (val)
	    py_print_single_arg (out, sym_name, NULL, val, opts,
				 mi_print_type,
				 print_mi_args_flag,
				 cli_print_frame_args_type,
				 language);
	}

      xfree (sym_name);

      /* Collect the next item from the iterator.  If
	 this is the last item, do not print the
	 comma.  */
      item = PyIter_Next (iter);
      if (item)
	ui_out_text (out, ", ");
      else
	if (PyErr_Occurred ())
	  goto error;

      annotate_arg_end ();
    }

  return 1;

 error:
  return 0;
}


/* Helper function to loop over variables provided by the
   "frame_locals" Python API.  Elements in the iterator must conform
   to the "Symbol Value" interface.  ITER is the Python iterator
   object, OUT is the output stream, MI_PRINT_TYPE is an enumerator to
   the value types that will be printed if the output is MI,
   PRINT_MI_ARGS_FLAG indicates whether to output the ARGS field in MI
   output, and FRAME is the backing frame.  If (all) of the variables
   values are provided via the "value" API call, FRAME is not
   needed.  */

static int
enumerate_locals (PyObject *iter,
		  struct ui_out *out,
		  int mi_print_type,
		  int indent,
		  int print_mi_args_flag,
		  struct frame_info *frame)
{
  PyObject *item;
  struct value_print_options opts;

  get_user_print_options (&opts);
  opts.deref_ref = 1;

  while ((item = PyIter_Next (iter)))
    {
      const struct language_defn *language;
      char *sym_name;
      struct value *val;
      int success = 0;
      struct symbol *sym;
      volatile struct gdb_exception except;
      struct cleanup *inner_cleanup =
	make_cleanup (null_cleanup, NULL);

      success = extract_sym (item, &sym_name, &sym, &language);
      if (! success)
	{
	  Py_DECREF (item);
	  goto error;
	}

      success = extract_value (item, &val);
      if (! success)
	{
	  xfree (sym_name);
	  Py_DECREF (item);
	  goto error;
	}

      Py_DECREF (item);

      if (sym && ui_out_is_mi_like_p (out) && ! mi_should_print (sym, "locals"))
	continue;

      /* If the object did not provide a value, read it.  */
      if (! val)
	{
	  TRY_CATCH (except, RETURN_MASK_ALL)
	    {
	      val = read_var_value (sym, frame);
	    }
	  if (except.reason > 0)
	    {
	      xfree (sym_name);
	      PyErr_SetString (PyExc_RuntimeError,
			       except.message);
	      goto error;
	    }
	}

      /* With PRINT_NO_VALUES, MI does not emit a tuple normally as
	 each output contains only one field.  The exception is
	 -stack-list-variables, which always provides a tuple.  */
      if (ui_out_is_mi_like_p (out))
	{
	  if (print_mi_args_flag || mi_print_type != PRINT_NO_VALUES)
	    {
	      inner_cleanup =
		make_cleanup_ui_out_tuple_begin_end (out,
						     NULL);
	    }
	}
      else
	/* If the output is not MI we indent locals.  */
	ui_out_spaces (out, (8 + (indent * 2)));

      ui_out_field_string (out, "name", sym_name);
      xfree (sym_name);

      if (! ui_out_is_mi_like_p (out))
	ui_out_text (out, " = ");

      if (ui_out_is_mi_like_p (out)
	  && mi_print_type == PRINT_SIMPLE_VALUES)
	{
	  if (! py_print_type (out, val))
	    goto error;
	}

      if (! ui_out_is_mi_like_p (out)
	  || (ui_out_is_mi_like_p (out)
	      && mi_print_type != PRINT_NO_VALUES))
	{
	  py_print_value (out, val, opts, mi_print_type, language);
	}

      ui_out_text (out, "\n");
      do_cleanups (inner_cleanup);
    }

  if (! item && PyErr_Occurred())
    goto error;

 done:
  return 1;

 error:
  return 0;
}

/*  Helper function for -stack-list-variables.  */

static int
py_mi_print_variables (PyObject *filter, struct ui_out *out,
		       int mi_print_type,
		       const char *cli_print_frame_args_type,
		       struct frame_info *frame)
{
  struct cleanup *old_chain;
  PyObject *args_iter;
  PyObject *locals_iter;

  args_iter = get_py_iter_from_func (filter, "frame_args");
  old_chain = make_cleanup_py_xdecref (args_iter);
  if (! args_iter)
    goto error;

  locals_iter = get_py_iter_from_func (filter, "frame_locals");
  if (! locals_iter)
    goto error;

  make_cleanup_py_decref (locals_iter);
  make_cleanup_ui_out_list_begin_end (out, "variables");

  if (args_iter != Py_None)
      if (! enumerate_args (args_iter, out, mi_print_type,
			    1, cli_print_frame_args_type, frame))
	goto error;

  if (locals_iter != Py_None)
    if (! enumerate_locals (locals_iter, out, mi_print_type, 1, 1,
			    frame))

      goto error;

  do_cleanups (old_chain);
  return 1;

 error:
  do_cleanups (old_chain);
  return 0;
}

/* Helper function for printing locals.  This function largely just
   creates the wrapping tuple, and calls enumerate_locals.  */

static int
py_print_locals (PyObject *filter,
		 struct ui_out *out,
		 int mi_print_type,
		 int indent,
		 struct frame_info *frame)
{
  PyObject *locals_iter = get_py_iter_from_func (filter,
						 "frame_locals");
  struct cleanup *old_chain = make_cleanup_py_xdecref (locals_iter);

  if (! locals_iter)
    goto locals_error;

  make_cleanup_ui_out_list_begin_end (out, "locals");
  if (locals_iter != Py_None)
    if (! enumerate_locals (locals_iter, out, mi_print_type,
			    indent, 0, frame))
      goto locals_error;

  do_cleanups (old_chain);
  return 1;

 locals_error:
  do_cleanups (old_chain);
  return 0;
}

/* Helper function for printing frame arguments.  This function
   largely just creates the wrapping tuple, and calls
   enumerate_args.  */

static int
py_print_args (PyObject *filter,
	       struct ui_out *out,
	       int mi_print_type,
	       const char *cli_print_frame_args_type,
	       struct frame_info *frame)
{
  PyObject *args_iter  = get_py_iter_from_func (filter, "frame_args");
  struct cleanup *old_chain = make_cleanup_py_xdecref (args_iter);

  if (! args_iter)
    goto args_error;

  make_cleanup_ui_out_list_begin_end (out, "args");
  annotate_frame_args ();

  if (! ui_out_is_mi_like_p (out))
    ui_out_text (out, " (");

  if (args_iter != Py_None)
    if (! enumerate_args (args_iter, out, mi_print_type,
			  0, cli_print_frame_args_type,
			  frame))
      goto args_error;

  if (! ui_out_is_mi_like_p (out))
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


/*  Print a single frame to the designated output stream, detecting
    whether the output is MI or console, and formatting the output
    according to the conventions of that protocol.  FILTER is the
    frame-filter associated with this frame.  PRINT_LEVEL is a flag
    and indicates whether to print the frame level.  PRINT_FRAME_INFO
    is a flag that indicates to print the frame information
    (everything other than arguments or locals).  PRINT_ARGS is a flag
    that indicates whether to print frame arguments, and PRINT_LOCALS
    is a flag that indicates whether to print frame local variables.
    MI_PRINT_TYPE is an enumerator to the value types that will be
    printed if the output is MI.  CLI_PRINT_FRAMES_ARGS_TYPE is an
    enumerator indicating what type of frame arguments to print.  OUT
    is the output stream to print too, INDENT is the level of
    indention for this frame (in the case of elided frames), and
    LEVELS_PRINTED is a hash-table containing all the frames level
    that have already been printed.  If a frame level has been
    printed, do not print it again (in the case of elided frames).  */

static int
py_print_frame (PyObject *filter, int print_level, int print_frame_info,
		int print_args, int print_locals, int mi_print_type,
		const char *cli_print_frame_args_type,
		struct ui_out *out, int indent, htab_t levels_printed)
{
  int has_addr = 0;
  CORE_ADDR address = 0;
  struct gdbarch *gdbarch = NULL;
  struct frame_info *frame = NULL;
  struct cleanup *cleanup_stack = make_cleanup (null_cleanup, NULL);
  struct value_print_options opts;
  PyObject *elided;

  get_user_print_options (&opts);

  /* Get the underlying frame.  This is needed to determine GDB
  architecture, and also, in the cases of frame variables/arguments to
  read them if they returned filter object requires us to do so.  */
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

  /* stack-list-variables.  */
  if (print_locals && print_args && ! print_frame_info)
    {
      if (! py_mi_print_variables (filter, out,
				   mi_print_type,
				   cli_print_frame_args_type,
				   frame))
	goto error;
      else
	return PY_BT_COMPLETED;
    }

  /* -stack-list-locals does not require a
     wrapping frame attribute.  */
  if (print_frame_info || (print_args && ! print_locals))
    make_cleanup_ui_out_tuple_begin_end (out, "frame");

  if (print_frame_info)
    {
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
		  address = PyLong_AsLong (paddr);
		  has_addr = 1;
		}
	      Py_DECREF (paddr);
	    }
	  else
	    goto error;
	}

    }

  /* Print frame level.  MI does not require the level if
     locals/variables only are being printed.  */
  if ((print_frame_info || print_args) && print_level)
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

  if (print_frame_info)
    {
      /* Print address to the address field.  If an address is not provided,
	 print nothing.  */
      if (opts.addressprint && has_addr)
	{
	  annotate_frame_address ();
	  ui_out_field_core_addr (out, "addr", gdbarch, address);
	  annotate_frame_address_end ();
	  ui_out_text (out, " in ");
	}

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
    }


  /* Frame arguments.  Check the result, and error if something went
     wrong.  */
  if (print_args)
    {
      if (! py_print_args (filter, out, mi_print_type,
			   cli_print_frame_args_type, frame))
	goto error;
    }

  /* File name/source/line number information.  */
  if (print_frame_info)
    {
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
    }

  /* For MI we need to deal with the "children" list population of
     elided frames, so if MI output detected do not send newline.  */
  if (! ui_out_is_mi_like_p (out))
    {
      annotate_frame_end ();
      ui_out_text (out, "\n");
    }

  if (print_locals)
    {
      if (! py_print_locals (filter, out, mi_print_type, indent,
			     frame))
	goto error;
    }

  /* Finally recursively print elided frames, if any.  */
  elided  = get_py_iter_from_func (filter, "elided");
  if (! elided)
    goto error;

  if (elided != Py_None)
    {
      PyObject *item;

      make_cleanup_py_decref (elided);
      make_cleanup_ui_out_list_begin_end (out, "children");

      if (! ui_out_is_mi_like_p (out))
	indent = indent + 4;

      while ((item = PyIter_Next (elided)))
	{
	  int success =  py_print_frame (item, print_level,
					 print_frame_info,
					 print_args,
					 print_locals,
					 mi_print_type,
					 cli_print_frame_args_type,
					 out, indent,
					 levels_printed);

	  if (success == 0 && PyErr_Occurred ())
	    {
	      Py_DECREF (item);
	      goto error;
	    }

	  Py_DECREF (item);
	}
    }

  /* In MI now we can signal the end.  */
  if (ui_out_is_mi_like_p (out))
      ui_out_text (out, "\n");

  do_cleanups (cleanup_stack);
  return PY_BT_COMPLETED;

 error:
  do_cleanups (cleanup_stack);
  return PY_BT_ERROR;
}

/* Helper function to initiate frame filter invocation at starting
   frame FRAME.  */
static PyObject *
bootstrap_python_frame_filters (struct frame_info *frame)
{

  PyObject *module, *sort_func, *iterable, *frame_obj;

  frame_obj = frame_info_to_frame_object (frame);
  if (! frame_obj)
    return NULL;

  module = PyImport_ImportModule ("gdb.command.frame_filters");
  if (! module)
    {
      Py_DECREF (frame_obj);
      return NULL;
    }

  sort_func = PyObject_GetAttrString (module, "invoke");
  if (! sort_func)
    {
      Py_DECREF (frame_obj);
      Py_DECREF (module);
      return NULL;
    }

  iterable = PyObject_CallFunctionObjArgs (sort_func, frame_obj, NULL);

  Py_DECREF (module);
  Py_DECREF (sort_func);
  Py_DECREF (frame_obj);

  if (! iterable)
    return NULL;

  return iterable;
}

/*  Public and dispatch function for frame filters.  This is the only
    publicly exported function in this file.  FRAME is the source
    frame to start frame-filter invocation.  FLAGS is an integer
    holding the flags for printing.  The following elements of the
    FRAME_FILTER_FLAGS enum denotes makeup of FLAGS: PRINT_LEVEL is a
    flag indicating whether to print the frame's relative level in the
    output.  PRINT_FRAME_INFO is a flag that indicates whether this
    function should print the frame information, PRINT_ARGS is a flag
    that indicates whether to print frame arguments, and PRINT_LOCALS,
    likewise, with frame local variables.  MI_PRINT_ARGS_TYPE is an
    enum from MI that indicates which values types to print.  This
    parameter is ignored if the output is detected to be CLI.
    CLI_PRINT_FRAME_ARGS_TYPE likewise is a an element of what value
    types to print from CLI.  OUT is the output stream to print, and
    COUNT is a delimiter (required for MI slices).  */

int apply_frame_filter (struct frame_info *frame, int flags,
			enum print_values mi_print_args_type,
			const char *cli_print_frame_args_type,
			struct ui_out *out, int count)
{
  struct gdbarch *gdbarch = get_frame_arch (frame);
  struct cleanup *cleanups;
  int result = 0;
  int print_result = 0;
  int success = 0;
  PyObject *iterable;
  int print_level, print_frame_info, print_args, print_locals;

  /* Extract print settings from FLAGS.  */
  print_level = (flags & PRINT_LEVEL) ? 1 : 0;
  print_frame_info = (flags & PRINT_FRAME_INFO) ? 1 : 0;
  print_args = (flags & PRINT_ARGS) ? 1 : 0;
  print_locals = (flags & PRINT_LOCALS) ? 1 : 0;

  cleanups = ensure_python_env (gdbarch, current_language);

  iterable = bootstrap_python_frame_filters (frame);

  if (!iterable)
    goto done;

  /*  If iterable is None, then there are not any frame filters
      registered.  If this is the case, defer to default GDB printing
      routines in MI and CLI.  */

  make_cleanup_py_decref (iterable);
  if (iterable == Py_None)
    {
      do_cleanups (cleanups);
      return PY_BT_NO_FILTERS;
    }

  /* Is it an iterator */
  if PyIter_Check (iterable)
    {
      PyObject *iterator = PyObject_GetIter (iterable);
      PyObject *item;
      htab_t levels_printed;

      if (! iterator)
	goto done;

      make_cleanup_py_decref (iterator);
      levels_printed = htab_create (20,
				    hash_printed_frame_entry,
				    eq_printed_frame_entry,
				    NULL);

      while ((item = PyIter_Next (iterator)) && count--)
	{
	  success =  py_print_frame (item, print_level,
				     print_frame_info, print_args,
				     print_locals,
				     mi_print_args_type,
				     cli_print_frame_args_type,
				     out, 0, levels_printed);

	  /* Do not exit on error printing the frame, continue with
	     other frames.  */
	  if (success == PY_BT_ERROR && PyErr_Occurred ())
	    gdbpy_print_stack ();

	  Py_DECREF (item);
	}

      htab_delete (levels_printed);
    }
  else
    {
      Py_DECREF (iterable);
      error (_("Frame filter must support iteration protocol."));
    }

 done:
  if (PyErr_Occurred ())
    gdbpy_print_stack ();
  do_cleanups (cleanups);
  return success;
}

#else /* HAVE_PYTHON */
int
apply_frame_filter (struct frame_info *frame, int flags,
		    enum print_values mi_print_args_type,
		    const char *cli_print_args_type,
		    struct ui_out *out, int count)
{
  return PY_BT_NO_FILTERS;
}

#endif /* HAVE_PYTHON */
