/* Python interface to stack frames

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
#include "charset.h"
#include "frame.h"
#include "exceptions.h"
#include "symtab.h"
#include "stack.h"
#include "value.h"
#include "python-internal.h"

typedef struct {
  PyObject_HEAD
  struct frame_id frame_id;
  struct gdbarch *gdbarch;

  /* Marks that the FRAME_ID member actually holds the ID of the frame next
     to this, and not this frames' ID itself.  This is a hack to permit Python
     frame objects which represent invalid frames (i.e., the last frame_info
     in a corrupt stack).  The problem arises from the fact that this code
     relies on FRAME_ID to uniquely identify a frame, which is not always true
     for the last "frame" in a corrupt stack (it can have a null ID, or the same
     ID as the  previous frame).  Whenever get_prev_frame returns NULL, we
     record the frame_id of the next frame and set FRAME_ID_IS_NEXT to 1.  */
  int frame_id_is_next;
} frame_object;

#define FRAPY_REQUIRE_VALID(frame_obj, frame)			      \
    do {							      \
      frame = frame_object_to_frame_info (frame_obj);		      \
      if (frame == NULL)					      \
        {							      \
	  PyErr_SetString (PyExc_RuntimeError, "Frame is invalid.");  \
	  return NULL;						      \
	}							      \
    } while (0)

static PyTypeObject frame_object_type;


static struct frame_info *
frame_object_to_frame_info (frame_object *frame_obj)
{
  struct frame_info *frame;

  frame = frame_find_by_id (frame_obj->frame_id);
  if (frame == NULL)
    return NULL;

  if (frame_obj->frame_id_is_next)
    frame = get_prev_frame (frame);

  return frame;
}

static PyObject *
frapy_str (PyObject *self)
{
  char *s;
  long len;
  PyObject *result;
  struct ui_file *strfile;
  
  strfile = mem_fileopen ();
  fprint_frame_id (strfile, ((frame_object *) self)->frame_id);
  s = ui_file_xstrdup (strfile, &len);
  result = PyString_FromString (s);
  xfree (s);

  return result;
}

static PyObject *
frapy_is_valid (PyObject *self, PyObject *args)
{
  struct frame_info *frame;

  frame = frame_object_to_frame_info ((frame_object *) self);
  if (frame == NULL)
    Py_RETURN_FALSE;

  Py_RETURN_TRUE;
}

static PyObject *
frapy_equal_p (PyObject *self, PyObject *args)
{
  int equalp = 0;	  /* Initialize to appease gcc warning.  */
  frame_object *self_frame = (frame_object *) self;
  frame_object *other;
  volatile struct gdb_exception except;

  if (!PyArg_ParseTuple (args, "O!", &frame_object_type, &other))
    return NULL;

  TRY_CATCH (except, RETURN_MASK_ALL)
    {
      equalp = frame_id_eq (self_frame->frame_id, other->frame_id);
    }
  GDB_PY_HANDLE_EXCEPTION (except);

  if (equalp)
    Py_RETURN_TRUE;

  Py_RETURN_FALSE;
}

static PyObject *
frapy_get_name (PyObject *self, PyObject *args)
{
  struct frame_info *frame;
  char *name;
  enum language lang;
  PyObject *result;
  volatile struct gdb_exception except;

  TRY_CATCH (except, RETURN_MASK_ALL)
    {
      FRAPY_REQUIRE_VALID ((frame_object *) self, frame);

      find_frame_funname (frame, &name, &lang);
    }
  GDB_PY_HANDLE_EXCEPTION (except);

  if (name)
    result = PyString_Decode (name, strlen (name), host_charset (), NULL);
  else
    {
      result = Py_None;
      Py_INCREF (Py_None);
    }

  return result;
}

static PyObject *
frapy_get_type (PyObject *self, PyObject *args)
{
  struct frame_info *frame;
  enum frame_type type = NORMAL_FRAME;/* Initialize to appease gcc warning.  */
  volatile struct gdb_exception except;

  TRY_CATCH (except, RETURN_MASK_ALL)
    {
      FRAPY_REQUIRE_VALID ((frame_object *) self, frame);

      type = get_frame_type (frame);
    }
  GDB_PY_HANDLE_EXCEPTION (except);

  return PyInt_FromLong (type);
}

static PyObject *
frapy_get_unwind_stop_reason (PyObject *self, PyObject *args)
{
  struct frame_info *frame = NULL;    /* Initialize to appease gcc warning.  */
  volatile struct gdb_exception except;
  enum unwind_stop_reason stop_reason;

  TRY_CATCH (except, RETURN_MASK_ALL)
    {
      FRAPY_REQUIRE_VALID ((frame_object *) self, frame);
    }
  GDB_PY_HANDLE_EXCEPTION (except);

  stop_reason = get_frame_unwind_stop_reason (frame);

  return PyInt_FromLong (stop_reason);
}

static PyObject *
frapy_get_pc (PyObject *self, PyObject *args)
{
  CORE_ADDR pc = 0;	      /* Initialize to appease gcc warning.  */
  struct frame_info *frame;
  volatile struct gdb_exception except;

  TRY_CATCH (except, RETURN_MASK_ALL)
    {
      FRAPY_REQUIRE_VALID ((frame_object *) self, frame);

      pc = get_frame_pc (frame);
    }
  GDB_PY_HANDLE_EXCEPTION (except);

  return PyLong_FromUnsignedLongLong (pc);
}

static PyObject *
frapy_get_address_in_block (PyObject *self, PyObject *args)
{
  CORE_ADDR pc = 0;	      /* Initialize to appease gcc warning.  */
  struct frame_info *frame;
  volatile struct gdb_exception except;

  TRY_CATCH (except, RETURN_MASK_ALL)
    {
      FRAPY_REQUIRE_VALID ((frame_object *) self, frame);

      pc = get_frame_address_in_block (frame);
    }
  GDB_PY_HANDLE_EXCEPTION (except);

  return PyLong_FromUnsignedLongLong (pc);
}

static frame_object *
frame_info_to_frame_object (struct frame_info *frame)
{
  frame_object *frame_obj;

  frame_obj = PyObject_New (frame_object, &frame_object_type);
  if (frame_obj == NULL)
    {
      PyErr_SetString (PyExc_MemoryError, "Could not allocate frame object.");
      return NULL;
    }

  /* Try to get the previous frame, to determine if this is the last frame
     in a corrupt stack.  If so, we need to store the frame_id of the next
     frame and not of this one (which is possibly invalid).  */
  if (get_prev_frame (frame) == NULL
      && get_frame_unwind_stop_reason (frame) != UNWIND_NO_REASON
      && get_next_frame (frame) != NULL)
    {
      frame_obj->frame_id = get_frame_id (get_next_frame (frame));
      frame_obj->frame_id_is_next = 1;
    }
  else
    {
      frame_obj->frame_id = get_frame_id (frame);
      frame_obj->frame_id_is_next = 0;
    }

  frame_obj->gdbarch = get_frame_arch (frame);

  return frame_obj;
}

static PyObject *
frapy_get_prev (PyObject *self, PyObject *args)
{
  struct frame_info *frame, *prev;
  volatile struct gdb_exception except;
  PyObject *prev_obj = NULL;   /* Initialize to appease gcc warning.  */

  TRY_CATCH (except, RETURN_MASK_ALL)
    {
      FRAPY_REQUIRE_VALID ((frame_object *) self, frame);

      prev = get_prev_frame (frame);
      if (prev)
	prev_obj = (PyObject *) frame_info_to_frame_object (prev);
      else
	{
	  Py_INCREF (Py_None);
	  prev_obj = Py_None;
	}
    }
  GDB_PY_HANDLE_EXCEPTION (except);

  return prev_obj;
}

static PyObject *
frapy_get_next (PyObject *self, PyObject *args)
{
  struct frame_info *frame, *next;
  volatile struct gdb_exception except;
  PyObject *next_obj = NULL;   /* Initialize to appease gcc warning.  */

  TRY_CATCH (except, RETURN_MASK_ALL)
    {
      FRAPY_REQUIRE_VALID ((frame_object *) self, frame);

      next = get_next_frame (frame);
      if (next)
	next_obj = (PyObject *) frame_info_to_frame_object (next);
      else
	{
	  Py_INCREF (Py_None);
	  next_obj = Py_None;
	}
    }
  GDB_PY_HANDLE_EXCEPTION (except);

  return next_obj;
}

static PyObject *
frapy_find_sal (PyObject *self, PyObject *args)
{
  struct frame_info *frame;
  struct symtab_and_line sal;
  volatile struct gdb_exception except;
  PyObject *sal_obj = NULL;   /* Initialize to appease gcc warning.  */

  TRY_CATCH (except, RETURN_MASK_ALL)
    {
      FRAPY_REQUIRE_VALID ((frame_object *) self, frame);

      find_frame_sal (frame, &sal);
      sal_obj = symtab_and_line_to_sal_object (sal);
    }
  GDB_PY_HANDLE_EXCEPTION (except);

  return sal_obj;
}

static PyObject *
frapy_read_var_value (PyObject *self, PyObject *args)
{
  struct frame_info *frame;
  PyObject *sym_obj;
  struct symbol *var;
  struct value *val = NULL;
  volatile struct gdb_exception except;

  if (!PyArg_ParseTuple (args, "O!", &symbol_object_type, &sym_obj))
    return NULL;

  var = symbol_object_to_symbol (sym_obj);
  if (! var)
    {
      PyErr_SetString (PyExc_RuntimeError, "second argument must be symbol");
      return NULL;
    }

  TRY_CATCH (except, RETURN_MASK_ALL)
    {
      FRAPY_REQUIRE_VALID ((frame_object *) self, frame);

      val = read_var_value (var, frame);
    }
  GDB_PY_HANDLE_EXCEPTION (except);

  if (val)
    return value_to_value_object (val);

  Py_RETURN_NONE;
}

PyObject *
gdbpy_get_frames (PyObject *self, PyObject *args)
{
  int result = 0;
  struct frame_info *frame;
  frame_object *frame_obj;
  PyObject *list, *tuple;
  volatile struct gdb_exception except;

  list = PyList_New (0);
  if (list == NULL)
    {
      PyErr_SetString (PyExc_MemoryError, "Could not allocate frames list.");
      return NULL;
    }

  TRY_CATCH (except, RETURN_MASK_ALL)
    {
      for (frame = get_current_frame (); frame; frame = get_prev_frame (frame))
	{
	  frame_obj = frame_info_to_frame_object (frame);
	  if (frame_obj == NULL)
	    {
	      Py_DECREF (list);
	      return NULL;
	    }

	  PyList_Append (list, (PyObject *) frame_obj);
	}
    }
  if (except.reason < 0)
    {
      Py_DECREF (list);
      return PyErr_Format (except.reason == RETURN_QUIT
			   ? PyExc_KeyboardInterrupt : PyExc_RuntimeError,
			   "%s", except.message);
    }

  tuple = PyList_AsTuple (list);
  Py_DECREF (list);

  return tuple;
}

PyObject *
gdbpy_get_current_frame (PyObject *self, PyObject *args)
{
  struct frame_info *frame;
  frame_object *frame_obj = NULL;   /* Initialize to appease gcc warning.  */
  volatile struct gdb_exception except;

  TRY_CATCH (except, RETURN_MASK_ALL)
    {
      frame = get_current_frame ();
      frame_obj = frame_info_to_frame_object (frame);
      if (frame_obj == NULL)
	return NULL;
    }
  GDB_PY_HANDLE_EXCEPTION (except);

  return (PyObject *) frame_obj;
}

PyObject *
gdbpy_get_selected_frame (PyObject *self, PyObject *args)
{
  struct frame_info *frame;
  frame_object *frame_obj = NULL;   /* Initialize to appease gcc warning.  */
  volatile struct gdb_exception except;

  TRY_CATCH (except, RETURN_MASK_ALL)
    {
      frame = get_selected_frame ("No frame is currently selected.");
      frame_obj = frame_info_to_frame_object (frame);
      if (frame_obj == NULL)
	return NULL;
    }
  GDB_PY_HANDLE_EXCEPTION (except);

  return (PyObject *) frame_obj;
}

PyObject *
gdbpy_frame_stop_reason_string (PyObject *self, PyObject *args)
{
  int reason;
  const char *str;

  if (!PyArg_ParseTuple (args, "i", &reason))
    return NULL;

  if (reason < 0 || reason > UNWIND_NO_SAVED_PC)
    {
      PyErr_SetString (PyExc_ValueError, "Invalid frame stop reason.");
      return NULL;
    }

  str = frame_stop_reason_string (reason);
  return PyString_Decode (str, strlen (str), host_charset (), NULL);
}

void
gdbpy_initialize_frames (void)
{
  frame_object_type.tp_new = PyType_GenericNew;
  if (PyType_Ready (&frame_object_type) < 0)
    return;

  /* FIXME: These would probably be best exposed as class attributes of Frame,
     but I don't know how to do it except by messing with the type's dictionary.
     That seems too messy.  */
  PyModule_AddIntConstant (gdb_module, "NORMAL_FRAME", NORMAL_FRAME);
  PyModule_AddIntConstant (gdb_module, "DUMMY_FRAME", DUMMY_FRAME);
  PyModule_AddIntConstant (gdb_module, "SIGTRAMP_FRAME", SIGTRAMP_FRAME);
  PyModule_AddIntConstant (gdb_module, "SENTINEL_FRAME", SENTINEL_FRAME);
  PyModule_AddIntConstant (gdb_module,
			   "FRAME_UNWIND_NO_REASON", UNWIND_NO_REASON);
  PyModule_AddIntConstant (gdb_module,
			   "FRAME_UNWIND_NULL_ID", UNWIND_NULL_ID);
  PyModule_AddIntConstant (gdb_module,
			   "FRAME_UNWIND_FIRST_ERROR", UNWIND_FIRST_ERROR);
  PyModule_AddIntConstant (gdb_module,
			   "FRAME_UNWIND_INNER_ID", UNWIND_INNER_ID);
  PyModule_AddIntConstant (gdb_module,
			   "FRAME_UNWIND_SAME_ID", UNWIND_SAME_ID);
  PyModule_AddIntConstant (gdb_module,
			   "FRAME_UNWIND_NO_SAVED_PC", UNWIND_NO_SAVED_PC);

  Py_INCREF (&frame_object_type);
  PyModule_AddObject (gdb_module, "Frame", (PyObject *) &frame_object_type);
}



static PyMethodDef frame_object_methods[] = {
  { "equals", frapy_equal_p, METH_VARARGS, "Compare frames." },
  { "is_valid", frapy_is_valid, METH_NOARGS,
    "Return true if this frame is valid, false if not." },
  { "get_name", frapy_get_name, METH_NOARGS,
    "Return the function name of the frame." },
  { "get_type", frapy_get_type, METH_NOARGS, "Return the type of the frame." },
  { "get_unwind_stop_reason", frapy_get_unwind_stop_reason,
    METH_NOARGS, "Return the function name of the frame." },
  { "get_pc", frapy_get_pc, METH_NOARGS, "Return the frame's resume address." },
  { "get_address_in_block", frapy_get_address_in_block, METH_NOARGS,
    "Return an address which falls within the frame's code block." },
  { "get_prev", frapy_get_prev, METH_NOARGS,
    "Return the previous (outer) frame." },
  { "get_next", frapy_get_next, METH_NOARGS, "Return the next (inner) frame." },
  { "find_sal", frapy_find_sal, METH_NOARGS,
    "Return the frame's symtab and line." },
  { "read_var_value", frapy_read_var_value, METH_VARARGS,
    "Return the value of the variable in this frame." },
  {NULL}  /* Sentinel */
};

static PyTypeObject frame_object_type = {
  PyObject_HEAD_INIT (NULL)
  0,				  /*ob_size*/
  "gdb.Frame",			  /*tp_name*/
  sizeof (frame_object),	  /*tp_basicsize*/
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
  frapy_str,			  /*tp_str*/
  0,				  /*tp_getattro*/
  0,				  /*tp_setattro*/
  0,				  /*tp_as_buffer*/
  Py_TPFLAGS_DEFAULT,		  /*tp_flags*/
  "GDB frame object",		  /* tp_doc */
  0,				  /* tp_traverse */
  0,				  /* tp_clear */
  0,				  /* tp_richcompare */
  0,				  /* tp_weaklistoffset */
  0,				  /* tp_iter */
  0,				  /* tp_iternext */
  frame_object_methods		  /* tp_methods */
};
