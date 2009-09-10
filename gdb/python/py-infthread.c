/* Python interface to inferior threads.

   Copyright (C) 2009 Free Software Foundation, Inc.

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
#include "gdbthread.h"
#include "inferior.h"
#include "python-internal.h"

static PyTypeObject thread_object_type;

/* Require that INFERIOR be a valid inferior ID.  */
#define THPY_REQUIRE_VALID(Thread)				\
  do {								\
    if (!Thread->thread)					\
      {								\
	PyErr_SetString (PyExc_RuntimeError,			\
			 "thread no longer exists");	        \
	return NULL;						\
      }								\
  } while (0)



thread_object *
create_thread_object (struct thread_info *tp)
{
  thread_object *thread_obj;
  
  thread_obj = PyObject_New (thread_object, &thread_object_type);
  if (!thread_obj)
    return NULL;

  thread_obj->thread = tp;
  thread_obj->inf_obj = find_inferior_object (PIDGET (tp->ptid));
  Py_INCREF (thread_obj->inf_obj);

  return thread_obj;
}



static void
thpy_dealloc (PyObject *self)
{
  Py_DECREF (((thread_object *) self)->inf_obj);
  self->ob_type->tp_free (self);
}

static PyObject *
thpy_get_num (PyObject *self, void *closure)
{
  thread_object *thread_obj = (thread_object *) self;

  THPY_REQUIRE_VALID (thread_obj);

  return PyLong_FromLong (thread_obj->thread->num);
}



/* Implementation of Inferior.frames () -> (gdb.Frame, ...).
   Returns a tuple of all frame objects.  */
PyObject *
thpy_frames (PyObject *self, PyObject *args)
{
  int result = 0;
  struct frame_info *frame;
  PyObject *frame_obj;
  PyObject *list, *tuple;
  thread_object *thread_obj = (thread_object *) self;
  struct cleanup *cleanup;
  volatile struct gdb_exception except;

  THPY_REQUIRE_VALID (thread_obj);

  list = PyList_New (0);
  if (list == NULL)
    {
      PyErr_SetString (PyExc_MemoryError, "Could not allocate frames list.");
      return NULL;
    }

  cleanup = make_cleanup_restore_current_thread ();

  TRY_CATCH (except, RETURN_MASK_ALL)
    {
      switch_to_thread (thread_obj->thread->ptid);

      for (frame = get_current_frame (); frame; frame = get_prev_frame (frame))
	{
	  frame_obj = frame_info_to_frame_object (frame);
	  if (frame_obj == NULL)
	    {
	      Py_DECREF (list);
	      list = NULL;
	      break;
	    }

	  PyList_Append (list, frame_obj);
	}
    }
  if (except.reason < 0)
    {
      Py_DECREF (list);
      return PyErr_Format (except.reason == RETURN_QUIT
			   ? PyExc_KeyboardInterrupt : PyExc_RuntimeError,
			   "%s", except.message);
    }

  do_cleanups (cleanup);

  if (list)
    {
      tuple = PyList_AsTuple (list);
      Py_DECREF (list);
    }
  else
    tuple = NULL;

  return tuple;
}

/* Implementation of InferiorThread.newest_frame () -> gdb.Frame.
   Returns the newest frame object.  */
PyObject *
thpy_newest_frame (PyObject *self, PyObject *args)
{
  struct frame_info *frame;
  PyObject *frame_obj = NULL;   /* Initialize to appease gcc warning.  */
  thread_object *thread_obj = (thread_object *) self;
  struct cleanup *cleanup;
  volatile struct gdb_exception except;

  THPY_REQUIRE_VALID (thread_obj);

  cleanup = make_cleanup_restore_current_thread ();

  TRY_CATCH (except, RETURN_MASK_ALL)
    {
      switch_to_thread (thread_obj->thread->ptid);

      frame = get_current_frame ();
      frame_obj = frame_info_to_frame_object (frame);
    }
  GDB_PY_HANDLE_EXCEPTION (except);

  do_cleanups (cleanup);

  return frame_obj;
}

/* Implementation of InferiorThread.switch ().
   Makes this the GDB selected thread.  */
static PyObject *
thpy_switch (PyObject *self, PyObject *args)
{
  thread_object *thread_obj = (thread_object *) self;
  struct cleanup *cleanup;
  volatile struct gdb_exception except;

  THPY_REQUIRE_VALID (thread_obj);

  TRY_CATCH (except, RETURN_MASK_ALL)
    {
      switch_to_thread (thread_obj->thread->ptid);
    }
  GDB_PY_HANDLE_EXCEPTION (except);

  Py_RETURN_NONE;
}



/* Implementation of gdb.selected_thread () -> gdb.InferiorThread.
   Returns the selected thread object.  */
PyObject *
gdbpy_selected_thread (PyObject *self, PyObject *args)
{
  PyObject *thread_obj;
  
  thread_obj = (PyObject *) find_thread_object (inferior_ptid);
  if (thread_obj)
    {
      Py_INCREF (thread_obj);
      return thread_obj;
    }

  Py_RETURN_NONE;
}



void
gdbpy_initialize_thread (void)
{
  if (PyType_Ready (&thread_object_type) < 0)
    return;

  Py_INCREF (&thread_object_type);
  PyModule_AddObject (gdb_module, "InferiorThread",
		      (PyObject *) &thread_object_type);
}



static PyGetSetDef thread_object_getset[] =
{
  { "num", thpy_get_num, NULL, "ID of the thread, as assigned by GDB.", NULL },

  { NULL }
};

static PyMethodDef thread_object_methods[] =
{
  { "frames", thpy_frames, METH_NOARGS,
    "frames () -> (gdb.Frame, ...)\n\
Return a tuple containing all frames in the thread." },
  { "newest_frame", thpy_newest_frame, METH_NOARGS,
    "newest_frame () -> gdb.Frame\n\
Return the newest frame in the thread." },
  { "switch", thpy_switch, METH_NOARGS,
    "switch ()\n\
Makes this the GDB selected thread." },

  { NULL }
};

static PyTypeObject thread_object_type =
{
  PyObject_HEAD_INIT (NULL)
  0,				  /*ob_size*/
  "gdb.InferiorThread",		  /*tp_name*/
  sizeof (thread_object),	  /*tp_basicsize*/
  0,				  /*tp_itemsize*/
  thpy_dealloc,			  /*tp_dealloc*/
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
  Py_TPFLAGS_DEFAULT | Py_TPFLAGS_HAVE_ITER,  /*tp_flags*/
  "GDB thread object",		  /* tp_doc */
  0,				  /* tp_traverse */
  0,				  /* tp_clear */
  0,				  /* tp_richcompare */
  0,				  /* tp_weaklistoffset */
  0,				  /* tp_iter */
  0,				  /* tp_iternext */
  thread_object_methods,	  /* tp_methods */
  0,				  /* tp_members */
  thread_object_getset,		  /* tp_getset */
  0,				  /* tp_base */
  0,				  /* tp_dict */
  0,				  /* tp_descr_get */
  0,				  /* tp_descr_set */
  0,				  /* tp_dictoffset */
  0,				  /* tp_init */
  0				  /* tp_alloc */
};
