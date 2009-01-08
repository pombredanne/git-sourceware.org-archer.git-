/* Python interface to the inferior memory.

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
#include "exceptions.h"
#include "gdbcore.h"
#include "python-internal.h"

typedef struct {
  PyObject_HEAD
  void *buffer;

  /* These are kept just for mbpy_str.  */
  CORE_ADDR addr;
  CORE_ADDR length;
} membuf_object;

static PyTypeObject membuf_object_type;

/* Implementation of gdb.read_memory (address, length).
   Returns a Python buffer object with LENGTH bytes of the inferior's memory
   at ADDRESS. Both arguments are integers.  */

PyObject *
gdbpy_read_memory (PyObject *self, PyObject *args)
{
  CORE_ADDR addr, length;
  void *buffer;
  membuf_object *membuf_obj;
  struct cleanup *cleanups;
  volatile struct gdb_exception except;

  /* Assume CORE_ADDR corresponds to unsigned long.  */
  if (! PyArg_ParseTuple (args, "kk", &addr, &length))
    return NULL;

  buffer = xmalloc (length);
  cleanups = make_cleanup (xfree, buffer);

  TRY_CATCH (except, RETURN_MASK_ALL)
    {
      read_memory (addr, buffer, length);
    }
  GDB_PY_HANDLE_EXCEPTION (except);

  discard_cleanups (cleanups);

  membuf_obj = PyObject_New (membuf_object, &membuf_object_type);
  if (membuf_obj == NULL)
    {
      xfree (buffer);
      PyErr_SetString (PyExc_MemoryError,
		       "Could not allocate memory buffer object.");
      return NULL;
    }

  membuf_obj->buffer = buffer;

  membuf_obj->addr = addr;
  membuf_obj->length = length;

  return PyBuffer_FromReadWriteObject ((PyObject *) membuf_obj, 0, Py_END_OF_BUFFER);
}

/* Implementation of gdb.write_memory (address, buffer [, length]).
   Writes the contents of BUFFER (a Python object supporting the read buffer
   protocol) at ADDRESS in the inferior's memory.  Write LENGTH bytes from
   BUFFER, or its entire contents if the argument is not provided.  The
   function returns nothing.  */

PyObject *
gdbpy_write_memory (PyObject *self, PyObject *args)
{
  int buf_len;
  const char *buffer;
  long length = -1;
  CORE_ADDR addr;
  volatile struct gdb_exception except;

  /* Assume CORE_ADDR corresponds to unsigned long.  */
  if (! PyArg_ParseTuple (args, "ks#|l", &addr, &buffer, &buf_len, &length))
    return NULL;

  if (length == -1)
    length = buf_len;

  TRY_CATCH (except, RETURN_MASK_ALL)
    {
      write_memory (addr, buffer, length);
    }
  GDB_PY_HANDLE_EXCEPTION (except);

  Py_RETURN_NONE;
}

/* Destructor of Membuf objects.  */

static void
mbpy_dealloc (PyObject *self)
{
  xfree (((membuf_object *) self)->buffer);
  self->ob_type->tp_free (self);
}

/* Return a description of the Membuf object.  */

static PyObject *
mbpy_str (PyObject *self)
{
  membuf_object *membuf_obj = (membuf_object *) self;

  return PyString_FromFormat ("memory buffer for address %p, %u bytes long",
			      (void *) membuf_obj->addr,
			      (unsigned int) membuf_obj->length);
}

Py_ssize_t
get_read_buffer (PyObject *self, Py_ssize_t segment, void **ptrptr)
{
  membuf_object *membuf_obj = (membuf_object *) self;

  if (segment)
    {
      PyErr_SetString (PyExc_SystemError,
		       "The memory buffer supports only one segment.");
      return -1;
    }

  *ptrptr = membuf_obj->buffer;

  return membuf_obj->length;
}

Py_ssize_t
get_write_buffer (PyObject *self, Py_ssize_t segment, void **ptrptr)
{
  return get_read_buffer (self, segment, ptrptr);
}

Py_ssize_t
get_seg_count (PyObject *self, Py_ssize_t *lenp)
{
  if (lenp)
    *lenp = ((membuf_object *) self)->length;

  return 1;
}

Py_ssize_t
get_char_buffer (PyObject *self, Py_ssize_t segment, char **ptrptr)
{
  return get_char_buffer (self, segment, ptrptr);
}

/* Python doesn't provide a decent way to get compatibility here.  */
#if HAVE_LIBPYTHON2_4
#define CHARBUFFERPROC_NAME getcharbufferproc
#else
#define CHARBUFFERPROC_NAME charbufferproc
#endif

static PyBufferProcs buffer_procs = {
  get_read_buffer,
  get_write_buffer,
  get_seg_count,
  /* The cast here works around a difference between Python 2.4 and
     Python 2.5.  */
  (CHARBUFFERPROC_NAME) get_char_buffer
};

static PyTypeObject membuf_object_type = {
  PyObject_HEAD_INIT (NULL)
  0,				  /*ob_size*/
  "gdb.Membuf",			  /*tp_name*/
  sizeof (membuf_object),	  /*tp_basicsize*/
  0,				  /*tp_itemsize*/
  mbpy_dealloc,			  /*tp_dealloc*/
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
  mbpy_str,			  /*tp_str*/
  0,				  /*tp_getattro*/
  0,				  /*tp_setattro*/
  &buffer_procs,		  /*tp_as_buffer*/
  Py_TPFLAGS_DEFAULT,		  /*tp_flags*/
  "GDB memory buffer object" 	  /*tp_doc*/
};

void
gdbpy_initialize_membuf (void)
{
  membuf_object_type.tp_new = PyType_GenericNew;
  if (PyType_Ready (&membuf_object_type) < 0)
    return;

  Py_INCREF (&membuf_object_type);
  PyModule_AddObject (gdb_module, "Membuf", (PyObject *) &membuf_object_type);
}
