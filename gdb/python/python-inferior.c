/* Python interface to inferiors.

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
#include "gdbcore.h"
#include "gdbthread.h"
#include "inferior.h"
#include "observer.h"
#include "python-internal.h"

struct threadlist_entry {
  thread_object *thread_obj;
  struct threadlist_entry *next;
};

typedef struct
{
  PyObject_HEAD

  /* The inferior we represent.  */
  struct inferior *inferior;

  /* thread_object instances under this inferior.  This list owns a reference
     to each object it contains. */
  struct threadlist_entry *threads;

  /* Number of threads in the list.  */
  int nthreads;
} inferior_object;

static PyTypeObject inferior_object_type;

typedef struct {
  PyObject_HEAD
  void *buffer;

  /* These are kept just for mbpy_str.  */
  CORE_ADDR addr;
  CORE_ADDR length;
} membuf_object;

static PyTypeObject membuf_object_type;

/* Require that INFERIOR be a valid inferior ID.  */
#define INFPY_REQUIRE_VALID(Inferior)				\
  do {								\
    if (!Inferior->inferior)					\
      {								\
	PyErr_SetString (PyExc_RuntimeError,			\
			 "inferior no longer exists");	        \
	return NULL;						\
      }								\
  } while (0)

struct inflist_entry {
  inferior_object *inf_obj;
  struct inflist_entry *next;
};



/* Inferior objects list.  */

/* List containing inferior_objects.  This list owns a reference to each
   object it contains.  */
static struct inflist_entry *inferior_list;

static int ninferiors;


/* An observer callback function that is called when an inferior has
   been created.  Creates a corresponding Python object for the inferior
   and adds it to the list.  */
static void
add_inferior_object (int pid)
{
  struct inferior *inf = find_inferior_pid (pid);
  inferior_object *inf_obj;
  struct inflist_entry *entry;
  PyGILState_STATE state;
  struct cleanup *cleanup;

  if (!inf)
    {
      warning (_("Can't create Python Inferior object."));
      return;
    }

  state = PyGILState_Ensure ();
  cleanup = make_cleanup_py_restore_gil (&state);

  inf_obj = PyObject_New (inferior_object, &inferior_object_type);
  if (!inf_obj)
    {
      warning (_("Can't create Python Inferior object."));
      gdbpy_print_stack ();
      do_cleanups (cleanup);
      return;
    }

  inf_obj->inferior = inf;
  inf_obj->threads = NULL;
  inf_obj->nthreads = 0;

  entry = xmalloc (sizeof (struct inflist_entry));
  entry->inf_obj = inf_obj;
  entry->next = inferior_list;

  inferior_list = entry;

  ninferiors++;

  do_cleanups (cleanup);
}

/* An observer callback function that is called when an inferior has
   been deleted.  Removes the corresponding Python object from the
   inferior list, and removes the list's reference to the object.  */
static void
delete_inferior_object (int pid)
{
  PyGILState_STATE state;
  struct inflist_entry **inf_entry, *inf_tmp;
  struct threadlist_entry *th_entry, *th_tmp;

  /* Find inferior_object for the given PID.  */
  for (inf_entry = &inferior_list; *inf_entry != NULL;
       inf_entry = &(*inf_entry)->next)
    if ((*inf_entry)->inf_obj->inferior->pid == pid)
      break;

  if (!*inf_entry)
    return;

  state = PyGILState_Ensure ();

  inf_tmp = *inf_entry;
  inf_tmp->inf_obj->inferior = NULL;

  /* Deallocate threads list.  */
  for (th_entry = inf_tmp->inf_obj->threads; th_entry != NULL;)
    {
      Py_DECREF (th_entry->thread_obj);

      th_tmp = th_entry;
      th_entry = th_entry->next;
      xfree (th_tmp);
    }

  inf_tmp->inf_obj->nthreads = 0;

  *inf_entry = (*inf_entry)->next;
  Py_DECREF (inf_tmp->inf_obj);
  xfree (inf_tmp);

  ninferiors--;

  PyGILState_Release (state);
}

/* Finds the Python Inferior object for the given pid.  Returns a borrowed
   reference.  */
PyObject *
find_inferior_object (int pid)
{
  struct inflist_entry *p;

  for (p = inferior_list; p != NULL; p = p->next)
    if (p->inf_obj->inferior->pid == pid)
      return (PyObject *) p->inf_obj;

  return NULL;
}

/* Finds the Python InferiorThread object for the given ptid.  Returns a
   borrowed reference.  */
thread_object *
find_thread_object (ptid_t ptid)
{
  int pid;
  struct inflist_entry *p;
  struct threadlist_entry *q;

  pid = PIDGET (ptid);
  for (p = inferior_list; p != NULL; p = p->next)
    if (p->inf_obj->inferior->pid == pid)
      for (q = p->inf_obj->threads; q != NULL; q = q->next)
	if (ptid_equal (q->thread_obj->thread->ptid, ptid))
	  return q->thread_obj;

  return NULL;
}



/* Inferior object.  */

static void
add_thread_object (struct thread_info *tp)
{
  PyGILState_STATE state;
  thread_object *thread_obj;
  inferior_object *inf_obj;
  struct threadlist_entry *entry;

  state = PyGILState_Ensure ();

  thread_obj = create_thread_object (tp);
  if (!thread_obj)
    {
      warning (_("Can't create Python InferiorThread object."));
      gdbpy_print_stack ();
      PyGILState_Release (state);
      return;
    }

  inf_obj = (inferior_object *) thread_obj->inf_obj;

  entry = xmalloc (sizeof (struct threadlist_entry));
  entry->thread_obj = thread_obj;
  entry->next = inf_obj->threads;

  inf_obj->threads = entry;
  inf_obj->nthreads++;

  PyGILState_Release (state);
}

static void
delete_thread_object (struct thread_info *tp)
{
  PyGILState_STATE state;
  inferior_object *inf_obj;
  thread_object *thread_obj;
  struct threadlist_entry **entry, *tmp;

  inf_obj = (inferior_object *) find_inferior_object (PIDGET(tp->ptid));
  if (!inf_obj)
    return;

  /* Find thread entry in its inferior's thread_list.  */
  for (entry = &inf_obj->threads; *entry != NULL; entry = &(*entry)->next)
    if ((*entry)->thread_obj->thread == tp)
      break;

  if (!*entry)
    return;

  state = PyGILState_Ensure ();

  tmp = *entry;
  tmp->thread_obj->thread = NULL;

  *entry = (*entry)->next;
  inf_obj->nthreads--;

  Py_DECREF (tmp->thread_obj);
  xfree (tmp);


  PyGILState_Release (state);
}

static PyObject *
infpy_threads (PyObject *self, PyObject *args)
{
  int i;
  struct threadlist_entry *entry;
  inferior_object *inf_obj = (inferior_object *) self;
  PyObject *tuple;

  INFPY_REQUIRE_VALID (inf_obj);


  tuple = PyTuple_New (inf_obj->nthreads);
  if (!tuple)
    return NULL;

  /* The list is in reverse order of thread age (i.e., newest comes first),
     is this a problem?  */
  for (i = 0, entry = inf_obj->threads; i < inf_obj->nthreads;
       i++, entry = entry->next)
    {
      Py_INCREF (entry->thread_obj);
      PyTuple_SET_ITEM (tuple, i, (PyObject *) entry->thread_obj);
    }

  return tuple;
}

static PyObject *
infpy_get_num (PyObject *self, void *closure)
{
  inferior_object *inf = (inferior_object *) self;

  INFPY_REQUIRE_VALID (inf);

  return PyLong_FromLong (inf->inferior->num);
}

static PyObject *
infpy_get_pid (PyObject *self, void *closure)
{
  inferior_object *inf = (inferior_object *) self;

  INFPY_REQUIRE_VALID (inf);

  return PyLong_FromLong (inf->inferior->pid);
}

static PyObject *
infpy_get_was_attached (PyObject *self, void *closure)
{
  inferior_object *inf = (inferior_object *) self;
  INFPY_REQUIRE_VALID (inf);
  if (inf->inferior->attach_flag)
    Py_RETURN_TRUE;
  Py_RETURN_FALSE;
}



/* Implementation of gdb.inferiors () -> (gdb.Inferior, ...).
   Returns a list of all inferiors.  */

PyObject *
gdbpy_inferiors (PyObject *unused, PyObject *unused2)
{
  int i;
  struct inflist_entry *entry;
  PyObject *tuple;

  tuple = PyTuple_New (ninferiors);
  if (!tuple)
    return NULL;

  /* The list is in reverse order of inferior age (i.e., newest comes first),
     is this a problem?  */
  for (i = 0, entry = inferior_list; i < ninferiors; i++, entry = entry->next)
    {
      Py_INCREF (entry->inf_obj);
      PyTuple_SET_ITEM (tuple, i, (PyObject *) entry->inf_obj);
    }

  return tuple;
}



/* Membuf and memory manipulation.  */

/* Implementation of gdb.read_memory (address, length).
   Returns a Python buffer object with LENGTH bytes of the inferior's memory
   at ADDRESS. Both arguments are integers.  */

static PyObject *
infpy_read_memory (PyObject *self, PyObject *args)
{
  int error = 0;
  CORE_ADDR addr, length;
  void *buffer = NULL;
  membuf_object *membuf_obj;
  PyObject *addr_obj, *length_obj;
  struct cleanup *cleanups = NULL;
  volatile struct gdb_exception except;

  if (! PyArg_ParseTuple (args, "OO", &addr_obj, &length_obj))
    return NULL;

  TRY_CATCH (except, RETURN_MASK_ALL)
    {
      if (!get_addr_from_python (addr_obj, &addr)
	  || !get_addr_from_python (length_obj, &length))
	{
	  error = 1;
	  break;
	}

      buffer = xmalloc (length);
      cleanups = make_cleanup (xfree, buffer);

      read_memory (addr, buffer, length);
    }
  GDB_PY_HANDLE_EXCEPTION (except);

  if (error)
    return NULL;

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

  return PyBuffer_FromReadWriteObject ((PyObject *) membuf_obj, 0,
				       Py_END_OF_BUFFER);
}

/* Implementation of gdb.write_memory (address, buffer [, length]).
   Writes the contents of BUFFER (a Python object supporting the read buffer
   protocol) at ADDRESS in the inferior's memory.  Write LENGTH bytes from
   BUFFER, or its entire contents if the argument is not provided.  The
   function returns nothing.  */

static PyObject *
infpy_write_memory (PyObject *self, PyObject *args)
{
  int buf_len, error = 0;
  const char *buffer;
  CORE_ADDR addr, length;
  PyObject *addr_obj, *length_obj = NULL;
  volatile struct gdb_exception except;

  if (! PyArg_ParseTuple (args, "Os#|O", &addr_obj, &buffer, &buf_len,
			  &length_obj))
    return NULL;

  TRY_CATCH (except, RETURN_MASK_ALL)
    {
      if (!get_addr_from_python (addr_obj, &addr))
	{
	  error = 1;
	  break;
	}
      
      if (!length_obj)
	length = buf_len;
      else if (!get_addr_from_python (length_obj, &length))
	{
	  error = 1;
	  break;
	}

      write_memory (addr, buffer, length);
    }
  GDB_PY_HANDLE_EXCEPTION (except);

  if (error)
    return NULL;

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

  return PyString_FromFormat ("memory buffer for address %s, %s bytes long",
			      paddress (membuf_obj->addr),
			      pulongest (membuf_obj->length));
}

static Py_ssize_t
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

static Py_ssize_t
get_write_buffer (PyObject *self, Py_ssize_t segment, void **ptrptr)
{
  return get_read_buffer (self, segment, ptrptr);
}

static Py_ssize_t
get_seg_count (PyObject *self, Py_ssize_t *lenp)
{
  if (lenp)
    *lenp = ((membuf_object *) self)->length;

  return 1;
}

static Py_ssize_t
get_char_buffer (PyObject *self, Py_ssize_t segment, char **ptrptr)
{
  void *ptr = NULL;
  Py_ssize_t ret;

  ret = get_read_buffer (self, segment, &ptr);
  *ptrptr = (char *) ptr;

  return ret;
}

/* Adds GDB value V to the pattern buffer in *PATTERN_BUF.  If SIZE is not zero,
   it specifies the number of bytes from V to copy to *PATTERN_BUF.  The
   function increases the size of *PATTERN_BUF as necessary, adjusting
   *PATTERN_BUF_END and *PATTERN_BUF_SIZE in the process.  */

static void
add_value_pattern (struct value *v, int size, char **pattern_buf,
		   char **pattern_buf_end, ULONGEST *pattern_buf_size)
{
  int val_bytes;

  if (size)
    {
      LONGEST x = value_as_long (v);

      if (size == 1)
	*(*pattern_buf_end)++ = x;
      else
	{
	  put_bits (x, *pattern_buf_end, size * 8,
		    gdbarch_byte_order (current_gdbarch) == BFD_ENDIAN_BIG);
	  *pattern_buf_end += size;
	}
    }
  else
   {
     val_bytes = TYPE_LENGTH (value_type (v));

     increase_pattern_buffer (pattern_buf, pattern_buf_end,
			      pattern_buf_size, val_bytes);

     memcpy (*pattern_buf_end, value_contents_raw (v), val_bytes);
     *pattern_buf_end += val_bytes;
   }
}

/* This function does the actual work of constructing the pattern buffer from
   OBJ.  If OBJ is an object which implements the read buffer protocol (such
   as a string, a byte array or gdb.Membuf), then its contents are directly
   copied to *PATTERN_BUF.  If it is a list, then this function is recursively
   called for each of its elements.  If OBJ is an object which can be converted
   to a GDB value, then the contents of the value are copied to PATTERN_BUF.
   If SIZE is different than zero, then it limits the number of bytes which
   are copied to the buffer in case OBJ is converted to a GDB value.  That
   means that SIZE influences only Python scalars and gdb.Value objects.
   The function increases the size of *PATTERN_BUF as necessary, adjusting
   *PATTERN_BUF_END and *PATTERN_BUF_SIZE in the process.

   Returns 1 on success or 0 on failure, with a Python exception set.  This
   function can also throw GDB exceptions.  */

static int
add_pattern_element (PyObject *obj, int size, char **pattern_buf,
		     char **pattern_buf_end, ULONGEST *pattern_buf_size)
{
  if (PyObject_CheckReadBuffer (obj))
    {
      /* Handle string, Unicode string, byte array, gdb.Membuf and any other
         object implementing the buffer protocol.  The SIZE parameter is
	 ignored in this case.  */

      Py_ssize_t val_bytes;
      const void *buffer;

      if (PyObject_AsReadBuffer (obj, &buffer, &val_bytes) == -1)
	return 0;

      increase_pattern_buffer (pattern_buf, pattern_buf_end,
			       pattern_buf_size, val_bytes);

      memcpy (*pattern_buf_end, buffer, val_bytes);
      *pattern_buf_end += val_bytes;
    }
  else if (gdbpy_is_value_object (obj))
    add_value_pattern (value_object_to_value (obj), size, pattern_buf,
		       pattern_buf_end, pattern_buf_size);
  else if (PySequence_Check (obj))
    {
      /* Handle lists and tuples.  */

      Py_ssize_t i, num_objs;

      num_objs = PySequence_Size (obj);
      for (i = 0; i < num_objs; i++)
	if (!add_pattern_element (PySequence_GetItem (obj, i), size,
				  pattern_buf, pattern_buf_end,
				  pattern_buf_size))
	  return 0;
    }
  else
    {
      /* See if we can convert from a Python object to a GDB value.  */

      struct value *v = convert_value_from_python (obj);

      if (v)
	add_value_pattern (v, size, pattern_buf, pattern_buf_end,
			   pattern_buf_size);
      else
	return 0;
    }

  return 1;
}

/* Constructs the search pattern from OBJ, putting it in *PATTERN_BUFP, and its
   size in *PATTERN_LENP.  See the function add_pattern_element to learn how
   the search pattern is obtained from OBJ.

   Returns 1 on success or 0 on failure, with a Python exception set.  This
   function can also throw GDB exceptions.  */

static int
get_search_pattern (PyObject *obj, int size, char **pattern_bufp,
		    ULONGEST *pattern_lenp)
{
  /* Buffer to hold the search pattern.  */
  char *pattern_buf;
  /* Current size of search pattern buffer.
     We realloc space as needed.  */
  ULONGEST pattern_buf_size;
  /* Pointer to one past the last in-use part of pattern_buf.  */
  char *pattern_buf_end;
  struct cleanup *old_cleanups;

  allocate_pattern_buffer (&pattern_buf, &pattern_buf_end, &pattern_buf_size);
  old_cleanups = make_cleanup (free_current_contents, &pattern_buf);

  if (!add_pattern_element (obj, size, &pattern_buf, &pattern_buf_end,
			    &pattern_buf_size))
    {
      do_cleanups (old_cleanups);

      return 0;
    }

  *pattern_bufp = pattern_buf;
  *pattern_lenp = pattern_buf_end - pattern_buf;

  discard_cleanups (old_cleanups);

  return 1;
}

/* Implementation of
   gdb.search_memory (address, length, pattern [, size] [, max_count]).
   The third argument may be either a pattern, or a list or tupple of patterns
   to be searched.  Size is the size in bytes of each search query value, either
   1, 2, 4 or 8.  Returns a list of the addresses where matches were found.  */

static PyObject *
infpy_search_memory (PyObject *self, PyObject *args, PyObject *kw)
{
  int size = 0;
  unsigned int found_count = 0;
  long max_count = 0;
  CORE_ADDR start_addr, length;
  char *pattern_buf;
  static char *keywords[] = { "address", "length", "pattern", "size",
			      "max_count", NULL };
  ULONGEST pattern_len, search_space_len;
  PyObject *pattern, *list = NULL, *start_addr_obj, *length_obj;
  volatile struct gdb_exception except;

  if (! PyArg_ParseTupleAndKeywords (args, kw, "OOO|il", keywords,
				     &start_addr_obj, &length_obj, &pattern,
				     &size, &max_count))
    return NULL;

  if (!max_count)
    max_count = LONG_MAX;

  if (size != 0 && size != 1 && size != 2 && size != 4 && size != 8)
    {
      PyErr_SetString (PyExc_ValueError, "invalid pattern size");
      return NULL;
    }

  TRY_CATCH (except, RETURN_MASK_ALL)
    {
      if (get_addr_from_python (start_addr_obj, &start_addr)
	  && get_addr_from_python (length_obj, &length))
	{
	  if (!length)
	    {
	      PyErr_SetString (PyExc_ValueError, "empty search range");
	      break;
	    }
	  /* Watch for overflows.  */
	  else if (length > CORE_ADDR_MAX
		   || (start_addr + length - 1) < start_addr)
	    {
	      PyErr_SetString (PyExc_ValueError, "search range too large");
	      break;
	    }

	  search_space_len = length;

	  if (get_search_pattern (pattern, size, &pattern_buf, &pattern_len))
	    {
	      /* Any cleanups get automatically executed on an exception.  */
	      struct cleanup *cleanups = make_cleanup (xfree, pattern_buf);

	      list = PyList_New (0);

	      while (search_space_len >= pattern_len && found_count < max_count)
		{
		  CORE_ADDR found_addr;
		  int found;

		  found = search_memory (&start_addr, &search_space_len,
					 pattern_buf, pattern_len, &found_addr);
		  if (found <= 0)
		    break;

		  PyList_Append (list, PyLong_FromUnsignedLong (found_addr));
		  ++found_count;
		}

	      do_cleanups (cleanups);
	    }
	}
    }
  GDB_PY_HANDLE_EXCEPTION (except);

  return list;
}



void
gdbpy_initialize_inferior (void)
{
  if (PyType_Ready (&inferior_object_type) < 0)
    return;

  Py_INCREF (&inferior_object_type);
  PyModule_AddObject (gdb_module, "Inferior",
		      (PyObject *) &inferior_object_type);

  inferior_list = NULL;
  ninferiors = 0;

  observer_attach_new_inferior (add_inferior_object);
  observer_attach_inferior_exit (delete_inferior_object);
  observer_attach_new_thread (add_thread_object);
  observer_attach_thread_exit (delete_thread_object);

  if (PyType_Ready (&membuf_object_type) < 0)
    return;

  Py_INCREF (&membuf_object_type);
  PyModule_AddObject (gdb_module, "Membuf", (PyObject *) &membuf_object_type);
}



static PyGetSetDef inferior_object_getset[] =
{
  { "num", infpy_get_num, NULL, "ID of inferior, as assigned by GDB.", NULL },
  { "pid", infpy_get_pid, NULL, "PID of inferior, as assigned by the OS.",
    NULL },
  { "was_attached", infpy_get_was_attached, NULL,
    "True if the inferior was created using 'attach'.", NULL },

  { NULL }
};

static PyMethodDef inferior_object_methods[] =
{
  { "threads", infpy_threads, METH_NOARGS,
    "Return all the threads of this inferior." },

  { "read_memory", infpy_read_memory, METH_VARARGS,
    "read_memory (address, length) -> buffer\n\
Return a buffer object for reading from the inferior's memory." },
  { "write_memory", infpy_write_memory, METH_VARARGS,
    "write_memory (address, buffer [, length])\n\
Write the given buffer object to the inferior's memory." },
  { "search_memory", (PyCFunction) infpy_search_memory, METH_VARARGS | METH_KEYWORDS,
    "search_memory (address, length, pattern [, size] [, max_count]) -> list\n\
Return a list with the addresses where matches were found." },

  { NULL }
};

static PyTypeObject inferior_object_type =
{
  PyObject_HEAD_INIT (NULL)
  0,				  /* ob_size */
  "gdb.Inferior",		  /* tp_name */
  sizeof (inferior_object),	  /* tp_basicsize */
  0,				  /* tp_itemsize */
  0,				  /* tp_dealloc */
  0,				  /* tp_print */
  0,				  /* tp_getattr */
  0,				  /* tp_setattr */
  0,				  /* tp_compare */
  0,				  /* tp_repr */
  0,				  /* tp_as_number */
  0,				  /* tp_as_sequence */
  0,				  /* tp_as_mapping */
  0,				  /* tp_hash  */
  0,				  /* tp_call */
  0,				  /* tp_str */
  0,				  /* tp_getattro */
  0,				  /* tp_setattro */
  0,				  /* tp_as_buffer */
  Py_TPFLAGS_DEFAULT | Py_TPFLAGS_HAVE_ITER,  /* tp_flags */
  "GDB inferior object",	  /* tp_doc */
  0,				  /* tp_traverse */
  0,				  /* tp_clear */
  0,				  /* tp_richcompare */
  0,				  /* tp_weaklistoffset */
  0,				  /* tp_iter */
  0,				  /* tp_iternext */
  inferior_object_methods,	  /* tp_methods */
  0,				  /* tp_members */
  inferior_object_getset,	  /* tp_getset */
  0,				  /* tp_base */
  0,				  /* tp_dict */
  0,				  /* tp_descr_get */
  0,				  /* tp_descr_set */
  0,				  /* tp_dictoffset */
  0,				  /* tp_init */
  0				  /* tp_alloc */
};



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
  "GDB memory buffer object", 	  /*tp_doc*/
  0,				  /* tp_traverse */
  0,				  /* tp_clear */
  0,				  /* tp_richcompare */
  0,				  /* tp_weaklistoffset */
  0,				  /* tp_iter */
  0,				  /* tp_iternext */
  0,				  /* tp_methods */
  0,				  /* tp_members */
  0,				  /* tp_getset */
  0,				  /* tp_base */
  0,				  /* tp_dict */
  0,				  /* tp_descr_get */
  0,				  /* tp_descr_set */
  0,				  /* tp_dictoffset */
  0,				  /* tp_init */
  0,				  /* tp_alloc */
  PyType_GenericNew		  /* tp_new */
};
