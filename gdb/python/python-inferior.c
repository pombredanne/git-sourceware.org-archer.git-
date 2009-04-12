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
