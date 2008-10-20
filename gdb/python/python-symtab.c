/* Python interface to symbol tables.

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
#include "symtab.h"
#include "source.h"
#include "python-internal.h"

typedef struct {
  PyObject_HEAD
  struct symtab *symtab;
} symtab_object;

static PyTypeObject symtab_object_type;

typedef struct {
  PyObject_HEAD
  symtab_object *symtab;
  struct symtab_and_line *sal;
} sal_object;

static PyTypeObject sal_object_type;


static PyObject *
stpy_str (PyObject *self)
{
  int ret;
  char *s;
  PyObject *result;

  ret = asprintf (&s, "symbol table for %s",
		  ((symtab_object *) self)->symtab->filename);
  if (ret < 0)
    Py_RETURN_NONE;

  result = PyString_FromString (s);
  xfree (s);

  return result;
}

/* FIXME: maybe this should be an attribute instead of a method?  */
static PyObject *
stpy_filename (PyObject *self, PyObject *args)
{
  symtab_object *self_symtab = (symtab_object *) self;
  PyObject *str_obj;

  /* FIXME: Can symtab->filename really be NULL?  */
  if (self_symtab->symtab->filename)
    str_obj = PyString_Decode (self_symtab->symtab->filename,
			       strlen (self_symtab->symtab->filename),
			       host_charset (), NULL);
  else
    {
      str_obj = Py_None;
      Py_INCREF (Py_None);
    }

  return str_obj;
}

static PyObject *
stpy_to_fullname (PyObject *self, PyObject *args)
{
  char *fullname;

  fullname = symtab_to_fullname (((symtab_object *) self)->symtab);
  if (fullname)
    return PyString_Decode (fullname, strlen (fullname), host_charset (), NULL);

  Py_RETURN_NONE;
}

static PyObject *
salpy_str (PyObject *self)
{
  int ret;
  char *s, *filename;
  sal_object *sal_obj;
  PyObject *result;

  sal_obj = (sal_object *) self;
  filename = (sal_obj->symtab == (symtab_object *) Py_None)? "<unknown>" :
					   sal_obj->symtab->symtab->filename;
  ret = asprintf (&s, "symbol and line for %s, line %d", filename,
		  sal_obj->sal->line);
  if (ret < 0)
    Py_RETURN_NONE;

  result = PyString_FromString (s);
  xfree (s);

  return result;
}

static PyObject *
salpy_pc (PyObject *self, PyObject *args)
{
  return PyLong_FromUnsignedLongLong (((sal_object *) self)->sal->pc);
}

static PyObject *
salpy_line (PyObject *self, PyObject *args)
{
  return PyLong_FromUnsignedLongLong (((sal_object *) self)->sal->line);
}

static PyObject *
salpy_getsymtab (PyObject *self, void *closure)
{
  sal_object *self_sal = (sal_object *) self;

  Py_INCREF (self_sal->symtab);

  return (PyObject *) self_sal->symtab;
}

static int
salpy_setsymtab (PyObject *self, PyObject *value, void *closure)
{
  PyErr_SetString (PyExc_TypeError, "The symtab attribute can't be modified.");

  return -1;
}

static void 
salpy_dealloc (PyObject *self)
{
  sal_object *self_sal = (sal_object *) self;

  Py_DECREF (self_sal->symtab);
  xfree (self_sal->sal);
  self_sal->ob_type->tp_free (self);
}

PyObject *
symtab_and_line_to_sal_object (struct symtab_and_line sal)
{
  sal_object *sal_obj;
  symtab_object *symtab_obj;

  sal_obj = PyObject_New (sal_object, &sal_object_type);
  if (sal_obj == NULL)
    {
      PyErr_SetString (PyExc_MemoryError,
		       "Could not allocate Symtab_and_line object.");
      return NULL;
    }

  if (sal.symtab)
    {
      symtab_obj = (symtab_object *) symtab_to_symtab_object (sal.symtab);
      if (symtab_obj == NULL)
	{
	  Py_DECREF (sal_obj);
	  return NULL;
	}

      symtab_obj->symtab = sal.symtab;
    }
  else
    {
      symtab_obj = (symtab_object *) Py_None;
      Py_INCREF (Py_None);
    }

  sal_obj->sal = (struct symtab_and_line *)
				    xmalloc (sizeof (struct symtab_and_line));
  *(sal_obj->sal) = sal;
  sal_obj->symtab = symtab_obj;

  return (PyObject *) sal_obj;
}

PyObject *
symtab_to_symtab_object (struct symtab *symtab)
{
  symtab_object *symtab_obj;
  
  symtab_obj = PyObject_New (symtab_object, &symtab_object_type);
  if (symtab_obj == NULL)
    {
      PyErr_SetString (PyExc_MemoryError,
	  "Could not allocate Symtab object.");

      return NULL;
    }

  symtab_obj->symtab = symtab;

  return (PyObject *) symtab_obj;
}

void
gdbpy_initialize_symtabs (void)
{
  symtab_object_type.tp_new = PyType_GenericNew;
  if (PyType_Ready (&symtab_object_type) < 0)
    return;

  sal_object_type.tp_new = PyType_GenericNew;
  if (PyType_Ready (&sal_object_type) < 0)
    return;

  Py_INCREF (&symtab_object_type);
  PyModule_AddObject (gdb_module, "Symtab", (PyObject *) &symtab_object_type);

  Py_INCREF (&sal_object_type);
  PyModule_AddObject (gdb_module, "Symtab_and_line",
		      (PyObject *) &sal_object_type);
}



static PyMethodDef symtab_object_methods[] = {
  { "get_filename", stpy_filename, METH_NOARGS,
    "Return the symtab's source filename." },
  { "to_fullname", stpy_to_fullname, METH_NOARGS,
    "Return the symtab's full source filename." },
  {NULL}  /* Sentinel */
};

static PyTypeObject symtab_object_type = {
  PyObject_HEAD_INIT (NULL)
  0,				  /*ob_size*/
  "gdb.Symtab",			  /*tp_name*/
  sizeof (symtab_object),	  /*tp_basicsize*/
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
  stpy_str,			  /*tp_str*/
  0,				  /*tp_getattro*/
  0,				  /*tp_setattro*/
  0,				  /*tp_as_buffer*/
  Py_TPFLAGS_DEFAULT,		  /*tp_flags*/
  "GDB symtab object",		  /* tp_doc */
  0,				  /* tp_traverse */
  0,				  /* tp_clear */
  0,				  /* tp_richcompare */
  0,				  /* tp_weaklistoffset */
  0,				  /* tp_iter */
  0,				  /* tp_iternext */
  symtab_object_methods		  /* tp_methods */
};

static PyGetSetDef sal_object_getseters[] = {
  { "symtab", salpy_getsymtab, salpy_setsymtab, "Symtab object.", NULL },
  {NULL}  /* Sentinel */
};

static PyMethodDef sal_object_methods[] = {
  { "get_pc", salpy_pc, METH_NOARGS,
    "Return the symtab_and_line's pc." },
  { "get_line", salpy_line, METH_NOARGS,
    "Return the symtab_and_line's line." },
  {NULL}  /* Sentinel */
};

static PyTypeObject sal_object_type = {
  PyObject_HEAD_INIT (NULL)
  0,				  /*ob_size*/
  "gdb.Symtab_and_line",	  /*tp_name*/
  sizeof (sal_object),		  /*tp_basicsize*/
  0,				  /*tp_itemsize*/
  salpy_dealloc,		  /*tp_dealloc*/
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
  salpy_str,			  /*tp_str*/
  0,				  /*tp_getattro*/
  0,				  /*tp_setattro*/
  0,				  /*tp_as_buffer*/
  Py_TPFLAGS_DEFAULT,		  /*tp_flags*/
  "GDB symtab_and_line object",	  /* tp_doc */
  0,				  /* tp_traverse */
  0,				  /* tp_clear */
  0,				  /* tp_richcompare */
  0,				  /* tp_weaklistoffset */
  0,				  /* tp_iter */
  0,				  /* tp_iternext */
  sal_object_methods,		  /* tp_methods */
  0,				  /* tp_members */
  sal_object_getseters		  /* tp_getset */
};
