/* Python interface to types.

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
#include "value.h"
#include "exceptions.h"
#include "python-internal.h"
#include "charset.h"
#include "gdbtypes.h"
#include "cp-support.h"
#include "demangle.h"

typedef struct
{
  PyObject_HEAD
  struct type *type;
} type_object;

static void typy_dealloc (PyObject *);

static PyObject *typy_str (PyObject *);
static PyObject *typy_pointer (PyObject *self, PyObject *args);
static PyObject *typy_template_argument (PyObject *self, PyObject *args);

static PyMethodDef type_object_methods[] =
{
  { "pointer", typy_pointer, METH_NOARGS, "Return pointer to this type" },
  { "template_argument", typy_template_argument, METH_VARARGS,
    "Return a single template argument type" },
  { NULL }
};

static PyTypeObject type_object_type =
{
  PyObject_HEAD_INIT (NULL)
  0,				  /*ob_size*/
  "gdb.Type",			  /*tp_name*/
  sizeof (type_object),		  /*tp_basicsize*/
  0,				  /*tp_itemsize*/
  typy_dealloc,			  /*tp_dealloc*/
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
  typy_str,			  /*tp_str*/
  0,				  /*tp_getattro*/
  0,				  /*tp_setattro*/
  0,				  /*tp_as_buffer*/
  Py_TPFLAGS_DEFAULT | Py_TPFLAGS_HAVE_ITER,  /*tp_flags*/
  "GDB type object",		  /* tp_doc */
  0,				  /* tp_traverse */
  0,				  /* tp_clear */
  0,				  /* tp_richcompare */
  0,				  /* tp_weaklistoffset */
  0,				  /* tp_iter */
  0,				  /* tp_iternext */
  type_object_methods		  /* tp_methods */
};

/* Return a Type object which represents a pointer to SELF.  */
static PyObject *
typy_pointer (PyObject *self, PyObject *args)
{
  struct type *type = ((type_object *) self)->type;
  volatile struct gdb_exception except;

  TRY_CATCH (except, RETURN_MASK_ALL)
    {
      type = lookup_pointer_type (type);
    }
  GDB_PY_HANDLE_EXCEPTION (except);

  return type_to_type_object (type);
}

static struct type *
typy_lookup_typename (char *type_name, struct block *block)
{
  struct type *type = NULL;
  volatile struct gdb_exception except;
  TRY_CATCH (except, RETURN_MASK_ALL)
    {
      type = lookup_typename (type_name, block, 1);
    }
  if (except.reason < 0)
    {
      PyErr_Format (except.reason == RETURN_QUIT
		    ? PyExc_KeyboardInterrupt : PyExc_RuntimeError,
		    "%s", except.message);
      return NULL;
    }

  return type;
}

static PyObject *
typy_template_argument (PyObject *self, PyObject *args)
{
  int i, argno;
  struct type *type = ((type_object *) self)->type;
  struct demangle_component *demangled;
  const char *err;
  char *type_name;
  struct type *argtype;
  struct block *block = NULL;
  PyObject *block_obj = NULL;

  if (! PyArg_ParseTuple (args, "i|O", &argno, &block_obj))
    return NULL;

  if (block_obj)
    {
      block = block_object_to_block (block_obj);
      if (! block)
	{
	  PyErr_SetString (PyExc_RuntimeError,
			   "second argument must be block");
	  return NULL;
	}
    }

  /* Note -- this is not thread-safe.  */
  demangled = cp_demangled_name_to_comp (TYPE_NAME (type), &err);
  if (! demangled)
    {
      PyErr_SetString (PyExc_RuntimeError, err);
      return NULL;
    }

  /* Strip off component names.  */
  while (demangled->type == DEMANGLE_COMPONENT_QUAL_NAME
	 || demangled->type == DEMANGLE_COMPONENT_LOCAL_NAME)
    demangled = demangled->u.s_binary.right;

  if (demangled->type != DEMANGLE_COMPONENT_TEMPLATE)
    {
      PyErr_SetString (PyExc_RuntimeError, "type is not a template");
      return NULL;
    }

  /* Skip from the template to the arguments.  */
  demangled = demangled->u.s_binary.right;

  for (i = 0; demangled && i < argno; ++i)
    demangled = demangled->u.s_binary.right;

  if (! demangled)
    {
      PyErr_Format (PyExc_RuntimeError, "no argument %d in template",
		    argno);
      return NULL;
    }

  type_name = cp_comp_to_string (demangled->u.s_binary.left, 10);

  argtype = typy_lookup_typename (type_name, block);
  if (! argtype)
    {
      PyErr_Format (PyExc_RuntimeError, "no such type named %s",
		    type_name);
      xfree (type_name);
      return NULL;
    }
  xfree (type_name);

  return type_to_type_object (argtype);
}

static PyObject *
typy_str (PyObject *self)
{
  volatile struct gdb_exception except;
  char *thetype = NULL;
  PyObject *result;

  TRY_CATCH (except, RETURN_MASK_ALL)
    {
      struct cleanup *old_chain;
      struct ui_file *stb;
      long length;

      stb = mem_fileopen ();
      old_chain = make_cleanup_ui_file_delete (stb);

      type_print (type_object_to_type (self), "", stb, -1);

      thetype = ui_file_xstrdup (stb, &length);
      do_cleanups (old_chain);
    }
  if (except.reason < 0)
    {
      xfree (thetype);
      GDB_PY_HANDLE_EXCEPTION (except);
    }

  result = PyUnicode_Decode (thetype, strlen (thetype), host_charset (), NULL);
  xfree (thetype);

  return result;
}



static PyObject *
typy_new (PyTypeObject *subtype, PyObject *args, PyObject *kwargs)
{
  char *type_name = NULL;
  struct type *type = NULL;
  type_object *result;
  PyObject *block_obj = NULL;
  struct block *block = NULL;

  if (! PyArg_ParseTuple (args, "|sO", &type_name, &block_obj))
    return NULL;

  if (block_obj)
    {
      block = block_object_to_block (block_obj);
      if (! block)
	{
	  PyErr_SetString (PyExc_RuntimeError,
			   "second argument must be block");
	  return NULL;
	}
    }

  if (type_name)
    {
      type = typy_lookup_typename (type_name, block);
      if (! type)
	{
	  PyErr_Format (PyExc_RuntimeError, "no such type named %s",
			type_name);
	  return NULL;
	}
    }

  result = (type_object *) subtype->tp_alloc (subtype, 1);
  if (! result)
    return NULL;

  result->type = type;

  return (PyObject *) result;
}

static void
typy_dealloc (PyObject *obj)
{
  type_object *type = (type_object *) obj;
  /* FIXME */

  type->ob_type->tp_free (type);
}

PyObject *
type_to_type_object (struct type *type)
{
  type_object *type_obj;

  type_obj = PyObject_New (type_object, &type_object_type);
  if (type_obj)
    {
      type_obj->type = type;
      /* FIXME */
    }

  return (PyObject *) type_obj;
}

struct type *
type_object_to_type (PyObject *obj)
{
  if (! PyObject_TypeCheck (obj, &type_object_type))
    return NULL;
  return ((type_object *) obj)->type;
}



void
gdbpy_initialize_types (void)
{
  type_object_type.tp_new = typy_new;
  if (PyType_Ready (&type_object_type) < 0)
    return;

  Py_INCREF (&type_object_type);
  PyModule_AddObject (gdb_module, "Type", (PyObject *) &type_object_type);
}
