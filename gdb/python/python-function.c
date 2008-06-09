/* gdb commands implemented in Python

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
#include "gdbcmd.h"
#include "cli/cli-decode.h"
#include "completer.h"
#include "expression.h"

static PyTypeObject fnpy_object_type =
{
  PyObject_HEAD_INIT (NULL)
  0,				  /*ob_size*/
  "gdb.Function",		  /*tp_name*/
  sizeof (PyObject),		  /*tp_basicsize*/
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
  0,				  /*tp_str*/
  0,				  /*tp_getattro*/
  0,				  /*tp_setattro*/
  0,				  /*tp_as_buffer*/
  Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE, /*tp_flags*/
  "GDB function object",	  /* tp_doc */
  0,				  /* tp_traverse */
  0,				  /* tp_clear */
  0,				  /* tp_richcompare */
  0,				  /* tp_weaklistoffset */
  0,				  /* tp_iter */
  0,				  /* tp_iternext */
  0				  /* tp_methods */
};



static PyObject *
convert_values_to_python (int argc, struct value **argv)
{
  int i;
  PyObject *result = PyTuple_New (argc);
  for (i = 0; i < argc; ++i)
    {
      PyObject *elt = value_to_value_object (argv[i]);
      if (! elt)
	{
	  Py_DECREF (result);
	  error ("could not convert value to Python object");
	}
      PyTuple_SetItem (result, i, elt);
    }
  return result;
}

/* Call a Python function object's invoke method.  */

static struct value *
fnpy_call (void *cookie, int argc, struct value **argv)
{
  int i;
  struct value *value = NULL;
  PyObject *result, *callable, *args;
  volatile struct gdb_exception except;

  args = convert_values_to_python (argc, argv);

  callable = PyObject_GetAttrString ((PyObject *) cookie, "invoke");
  if (! callable)
    {
      Py_DECREF (args);
      error ("no method named 'invoke' in object");
    }

  result = PyObject_Call (callable, args, NULL);
  Py_DECREF (callable);
  Py_DECREF (args);

  if (!result)
    return NULL;

  TRY_CATCH (except, RETURN_MASK_ALL)
    {
      value = convert_value_from_python (result);
    }
  if (except.reason < 0)
    {
      Py_DECREF (result);
      gdbpy_print_stack ();
      throw_exception (except);
    }

  Py_DECREF (result);
  
  return value;
}

/* Called when destroying a struct internal_function.  */

static void
fnpy_destroy (void *cookie)
{
  Py_DECREF ((PyObject *) cookie);
}

/* Initializer for a Function object.  It takes one argument, the name
   of the function.  */

static int
fnpy_init (PyObject *self, PyObject *args, PyObject *kwds)
{
  char *name, *docstring = NULL;
  if (! PyArg_ParseTuple (args, "s", &name))
    return -1;
  Py_INCREF (self);

  if (PyObject_HasAttrString (self, "__doc__"))
    {
      PyObject *ds_obj = PyObject_GetAttrString (self, "__doc__");
      if (ds_obj && PyString_Check (ds_obj))
	docstring = PyString_AsString (ds_obj);
    }
  if (! docstring)
    docstring = "This command is not documented.";

  add_internal_function (name, docstring, fnpy_call, self, fnpy_destroy);
  return 0;
}

/* Initialize internal function support.  */

void
gdbpy_initialize_functions (void)
{
  fnpy_object_type.tp_new = PyType_GenericNew;
  fnpy_object_type.tp_init = fnpy_init;
  if (PyType_Ready (&fnpy_object_type) < 0)
    return;
  Py_INCREF (&fnpy_object_type);
  PyModule_AddObject (gdb_module, "Function", (PyObject *) &fnpy_object_type);
}
