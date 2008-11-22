/* Python interface to symbols.

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
#include "symtab.h"
#include "python-internal.h"

typedef struct {
  PyObject_HEAD
  struct symbol *symbol;
} symbol_object;


static PyObject *
sympy_str (PyObject *self)
{
  int ret;
  char *s;
  PyObject *result;

  ret = asprintf (&s, "symbol for %s",
		  SYMBOL_PRINT_NAME (((symbol_object *) self)->symbol));
  if (ret < 0)
    Py_RETURN_NONE;

  result = PyString_FromString (s);
  xfree (s);

  return result;
}

static PyObject *
sympy_get_value (PyObject *self, PyObject *args)
{
  symbol_object *self_sym = (symbol_object *) self;

  switch (SYMBOL_CLASS (self_sym->symbol))
    {
    case LOC_BLOCK:
      return block_to_block_object (SYMBOL_BLOCK_VALUE (self_sym->symbol));
    }

  PyErr_SetString (PyExc_NotImplementedError,
		   "Symbol type not yet supported in Python scripts.");
  return NULL;
}

static PyObject *
sympy_get_symtab (PyObject *self, PyObject *args)
{
  symbol_object *self_sym = (symbol_object *) self;

  return symtab_to_symtab_object (SYMBOL_SYMTAB (self_sym->symbol));
}

static PyObject *
sympy_get_natural_name (PyObject *self, PyObject *args)
{
  symbol_object *self_sym = (symbol_object *) self;

  return PyString_FromString (SYMBOL_NATURAL_NAME (self_sym->symbol));
}

static PyObject *
sympy_get_linkage_name (PyObject *self, PyObject *args)
{
  symbol_object *self_sym = (symbol_object *) self;

  return PyString_FromString (SYMBOL_LINKAGE_NAME (self_sym->symbol));
}

static PyObject *
sympy_get_print_name (PyObject *self, PyObject *args)
{
  symbol_object *self_sym = (symbol_object *) self;

  return PyString_FromString (SYMBOL_PRINT_NAME (self_sym->symbol));
}

static PyObject *
sympy_get_class (PyObject *self, PyObject *args)
{
  symbol_object *self_sym = (symbol_object *) self;

  return PyInt_FromLong (SYMBOL_CLASS (self_sym->symbol));
}

static PyObject *
sympy_is_argument (PyObject *self, PyObject *args)
{
  symbol_object *self_sym = (symbol_object *) self;

  return PyBool_FromLong (SYMBOL_IS_ARGUMENT (self_sym->symbol));
}

static PyObject *
sympy_is_constant (PyObject *self, PyObject *args)
{
  symbol_object *self_sym = (symbol_object *) self;
  enum address_class class = SYMBOL_CLASS (self_sym->symbol);

  return PyBool_FromLong (class == LOC_CONST || class == LOC_CONST_BYTES);
}

static PyObject *
sympy_is_function (PyObject *self, PyObject *args)
{
  symbol_object *self_sym = (symbol_object *) self;
  enum address_class class = SYMBOL_CLASS (self_sym->symbol);

  return PyBool_FromLong (class == LOC_BLOCK);
}

static PyObject *
sympy_is_variable (PyObject *self, PyObject *args)
{
  symbol_object *self_sym = (symbol_object *) self;
  enum address_class class = SYMBOL_CLASS (self_sym->symbol);

  return PyBool_FromLong (!SYMBOL_IS_ARGUMENT (self_sym->symbol)
      && (class == LOC_LOCAL || class == LOC_REGISTER || class == LOC_STATIC
	  || class == LOC_COMPUTED || class == LOC_OPTIMIZED_OUT));
}

PyObject *
symbol_to_symbol_object (struct symbol *sym)
{
  symbol_object *sym_obj;

  sym_obj = PyObject_New (symbol_object, &symbol_object_type);
  if (sym_obj == NULL)
    {
      PyErr_SetString (PyExc_MemoryError, "Could not allocate symbol object.");
      return NULL;
    }

  sym_obj->symbol = sym;

  return (PyObject *) sym_obj;
}

struct symbol *
symbol_object_to_symbol (PyObject *obj)
{
  if (! PyObject_TypeCheck (obj, &symbol_object_type))
    return NULL;
  return ((symbol_object *) obj)->symbol;
}

/* This function has less arguments than its C counterpart, to simplify the
   Python interface: name, block and domain. The other two arguments are always
   assumed to be set, and a tuple with 2 elements is always returned. The first
   is the symbol object or None, the second is a boolean with the value of
   is_a_field_of_this.  */
PyObject *gdbpy_lookup_symbol (PyObject *self, PyObject *args)
{
  int domain, is_a_field_of_this = 0;
  const char *name;
  struct symbol *symbol;
  PyObject *block_obj, *ret_tuple, *sym_obj, *bool_obj;
  struct block *block;

  if (! PyArg_ParseTuple (args, "sO!i", &name, &block_object_type, &block_obj,
			  &domain))
    return NULL;

  block = block_object_to_block (block_obj);
  if (! block)
    {
      PyErr_SetString (PyExc_RuntimeError, "second argument must be block");
      return NULL;
    }

  symbol = lookup_symbol (name, block, domain, &is_a_field_of_this);

  ret_tuple = PyTuple_New (2);
  if (!ret_tuple)
    {
      PyErr_SetString (PyExc_MemoryError, "Could not allocate tuple object.");
      return NULL;
    }

  if (symbol)
    {
      sym_obj = symbol_to_symbol_object (symbol);
      if (!sym_obj)
	{
	  Py_DECREF (ret_tuple);
	  return NULL;
	}
    }
  else
    {
      sym_obj = Py_None;
      Py_INCREF (Py_None);
    }
  PyTuple_SET_ITEM (ret_tuple, 0, sym_obj);

  bool_obj = is_a_field_of_this? Py_True : Py_False;
  Py_INCREF (bool_obj);
  PyTuple_SET_ITEM (ret_tuple, 1, bool_obj);

  return ret_tuple;
}

void
gdbpy_initialize_symbols (void)
{
  symbol_object_type.tp_new = PyType_GenericNew;
  if (PyType_Ready (&symbol_object_type) < 0)
    return;

  /* FIXME: These would probably be best exposed as class attributes of Symbol,
     but I don't know how to do it except by messing with the type's dictionary.
     That seems too messy.  */
  /* FIXME 2: Some of these were removed from GDB since I first wrote this code,
     so it's probably a good idea not to expose them to Python.  */
  PyModule_AddIntConstant (gdb_module, "SYMBOL_LOC_UNDEF", LOC_UNDEF);
  PyModule_AddIntConstant (gdb_module, "SYMBOL_LOC_CONST", LOC_CONST);
  PyModule_AddIntConstant (gdb_module, "SYMBOL_LOC_STATIC", LOC_STATIC);
  PyModule_AddIntConstant (gdb_module, "SYMBOL_LOC_REGISTER", LOC_REGISTER);
  PyModule_AddIntConstant (gdb_module, "SYMBOL_LOC_ARG", LOC_ARG);
  PyModule_AddIntConstant (gdb_module, "SYMBOL_LOC_REF_ARG", LOC_REF_ARG);
  PyModule_AddIntConstant (gdb_module, "SYMBOL_LOC_LOCAL", LOC_LOCAL);
  PyModule_AddIntConstant (gdb_module, "SYMBOL_LOC_TYPEDEF", LOC_TYPEDEF);
  PyModule_AddIntConstant (gdb_module, "SYMBOL_LOC_LABEL", LOC_LABEL);
  PyModule_AddIntConstant (gdb_module, "SYMBOL_LOC_BLOCK", LOC_BLOCK);
  PyModule_AddIntConstant (gdb_module, "SYMBOL_LOC_CONST_BYTES",
			   LOC_CONST_BYTES);
  PyModule_AddIntConstant (gdb_module, "SYMBOL_LOC_UNRESOLVED", LOC_UNRESOLVED);
  PyModule_AddIntConstant (gdb_module, "SYMBOL_LOC_OPTIMIZED_OUT",
			   LOC_OPTIMIZED_OUT);
  PyModule_AddIntConstant (gdb_module, "SYMBOL_LOC_COMPUTED", LOC_COMPUTED);
  PyModule_AddIntConstant (gdb_module, "SYMBOL_LOC_REGPARM_ADDR",
			   LOC_REGPARM_ADDR);
  PyModule_AddIntConstant (gdb_module, "SYMBOL_UNDEF_DOMAIN", UNDEF_DOMAIN);
  PyModule_AddIntConstant (gdb_module, "SYMBOL_VAR_DOMAIN", VAR_DOMAIN);
  PyModule_AddIntConstant (gdb_module, "SYMBOL_STRUCT_DOMAIN", STRUCT_DOMAIN);
  PyModule_AddIntConstant (gdb_module, "SYMBOL_LABEL_DOMAIN", LABEL_DOMAIN);
  PyModule_AddIntConstant (gdb_module, "SYMBOL_VARIABLES_DOMAIN",
			   VARIABLES_DOMAIN);
  PyModule_AddIntConstant (gdb_module, "SYMBOL_FUNCTIONS_DOMAIN",
			   FUNCTIONS_DOMAIN);
  PyModule_AddIntConstant (gdb_module, "SYMBOL_TYPES_DOMAIN", TYPES_DOMAIN);

  Py_INCREF (&symbol_object_type);
  PyModule_AddObject (gdb_module, "Symbol", (PyObject *) &symbol_object_type);
}



static PyMethodDef symbol_object_methods[] = {
  { "get_value", sympy_get_value, METH_NOARGS,
    "Return the value of the symbol." },
  { "get_symtab", sympy_get_symtab, METH_NOARGS,
    "Return the value of the symbol." },
  { "get_natural_name", sympy_get_natural_name, METH_NOARGS,
    "Return the \"natural\" name of the symbol." },
  { "get_linkage_name", sympy_get_linkage_name, METH_NOARGS,
    "Return the name of the symbol as used by the linker." },
  { "get_print_name", sympy_get_print_name, METH_NOARGS,
    "Return the name of the symbol in a form suitable for output." },
  { "get_class", sympy_get_class, METH_NOARGS,
    "Return the class of the symbol." },
  { "is_argument", sympy_is_argument, METH_NOARGS,
    "Return True if symbol is the argument of a function." },
  { "is_constant", sympy_is_constant, METH_NOARGS,
    "Return True if symbol is a function or method." },
  { "is_function", sympy_is_function, METH_NOARGS,
    "Return True if symbol is a function or method." },
  { "is_variable", sympy_is_variable, METH_NOARGS,
    "Return True if symbol is a variable." },
  {NULL}  /* Sentinel */
};

PyTypeObject symbol_object_type = {
  PyObject_HEAD_INIT (NULL)
  0,				  /*ob_size*/
  "gdb.Symbol",			  /*tp_name*/
  sizeof (symbol_object),	  /*tp_basicsize*/
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
  sympy_str,			  /*tp_str*/
  0,				  /*tp_getattro*/
  0,				  /*tp_setattro*/
  0,				  /*tp_as_buffer*/
  Py_TPFLAGS_DEFAULT,		  /*tp_flags*/
  "GDB symbol object",		  /* tp_doc */
  0,				  /* tp_traverse */
  0,				  /* tp_clear */
  0,				  /* tp_richcompare */
  0,				  /* tp_weaklistoffset */
  0,				  /* tp_iter */
  0,				  /* tp_iternext */
  symbol_object_methods		  /* tp_methods */
};
