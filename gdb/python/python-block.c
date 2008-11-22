/* Python interface to blocks.

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
#include "block.h"
#include "dictionary.h"
#include "symtab.h"
#include "python-internal.h"

typedef struct {
  PyObject_HEAD
  struct block *block;
} block_object;

typedef struct {
  PyObject_HEAD
  struct dictionary *dict;
  struct dict_iterator iter;
  int initialized_p;
} block_syms_iterator_object;

static PyTypeObject block_syms_iterator_object_type;

static PyObject *
blpy_iter (PyObject *self)
{
  block_syms_iterator_object *block_iter_obj;

  block_iter_obj = PyObject_New (block_syms_iterator_object,
			    &block_syms_iterator_object_type);
  if (block_iter_obj == NULL)
    {
      PyErr_SetString (PyExc_MemoryError,
		       "Could not allocate iterator object.");
      return NULL;
    }

  block_iter_obj->dict = BLOCK_DICT (((block_object *) self)->block);
  block_iter_obj->initialized_p = 0;

  return (PyObject *) block_iter_obj;
}

static PyObject *
blpy_itersymbols (PyObject *self, PyObject *args)
{
  return blpy_iter (self);
}

static PyObject *
blpy_get_start (PyObject *self, PyObject *args)
{
  block_object *self_block = (block_object *) self;

  return PyLong_FromUnsignedLongLong (BLOCK_START (self_block->block));
}

static PyObject *
blpy_get_end (PyObject *self, PyObject *args)
{
  block_object *self_block = (block_object *) self;

  return PyLong_FromUnsignedLongLong (BLOCK_END (self_block->block));
}

static PyObject *
blpy_get_function (PyObject *self, PyObject *args)
{
  block_object *self_block = (block_object *) self;
  struct symbol *sym;

  sym = BLOCK_FUNCTION (self_block->block);
  if (sym)
    return symbol_to_symbol_object (sym);

  Py_RETURN_NONE;
}

static PyObject *
blpy_get_superblock (PyObject *self, PyObject *args)
{
  block_object *self_block = (block_object *) self;
  struct block *block;

  block = BLOCK_SUPERBLOCK (self_block->block);
  if (block)
    return block_to_block_object (block);

  Py_RETURN_NONE;
}

PyObject *
block_to_block_object (struct block *block)
{
  block_object *block_obj;

  block_obj = PyObject_New (block_object, &block_object_type);
  if (block_obj == NULL)
    {
      PyErr_SetString (PyExc_MemoryError, "Could not allocate block object.");
      return NULL;
    }

  block_obj->block = block;

  return (PyObject *) block_obj;
}

struct block *
block_object_to_block (PyObject *obj)
{
  if (! PyObject_TypeCheck (obj, &block_object_type))
    return NULL;
  return ((block_object *) obj)->block;
}

static PyObject *
blpy_block_syms_iter (PyObject *self)
{
  return self;
}

static PyObject *
blpy_block_syms_iternext (PyObject *self)
{
  block_syms_iterator_object *iter_obj = (block_syms_iterator_object *) self;
  struct symbol *sym;

  if (!iter_obj->initialized_p)
    {
      sym = dict_iterator_first (iter_obj->dict,  &(iter_obj->iter));
      iter_obj->initialized_p = 1;
    }
  else
    sym = dict_iterator_next (&(iter_obj->iter));

  return (sym == NULL)? NULL : symbol_to_symbol_object (sym);
}

/* Return the innermost lexical block containing the specified pc value,
   or 0 if there is none.  */

PyObject *
gdbpy_get_block_for_pc (PyObject *self, PyObject *args)
{
  unsigned PY_LONG_LONG pc;
  struct block *block;
  PyObject *sym_obj;

  if (!PyArg_ParseTuple (args, "K", &pc))
    return NULL;

  block = block_for_pc (pc);
  if (block)
    return block_to_block_object (block);

  Py_RETURN_NONE;
}

void
gdbpy_initialize_blocks (void)
{
  block_object_type.tp_new = PyType_GenericNew;
  if (PyType_Ready (&block_object_type) < 0)
    return;

  block_syms_iterator_object_type.tp_new = PyType_GenericNew;
  if (PyType_Ready (&block_syms_iterator_object_type) < 0)
    return;

  Py_INCREF (&block_object_type);
  PyModule_AddObject (gdb_module, "Block", (PyObject *) &block_object_type);

  Py_INCREF (&block_syms_iterator_object_type);
  PyModule_AddObject (gdb_module, "BlockIterator",
		      (PyObject *) &block_syms_iterator_object_type);
}



static PyMethodDef block_object_methods[] = {
  { "itersymbols", blpy_itersymbols, METH_NOARGS,
    "Return an iterator to walk through the symbols in the block." },
  { "get_start", blpy_get_start, METH_NOARGS,
    "Return the start address of this block." },
  { "get_end", blpy_get_end, METH_NOARGS,
    "Return the end address of this block." },
  { "get_function", blpy_get_function, METH_NOARGS,
    "Return the symbol that names this block, or None." },
  { "get_superblock", blpy_get_superblock, METH_NOARGS,
    "Return the block containing this block, or None." },
  {NULL}  /* Sentinel */
};

PyTypeObject block_object_type = {
  PyObject_HEAD_INIT (NULL)
  0,				  /*ob_size*/
  "gdb.Block",			  /*tp_name*/
  sizeof (block_object),	  /*tp_basicsize*/
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
  Py_TPFLAGS_DEFAULT | Py_TPFLAGS_HAVE_ITER,  /*tp_flags*/
  "GDB block object",		  /* tp_doc */
  0,				  /* tp_traverse */
  0,				  /* tp_clear */
  0,				  /* tp_richcompare */
  0,				  /* tp_weaklistoffset */
  blpy_iter,			  /* tp_iter */
  0,				  /* tp_iternext */
  block_object_methods		  /* tp_methods */
};

static PyTypeObject block_syms_iterator_object_type = {
  PyObject_HEAD_INIT (NULL)
  0,				  /*ob_size*/
  "gdb.BlockIterator",		  /*tp_name*/
  sizeof (block_syms_iterator_object),	      /*tp_basicsize*/
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
  Py_TPFLAGS_DEFAULT | Py_TPFLAGS_HAVE_ITER,  /*tp_flags*/
  "GDB block syms iterator object",	      /* tp_doc */
  0,				  /* tp_traverse */
  0,				  /* tp_clear */
  0,				  /* tp_richcompare */
  0,				  /* tp_weaklistoffset */
  blpy_block_syms_iter,		  /* tp_iter */
  blpy_block_syms_iternext,	  /* tp_iternext */
  0				  /* tp_methods */
};
