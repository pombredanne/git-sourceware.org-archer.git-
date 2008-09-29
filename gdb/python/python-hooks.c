/* Notifications from gdb to Python

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
#include "cli/cli-decode.h"
#include "charset.h"
#include "python.h"
#include "python-internal.h"
#include "observer.h"

PyObject *
gdbpy_get_hook_function (const char *name)
{
  PyObject *hooks;
  PyObject *result;

  if (! PyObject_HasAttrString (gdb_module, "hooks"))
    return NULL;
  hooks = PyObject_GetAttrString (gdb_module, "hooks");
  if (! hooks)
    return NULL;
  /* The cast is because the Python function doesn't declare const argument.
     This is a problem in Python version 2.4, but not in 2.5.  */
  if (! PyObject_HasAttrString (hooks, (char *) name))
    {
      Py_DECREF (hooks);
      return NULL;
    }
  /* The cast is because the Python function doesn't declare const argument.
     This is a problem in Python version 2.4, but not in 2.5.  */
  result = PyObject_GetAttrString (hooks, (char *) name);
  Py_DECREF (hooks);
  return result;
}
