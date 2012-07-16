/* Copyright 2012 Free Software Foundation, Inc.

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

#define _GNU_SOURCE
#include <dlfcn.h>

void
stop ()
{
}

int
main ()
{
  void *handle1, *handle2, *handle3;
  void (*func)(int);

  handle1 = dlmopen (LM_ID_NEWLM, SHLIB_NAME, RTLD_LAZY);
  stop ();

  func = (void (*)(int)) dlsym (handle1, "foo");
  func (1);

  handle2 = dlmopen (LM_ID_NEWLM, SHLIB_NAME, RTLD_LAZY);
  stop ();

  func = (void (*)(int)) dlsym (handle2, "foo");
  func (2);

  handle3 = dlopen (SHLIB_NAME, RTLD_LAZY);
  stop ();

  func = (void (*)(int)) dlsym (handle3, "foo");
  func (3);

  dlclose (handle1);
  stop ();

  dlclose (handle2);
  stop ();

  dlclose (handle3);
  stop ();

  return 0;
}
