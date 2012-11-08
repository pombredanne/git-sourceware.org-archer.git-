#                                                       -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

# Copyright 2002-2013 Free Software Foundation, Inc.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

AC_PREREQ(2.59)
AC_INIT(gdb.base)

AC_CANONICAL_BUILD
AC_CANONICAL_HOST
AC_CANONICAL_TARGET

ACX_NONCANONICAL_TARGET

# Check for the 'make' the user wants to use.
AC_CHECK_PROGS(MAKE, make)
MAKE_IS_GNU=
case "`$MAKE --version 2>&1 | sed 1q`" in
  *GNU*)
    MAKE_IS_GNU=yes
    ;;
esac
AM_CONDITIONAL(GMAKE, test "$MAKE_IS_GNU" = yes)
AC_PROG_MAKE_SET

# Enable shared libraries.
AC_ARG_ENABLE(shared,
[  --enable-shared         build shared libraries [default=yes]],,
  enable_shared=yes)

# If we have shared libraries, try to set RPATH_ENVVAR reasonably,
# such that we can find the shared libraries in the build tree.
if test $enable_shared = no; then
  # The variable `RPATH_ENVVAR' itself is not likely to be used on any
  # platform.
  RPATH_ENVVAR=RPATH_ENVVAR
else
  # The variable `LD_LIBRARY_PATH' is used on most platforms.
  RPATH_ENVVAR=LD_LIBRARY_PATH
  # The following exceptions are taken from Libtool 1.4.3.
  case $host_os in
    aix*)
      if test $host_cpu != ia64; then
        RPATH_ENVVAR=LIBPATH
      fi ;;
    darwin* | rhapsody*)
      RPATH_ENVVAR=DYLD_LIBRARY_PATH ;;
    hpux*)
      RPATH_ENVVAR=SHLIB_PATH ;;
  esac
fi
AC_SUBST(RPATH_ENVVAR)

AC_EXEEXT

AC_OUTPUT([Makefile])
