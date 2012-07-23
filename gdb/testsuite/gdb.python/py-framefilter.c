/* This testcase is part of GDB, the GNU debugger.

   Copyright 2008-2012 Free Software Foundation, Inc.

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


void funca(void);
int count = 0;

void end_func (void)
{
  const char *str = "The End";
  const char *st2 = "Is Near";
  int b = 12;
  short c = 5;

  return; /* Backtrace end breakpoint */
}

void funcb(int j)
{
  struct foo
  {
    int a;
    int b;
  };

  struct foo bar;

  bar.a = 42;
  bar.b = 84;

  funca();
  return;
}

void funca(void)
{
  if (count < 10)
    {
      count++;
      funcb(count);
    }

  end_func();
  return;
}


void func1(void)
{
  funca();
  return;
}

int func2(void)
{
  func1();
  return 1;
}

void func3(int i)
{
  func2();

  return;
}

int func4(int j)
{
  func3(j);

  return 2;
}

int func5(int f, int d)
{
  int i = 0;
  char *random = "random";
  i=i+f;

  func4(i);
  return i;
}

main()
{
  func5(3,5);
}
