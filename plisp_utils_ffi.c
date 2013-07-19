/**
  Copyright 2011-2013 Rajesh Jayaprakash <rajesh.jayaprakash@gmail.com>

  This file is part of pLisp.

  pLisp is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  pLisp is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with pLisp.  If not, see <http://www.gnu.org/licenses/>.
**/

/**
commands to build .so file:

  gcc -c -fPIC plisp_utils_ffi.c -o plisp_utils_ffi.o
  gcc -shared -W1,-soname,libplisp.so -o libplisp.so plisp_utils_ffi.o

 **/

#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>

float plisp_random()
{
  int i = rand();
  float ret = (i*1.0)/RAND_MAX;
  return ret;
}

void format(const char *fmt, ...)
{
  va_list args;
  va_start(args, fmt);
  vprintf(fmt, args);
  va_end(args);
}

//needed because we're not able
//to pass \n in the format string for format()
void print_line()
{
  printf("\n");
}

int plisp_floor(float x)
{
  if(x >= 0)
    return (int)x;
  else
    return (-1) * (int)round(abs(x) + 0.5 );
}

void plisp_system(char *cmd)
{
  system(cmd);
}
