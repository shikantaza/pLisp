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
#include <limits.h>
#include <errno.h>
#include <string.h>
#include <math.h>

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

int plisp_read(int *int_val, 
               float *float_val, 
               char *str_val)
{
  int base = 10;
  char *end_ptr;

  long i_val;
  float f_val;

  int maybe_float = 0;

  char inp[500];

  fscanf(stdin, "%s", inp);

  int i;
  for(i=0; i<strlen(inp); i++)
  {
    if(inp[i] == '.')
    {
      maybe_float = 1;
      break;
    }
  }

  if(maybe_float)
  {
    errno = 0;
  
    f_val = strtof(inp, &end_ptr);

    if((errno == ERANGE) || (errno != 0 && f_val == 0))
      return -1;

    if(errno == 0 && (end_ptr != inp))
    {
      *float_val = f_val;
      return 2;
    }
  }

  errno = 0;
  
  i_val = strtol(inp, &end_ptr, base);

  if((errno == ERANGE && (i_val == LONG_MAX || i_val == LONG_MIN)) ||
     (errno != 0 && i_val == 0) ||
     (errno == 0 && (i_val < INT_MIN || i_val > INT_MAX)))
    return -1;

  if(errno == 0 && (end_ptr != inp))
  {
    *int_val = i_val;
    return 1;
  }
     
  strcpy(str_val, inp);
  return 3;

}
