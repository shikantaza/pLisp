/**
  Copyright 2011-2021 Rajesh Jayaprakash <rajesh.jayaprakash@pm.me>

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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

/**
commands to build .so file:

  gcc -c -fPIC test_so.c -o test_so.o
  gcc -shared -W1,-soname,libtest.so -o libtest.so test_so.o

 **/

int fn_ret_int(int i, double f, char c, char *s)
{
  printf("entering funtion1\n");
  printf("%d\n",i);
  printf("%lf\n",f);
  printf("%c\n",c);
  printf("%s\n",s);
  printf("exiting function1\n");
  return i * i;
}

double fn_ret_float(int i, double f, char c, char *s)
{
  printf("%d\n",i);
  printf("%lf\n",f);
  printf("%c\n",c);
  printf("%s\n",s);
  printf("%lf\n", f + f);
  printf("exiting fn_ret_float\n");
  return f+f;
}

char fn_ret_char(int i, double f, char c, char *s)
{
  printf("%d\n",i);
  printf("%lf\n",f);
  printf("%c\n",c);
  printf("%s\n",s);
  return c;
}

char *fn_ret_char_ptr(int i, double f, char c, char *s)
{
  printf("%d\n",i);
  printf("%lf\n",f);
  printf("%c\n",c);
  printf("%s\n",s);
  
  char *ret = (char *)malloc(10 * sizeof(char));

  memset(ret, 'a', 10);
  //ret[10] = (char)NULL;
  ret[10] = '\0';

  return ret;
}

int fn_arg_int_ptr(int *i)
{
  printf("passed value is %d\n", *i);
  *i = 100;
  return 0;
}

//int fn_arg_float_ptr(float *f)
int fn_arg_float_ptr(double *f)
{
  printf("passed value is %lf\n", *f);
  *f = 100.97;
  return 0;
}

int fn_arg_char_ptr(char *str)
{
  printf("passed value is %s\n", str);
  char *ptr = NULL;

  for(ptr=str; *ptr; ptr++)
    *ptr=toupper(*ptr);

  return 0;
}

void function_ret_void(int i, double f, char c, char *s)
{
  printf("%d\n",i);
  printf("%lf\n",f);
  printf("%c\n",c);
  printf("%s\n",s);

  return;
}
