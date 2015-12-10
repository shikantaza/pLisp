/**
  Copyright 2011-2015 Rajesh Jayaprakash <rajesh.jayaprakash@gmail.com>

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
#include <stdint.h>
#include <fcntl.h>

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

int alloc_memory_int(int size)
{
  return (int)malloc(size * sizeof(int));
}

int alloc_memory_float(int size)
{
  return (int)malloc(size * sizeof(float));
}

int alloc_memory_char(int size)
{
  return (int)malloc(size * sizeof(char));
}

void set_memory_ref_int(void *ptr, int idx, int val)
{
  int *int_ptr = (int *)ptr;
  int_ptr[idx] = val;
}

void set_memory_ref_float(void *ptr, int idx, float val)
{
  float *float_ptr = (float *)ptr;
  float_ptr[idx] = val;
}

void set_memory_ref_char(void *ptr, int idx, char val)
{
  char *char_ptr = (char *)ptr;
  char_ptr[idx] = val;
}

int get_memory_ref_int(void *ptr, int idx)
{
  int *int_ptr = (int *)ptr;
  return int_ptr[idx];
}

float get_memory_ref_float(void *ptr, int idx)
{
  float *float_ptr = (float *)ptr;
  return float_ptr[idx];
}

char get_memory_ref_char(void *ptr, int idx)
{
  char *char_ptr = (char *)ptr;
  return char_ptr[idx];
}

void print_memory_int(void *ptr, int size)
{
  int *int_ptr = (int *)ptr;
  int i;
  for(i=0; i<size; i++)
    printf("%d\n", int_ptr[i]);
}

void print_memory_float(void *ptr, int size)
{
  float *float_ptr = (float *)ptr;
  int i;
  for(i=0; i<size; i++)
    printf("%f\n", float_ptr[i]);
}

void print_memory_char(void *ptr, int size)
{
  char *char_ptr = (char *)ptr;
  int i;
  for(i=0; i<size; i++)
    printf("%c\n", char_ptr[i]);
}

void free_memory(void *ptr)
{
  free(ptr);
}

int open_file(char *file_name, char *mode)
{
  if(!file_name || !mode)
    return -1;

  /* FILE *fp = fopen(file_name, mode); */

  /* if(!fp) */
  /*   return -1; */

  /* return (uintptr_t)fp; */
  int fd;
  mode_t mode1 = S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH;
  if(!strcmp(mode, "r"))
    fd = open(file_name, O_RDONLY);
  else if(!strcmp(mode, "w"))
    fd = open(file_name, O_WRONLY | O_CREAT, mode1);
  else if(!strcmp(mode, "a"))
    fd = open(file_name, O_APPEND | O_CREAT, mode1);
  else
    return -1;

  return (uintptr_t)fd;
}

void close_file(int fd)
{
  //fclose((FILE*)fp);
  close(fd);
}
