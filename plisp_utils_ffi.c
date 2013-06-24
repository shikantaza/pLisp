/**
commands to build .so file:

  gcc -c -fPIC plisp_utils_ffi.c -o plisp_utils_ffi.o
  gcc -shared -W1,-soname,libplisp.so -o libplisp.so plisp_utils_ffi.o

 **/

#include <stdlib.h>
#include <stdarg.h>

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

