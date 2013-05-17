/**
commands to build .so file:

  gcc -c -fPIC plisp_utils_ffi.c -o plisp_utils_ffi.o
  gcc -shared -W1,-soname,libplisp.so -o libplisp.so plisp_utils_ffi.o

 **/

#include <stdlib.h>

float plisp_random()
{
  int i = rand();
  float ret = (i*1.0)/RAND_MAX;
  return ret;
}
