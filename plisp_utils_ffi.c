/**
commands to build .so file:

  gcc -c -fPIC plisp_utils_ffi.c -o plisp_utils_ffi.o
  gcc -shared -W1,-soname,libplisp.so -o libplisp.so plisp_utils_ffi.o

 **/

#include <stdlib.h>


float plisp_random()
{
  return (1.0 * rand()) / RAND_MAX ;
}
