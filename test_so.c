#include <stdio.h>
#include <stdlib.h>

/**
commands to build .so file:

  gcc -c -fPIC test_so.c -o test_so.o
  gcc -shared -W1,-soname,libtest.so -o libtest.so test_so.o

 **/

int fn_ret_int(int i, float f, char c, char *s)
{
  printf("entering funtion1\n");
  printf("%d\n",i);
  printf("%f\n",f);
  printf("%c\n",c);
  printf("%s\n",s);
  printf("exiting function1\n");
  return i * i;
}

float fn_ret_float(int i, float f, char c, char *s)
{
  printf("%d\n",i);
  printf("%f\n",f);
  printf("%c\n",c);
  printf("%s\n",s);
  printf("%f\n", f + f);
  return f;
}

char fn_ret_char(int i, float f, char c, char *s)
{
  printf("%d\n",i);
  printf("%f\n",f);
  printf("%c\n",c);
  printf("%s\n",s);
  return c;
}

char *fn_ret_char_ptr(int i, float f, char c, char *s)
{
  printf("%d\n",i);
  printf("%f\n",f);
  printf("%c\n",c);
  printf("%s\n",s);
  
  char *ret = (char *)malloc(10 * sizeof(char));

  memset(ret, 'a', 10);
  ret[10] = (char)NULL;

  return ret;
}

int fn_arg_int_ptr(int *i)
{
  printf("passed value is %d\n", *i);
  *i = 100;
  return 0;
}

int fn_arg_float_ptr(float *f)
{
  printf("passed value is %f\n", *f);
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

void function_ret_void(int i, float f, char c, char *s)
{
  printf("%d\n",i);
  printf("%f\n",f);
  printf("%c\n",c);
  printf("%s\n",s);

  return;
}
