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

#include <stdio.h>
#include <stdarg.h>
#include <assert.h>
#include <stdint.h>

#include "plisp.h"

extern OBJECT_PTR NIL;
extern OBJECT_PTR TRUE;

extern char **strings;

extern BOOLEAN console_mode, single_expression_mode, pipe_mode;

extern unsigned int POINTER_MASK;

OBJECT_PTR quote(OBJECT_PTR count, OBJECT_PTR exp)
{
  return exp;
}

OBJECT_PTR primitive_add(OBJECT_PTR count1, ...)
{
  va_list ap;
  OBJECT_PTR arg;
  int i;
  float sum = 0;

  BOOLEAN is_float = false;

  unsigned int count = get_int_value(count1);

  if(count < 2)
  {
    raise_error("ADD requires at least two arguments");
    return NIL;
  }

  va_start(ap, count1);

  for(i=0; i<count; i++)
  {
    arg = (OBJECT_PTR)va_arg(ap, int);

    if(IS_INTEGER_OBJECT(arg))
    {
      sum += get_int_value(arg);
    }
    else if(IS_FLOAT_OBJECT(arg))
    {
      is_float = true;
      sum += get_float_value(arg);
    }
    else
    {
      raise_error("Arguments to ADD should be integers or floats");
      return NIL;
    }
  }

  va_end(ap);

  return is_float ? convert_float_to_object(sum) : convert_int_to_object(sum);  
}

OBJECT_PTR primitive_sub(OBJECT_PTR count1, ...)
{
  va_list ap;
  OBJECT_PTR arg;
  int i;
  int int_res;
  float float_res;

  BOOLEAN is_float = false;

  unsigned int count = get_int_value(count1);

  if(count < 2)
  {
    raise_error("'-' requires at least two arguments");
    return NIL;
  }

  va_start(ap, count1);

  OBJECT_PTR first_arg = (OBJECT_PTR)va_arg(ap, int);

  if(IS_FLOAT_OBJECT(first_arg))
  {
    float_res = get_float_value(first_arg);
    is_float = true;
  }
  else if(IS_INTEGER_OBJECT(first_arg))
    int_res = get_int_value(first_arg);
  else
  {
    raise_error("Invalid argument for '-' operator");
    return NIL;
  }

  for(i=1; i < count; i++)
  {
    OBJECT_PTR arg = (OBJECT_PTR)va_arg(ap, int);

    if(IS_INTEGER_OBJECT(arg))
    {
      int ival = get_int_value(arg);
      if(!is_float)
      {
        int_res  -= ival;
        float_res -= ival;
      }
      else
        float_res -= ival;
    }
    else if(IS_FLOAT_OBJECT(arg))
    {
      if(!is_float)
        float_res = int_res;
      is_float = true;
      float_res -= get_float_value(arg);
    }
    else
    {
      raise_error("Invalid argument for '-' operator");
      return NIL;
    }    
  }

  return is_float ? convert_float_to_object(float_res) : convert_int_to_object(int_res);
}

OBJECT_PTR primitive_lt(OBJECT_PTR v1, OBJECT_PTR v2)
{
  float val1, val2;

  if((!(IS_INTEGER_OBJECT(v1)) && !(IS_FLOAT_OBJECT(v1))) ||
     (!(IS_INTEGER_OBJECT(v2)) && !(IS_FLOAT_OBJECT(v2))))

  {
    raise_error("Arguments to < should be integers or floats");
    return NIL;
  }

  if(IS_FLOAT_OBJECT(v1))
    val1 = get_float_value(v1);
  else
    val1 = get_int_value(v1);

  if(IS_FLOAT_OBJECT(v2))
    val2 = get_float_value(v2);
  else
    val2 = get_int_value(v2);
	    
  return (val1 < val2) ? TRUE : NIL;
}

OBJECT_PTR primitive_if(OBJECT_PTR test, OBJECT_PTR then, OBJECT_PTR else1)
{
  if(test != NIL)
    return then;
  else return else1;
}

OBJECT_PTR primitive_error(OBJECT_PTR error_obj)
{
  raise_error(is_string_object(error_obj) ? get_string(error_obj) : strings[(int)error_obj >> OBJECT_SHIFT]);
  return NIL;
}

OBJECT_PTR primitive_print(OBJECT_PTR obj)
{
  print_object(obj);
  if(!console_mode && !single_expression_mode && !pipe_mode)
    print_to_transcript("\n");
  else
    fprintf(stdout, "\n");  

  return obj;
}

OBJECT_PTR primitive_setcar(OBJECT_PTR obj, OBJECT_PTR val)
{
  if(!(IS_CONS_OBJECT(obj)))
  {
    raise_error("First argument to SETCAR should be a CONS object");
    return NIL;
  }

  set_heap(obj & POINTER_MASK, 0, val);

  return val;
}

OBJECT_PTR primitive_list(OBJECT_PTR count1, ...)
{
  va_list ap;
  OBJECT_PTR ret;
  int i;

  unsigned int count = get_int_value(count1);

  if(!count)
    return NIL;

  va_start(ap, count1);

  ret = cons((OBJECT_PTR)va_arg(ap, int), NIL);

  for(i=1; i<count; i++)
  {
    uintptr_t ptr = last_cell(ret) & POINTER_MASK;
    set_heap(ptr, 1, cons((OBJECT_PTR)va_arg(ap, int), NIL));
  }

  va_end(ap);

  return ret;
}
