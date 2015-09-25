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

OBJECT_PTR primitive_setcdr(OBJECT_PTR obj, OBJECT_PTR val)
{
  if(!(IS_CONS_OBJECT(obj)))
  {
    raise_error("First argument to SETCDR should be a CONS object");
    return NIL;
  }

  set_heap(obj & POINTER_MASK, 1, val);

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

OBJECT_PTR primitive_mult(OBJECT_PTR count1, ...)
{
  va_list ap;
  OBJECT_PTR arg;
  int i;
  float prod = 1;

  BOOLEAN is_float = false;

  unsigned int count = get_int_value(count1);

  if(count < 2)
  {
    raise_error("Operator '*' requires at least two arguments");
    return NIL;
  }

  va_start(ap, count1);

  for(i=0; i<count; i++)
  {
    arg = (OBJECT_PTR)va_arg(ap, int);

    if(IS_FLOAT_OBJECT(arg))
    {
      is_float = true;
      prod *= get_float_value(arg);
    }
    else if(IS_INTEGER_OBJECT(arg))
      prod *= get_int_value(arg);
    else
    {
      raise_error("Argument to operator '*' should be a number");
      return 1;
    }
  }

  va_end(ap);

  return is_float ? convert_float_to_object(prod) : convert_int_to_object((int)prod);
}

OBJECT_PTR primitive_equal(OBJECT_PTR obj1, OBJECT_PTR obj2)
{
  BOOLEAN ret = false;

  //TODO: extend this for array objects (any others?)

  if(obj1 == obj2)
    ret = true;
  else
  {
    if(IS_CONS_OBJECT(obj1) && IS_CONS_OBJECT(obj2))
      ret = (equal(car(obj1), car(obj2)) && 
	     equal(cdr(obj1), cdr(obj2)));
    else if(IS_INTEGER_OBJECT(obj1))
    {
      int val = get_int_value(obj1);
      if(IS_INTEGER_OBJECT(obj2))
        ret = (val  == get_int_value(obj2));
      else if(IS_FLOAT_OBJECT(obj2))
        ret = (val == get_float_value(obj2));
    }
    else if(IS_FLOAT_OBJECT(obj1))
    {
      float val = get_float_value(obj1);
      if(IS_INTEGER_OBJECT(obj2))
        ret = (val  == get_int_value(obj2));
      else if(IS_FLOAT_OBJECT(obj2))
        ret = (val == get_float_value(obj2));
    }
    else if(is_string_object(obj1))
    {
      char *str1 = get_string(obj1);
      if(is_string_object(obj2))
        ret = !strcmp(str1, get_string(obj2));
      else if(IS_STRING_LITERAL_OBJECT(obj2))
        ret = !strcmp(str1, strings[(int)obj2 >> OBJECT_SHIFT]);
    }
    else if(IS_STRING_LITERAL_OBJECT(obj1))
    {
      char *str1 = strings[(int)obj1 >> OBJECT_SHIFT];
      if(is_string_object(obj2))
        ret = !strcmp(str1, get_string(obj2));
      else if(IS_STRING_LITERAL_OBJECT(obj2))
        ret = !strcmp(str1, strings[(int)obj2 >> OBJECT_SHIFT]);
    }    
  }
  
  return ret ? TRUE : NIL;
}

OBJECT_PTR primitive_concat(OBJECT_PTR count1, ...)
{
  va_list ap;
  OBJECT_PTR lst, ret, rest;
  int i, start = 1;

  unsigned int count = get_int_value(count1);

  if(!count)
    return NIL;

  va_start(ap, count1);

  lst = (OBJECT_PTR)va_arg(ap, int);

  if(!IS_CONS_OBJECT(lst) && lst != NIL)
  {
    raise_error("Invalid argument to CONCAT");
    return NIL;
  }

  //to skip NILs
  while(lst == NIL)
  {
    start++;

    if(start > count)
      return NIL;

    lst = (OBJECT_PTR)va_arg(ap, int);

    if(!IS_CONS_OBJECT(lst) && lst != NIL)
    {
      raise_error("Invalid argument to CONCAT");
      return NIL;
    }
  }

  ret = clone_object(lst);

  for(i=start; i<count; i++)
  {
    lst = (OBJECT_PTR)va_arg(ap, int);

    if(lst == NIL)
      continue;

    if(!IS_CONS_OBJECT(lst))
    {
      raise_error("Invalid argument to CONCAT");
      return NIL;
    }

    rest = lst;

    while(rest != NIL)
    {
      uintptr_t ptr = last_cell(ret) & POINTER_MASK;
      set_heap(ptr, 1, cons(clone_object(car(rest)), NIL));

      rest = cdr(rest);
    }
  }

  va_end(ap);

  return ret;
}

OBJECT_PTR primitive_not(OBJECT_PTR obj)
{
  return obj == NIL ? TRUE : NIL;
}

OBJECT_PTR primitive_car(OBJECT_PTR obj)
{
  if(obj != NIL && !IS_CONS_OBJECT(obj))
  {
    print_object(obj);
    raise_error("Argument to CAR must be a CONS object");
    return NIL;
  }
  return car(obj);
}

OBJECT_PTR primitive_gt(OBJECT_PTR v1, OBJECT_PTR v2)
{
  float val1, val2;

  if((!(IS_INTEGER_OBJECT(v1)) && !(IS_FLOAT_OBJECT(v1))) ||
     (!(IS_INTEGER_OBJECT(v2)) && !(IS_FLOAT_OBJECT(v2))))

  {
    raise_error("Arguments to > should be integers or floats");
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
	    
  return (val1 > val2) ? TRUE : NIL;
}

OBJECT_PTR primitive_geq(OBJECT_PTR v1, OBJECT_PTR v2)
{
  float val1, val2;

  if((!(IS_INTEGER_OBJECT(v1)) && !(IS_FLOAT_OBJECT(v1))) ||
     (!(IS_INTEGER_OBJECT(v2)) && !(IS_FLOAT_OBJECT(v2))))

  {
    raise_error("Arguments to >= should be integers or floats");
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
	    
  return (val1 >= val2) ? TRUE : NIL;
}

OBJECT_PTR primitive_leq(OBJECT_PTR v1, OBJECT_PTR v2)
{
  float val1, val2;

  if((!(IS_INTEGER_OBJECT(v1)) && !(IS_FLOAT_OBJECT(v1))) ||
     (!(IS_INTEGER_OBJECT(v2)) && !(IS_FLOAT_OBJECT(v2))))

  {
    raise_error("Arguments to <= should be integers or floats");
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
	    
  return (val1 <= val2) ? TRUE : NIL;
}

OBJECT_PTR primitive_atom(OBJECT_PTR obj)
{
  return is_atom(obj) ? TRUE : NIL;
}

OBJECT_PTR prim_symbol_value(OBJECT_PTR sym)
{
  if(!IS_SYMBOL_OBJECT(sym))
  {
    raise_error("Argument to SYMBOL-VALUE must be a symbol");
    return NIL;
  }

  int retval;
  OBJECT_PTR out;

  retval = get_top_level_sym_value(sym, &out);

  if(retval)
    return NIL;
  else
    return car(out);
}

OBJECT_PTR primitive_apply(OBJECT_PTR obj, OBJECT_PTR args)
{
  if(!IS_FUNCTION2_OBJECT(obj))
  {
    raise_error("First argument to APPLY should be a function");
    return NIL;
  }

  OBJECT_PTR top_level_sym = reverse_sym_lookup(obj);

  if(top_level_sym != NIL)
  {
    int pos = and_rest_closure_pos(top_level_sym);

    if(pos != -1)
    {
      OBJECT_PTR rest = args;
      int i = 0;
      OBJECT_PTR ret = NIL;

      if(cons_length(rest) < pos)
      {
        raise_error("Number of parameters to the function less than the number of fixed parameters");
        return NIL;
      }

      while(rest != NIL && i < pos)
      {
        OBJECT_PTR val = car(rest);

        if(ret == NIL)
          ret = cons(val, NIL);
        else
        {
          uintptr_t ptr = last_cell(ret) & POINTER_MASK;
          set_heap(ptr, 1, cons(val, NIL));
        }

        i++;
        rest = cdr(rest);
      }

      OBJECT_PTR val;

      if(rest != NIL)
      {
        val = rest;

        if(ret == NIL)
          ret = cons(val, NIL);
        else
        {
          uintptr_t ptr = last_cell(ret) & POINTER_MASK;
          set_heap(ptr, 1, cons(val, NIL));
        }
      }
      else
      {
        uintptr_t ptr = last_cell(ret) & POINTER_MASK;
        set_heap(ptr, 1, cons(NIL, NIL));
      }

      return apply_macro_or_fn(obj, ret);
    
    }
    else
      return apply_macro_or_fn(obj, args);
  }
  else
    return apply_macro_or_fn(obj, args);
}

OBJECT_PTR primitive_symbol(OBJECT_PTR str)
{
  if(!IS_STRING_LITERAL_OBJECT(str) && !is_string_object(str))
  {
    raise_error("SYMBOL needs one argument, a string object/literal");
    return NIL;
  }

  if(IS_STRING_LITERAL_OBJECT(str))
  {
    return get_symbol_object((char *)convert_to_upper_case(strdup(strings[(int)str >> OBJECT_SHIFT])));
  }
  else if(is_string_object(str))
  {
    char msg[500];

    uintptr_t ptr = str & POINTER_MASK;

    int len = *((unsigned int *)ptr);

    int i;

    memset(msg, '\0', 500);

    for(i=1; i <= len; i++)
      msg[i-1] = (int)get_heap(ptr, i) >> OBJECT_SHIFT;

    return get_symbol_object((char *)convert_to_upper_case(msg));
  }  
}

OBJECT_PTR prim_symbol_name(OBJECT_PTR sym)
{
  char buf[SYMBOL_STRING_SIZE];

  if(!IS_SYMBOL_OBJECT(sym))
  {
    raise_error("Parameter to SYMBOL-NAME should be a symbol object");
    return NIL;
  }

  memset(buf,'\0',SYMBOL_STRING_SIZE);

  print_symbol(sym, buf);

  return  (OBJECT_PTR)((add_string(buf) << OBJECT_SHIFT) + STRING_LITERAL_TAG);
}

OBJECT_PTR primitive_format(OBJECT_PTR count1, OBJECT_PTR fd, OBJECT_PTR spec, ...)
{
  va_list ap;

  unsigned int count = get_int_value(count1);

  OBJECT_PTR ret = NIL;

  unsigned int i = 1;

  if(count < 2)
  {
    raise_error("FORMAT requires at least two arguments, a file descriptor and a format specification string");
    return NIL;
  }

  if(fd != NIL && !IS_INTEGER_OBJECT(fd))
  {
    raise_error("First parameter to FORMAT must be NIL or an integer denoting a file descriptor");
    return NIL;
  }

  if(!IS_STRING_LITERAL_OBJECT(spec) && !is_string_object(spec))
  {
    raise_error("Second parameter to FORMAT must be a format specification string");
    return NIL;
  }

  va_start(ap, spec);

  while(i <= count - 2)
  {
    OBJECT_PTR val = (OBJECT_PTR)va_arg(ap, int);

    if(!(IS_INTEGER_OBJECT(val)        ||
         IS_FLOAT_OBJECT(val)          ||
         IS_CHAR_OBJECT(val)           ||
         IS_STRING_LITERAL_OBJECT(val) ||
         is_string_object(val)))
    {
      raise_error("Parameters to FORMAT should be integers, floats, characters or strings");
      return NIL;
    }

    if(ret == NIL)
      ret = cons(val, NIL);
    else
    {
      uintptr_t ptr = last_cell(ret) & POINTER_MASK;
      set_heap(ptr, 1, cons(val, NIL));      
    }

    i++;
  }

  ret = cons(fd, cons(spec, ret));
        
  if(console_mode)
  {
    if(format(ret) == -1)
    {
      raise_error("FORMAT error");
      return NIL;
    }
  }
  else
  {
    if(!console_mode && !single_expression_mode && !pipe_mode)
    {
      if(format_for_gui(ret) == -1)
      {
        raise_error("FORMAT error");
	return NIL;
      }
    }
    else
    {
      if(format(ret) == -1)
      //error message would have been set in format()
	return NIL;
    }
  }

  return NIL;
}
