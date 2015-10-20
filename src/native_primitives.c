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
#include <dlfcn.h>
#include <time.h>

#include "plisp.h"
#include "util.h"

extern OBJECT_PTR NIL;
extern OBJECT_PTR TRUE;

extern char **strings;

extern BOOLEAN console_mode, single_expression_mode, pipe_mode;

extern unsigned int POINTER_MASK;

extern int nof_dl_handles;
extern void **dl_handles;
extern char *foreign_library_names[];

extern OBJECT_PTR INTEGR;
extern OBJECT_PTR FLOT;
extern OBJECT_PTR CHAR;
extern OBJECT_PTR VOID;
extern OBJECT_PTR INT_POINTER;
extern OBJECT_PTR FLOAT_POINTER;
extern OBJECT_PTR CHAR_POINTER;

extern OBJECT_PTR exception_object;

extern unsigned int current_package;

extern unsigned int nof_global_vars;
extern global_var_mapping_t *top_level_symbols;

extern OBJECT_PTR DEFUN, DEFMACRO, DEFINE;

extern BOOLEAN in_error;

extern BOOLEAN system_changed;

extern void throw_exception1(char *, char *);

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
    throw_exception1("ARG-MISMATCH", "ADD requires at least two arguments");
    return NIL;
  }

  va_start(ap, count1);

  for(i=0; i<count; i++)
  {
    arg = (OBJECT_PTR)va_arg(ap, OBJECT_PTR);

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
    arg = (OBJECT_PTR)va_arg(ap, OBJECT_PTR);

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


OBJECT_PTR primitive_div(OBJECT_PTR count1, ...)
{
  va_list ap;
  OBJECT_PTR arg;
  int i;
  float val;
  BOOLEAN is_float = false;

  OBJECT_PTR first;        

  float prod = 1;

  unsigned int count = get_int_value(count1);

  if(count < 2)
  {
    raise_error("Operator '/' requires at least two arguments");
    return NIL;
  }

  va_start(ap, count1);

  first = (OBJECT_PTR)va_arg(ap, int);

  if(IS_FLOAT_OBJECT(first))
  {
    is_float = true;
    val = get_float_value(first);
  }
  else if(IS_INTEGER_OBJECT(first))
    val = get_int_value(first);
  else
  {
    raise_error("Argument to operator '/' should be a number");
    return NIL;
  }

  for(i=1; i<count; i++)
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
      raise_error("Argument to operator '/' should be a number");
      return NIL;
    }    
  }

  if(prod == 0)
  {
    throw_exception1("EXCEPTION", "Division by zero");
    return NIL;
  }

  va_end(ap);

  if(is_float)
    return convert_float_to_object(val / prod);
  else
    return convert_int_to_object((int)(val / prod));
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

OBJECT_PTR primitive_clone(OBJECT_PTR obj)
{
  return clone_object(obj);
}

OBJECT_PTR primitive_unbind(OBJECT_PTR sym)
{
  if(!IS_SYMBOL_OBJECT(sym))
  {
    raise_error("Argument to UNBIND must be a symbol");
    return NIL;
  }

  int retval;
  OBJECT_PTR out;

  retval = get_top_level_sym_value(sym, &out);

  if(retval)
  {
    raise_error("Symbol to be unbound does not exist");
    return NIL;
  }

  if(remove_top_level_sym(sym) == NOT_OK)
  {
    raise_error("Error unbinding symbol");
    return NIL;
  }

  return NIL;
}

OBJECT_PTR primitive_newline(OBJECT_PTR obj)
{
  if(obj != NIL && !IS_INTEGER_OBJECT(obj))
  {
    raise_error("NEWLINE should be passed exactly one argument: NIL or an integer denoting a file descriptor");
    return;
  }

  if(obj != NIL)
  {
    fprintf((FILE *)get_int_value(obj), "\n");
  }
  else
  {
    if(!console_mode && !single_expression_mode && !pipe_mode)
      print_to_transcript("\n");
    else
      fprintf(stdout, "\n");
  }

  return NIL;
}

OBJECT_PTR primitive_consp(OBJECT_PTR obj)
{
  return IS_CONS_OBJECT(obj) ? TRUE : NIL;
}

OBJECT_PTR primitive_listp(OBJECT_PTR obj)
{
  return IS_CONS_OBJECT(obj) ? TRUE : NIL;
}

OBJECT_PTR primitive_integerp(OBJECT_PTR obj)
{
  return IS_INTEGER_OBJECT(obj) ? TRUE : NIL;
}

OBJECT_PTR primitive_floatp(OBJECT_PTR obj)
{
  return IS_FLOAT_OBJECT(obj) ? TRUE : NIL;
}

OBJECT_PTR prim_characterp(OBJECT_PTR obj)
{
  return IS_CHAR_OBJECT(obj) ? TRUE : NIL;
}

OBJECT_PTR primitive_symbolp(OBJECT_PTR obj)
{
  return IS_SYMBOL_OBJECT(obj) ? TRUE : NIL;
}

OBJECT_PTR primitive_stringp(OBJECT_PTR obj)
{
  return IS_STRING_LITERAL_OBJECT(obj) || is_string_object(obj) ? TRUE : NIL;
}

OBJECT_PTR primitive_arrayp(OBJECT_PTR obj)
{
  return IS_ARRAY_OBJECT(obj) ? TRUE : NIL;
}

OBJECT_PTR primitive_closurep(OBJECT_PTR obj)
{
  return IS_FUNCTION2_OBJECT(obj) ? TRUE : NIL;
}

OBJECT_PTR primitive_macrop(OBJECT_PTR obj)
{
  return IS_MACRO2_OBJECT(obj) ? TRUE : NIL;
}

OBJECT_PTR primitive_string(OBJECT_PTR string_literal)
{
  if((!(IS_STRING_LITERAL_OBJECT(string_literal))))
  {
    raise_error("Argument to STRING should be a literal string");
    return NIL;
  }        
 
  return eval_string(string_literal);
}

OBJECT_PTR prim_make_array(OBJECT_PTR count1, OBJECT_PTR size, ...)
{
  va_list ap;

  unsigned int count = get_int_value(count1);

  va_start(ap, size);

  OBJECT_PTR default_value = (count > 1) ? (OBJECT_PTR)va_arg(ap, int) : NIL;

  if((!(IS_INTEGER_OBJECT(size))))
  {
    raise_error("First argument to MAKE-ARRAY should be the size of the array (integer)");
    return NIL;
  }        

  return eval_make_array(size, default_value);
}

OBJECT_PTR prim_array_set(OBJECT_PTR array_obj, OBJECT_PTR idx, OBJECT_PTR val)
{
  uintptr_t ptr;
  int array_len, index;

  ptr = array_obj & POINTER_MASK;

  if((!(IS_ARRAY_OBJECT(array_obj))))
  {
    raise_error("First argument to ARRAY-SET should be an array");
    return NIL;
  }        

  if((!(IS_INTEGER_OBJECT(idx))))
  {
    raise_error("Second argument to ARRAY-SET should be an integer (index into the array)");
    return 1;
  }        

  array_len = *((unsigned int *)ptr);

  index = get_int_value(idx);

  if(index < 0 || (index >= array_len))
  {
    raise_error( "Array index out of bounds");
    return 1;
  }        
 
  set_heap(ptr, index + 1, val);

  return val;
}

OBJECT_PTR prim_array_get(OBJECT_PTR array_obj, OBJECT_PTR idx)
{
  uintptr_t ptr;
  int index;

  ptr = array_obj & POINTER_MASK;

  if(!IS_INTEGER_OBJECT(idx))
  {
    raise_error("Second argument to ARRAY-GET should be an integer (index into the array)");
    return NIL;
  }        

  index = get_int_value(idx);

  if(IS_STRING_LITERAL_OBJECT(array_obj))
  {
    char *str = strings[(int)array_obj >> OBJECT_SHIFT];

    if(index < 0 || index >= strlen(str))
    {
      raise_error("Array index out of bounds");
      return NIL;
    }        

    return (OBJECT_PTR)((str[index] << OBJECT_SHIFT) + CHAR_TAG);
  }
  else
  {
    int array_len;

    if(!IS_ARRAY_OBJECT(array_obj))
    {
      raise_error("First argument to ARRAY-GET should be an array");
      return NIL;
    }        

    array_len = *((unsigned int *)ptr);

    if(index < 0 || (index >= array_len))
    {
      raise_error("Array index out of bounds");
      return NIL;
    }        

    return get_heap(ptr, index + 1);
  }
}

OBJECT_PTR prim_sub_array(OBJECT_PTR array, OBJECT_PTR start, OBJECT_PTR array_length)
{
  int len;

  if(!(IS_ARRAY_OBJECT(array)))
  {
    raise_error("First argument to SUB-ARRAY should be an ARRAY object");
    return NIL;
  }

  if(!(IS_INTEGER_OBJECT(start)))
  {
    raise_error("Second argument to SUB-ARRAY should be an integer (start index)");
    return NIL;
  }

  if(!(get_int_value(start) >= 0))
  {
    raise_error("Second argument to SUB-ARRAY should be a non-negative integer");
    return NIL;
  }

  if(!(IS_INTEGER_OBJECT(array_length)))
  {
    raise_error("Third argument to SUB-ARRAY should be an integer (length of the sub-array)");
    return NIL;
  }

  if(!(get_int_value(array_length) >= 0))
  {
    raise_error("Third argument to SUB-ARRAY should be a non-negative integer");
    return NIL;
  }

  len = *((unsigned int *)(array & POINTER_MASK));

  if((get_int_value(start) + get_int_value(array_length)) > len)
  {
    raise_error("Range (start, length) for SUB-ARRAY out of bounds of the array");
    return NIL;
  }

  return eval_sub_array(array, start, array_length);
}

OBJECT_PTR prim_array_length(OBJECT_PTR array)
{
  if(array == NIL)
    return NIL;

  if(IS_STRING_LITERAL_OBJECT(array))
    return convert_int_to_object(strlen(strings[(int)array >> OBJECT_SHIFT]));
  else
  {
    if(!(IS_ARRAY_OBJECT(array)))
    {
      raise_error("Argument to ARRAY-LENGTH should be an ARRAY object");
      return NIL;
    }

    return convert_int_to_object(*((unsigned int *)(array & POINTER_MASK)));
  }
}

//TODO: Do we need this? Can't we just use the PRINT primitive?
OBJECT_PTR prim_print_string(OBJECT_PTR str)
{
  if(!(is_string_object(str)) && (!(IS_STRING_LITERAL_OBJECT(str))))
  {
    raise_error("Argument to PRINT_STRING should be a string object");
    return NIL;
  }

  print_object(str);
  fprintf(stdout, "\n");

  return NIL;
}

OBJECT_PTR prim_load_fgn_lib(OBJECT_PTR file_name)
{
  void **temp;
  char *fname;
  void *ret;

  if(!(is_string_object(file_name)) && (!(IS_STRING_LITERAL_OBJECT(file_name))))
  {
    raise_error("Argument to LOAD-FOREIGN-LIBRARY should be a string object denoting the library name");
    return NIL;
  }

  if(nof_dl_handles == MAX_FOREIGN_LIBRARY_COUNT)
  {
    raise_error("Maximum number of foreign libraries has been exceeded");
    return NIL;
  }

  nof_dl_handles++;
  
  temp = (void **)realloc(dl_handles, nof_dl_handles * sizeof(void *));

  if(temp == NULL)
  {
    raise_error("Unable to extend memory for dl_handles");
    return NIL;
  }

  dl_handles = temp;

  fname = IS_STRING_LITERAL_OBJECT(file_name) ? strings[(int)file_name >> OBJECT_SHIFT] : get_string(file_name);

  ret = dlopen(fname, RTLD_LAZY);

  if(!ret)
  {
    raise_error("dl_open() failed");
    return NIL;
  }

  dl_handles[nof_dl_handles - 1] = ret;

  foreign_library_names[nof_dl_handles - 1] = strdup(fname);

  return NIL;
}

OBJECT_PTR prim_call_fgn_func(OBJECT_PTR fn_name, OBJECT_PTR ret_type, OBJECT_PTR args)
{
  OBJECT_PTR rest_args;

  if(!IS_STRING_LITERAL_OBJECT(fn_name) && !is_string_object(fn_name))
  {
    raise_error("First argument to CALL-FOREIGN-FUNCTION should be the funtion name (string)");
    return NIL;
  }

  if(!(ret_type == INTEGR        ||
       ret_type == FLOT          ||
       ret_type == CHAR          ||
       ret_type == VOID          ||
       /* ret_type == INT_POINTER   || */ //not handling int and float pointer return values
       /* ret_type == FLOAT_POINTER || */
       ret_type == CHAR_POINTER))
  {
    raise_error("Second parameter to CALL-FOREIGN-FUNCTION should be a valid return type");
    return NIL;
  }

  if(args != NIL && !IS_CONS_OBJECT(args))
  {
    raise_error("Arguments should be a list of two-element lists (argument, type) -- 1");
    return NIL;
  }

  rest_args = args;

  while(rest_args != NIL)
  {
    OBJECT_PTR car_rest_args = car(rest_args);

    OBJECT_PTR val, type;

    if(!IS_CONS_OBJECT(car_rest_args))
    {
      raise_error("Arguments should be a list of two-element lists (argument, type) -- 2");
      return NIL;
    }

    if(cons_length(car_rest_args) != 2)
    {
      raise_error("Arguments should be a list of two-element lists (argument, type) -- 3");
      return NIL;
    }

    val = CAAR(rest_args);

    if(!(IS_INTEGER_OBJECT(val)        ||
	 IS_FLOAT_OBJECT(val)          ||
	 IS_CHAR_OBJECT(val)           ||
	 IS_STRING_LITERAL_OBJECT(val) ||
	 is_string_object(val)         ||
	 IS_SYMBOL_OBJECT(val)))
      {
	raise_error("Parameter should be integer-, float-, charcter-, or string constant, or a symbol");
	return NIL;
      }

    //TODO: this works only for top-level symbols
    if(IS_SYMBOL_OBJECT(val))
    {
      OBJECT_PTR retval, out;
      retval = get_top_level_sym_value(val, &out);
      if(retval)
      {
	char buf[SYMBOL_STRING_SIZE];
        char err_buf[500];
	print_qualified_symbol(val, buf);

        memset(err_buf, '\0', 500);
	sprintf(err_buf, "Symbol not bound(3): %s", buf);

	raise_error(err_buf);
	return NIL;
      }
      val = out;
    }

    type = CADAR(rest_args);

    if(type == INTEGR)
    {
      if(!IS_INTEGER_OBJECT(val))
      {
	raise_error("Argument type mismatch: integer expected");
	return NIL;
      }
    }          
    else if(type == FLOT)
    {
      if(!IS_FLOAT_OBJECT(val))
      {
	raise_error("Argument type mismatch: float expected");
	return NIL;
      }
    }
    else if(type == CHAR)
    {
      if(!IS_CHAR_OBJECT(val))
      {
	raise_error("Argument type mismatch: character expected");
	return NIL;
      }
    }
    else if(type == CHAR_POINTER)
    {
      if(!IS_STRING_LITERAL_OBJECT(val) && !is_string_object(val))
      {
	raise_error("Argument type mismatch: string object/literal expected");
	return NIL;
      }
    }
    else if(type == INT_POINTER)
    {
      if(!IS_SYMBOL_OBJECT(CAAR(rest_args)) || !IS_INTEGER_OBJECT(val))
      {
	raise_error("Mapping a non-variable to INTEGER-POINTER / Argument type mismatch");
	return NIL;
      }
    }
    else if(type == FLOAT_POINTER)
    {
      if(!IS_SYMBOL_OBJECT(CAAR(rest_args)) || !IS_FLOAT_OBJECT(val))
      {
	raise_error("Mapping a non-variable to FLOAT-POINTER / Argument type mismatch");
	return NIL;
      }
    }
    else
    {
      raise_error("call_foreign_function(): non-primitive object type not handled");
      return NIL;
    }

    rest_args = cdr(rest_args);
  }

  return call_foreign_function(fn_name, ret_type, args);
}

OBJECT_PTR prim_create_pkg(OBJECT_PTR package)
{
  char *package_name;

  if(!IS_STRING_LITERAL_OBJECT(package) && !is_string_object(package))
  {
    raise_error("CREATE-PACKAGE requires a string object or string literal as its argument");
    return NIL;
  }

  package_name = (char *)convert_to_upper_case(strings[(int)package >> OBJECT_SHIFT]);

  if(find_package(package_name) != NOT_FOUND)
  {
    raise_error("Package already exists");
    return NIL;
  }

  create_package(package_name);

  return package;
}

OBJECT_PTR prim_in_package(OBJECT_PTR package)
{
  char *package_name;

  if(!IS_STRING_LITERAL_OBJECT(package) && !is_string_object(package))
  {
    raise_error("IN-PACKAGE requires a string object or string literal as its argument");
    return NIL;
  }

  package_name = (char *)convert_to_upper_case(strings[(int)package >> OBJECT_SHIFT]);

  if(!strcmp(package_name,"CORE"))
  {
    raise_error("Core package cannot be updated");
    return NIL;
  }
  else
  {
    int index = find_package(package_name);
    if(index == NOT_FOUND)
    {
      raise_error("Package does not exist");
      return NIL;
    }
    else
    {
      current_package = index;
      return NIL;
    }
  }

  return NIL;
}

OBJECT_PTR prim_export_pkg(OBJECT_PTR package, OBJECT_PTR file)
{
  char *package_name, *file_name;

  if(!IS_STRING_LITERAL_OBJECT(package) && !is_string_object(package))
  {
    raise_error("EXPORT-PACKAGE requires a string object or string literal as its first argument");
    return NIL;
  }

  package_name = (char *)convert_to_upper_case(strings[(int)package >> OBJECT_SHIFT]);

  if(!IS_STRING_LITERAL_OBJECT(file) && !is_string_object(file))
  {
    raise_error("EXPORT-PACKAGE requires a string object or string literal as its second argument");
    return NIL;
  }

  file_name = (char *)(strings[(int)file >> OBJECT_SHIFT]);

  if(find_package(package_name) == NOT_FOUND)
  {
    raise_error("Package does not exist");
    return NIL;
  }

  int index = find_package(package_name);

  FILE *fp = fopen(file_name, "w");

  if(!fp)
  {
    raise_error("Unable to open file");
    return NIL;
  }

  fprintf(fp, "(create-package \"%s\")\n\n", convert_to_lower_case(package_name));

  fprintf(fp, "(in-package \"%s\")\n\n", convert_to_lower_case(package_name));

  char buf[MAX_STRING_LENGTH];

  int i;

  for(i=0; i<nof_global_vars; i++)
  {
    if(top_level_symbols[i].delete_flag)
      continue;

    OBJECT_PTR sym = top_level_symbols[i].sym;

    if(((int)sym >> (SYMBOL_BITS + OBJECT_SHIFT)) == index)
    {
      if(IS_NATIVE_FN_OBJECT(top_level_symbols[i].val))
        continue;

      OBJECT_PTR obj = car(top_level_symbols[i].val);

      if(IS_FUNCTION2_OBJECT(obj))
      {
	memset(buf, '\0', MAX_STRING_LENGTH);
	OBJECT_PTR temp = cons(DEFUN, 
			       cons(sym,
				    cons(get_params_object(obj),
					 get_source_object(obj))));

	print_object_to_string(temp, buf, 0);
      }
      else if(IS_MACRO2_OBJECT(obj))
      {
	memset(buf, '\0', MAX_STRING_LENGTH);
	OBJECT_PTR temp = cons(DEFMACRO, 
			       cons(sym,
				    cons(get_params_object(obj),
					 get_source_object(obj))));

	print_object_to_string(temp, buf, 0);
      }
      else if(IS_CONTINUATION_OBJECT(obj))
      {
	//not doing anything for continuation objects
      }
      else
      {
	memset(buf, '\0', MAX_STRING_LENGTH);
	print_object_to_string(cons(DEFINE,
				    cons(sym, cons(obj, NIL))), buf, 0);
      }

      fprintf(fp, "%s\n\n", convert_to_lower_case(buf));
    }
  } //end of for

  fclose(fp);

  return NIL;
}

OBJECT_PTR prim_expand_macro(OBJECT_PTR exp)
{
  return expand_macro_full(exp, true);
}

OBJECT_PTR primitive_env(OBJECT_PTR dummy)
{
  int i;

  OBJECT_PTR ret = NIL;

  for(i=0; i<nof_global_vars; i++)
  {
    if(top_level_symbols[i].delete_flag || IS_NATIVE_FN_OBJECT(top_level_symbols[i].val))
      continue;

    OBJECT_PTR val = cons(top_level_symbols[i].sym, car(top_level_symbols[i].val));

    if(ret == NIL)
      ret = cons(val, NIL);
    else
    {
      uintptr_t ptr = last_cell(ret) & POINTER_MASK;
      set_heap(ptr, 1, cons(val, NIL));
    }
  }

  return ret;
}

OBJECT_PTR primitive_time(OBJECT_PTR exp)
{
  clock_t start, diff;
  int msec;

  char buf[100];

  char form[500];

  memset(form, '\0', 500);
  print_object_to_string(exp, form, 0);


  start = clock();

  OBJECT_PTR res = full_monty_eval(exp);

  if(in_error)
  {
    raise_error("TIME failed");
    return NIL;
  }

  diff = clock() - start;
  msec = diff * 1000 / CLOCKS_PER_SEC;

  if(!console_mode && !single_expression_mode && !pipe_mode)
  {
    memset(buf, '\0', 100);
    sprintf(buf, "%s took %d seconds %d milliseconds\n", form, msec/1000, msec%1000);
    print_to_transcript(buf);
  }
  else
    printf("%s took %d seconds %d milliseconds\n", form, msec/1000, msec%1000);

  return res;
}

OBJECT_PTR prim_serialize(OBJECT_PTR obj, OBJECT_PTR file_name)
{
  if(!(is_string_object(file_name)) && (!(IS_STRING_LITERAL_OBJECT(file_name))))
  {
    raise_error("Second argument to SAVE-OBJECT should be a string object or a string literal denoting the file name");
    return NIL;
  }

  if(is_string_object(file_name))
    serialize(obj, get_string(file_name));
  else
    serialize(obj, strings[(int)file_name >> OBJECT_SHIFT]);

  return NIL;
}

OBJECT_PTR prim_deserialize(OBJECT_PTR file_name)
{
  if(!(is_string_object(file_name)) && (!(IS_STRING_LITERAL_OBJECT(file_name))))
  {
    raise_error("Argument to LOAD-OBJECT should be a string object or a string literal denoting the file name");
    return NIL;
  }

  int ret;

  if(is_string_object(file_name))
    ret = deserialize(get_string(file_name));
  else
    ret = deserialize(strings[(int)file_name >> OBJECT_SHIFT]);

  if(ret == -1)
  {
    raise_error("Error in LOAD-OBJECT");
    return NIL;
  }

  if(IS_FUNCTION2_OBJECT(ret) || IS_MACRO2_OBJECT(ret))
  {
    OBJECT_PTR cons_equiv = ((ret >> OBJECT_SHIFT) << OBJECT_SHIFT) + CONS_TAG;
    return compile_and_evaluate(car(cons_equiv));
  }

  return ret;
}

OBJECT_PTR prim_load_file(OBJECT_PTR file_name)
{
  FILE *temp;

  int ret;

  if(!is_string_object(file_name) && (!IS_STRING_LITERAL_OBJECT(file_name)))
  {
    raise_error("Argument to LOAD-FILE should be a string");
    return NIL;
  }

  temp = fopen(is_string_object(file_name) ?  get_string(file_name) : strings[(int)file_name >> OBJECT_SHIFT], "r");

  if(!temp)
  {
    raise_error("LOAD-FILE unable to open file");
    return NIL;
  }

  if(set_up_new_yyin(temp))
  {
    raise_error("Unable to read from file");
    return NIL;
  }

  while(!yyparse())
  {
    repl2();
  }

  pop_yyin();

  return NIL;
}

OBJECT_PTR prim_create_image(OBJECT_PTR file_name)
{
  if(!(is_string_object(file_name)) && (!(IS_STRING_LITERAL_OBJECT(file_name))))
  {
    raise_error("Argument to CREATE-IMAGE should be a string object or a string literal denoting the file name of the image");
    return NIL;
  }

  if(is_string_object(file_name))
    create_image(get_string(file_name));
  else
    create_image(strings[(int)file_name >> OBJECT_SHIFT]);

  system_changed = false;

  return NIL;
}

OBJECT_PTR primitive_throw(OBJECT_PTR excp)
{
  exception_object = excp;
  return handle_exception();
}
