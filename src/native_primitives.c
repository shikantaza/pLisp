/**
  Copyright 2011-2021 Rajesh Jayaprakash <rajesh.jayaprakash@gmail.com>

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

#include <time.h>

#include <unistd.h>
#include <fcntl.h>

#ifdef WIN32
#include <errno.h>
#include <windows.h>
#else
#include <sys/errno.h>
#endif

#include "plisp.h"
#include "util.h"

#include "memory.h"

enum {WHITE, GREY, BLACK};

extern OBJECT_PTR NIL;

#ifdef WIN32
#define TRUE SYMBOL_TAG
#else
extern OBJECT_PTR TRUE;
#endif

extern char **strings;

extern BOOLEAN console_mode, single_expression_mode, pipe_mode;

extern uintptr_t POINTER_MASK;

extern int nof_dl_handles;

#ifdef WIN32
extern HMODULE *dl_handles;
#else
extern void **dl_handles;
#endif

extern char *foreign_library_names[];

extern OBJECT_PTR INTEGR;
extern OBJECT_PTR FLOT;

#ifdef WIN32
extern OBJECT_PTR CHAR1;
extern OBJECT_PTR VOID1;
#else
extern OBJECT_PTR CHAR;
extern OBJECT_PTR VOID;
#endif

extern OBJECT_PTR INT_POINTER;
extern OBJECT_PTR FLOAT_POINTER;
extern OBJECT_PTR CHAR_POINTER;

extern OBJECT_PTR GET_SOURCE;
extern OBJECT_PTR LAMBDA;
extern OBJECT_PTR MACRO;

extern OBJECT_PTR exception_object;

extern unsigned int current_package;

extern unsigned int nof_global_vars;
extern global_var_mapping_t *top_level_symbols;

extern OBJECT_PTR DEFUN, DEFMACRO, DEFINE;

extern BOOLEAN in_error;

extern BOOLEAN system_changed;

extern void throw_exception1(char *, char *);

extern void build_autocomplete_words();
extern void set_up_autocomplete_words();

extern BOOLEAN is_continuation_object(OBJECT_PTR);

extern OBJECT_PTR expand_macro_full(OBJECT_PTR, BOOLEAN);

extern BOOLEAN IS_INTEGER_OBJECT(OBJECT_PTR);
extern BOOLEAN IS_FLOAT_OBJECT(OBJECT_PTR);
extern BOOLEAN IS_CONS_OBJECT(OBJECT_PTR);
extern BOOLEAN IS_STRING_LITERAL_OBJECT(OBJECT_PTR);
extern BOOLEAN IS_SYMBOL_OBJECT(OBJECT_PTR);
extern int get_top_level_sym_value(OBJECT_PTR, OBJECT_PTR*);
extern BOOLEAN IS_FUNCTION2_OBJECT(OBJECT_PTR);
extern OBJECT_PTR reverse_sym_lookup(OBJECT_PTR);
extern int and_rest_closure_pos(OBJECT_PTR);
extern int cons_length(OBJECT_PTR);
extern OBJECT_PTR apply_macro_or_fn(OBJECT_PTR, OBJECT_PTR);
extern int add_string(char *);
extern BOOLEAN IS_CHAR_OBJECT(OBJECT_PTR);
extern int format_for_gui(OBJECT_PTR);
extern int remove_top_level_sym(OBJECT_PTR);
extern BOOLEAN IS_ARRAY_OBJECT(OBJECT_PTR);
extern BOOLEAN IS_MACRO2_OBJECT(OBJECT_PTR);
extern OBJECT_PTR CAAR(OBJECT_PTR);
extern OBJECT_PTR CADAR(OBJECT_PTR);
extern BOOLEAN IS_NATIVE_FN_OBJECT(OBJECT_PTR);
extern BOOLEAN IS_CONTINUATION_OBJECT(OBJECT_PTR);
extern unsigned int memory_deallocated();
extern OBJECT_PTR full_monty_eval(OBJECT_PTR);
extern BOOLEAN is_dynamic_memory_object(OBJECT_PTR);
extern void serialize(OBJECT_PTR, char *);
extern int deserialize(char *);
extern OBJECT_PTR compile_and_evaluate(OBJECT_PTR, OBJECT_PTR);
extern OBJECT_PTR handle_exception();

extern unsigned int nof_pkg_import_entries;
extern pkg_import_t *pkg_import_entries;

extern unsigned int nof_packages;
extern package_t *packages;

extern unsigned int print_context_pkg_index;

extern OBJECT_PTR get_source_object(OBJECT_PTR);
extern OBJECT_PTR get_params_object(OBJECT_PTR);

extern void print_to_transcript(char *);

extern int set_up_new_yyin(FILE *);
extern int yyparse();
extern int repl2();
extern void pop_yyin();
extern OBJECT_PTR second(OBJECT_PTR);
extern void show_error_dialog(char *);
extern OBJECT_PTR get_string_object(char *);

OBJECT_PTR quote(OBJECT_PTR count, OBJECT_PTR exp)
{
  return exp;
}

OBJECT_PTR primitive_add(OBJECT_PTR count1, ...)
{
  va_list ap;
  OBJECT_PTR arg;
  int i;
  double sum = 0.0;

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
      double val = get_float_value(arg);
      sum += val;
    }
    else
    {
      throw_exception1("INVALID-ARGUMENT", "Arguments to ADD should be integers or floats");
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
  double float_res;

  BOOLEAN is_float = false;

  unsigned int count = get_int_value(count1);

  if(count < 2)
  {
    throw_exception1("ARG-MISMATCH", "'-' requires at least two arguments");
    return NIL;
  }

  va_start(ap, count1);

  OBJECT_PTR first_arg = (OBJECT_PTR)va_arg(ap, OBJECT_PTR);

  if(IS_FLOAT_OBJECT(first_arg))
  {
    float_res = get_float_value(first_arg);
    is_float = true;
  }
  else if(IS_INTEGER_OBJECT(first_arg))
    int_res = get_int_value(first_arg);
  else
  {
    throw_exception1("INVALID-ARGUMENT", "Invalid argument for '-' operator");
    return NIL;
  }

  for(i=1; i < count; i++)
  {
    OBJECT_PTR arg = (OBJECT_PTR)va_arg(ap, OBJECT_PTR);

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
      throw_exception1("INVALID-ARGUMENT", "Invalid argument for '-' operator");
      return NIL;
    }    
  }

  return is_float ? convert_float_to_object(float_res) : convert_int_to_object(int_res);
}

OBJECT_PTR primitive_lt(OBJECT_PTR v1, OBJECT_PTR v2)
{
  double val1, val2;

  if((!(IS_INTEGER_OBJECT(v1)) && !(IS_FLOAT_OBJECT(v1))) ||
     (!(IS_INTEGER_OBJECT(v2)) && !(IS_FLOAT_OBJECT(v2))))

  {
    throw_exception1("INVALID-ARGUMENT", "Arguments to < should be integers or floats");
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
    throw_exception1("NOT-A-CONS", "First argument to SETCAR should be a CONS object");
    return NIL;
  }

  //if(is_dynamic_memory_object(val))
  //  insert_node(GREY, val);

  set_heap(extract_ptr(obj), 0, val);

  return val;
}

OBJECT_PTR primitive_setcdr(OBJECT_PTR obj, OBJECT_PTR val)
{
  if(!(IS_CONS_OBJECT(obj)))
  {
    throw_exception1("NOT-A-CONS", "First argument to SETCDR should be a CONS object");
    return NIL;
  }

  //if(is_dynamic_memory_object(val))
  //  insert_node(GREY, val);

  set_heap(extract_ptr(obj), 1, val);

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

  ret = cons((OBJECT_PTR)va_arg(ap, OBJECT_PTR), NIL);

  for(i=1; i<count; i++)
  {
    uintptr_t ptr = extract_ptr(last_cell(ret));
    set_heap(ptr, 1, cons((OBJECT_PTR)va_arg(ap, OBJECT_PTR), NIL));
  }

  va_end(ap);

  return ret;
}

OBJECT_PTR primitive_mult(OBJECT_PTR count1, ...)
{
  va_list ap;
  OBJECT_PTR arg;
  int i;
  double prod = 1;

  BOOLEAN is_float = false;

  unsigned int count = get_int_value(count1);

  if(count < 2)
  {
    throw_exception1("ARG-MISMATCH", "Operator '*' requires at least two arguments");
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
      throw_exception1("INVALID-ARGUMENT", "Argument to operator '*' should be a number");
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
  double val;
  BOOLEAN is_float = false;

  OBJECT_PTR first;        

  double prod = 1;

  unsigned int count = get_int_value(count1);

  if(count < 2)
  {
    throw_exception1("ARG-MISMATCH", "Operator '/' requires at least two arguments");
    return NIL;
  }

  va_start(ap, count1);

  first = (OBJECT_PTR)va_arg(ap, OBJECT_PTR);

  if(IS_FLOAT_OBJECT(first))
  {
    is_float = true;
    val = get_float_value(first);
  }
  else if(IS_INTEGER_OBJECT(first))
    val = get_int_value(first);
  else
  {
    throw_exception1("INVALID-ARGUMENT", "Argument to operator '/' should be a number");
    return NIL;
  }

  for(i=1; i<count; i++)
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
      throw_exception1("INVALID-ARGUMENT", "Argument to operator '/' should be a number");
      return NIL;
    }    
  }

  if(prod == 0)
  {
    throw_exception1("DIV-BY-ZERO-EXCEPTION", "Division by zero");
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
    else if(IS_SYMBOL_OBJECT(obj1) && IS_SYMBOL_OBJECT(obj2))
      ret = !strcmp(get_symbol_name(obj1), get_symbol_name(obj2));
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

  lst = (OBJECT_PTR)va_arg(ap, OBJECT_PTR);

  if(!IS_CONS_OBJECT(lst) && lst != NIL)
  {
    throw_exception1("INVALID-ARGUMENT", "Invalid argument to CONCAT");
    return NIL;
  }

  //to skip NILs
  while(lst == NIL)
  {
    start++;

    if(start > count)
      return NIL;

    lst = (OBJECT_PTR)va_arg(ap, OBJECT_PTR);

    if(!IS_CONS_OBJECT(lst) && lst != NIL)
    {
      throw_exception1("INVALID-ARGUMENT", "Invalid argument to CONCAT");
      return NIL;
    }
  }

  ret = clone_object(lst);

  for(i=start; i<count; i++)
  {
    lst = (OBJECT_PTR)va_arg(ap, OBJECT_PTR);

    if(lst == NIL)
      continue;

    if(!IS_CONS_OBJECT(lst))
    {
      throw_exception1("INVALID-ARGUMENT", "Invalid argument to CONCAT");
      return NIL;
    }

    rest = lst;

    while(rest != NIL)
    {
      uintptr_t ptr = extract_ptr(last_cell(ret));
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
    //print_object(obj);
    throw_exception1("NOT-A-CONS", "Argument to CAR must be a CONS object");
    return NIL;
  }
  return car(obj);
}

OBJECT_PTR primitive_cdr(OBJECT_PTR obj)
{
  if(obj != NIL && !IS_CONS_OBJECT(obj))
  {
    //print_object(obj);
    throw_exception1("NOT-A-CONS", "Argument to CDR must be a CONS object");
    return NIL;
  }
  return cdr(obj);
}

OBJECT_PTR primitive_gt(OBJECT_PTR v1, OBJECT_PTR v2)
{
  double val1, val2;

  if((!(IS_INTEGER_OBJECT(v1)) && !(IS_FLOAT_OBJECT(v1))) ||
     (!(IS_INTEGER_OBJECT(v2)) && !(IS_FLOAT_OBJECT(v2))))

  {
    throw_exception1("INVALID-ARGUMENT", "Arguments to > should be integers or floats");
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
  double val1, val2;

  if((!(IS_INTEGER_OBJECT(v1)) && !(IS_FLOAT_OBJECT(v1))) ||
     (!(IS_INTEGER_OBJECT(v2)) && !(IS_FLOAT_OBJECT(v2))))

  {
    throw_exception1("INVALID-ARGUMENT", "Arguments to >= should be integers or floats");
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
  double val1, val2;

  if((!(IS_INTEGER_OBJECT(v1)) && !(IS_FLOAT_OBJECT(v1))) ||
     (!(IS_INTEGER_OBJECT(v2)) && !(IS_FLOAT_OBJECT(v2))))

  {
    throw_exception1("INVALID-ARGUMENT", "Arguments to <= should be integers or floats");
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
    throw_exception1("INVALID-ARGUMENT", "Argument to SYMBOL-VALUE must be a symbol");
    return NIL;
  }

  int retval;
  OBJECT_PTR out;

  retval = get_top_level_sym_value(sym, &out);

  if(retval)
  {
    throw_exception1("SYMBOL-NOT-BOUND", "Symbol not bound");
    return NIL;
  }
  else
    return car(out);
}

OBJECT_PTR primitive_apply(OBJECT_PTR obj, OBJECT_PTR args)
{
  if(!IS_FUNCTION2_OBJECT(obj))
  {
    throw_exception1("INVALID-ARGUMENT", "First argument to APPLY should be a function");
    return NIL;
  }

  if(args != NIL && !IS_CONS_OBJECT(args))
  {
    throw_exception1("INVALID-ARGUMENT", "Second argument to APPLY should be a list");
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
        throw_exception1("ARG-MISMATCH", "Number of parameters to the function less than the number of fixed parameters");
        return NIL;
      }

      while(rest != NIL && i < pos)
      {
        OBJECT_PTR val = car(rest);

        if(ret == NIL)
          ret = cons(val, NIL);
        else
        {
          uintptr_t ptr = extract_ptr(last_cell(ret));
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
          uintptr_t ptr = extract_ptr(last_cell(ret));
          set_heap(ptr, 1, cons(val, NIL));
        }
      }
      else
      {
        uintptr_t ptr = extract_ptr(last_cell(ret));
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
    throw_exception1("INVALID-ARGUMENT", "SYMBOL needs one argument, a string object/literal");
    return NIL;
  }

  if(IS_STRING_LITERAL_OBJECT(str))
  {
    return get_symbol_object((char *)convert_to_upper_case(GC_strdup(strings[(int)str >> OBJECT_SHIFT])));
  }
  else if(is_string_object(str))
  {
    char msg[500];

    uintptr_t ptr = extract_ptr(str);

    int len = *((uintptr_t *)ptr);

    int i;

    memset(msg, '\0', 500);

    for(i=1; i <= len; i++)
      msg[i-1] = (int)get_heap(ptr, i) >> OBJECT_SHIFT;

    return get_symbol_object((char *)convert_to_upper_case(msg));
  }
  else
  {
    printf("Warning: invalid object passed to primitive_symbol()\n");
    return NIL;
  }
}

OBJECT_PTR prim_symbol_name(OBJECT_PTR sym)
{
  char buf[SYMBOL_STRING_SIZE];

  if(!IS_SYMBOL_OBJECT(sym))
  {
    throw_exception1("INVALID-ARGUMENT", "Parameter to SYMBOL-NAME should be a symbol object");
    return NIL;
  }

  memset(buf,'\0',SYMBOL_STRING_SIZE);

  print_context_pkg_index = current_package;

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
    throw_exception1("ARG-MISMATCH", "FORMAT requires at least two arguments, a file descriptor and a format specification string");
    return NIL;
  }

  if(fd != NIL && !IS_INTEGER_OBJECT(fd))
  {
    throw_exception1("INVALID-ARGUMENT", "First parameter to FORMAT must be NIL or an integer denoting a file descriptor");
    return NIL;
  }

  if(IS_INTEGER_OBJECT(fd))
  {
    int fd1 = get_int_value(fd);

#ifdef WIN32
    BY_HANDLE_FILE_INFORMATION info;

    if(!GetFileInformationByHandle(fd1, &info))
    {
      throw_exception1("INVALID-ARGUMENT", "Invalid file descriptor");
      return;
    }
#else
    fcntl(fd1, F_GETFD);

    if(errno != 0 && errno != EAGAIN)
    {
      throw_exception1("INVALID-ARGUMENT", "Invalid file descriptor");
      return NIL;
    }
#endif
  
  }

  if(!IS_STRING_LITERAL_OBJECT(spec) && !is_string_object(spec))
  {
    throw_exception1("INVALID-ARGUMENT", "Second parameter to FORMAT must be a format specification string");
    return NIL;
  }

  va_start(ap, spec);

  while(i <= count - 2)
  {
    OBJECT_PTR val = (OBJECT_PTR)va_arg(ap, OBJECT_PTR);

    if(!(IS_INTEGER_OBJECT(val)        ||
         IS_FLOAT_OBJECT(val)          ||
         IS_CHAR_OBJECT(val)           ||
         IS_STRING_LITERAL_OBJECT(val) ||
         is_string_object(val)))
    {
      throw_exception1("INVALID-ARGUMENT", "Parameters to FORMAT should be integers, floats, characters or strings");
      return NIL;
    }

    if(ret == NIL)
      ret = cons(val, NIL);
    else
    {
      uintptr_t ptr = extract_ptr(last_cell(ret));
      set_heap(ptr, 1, cons(val, NIL));      
    }

    i++;
  }

  ret = cons(fd, cons(spec, ret));
        
  if(console_mode)
  {
    if(format(ret) == -1)
    {
      throw_exception1("EXCEPTION", "FORMAT error");
      return NIL;
    }
  }
  else
  {
    if(!console_mode && !single_expression_mode && !pipe_mode)
    {
      if(format_for_gui(ret) == -1)
      {
        throw_exception1("EXCEPTION", "FORMAT error");
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
    throw_exception1("INVALID-ARGUMENT", "Argument to UNBIND must be a symbol");
    return NIL;
  }

  int retval;
  OBJECT_PTR out;

  retval = get_top_level_sym_value(sym, &out);

  if(retval)
  {
    throw_exception1("SYMBOL-NOT-BOUND", "Symbol to be unbound does not exist");
    return NIL;
  }

  //int package_index = (int)sym >> (SYMBOL_BITS + OBJECT_SHIFT);
  int package_index =extract_package_index(sym);
  
  if(package_index == 0)
  {
    throw_exception1("ACCESS-VIOLATION", "Cannot unbind CORE package symbol");
    return NIL;
  }

  if(remove_top_level_sym(sym) == NOT_OK)
  {
    throw_exception1("EXCEPTION", "Error unbinding symbol");
    return NIL;
  }

  system_changed = true;

  set_up_autocomplete_words();
  
  return NIL;
}

OBJECT_PTR primitive_newline(OBJECT_PTR obj)
{
  if(obj != NIL && !IS_INTEGER_OBJECT(obj))
  {
    throw_exception1("INVALID-ARGUMENT", "NEWLINE should be passed exactly one argument: NIL or an integer denoting a file descriptor");
    return NIL;
  }

  if(obj != NIL)
  {
    int fd = get_int_value(obj);

#ifdef WIN32
    BY_HANDLE_FILE_INFORMATION info;

    if(!GetFileInformationByHandle(fd, &info))
    {
      throw_exception1("INVALID-ARGUMENT", "Invalid file descriptor");
      return;
    }
#else
    fcntl(fd, F_GETFD);

    if(errno != 0 && errno != EAGAIN)
    {
      throw_exception1("INVALID-ARGUMENT", "Invalid file descriptor");
      return NIL;
    }  
#endif

    write(fd, "\n", 1);
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
  if(obj == NIL)
    return TRUE;
  else
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

OBJECT_PTR primitive_contp(OBJECT_PTR obj)
{
  return is_continuation_object(obj) ? TRUE : NIL;
}

OBJECT_PTR primitive_string(OBJECT_PTR string_literal)
{
  if((!(IS_STRING_LITERAL_OBJECT(string_literal))))
  {
    throw_exception1("INVALID-ARGUMENT", "Argument to STRING should be a literal string");
    return NIL;
  }        
 
  return eval_string(string_literal);
}

OBJECT_PTR prim_make_array(OBJECT_PTR count1, OBJECT_PTR size, ...)
{
  va_list ap;

  unsigned int count = get_int_value(count1);

  va_start(ap, size);

  OBJECT_PTR default_value = (count > 1) ? (OBJECT_PTR)va_arg(ap, OBJECT_PTR) : NIL;

  if((!(IS_INTEGER_OBJECT(size))))
  {
    throw_exception1("INVALID-ARGUMENT", "First argument to MAKE-ARRAY should be the size of the array (integer)");
    return NIL;
  }        

  return eval_make_array(size, default_value);
}

OBJECT_PTR prim_array_set(OBJECT_PTR array_obj, OBJECT_PTR idx, OBJECT_PTR val)
{
  uintptr_t ptr;
  int array_len, index;

  ptr = extract_ptr(array_obj);

  if((!(IS_ARRAY_OBJECT(array_obj))))
  {
    throw_exception1("INVALID-ARGUMENT", "First argument to ARRAY-SET should be an array");
    return NIL;
  }        

  if((!(IS_INTEGER_OBJECT(idx))))
  {
    throw_exception1("INVALID-ARGUMENT", "Second argument to ARRAY-SET should be an integer (index into the array)");
    return NIL;
  }        

  array_len = *((uintptr_t *)ptr);

  index = get_int_value(idx);

  if(index < 0 || (index >= array_len))
  {
    throw_exception1("INDEX-OUT-OF-BOUNDS", "Array index out of bounds");
    return NIL;
  }        
 
  //if(is_dynamic_memory_object(val))
  //  insert_node(GREY, val);

  set_heap(ptr, index + 1, val);

  return val;
}

OBJECT_PTR prim_array_get(OBJECT_PTR array_obj, OBJECT_PTR idx)
{
  uintptr_t ptr;
  int index;

  ptr = extract_ptr(array_obj);

  if(!IS_INTEGER_OBJECT(idx))
  {
    throw_exception1("INVALID-ARGUMENT", "Second argument to ARRAY-GET should be an integer (index into the array)");
    return NIL;
  }        

  index = get_int_value(idx);

  if(IS_STRING_LITERAL_OBJECT(array_obj))
  {
    char *str = strings[(int)array_obj >> OBJECT_SHIFT];

    if(index < 0 || index >= strlen(str))
    {
      throw_exception1("INDEX-OUT-OF-BOUNDS", "Array index out of bounds");
      return NIL;
    }        

    return (OBJECT_PTR)((str[index] << OBJECT_SHIFT) + CHAR_TAG);
  }
  else
  {
    int array_len;

    if(!IS_ARRAY_OBJECT(array_obj))
    {
      throw_exception1("INVALID-ARGUMENT", "First argument to ARRAY-GET should be an array");
      return NIL;
    }        

    array_len = *((uintptr_t *)ptr);

    if(index < 0 || (index >= array_len))
    {
      throw_exception1("INDEX-OUT-OF-BOUNDS", "Array index out of bounds");
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
    throw_exception1("INVALID-ARGUMENT", "First argument to SUB-ARRAY should be an ARRAY object");
    return NIL;
  }

  if(!(IS_INTEGER_OBJECT(start)))
  {
    throw_exception1("INVALID-ARGUMENT", "Second argument to SUB-ARRAY should be an integer (start index)");
    return NIL;
  }

  if(!(get_int_value(start) >= 0))
  {
    throw_exception1("INVALID-ARGUMENT", "Second argument to SUB-ARRAY should be a non-negative integer");
    return NIL;
  }

  if(!(IS_INTEGER_OBJECT(array_length)))
  {
    throw_exception1("INVALID-ARGUMENT", "Third argument to SUB-ARRAY should be an integer (length of the sub-array)");
    return NIL;
  }

  if(!(get_int_value(array_length) >= 0))
  {
    throw_exception1("INVALID-ARGUMENT", "Third argument to SUB-ARRAY should be a non-negative integer");
    return NIL;
  }

  len = *((OBJECT_PTR *)extract_ptr(array));

  if((get_int_value(start) + get_int_value(array_length)) > len)
  {
    throw_exception1("INDEX-OUT-OF-BOUNDS", "Range (start, length) for SUB-ARRAY out of bounds of the array");
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
      throw_exception1("INVALID-ARGUMENT", "Argument to ARRAY-LENGTH should be an ARRAY object");
      return NIL;
    }

    return convert_int_to_object(*((OBJECT_PTR *)extract_ptr(array)));
  }
}

//TODO: Do we need this? Can't we just use the PRINT primitive?
OBJECT_PTR prim_print_string(OBJECT_PTR str)
{
  if(!(is_string_object(str)) && (!(IS_STRING_LITERAL_OBJECT(str))))
  {
    throw_exception1("INVALID-ARGUMENT", "Argument to PRINT_STRING should be a string object");
    return NIL;
  }

  print_object(str);
  fprintf(stdout, "\n");

  return NIL;
}

OBJECT_PTR prim_load_fgn_lib(OBJECT_PTR file_name)
{
#ifdef WIN32
  HMODULE *temp;
#else
  void **temp;
#endif

  char *fname;
  void *ret;

  if(!(is_string_object(file_name)) && (!(IS_STRING_LITERAL_OBJECT(file_name))))
  {
    throw_exception1("INVALID-ARGUMENT", "Argument to LOAD-FOREIGN-LIBRARY should be a string object denoting the library name");
    return NIL;
  }

  if(nof_dl_handles == MAX_FOREIGN_LIBRARY_COUNT)
  {
    throw_exception1("EXCEPTION", "Maximum number of foreign libraries has been exceeded");
    return NIL;
  }

  nof_dl_handles++;

#ifdef WIN32
  temp = (HMODULE *)GC_REALLOC(dl_handles, nof_dl_handles * sizeof(HMODULE));
#else  
  temp = (void **)GC_REALLOC(dl_handles, nof_dl_handles * sizeof(void *));
#endif

  if(temp == NULL)
  {
    throw_exception1("OUT-OF-MEMORY", "Unable to extend memory for dl_handles");
    return NIL;
  }

  dl_handles = temp;

  fname = IS_STRING_LITERAL_OBJECT(file_name) ? strings[(int)file_name >> OBJECT_SHIFT] : get_string(file_name);

  ret = open_library(fname);
  
  if(!ret)
  {
    throw_exception1("EXCEPTION", "dl_open() failed");
    return NIL;
  }

  dl_handles[nof_dl_handles - 1] = ret;

  foreign_library_names[nof_dl_handles - 1] = GC_strdup(fname);

  return NIL;
}

OBJECT_PTR prim_call_fgn_func(OBJECT_PTR fn_name, OBJECT_PTR ret_type, OBJECT_PTR args)
{
  OBJECT_PTR rest_args;

  if(!IS_STRING_LITERAL_OBJECT(fn_name) && !is_string_object(fn_name))
  {
    throw_exception1("INVALID-ARGUMENT", "First argument to CALL-FOREIGN-FUNCTION should be the funtion name (string)");
    return NIL;
  }

  char *s = get_symbol_name(ret_type);

/*   if(!(ret_type == INTEGR        || */
/*        ret_type == FLOT          || */
/* #ifdef WIN32 */
/*        ret_type == CHAR1         || */
/*        ret_type == VOID1         || */
/* #else */
/*        ret_type == CHAR          || */
/*        ret_type == VOID          || */
/* #endif */
  if(strcmp(s, "INTEGER") &&
     strcmp(s, "FLOAT") &&
     strcmp(s, "CHARACTER") &&
     strcmp(s, "VOID") &&
     strcmp(s, "CHARACTER-POINTER"))
       /* ret_type == INT_POINTER   || */ //not handling int and float pointer return values
       /* ret_type == FLOAT_POINTER || */
  {
    throw_exception1("INVALID-ARGUMENT", "Second parameter to CALL-FOREIGN-FUNCTION should be a valid return type");
    return NIL;
  }

  if(args != NIL && !IS_CONS_OBJECT(args))
  {
    throw_exception1("INVALID-ARGUMENT", "Arguments should be a list of two-element lists (argument, type) -- 1");
    return NIL;
  }

  rest_args = args;

  while(rest_args != NIL)
  {
    OBJECT_PTR car_rest_args = car(rest_args);

    OBJECT_PTR val, type;

    if(!IS_CONS_OBJECT(car_rest_args))
    {
      throw_exception1("INVALID-ARGUMENT", "Arguments should be a list of two-element lists (argument, type) -- 2");
      return NIL;
    }

    if(cons_length(car_rest_args) != 2)
    {
      throw_exception1("INVALID-ARGUMENT", "Arguments should be a list of two-element lists (argument, type) -- 3");
      return NIL;
    }

    val = CAAR(rest_args);

    if(!(IS_INTEGER_OBJECT(val)        ||
	 IS_FLOAT_OBJECT(val)          ||
	 IS_CHAR_OBJECT(val)           ||
	 IS_STRING_LITERAL_OBJECT(val) ||
	 is_string_object(val)))
    {
      throw_exception1("INVALID-ARGUMENT", "Parameter should be an integer, float, charcter, or string object");
      return NIL;
    }

    type = CADAR(rest_args);

    if(IS_SYMBOL_OBJECT(type))
      s = get_symbol_name(type);
    else
      s = NULL;

    //if(type == INTEGR)
    if(s && !strcmp(s, "INTEGER"))
    {
      if(!IS_INTEGER_OBJECT(val))
      {
	throw_exception1("INVALID-ARGUMENT", "Argument type mismatch: integer expected");
	return NIL;
      }
    }          
    //else if(type == FLOT)
    else if(s && !strcmp(s, "FLOAT"))
    {
      if(!IS_FLOAT_OBJECT(val))
      {
	throw_exception1("INVALID-ARGUMENT", "Argument type mismatch: float expected");
	return NIL;
      }
    }
#ifdef WIN32
    //else if(type == CHAR1)
    else if(s && !strcmp(s, "CHARACTER"))
#else
    //else if(type == CHAR)
    else if(s && !strcmp(s, "CHARACTER"))
#endif
    {
      if(!IS_CHAR_OBJECT(val))
      {
	throw_exception1("INVALID-ARGUMENT", "Argument type mismatch: character expected");
	return NIL;
      }
    }
    //else if(type == CHAR_POINTER)
    else if(s && !strcmp(s, "CHARACTER-POINTER"))
    {
      if(!IS_STRING_LITERAL_OBJECT(val) && !is_string_object(val))
      {
	throw_exception1("INVALID-ARGUMENT", "Argument type mismatch: string object/literal expected");
	return NIL;
      }
    }
    //else if(type == INT_POINTER)
    else if(s && !strcmp(s, "INTEGER-POINTER"))
    {
      if(!IS_INTEGER_OBJECT(val))
      {
        throw_exception1("INVALID-ARGUMENT", "Mapping a non-variable to INTEGER-POINTER / Argument type mismatch");
        return NIL;
      }
    }
    //else if(type == FLOAT_POINTER)
    else if(s && !strcmp(s, "FLOAT-POINTER"))
    {
      if(!IS_FLOAT_OBJECT(val))
      {
        throw_exception1("INVALID-ARGUMENT", "Mapping a non-variable to FLOAT-POINTER / Argument type mismatch");
        return NIL;
      }
    }
    else
    {
      throw_exception1("INVALID-ARGUMENT", "call_foreign_function(): non-primitive object type not handled");
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
    throw_exception1("INVALID-ARGUMENT", "CREATE-PACKAGE requires a string object or string literal as its argument");
    return NIL;
  }

  package_name = (char *)convert_to_upper_case(strings[(int)package >> OBJECT_SHIFT]);

  if(find_package(package_name) != NOT_FOUND)
  {
    throw_exception1("PACKAGE-ALREADY-EXISTS", "Package already exists");
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
    throw_exception1("INVALID-ARGUMENT", "IN-PACKAGE requires a string object or string literal as its argument");
    return NIL;
  }

  package_name = (char *)convert_to_upper_case(strings[(int)package >> OBJECT_SHIFT]);

  if(!strcmp(package_name,"CORE"))
  {
    throw_exception1("ACCESS-VIOLATION", "Core package cannot be updated");
    return NIL;
  }
  else
  {
    int index = find_package(package_name);
    if(index == NOT_FOUND)
    {
      throw_exception1("PACKAGE-NOT-FOUND", "Package does not exist");
      return NIL;
    }
    else
    {
      current_package = index;
      build_autocomplete_words();
      set_up_autocomplete_words();
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
    throw_exception1("INVALID-ARGUMENT", "EXPORT-PACKAGE requires a string object or string literal as its first argument");
    return NIL;
  }

  package_name = (char *)convert_to_upper_case(strings[(int)package >> OBJECT_SHIFT]);

  if(!IS_STRING_LITERAL_OBJECT(file) && !is_string_object(file))
  {
    throw_exception1("INVALID-ARGUMENT", "EXPORT-PACKAGE requires a string object or string literal as its second argument");
    return NIL;
  }

  file_name = (char *)(strings[(int)file >> OBJECT_SHIFT]);

  if(find_package(package_name) == NOT_FOUND)
  {
    throw_exception1("PACKAGE-NOT-FOUND", "Package does not exist");
    return NIL;
  }

  int index = find_package(package_name);

  FILE *fp = fopen(file_name, "w");

  if(!fp)
  {
    throw_exception1("FILE-OPEN-ERROR", "Unable to open file");
    return NIL;
  }

  if(strcmp(package_name, "USER"))
    fprintf(fp, "(create-package \"%s\")\n\n", convert_to_lower_case(package_name));

  fprintf(fp, "(in-package \"%s\")\n\n", convert_to_lower_case(package_name));

  //code for import-package statements
  int i;

  for(i=0; i<nof_pkg_import_entries; i++)
  {
    if(pkg_import_entries[i].delete_flag)
      continue;

    if(pkg_import_entries[i].pkg_index == index)
    {
      char *pkg_name = convert_to_lower_case(GC_strdup(packages[pkg_import_entries[i].imported_pkg_index].name));
      fprintf(fp, "(import-package \"%s\")\n\n", pkg_name);
      //free(pkg_name);
    }
  }
  //end of code for import-package statements


  char buf[MAX_STRING_LENGTH];

  for(i=0; i<nof_global_vars; i++)
  {
    if(top_level_symbols[i].delete_flag)
      continue;

    OBJECT_PTR sym = top_level_symbols[i].sym;

    //if(((int)sym >> (SYMBOL_BITS + OBJECT_SHIFT)) == index)
    if(extract_package_index(sym) == index)
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
      //exporting only macros and closures
      /* else if(IS_CONTINUATION_OBJECT(obj)) */
      /* { */
      /*   //not doing anything for continuation objects */
      /* } */
      /* else */
      /* { */
      /*   memset(buf, '\0', MAX_STRING_LENGTH); */
      /*   print_object_to_string(cons(DEFINE, */
      /*   			    cons(sym, cons(obj, NIL))), buf, 0); */
      /* } */

      fprintf(fp, "%s\n\n", convert_to_lower_case(buf));
    }
  } //end of for

  fclose(fp);

  return NIL;
}

OBJECT_PTR prim_import_package(OBJECT_PTR package)
{
  char *package_name;

  if(!IS_STRING_LITERAL_OBJECT(package) && !is_string_object(package))
  {
    throw_exception1("INVALID-ARGUMENT", "IMPORT-PACKAGE requires a string object or string literal as its argument");
    return NIL;
  }

  package_name = (char *)convert_to_upper_case(strings[(int)package >> OBJECT_SHIFT]);

  if(!strcmp(package_name,"CORE"))
  {
    //TODO: see if this really needs to be an exception
    throw_exception1("EXCEPTION", "Core package doesn't have to be explicitly imported");
    return NIL;
  }
  else
  {
    int index = find_package(package_name);

    if(index == current_package)
    {
      throw_exception1("EXCEPTION", "Attempting to import package into itself");
      return NIL;      
    }
    else if(index == NOT_FOUND)
    {
      throw_exception1("PACKAGE-NOT-FOUND", "Package does not exist");
      return NIL;
    }
    else
    {
      int i;

      for(i=0; i<nof_pkg_import_entries; i++)
      {
        if(pkg_import_entries[i].delete_flag)
          continue;
        
        //the to-be-imported package has already been imported earlier
        if(pkg_import_entries[i].pkg_index == current_package && pkg_import_entries[i].imported_pkg_index == index)
          return NIL;
      }
       
      nof_pkg_import_entries++;

      pkg_import_t *temp;

      if(!pkg_import_entries)
        temp = (pkg_import_t *)GC_MALLOC(nof_pkg_import_entries * sizeof(pkg_import_t));
      else
        temp = (pkg_import_t *)GC_REALLOC(pkg_import_entries, nof_pkg_import_entries * sizeof(pkg_import_t));

      assert(temp);

      pkg_import_entries = temp;

      pkg_import_entries[nof_pkg_import_entries - 1].delete_flag          = false;
      pkg_import_entries[nof_pkg_import_entries - 1].pkg_index            = current_package;
      pkg_import_entries[nof_pkg_import_entries - 1].imported_pkg_index   = index;
    }
  }

  return NIL;
}

OBJECT_PTR prim_expand_macro(OBJECT_PTR exp)
{
  return expand_macro_full(exp, false);
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
      uintptr_t ptr = extract_ptr(last_cell(ret));
      set_heap(ptr, 1, cons(val, NIL));
    }
  }

  return ret;
}

unsigned int words_alloc_for_profiled_exp;

OBJECT_PTR primitive_profile(OBJECT_PTR exp)
{
  double initial_wall_time, final_wall_time;
  clock_t initial_cpu_time, final_cpu_time;;
  unsigned int initial_mem_alloc, final_mem_alloc;
  unsigned int initial_mem_dealloc, final_mem_dealloc;

  char buf[500];

  char form[500];

  memset(form, '\0', 500);
  print_object_to_string(exp, form, 0);

  initial_wall_time = get_wall_time();
  initial_cpu_time = clock();
  //initial_mem_alloc = memory_allocated();
  //initial_mem_dealloc = memory_deallocated();

  words_alloc_for_profiled_exp = 0;
  
  OBJECT_PTR res = full_monty_eval(exp);

  /* if(is_dynamic_memory_object(res)) */
  /*   insert_node(GREY, res); */

  /* gc(false, false); */

  if(in_error)
  {
    throw_exception1("EXCEPTION", "PROFILE failed");
    return NIL;
  }

  final_wall_time = get_wall_time();
  final_cpu_time = clock();
  //final_mem_alloc = memory_allocated();
  //final_mem_dealloc = memory_deallocated();

  /* if(!console_mode && !single_expression_mode && !pipe_mode) */
  /* { */
  /*   memset(buf, '\0', 500); */
  /*   sprintf(buf, */
  /*           "Expression took %lf seconds (elapsed), %lf seconds (CPU), %d words allocated, %d words deallocated\n", */
  /*           final_wall_time - initial_wall_time, */
  /*           (final_cpu_time - initial_cpu_time) * 1.0 / CLOCKS_PER_SEC, */
  /*           final_mem_alloc - initial_mem_alloc, */
  /*           final_mem_dealloc - initial_mem_dealloc); */
  /*   print_to_transcript(buf); */
  /* } */
  /* else */
  /*   printf("Expression took %lf seconds (elapsed), %lf seconds (CPU), %d words allocated, %d words deallocated\n", */
  /*          final_wall_time - initial_wall_time, */
  /*          (final_cpu_time - initial_cpu_time) * 1.0 / CLOCKS_PER_SEC, */
  /*          final_mem_alloc - initial_mem_alloc, */
  /*          final_mem_dealloc - initial_mem_dealloc); */
  if(!console_mode && !single_expression_mode && !pipe_mode)
  {
    memset(buf, '\0', 500);
    sprintf(buf,
	    "Expression took %lf seconds (elapsed), %lf seconds (CPU), %d words allocated\n",
	    final_wall_time - initial_wall_time,
	    (final_cpu_time - initial_cpu_time) * 1.0 / CLOCKS_PER_SEC,
            words_alloc_for_profiled_exp);
    print_to_transcript(buf);
  }
  else
    printf("Expression took %lf seconds (elapsed), %lf seconds (CPU), %d words allocated\n",
           final_wall_time - initial_wall_time,
           (final_cpu_time - initial_cpu_time) * 1.0 / CLOCKS_PER_SEC,
           words_alloc_for_profiled_exp);
  
  words_alloc_for_profiled_exp = 0;
  
  return res;
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
    throw_exception1("EXCEPTION", "TIME failed");
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
  if(IS_FUNCTION2_OBJECT(obj))
  {
    //continuation objects cannot be serialized (yet)
    //because they don't have pLisp source forms that
    //are needed to recreate functions and macros.
    //continuation objects are those closures whose
    //cons equivalents have another closure object
    //in their last cell (and not the source CONS form)
    if(is_continuation_object(obj))
    {
      throw_exception1("EXCEPTION", "Continuation objects cannot be serialized");
      return NIL;
    }
  }

  if(!(is_string_object(file_name)) && (!(IS_STRING_LITERAL_OBJECT(file_name))))
  {
    throw_exception1("INVALID-ARGUMENT", "Second argument to SAVE-OBJECT should be a string object or a string literal denoting the file name");
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
    throw_exception1("INVALID-ARGUMENT", "Argument to LOAD-OBJECT should be a string object or a string literal denoting the file name");
    return NIL;
  }

  int ret;

  if(is_string_object(file_name))
    ret = deserialize(get_string(file_name));
  else
    ret = deserialize(strings[(int)file_name >> OBJECT_SHIFT]);

  if(ret == -1)
  {
    throw_exception1("EXCEPTION", "Error in LOAD-OBJECT");
    return NIL;
  }

  if(IS_FUNCTION2_OBJECT(ret) || IS_MACRO2_OBJECT(ret))
  {
    OBJECT_PTR cons_equiv = extract_ptr(ret) + CONS_TAG;
    return compile_and_evaluate(car(cons_equiv), car(cons_equiv));
  }

  return ret;
}

OBJECT_PTR prim_load_file(OBJECT_PTR file_name)
{
  FILE *temp;

  int ret;

  if(!is_string_object(file_name) && (!IS_STRING_LITERAL_OBJECT(file_name)))
  {
    throw_exception1("INVALID-ARGUMENT", "Argument to LOAD-FILE should be a string");
    return NIL;
  }

  temp = fopen(is_string_object(file_name) ?  get_string(file_name) : strings[(int)file_name >> OBJECT_SHIFT], "r");

  if(!temp)
  {
    throw_exception1("FILE-OPEN-ERROR", "LOAD-FILE unable to open file");
    return NIL;
  }

  if(set_up_new_yyin(temp))
  {
    throw_exception1("FILE-READ-ERROR", "Unable to read from file");
    return NIL;
  }

  while(!yyparse() && !in_error)
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
    throw_exception1("INVALID-ARGUMENT", "Argument to CREATE-IMAGE should be a string object or a string literal denoting the file name of the image");
    return NIL;
  }

  if(is_string_object(file_name))
  {
    char *s = get_string(file_name);
    create_image(s);
    //free(s);
  }
  else
    create_image(strings[(int)file_name >> OBJECT_SHIFT]);

  system_changed = false;

  return NIL;
}

BOOLEAN is_valid_exception_object(OBJECT_PTR obj)
{
  if(!IS_CONS_OBJECT(obj))
    return false;
  else if(!IS_SYMBOL_OBJECT(car(obj)))
    return false;
  //else if(!IS_STRING_LITERAL_OBJECT(cdr(obj)) && !is_string_object(cdr(obj)))
  else if(!IS_STRING_LITERAL_OBJECT(second(obj)) && !is_string_object(second(obj)))
    return false;

  return true;
}

OBJECT_PTR primitive_throw(OBJECT_PTR excp)
{
  if(!is_valid_exception_object(excp))
  {
    throw_exception1("INVALID-ARGUMENT", "Invalid exception object passed to THROW");
    return NIL;
  }

  exception_object = excp;
  in_error = true;
  return handle_exception();
}

OBJECT_PTR eval_string(OBJECT_PTR literal)
{
  char *str_val = strings[(int)literal >> OBJECT_SHIFT];

  char *ptr = NULL;

  unsigned int len = strlen(str_val);

  uintptr_t *raw_ptr1;

  uintptr_t raw_ptr;

  int i=1;

  assert(IS_STRING_LITERAL_OBJECT(literal));

  //see comment in main.c for why we're not using object_alloc()
  //posix_memalign((void **)&raw_ptr1, 16, sizeof(unsigned int *));
  //*((int *)raw_ptr1) = len;

  raw_ptr = object_alloc(len + 1, ARRAY_TAG);

  //set_heap(raw_ptr, 0, convert_int_to_object(len));
  //set_heap(raw_ptr, 0, (uintptr_t)raw_ptr1 + INTEGER_TAG);
  *((uintptr_t *)raw_ptr) = len;

  for(ptr=str_val;*ptr;ptr++) 
  { 
    set_heap(raw_ptr, i, (OBJECT_PTR)((*ptr << OBJECT_SHIFT) + CHAR_TAG));
    i++;
  }

  return raw_ptr + ARRAY_TAG;
}

OBJECT_PTR eval_make_array(OBJECT_PTR size, OBJECT_PTR default_value)
{
  int sz = get_int_value(size);

  uintptr_t *raw_ptr;

  uintptr_t ptr;

  int i;

  assert(IS_INTEGER_OBJECT(size));

  //see comment in main.c for why we're not using object_alloc()
  //posix_memalign((void **)&raw_ptr, 16, sizeof(unsigned int *));
  //*((int *)raw_ptr) = sz;

  ptr = object_alloc(sz+1, ARRAY_TAG);

  //set_heap(ptr, 0, size);
  //set_heap(ptr, 0, (uintptr_t)raw_ptr + INTEGER_TAG);
  *((uintptr_t *)ptr) = sz;

  for(i=0; i<sz; i++)
    set_heap(ptr, i + 1, clone_object(default_value));

  return ptr + ARRAY_TAG;
}

OBJECT_PTR eval_sub_array(OBJECT_PTR array, OBJECT_PTR start, OBJECT_PTR length)
{
  OBJECT_PTR ret;
  int st = get_int_value(start);
  int len = get_int_value(length);

  uintptr_t *raw_ptr;

  uintptr_t orig_ptr, ptr;

  int i;

  //see comment in main.c for why we're not using object_alloc()
  //posix_memalign((void **)&raw_ptr, 16, sizeof(unsigned int *));
  //*((int *)raw_ptr) = len;

  orig_ptr = extract_ptr(array);

  ptr = object_alloc(len + 1, ARRAY_TAG);

  //set_heap(ptr, 0, convert_int_to_object(len));
  //set_heap(ptr, 0, (uintptr_t)raw_ptr + INTEGER_TAG);
  *((uintptr_t *)ptr) = len;

  for(i=1; i<=len; i++)
    set_heap(ptr, i, get_heap(orig_ptr, st + i));

  ret = ptr;

  return ret + ARRAY_TAG;
}

void raise_error(char *err_str)
{
  if(!console_mode && !single_expression_mode && !pipe_mode)
  {
    show_error_dialog(err_str);
  }
  else
    fprintf(stdout, "%s\n", err_str);

  //to stay commented out till we are
  //able to prpvide a meaningful backtrace
  //fprintf(stdout, "Begin backtrace\n");
  //print_backtrace();
  //fprintf(stdout, "End backtrace\n");

  in_error = true;
  //exception_object = cons(get_symbol_object("EXCEPTION"), get_string_object(err_str));
  exception_object = list(2, get_symbol_object("EXCEPTION"), get_string_object(err_str));
}

void throw_generic_exception(char *err_str)
{
  throw_exception1("EXCEPTION", err_str);
}

OBJECT_PTR prim_get_source(OBJECT_PTR obj)
{
  if(!IS_FUNCTION2_OBJECT(obj) && !IS_MACRO2_OBJECT(obj))
  {
    throw_exception1("Exception", "GET-SOURCE expects a closure or macro object as argument");
    return NIL;
  }

  return cons(IS_FUNCTION2_OBJECT(obj) ? LAMBDA : MACRO,
              cons(get_params_object(obj),get_source_object(obj)));
}

extern void create_object_inspector_window(OBJECT_PTR);

OBJECT_PTR prim_inspect_object(OBJECT_PTR obj)
{
  create_object_inspector_window(obj);
  return NIL;
}
