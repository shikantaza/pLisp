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

#include <stdio.h>
#include <dlfcn.h>
#include <ffi.h>
#include <assert.h>

#include "plisp.h"

#include "memory.h"

typedef OBJECT_PTR (*nativefn)(OBJECT_PTR, ...);

extern OBJECT_PTR CAAR(OBJECT_PTR);
extern OBJECT_PTR CADAR(OBJECT_PTR);

extern int nof_dl_handles;
extern void **dl_handles;
extern char err_buf[];

extern OBJECT_PTR NIL;

extern char **strings;

extern OBJECT_PTR reg_current_env;

extern OBJECT_PTR INTEGR;
extern OBJECT_PTR FLOT;
extern OBJECT_PTR CHAR;
extern OBJECT_PTR VOID;
extern OBJECT_PTR INT_POINTER;
extern OBJECT_PTR FLOAT_POINTER;
extern OBJECT_PTR CHAR_POINTER;

extern BOOLEAN console_mode, single_expression_mode, pipe_mode;

extern OBJECT_PTR idclo;

extern unsigned int POINTER_MASK;

void free_arg_values(ffi_type **, void **, OBJECT_PTR, int);
void free_arg_values_for_format(ffi_type **, void **, OBJECT_PTR, int);

//all error-checking is done by eval() before calling
//call_foreign_function
OBJECT_PTR call_foreign_function(OBJECT_PTR fn_name, OBJECT_PTR ret_type, OBJECT_PTR args)
{
  char *fn_name_str;

  if(IS_STRING_LITERAL_OBJECT(fn_name))
    fn_name_str = strings[(int)fn_name >> OBJECT_SHIFT];
  else
    fn_name_str = get_string(fn_name);

  void (*function_ptr)(void);

  int i;

  for(i=0;i<nof_dl_handles; i++)
  {
    function_ptr = dlsym(dl_handles[i], fn_name_str);
    if(function_ptr != NULL)
      break;
  }
  
  if(function_ptr == NULL)
  {
    throw_generic_exception("Foreign function not found");
    return NIL;
  }

  int nof_args = cons_length(args);

  ffi_type **arg_types = (ffi_type **)malloc(nof_args * sizeof(ffi_type *));
  if(!arg_types)
  {
    throw_generic_exception("Unable to allocate memory for argument types buffer");
    return NIL;
  }

  void **arg_values = (void **)malloc(nof_args * sizeof(void *));
  if(!arg_values)
  {
    throw_generic_exception("Unable to allocate memory for argument values buffer");
    free(arg_types);
    return NIL;
  }
  
  OBJECT_PTR rest_args = args;

  i=0;

  int   i_val, *i_val_ptr;
  float f_val, *f_val_ptr;
  char  c_val, *c_val_ptr;

  while(rest_args != NIL)
  {
    OBJECT_PTR car_rest_args = car(rest_args);

    OBJECT_PTR val = CAAR(rest_args);

    OBJECT_PTR type = CADAR(rest_args);

    if(IS_SYMBOL_OBJECT(val))
    {
#ifdef INTERPRETER_MODE
      OBJECT_PTR res = get_symbol_value(val, reg_current_env);
      val = cdr(res);
#else
      OBJECT_PTR retval, out;
      retval = get_top_level_sym_value(val, &out);
      if(retval)
      {
	char buf[SYMBOL_STRING_SIZE];
        char err_buf[500];
	print_qualified_symbol(val, buf);

        memset(err_buf, '\0', 500);
	sprintf(err_buf, "Symbol not bound(3): %s", buf);

	throw_exception1("EXCEPTION", err_buf);
	return NIL;
      }
      val = car(out);
#endif
    }

    if(type == INTEGR) //INTEGER spelt wrongly because it's already bean #define'd
    {
      arg_types[i] = &ffi_type_sint;
      i_val = get_int_value(val);
      arg_values[i] = (int *)malloc(sizeof(int));
      *(int *)arg_values[i] = i_val;
    }
    else if(type == FLOT) //FLOAT spelt wrongly because it's already bean #define'd
    {
      arg_types[i] = &ffi_type_float;
      f_val = get_float_value(val);
      arg_values[i] = (float *)malloc(sizeof(float));
      *(float *)arg_values[i] = f_val;
    }
    else if(type == CHAR)
    {
      arg_types[i] = &ffi_type_schar;
      c_val = (int)val >> OBJECT_SHIFT;
      arg_values[i] = (char *)malloc(sizeof(char));
      *(char *)arg_values[i] = c_val;
    }
    else if(type == CHAR_POINTER)
    {
      arg_types[i] = &ffi_type_pointer;
      c_val_ptr = IS_STRING_LITERAL_OBJECT(val) ? strdup(strings[(int)val >> OBJECT_SHIFT]) : get_string(val);
      arg_values[i] = (char **)malloc(sizeof(char *));
      *(char **)arg_values[i] = c_val_ptr;
    }
    else if(type == INT_POINTER)
    {
      arg_types[i] = &ffi_type_pointer;
      i_val_ptr = (int *)malloc(sizeof(int));
      *i_val_ptr = get_int_value(val);
      arg_values[i] = (int **)malloc(sizeof(int *));
      *(int **)arg_values[i] = i_val_ptr;
    }
    else if(type == FLOAT_POINTER)
    {
      arg_types[i] = &ffi_type_pointer;
      f_val_ptr = (float *)malloc(sizeof(float));
      *f_val_ptr = get_float_value(val);
      arg_values[i] = (float **)malloc(sizeof(float *));
      *(float **)arg_values[i] = f_val_ptr;
    }

    i++;
    rest_args = cdr(rest_args);
  }

  ffi_cif cif;
  ffi_status status;

  if(ret_type == INTEGR)
    status = ffi_prep_cif(&cif, FFI_DEFAULT_ABI, nof_args, &ffi_type_sint, arg_types);
  else if(ret_type == CHAR)
    status = ffi_prep_cif(&cif, FFI_DEFAULT_ABI, nof_args, &ffi_type_schar, arg_types);
  else if(ret_type == FLOT)
    status = ffi_prep_cif(&cif, FFI_DEFAULT_ABI, nof_args, &ffi_type_float, arg_types);
  else if(ret_type == CHAR_POINTER) //not handling int and float pointer return values
    status = ffi_prep_cif(&cif, FFI_DEFAULT_ABI, nof_args, &ffi_type_pointer, arg_types);
  else if(ret_type == VOID)
    status = ffi_prep_cif(&cif, FFI_DEFAULT_ABI, nof_args, &ffi_type_void, arg_types);

  if(status != FFI_OK)
  {
    throw_generic_exception("call_foreign_function(): ffi_prep_cif() failed");
    free_arg_values(arg_types, arg_values, args, nof_args);
    free(arg_values);
    free(arg_types);
    return NIL;
  }

  ffi_arg ret_val;
  float float_ret_val;

  if(ret_type == FLOT)
    ffi_call(&cif, function_ptr, &float_ret_val, arg_values);
  else
    ffi_call(&cif, function_ptr, &ret_val, arg_values);

  //processing IN-OUT parameters

  i=0;
  rest_args = args;

  while(rest_args != NIL)
  {
    OBJECT_PTR sym = CAAR(rest_args);
    OBJECT_PTR type = CADAR(rest_args);
    OBJECT_PTR value;

#ifdef INTERPRETER_MODE
    if(type == INT_POINTER && IS_SYMBOL_OBJECT(sym))
#else
    if(type == INT_POINTER)
#endif
    {

#ifdef INTERPRETER_MODE
      value = convert_int_to_object(*((int *)*(int **)arg_values[i]));

      if(update_environment(reg_current_env, sym, value) == NIL)
      {
        throw_generic_exception("update_environment failed");
        free_arg_values(arg_types, arg_values, args, nof_args);
        free(arg_values);
        free(arg_types);
        return NIL;
      }
#else
      *((int *)(sym & POINTER_MASK)) = *((int *)*(int **)arg_values[i]);
#endif
    }

#ifdef INTERPRETER_MODE
    else if(type == FLOAT_POINTER && IS_SYMBOL_OBJECT(sym))
#else
    else if(type == FLOAT_POINTER)
#endif
    {

#ifdef INTERPRETER_MODE
      value = convert_float_to_object(*((float *)*(float **)arg_values[i]));

      if(update_environment(reg_current_env, sym, value) == NIL)
      {
        throw_generic_exception("update_environment failed");
        free_arg_values(arg_types, arg_values, args, nof_args);
        free(arg_values);
        free(arg_types);
        return NIL;
      }
#else
      *((float *)(sym & POINTER_MASK)) = *((float *)*(float **)arg_values[i]);
#endif

    }

#ifdef INTERPRETER_MODE
    else if(type == CHAR_POINTER && IS_SYMBOL_OBJECT(sym))
    {
      char *str = *(char **)arg_values[i];

      int sz = strlen(str);

      uintptr_t ptr = object_alloc(sz+1, ARRAY_TAG);

      *((unsigned int *)ptr) = sz;

      int j;
      for(j=0; j< sz; j++)
        set_heap(ptr, j + 1, (OBJECT_PTR)((str[j] << OBJECT_SHIFT) + CHAR_TAG));

      if(update_environment(reg_current_env, sym, ptr+ARRAY_TAG) == NIL)
      {
        throw_generic_exception("update_environment failed");
        free_arg_values(arg_types, arg_values, args, nof_args);
        free(arg_values);
        free(arg_types);
        return NIL;
      }
    }
#else
    else if(type == CHAR_POINTER)
    {
      char *str = *(char **)arg_values[i];

      int sz = strlen(str);

      if(sz > 100)
      {
        throw_exception1("EXCEPTION", "Maximum string length permitted for FFI parameters exceeded");
        free_arg_values(arg_types, arg_values, args, nof_args);
        free(arg_values);
        free(arg_types);
        return NIL;        
      }

      uintptr_t ptr = sym & POINTER_MASK;

      int orig_len = *((unsigned int *)ptr);

      if(sz > *((unsigned int *)ptr))
      {
        throw_exception1("EXCEPTION", "Attempting to write beyond the size of the string");
        free_arg_values(arg_types, arg_values, args, nof_args);
        free(arg_values);
        free(arg_types);
        return NIL;
      }

      *((unsigned int *)ptr) = sz;

      int j;
      for(j=0; j< sz; j++)
        set_heap(ptr, j + 1, (OBJECT_PTR)((str[j] << OBJECT_SHIFT) + CHAR_TAG));

      for(j=sz+1; j<orig_len+1; j++)
      {
        OBJECT_PTR temp = get_heap(sym & POINTER_MASK, j);
        if(is_dynamic_memory_object(temp))
          dealloc(temp);
      }
    }
#endif

    rest_args = cdr(rest_args);
    i++;
  }

  OBJECT_PTR ret;

  if(ret_type == INTEGR)
    ret = convert_int_to_object((int)ret_val);
  else if(ret_type == CHAR)
    ret = (OBJECT_PTR)(((char)ret_val << OBJECT_SHIFT) + CHAR_TAG);
  else if(ret_type == FLOT)
    ret = convert_float_to_object(float_ret_val);
  else if(ret_type == CHAR_POINTER)
  {

    char *str = (char *)ret_val;

    int sz = strlen(str);

    //see comment in main.c for why we're not using object_alloc()
    //unsigned int *raw_ptr;
    //posix_memalign((void **)&raw_ptr, 16, sizeof(unsigned int *));
    //*((int *)raw_ptr) = sz;

    uintptr_t ptr = object_alloc(sz+1, ARRAY_TAG);

    //heap[ptr] = convert_int_to_object(sz);
    //set_heap(ptr, 0, convert_int_to_object(sz));
    //set_heap(ptr, 0, (uintptr_t)raw_ptr + INTEGER_TAG);
    *((unsigned int *)ptr) = sz;

    for(i=0; i< sz; i++)
      //heap[ptr + i + 1] = (str[i] << OBJECT_SHIFT) + CHAR_TAG;
      set_heap(ptr, i + 1, (OBJECT_PTR)((str[i] << OBJECT_SHIFT) + CHAR_TAG));

    free(str);

    ret = ptr + ARRAY_TAG;

  }
  else if(ret_type == VOID)
    ret = NIL;

  free_arg_values(arg_types, arg_values, args, nof_args);

  free(arg_values);
  free(arg_types);

  return ret;
}

void free_arg_values(ffi_type **types, void **values, OBJECT_PTR args, int nargs)
{
  OBJECT_PTR rest = args;

  int i;

  for(i=0; i<nargs; i++)
  {    
    OBJECT_PTR type = CADAR(rest);

    if(types[i] == &ffi_type_pointer)
    {
      if(type == INT_POINTER)
        free(*(int **)values[i]);
      else if(type  == FLOAT_POINTER)
        free(*(float **)values[i]);
      else if(type == CHAR_POINTER)
        free(*(char **)values[i]);
    }
    else if(types[i] == &ffi_type_sint)
      free((int *)values[i]);
    else if(types[i] == &ffi_type_float)
      free((float *)values[i]);
    else if(types[i] == &ffi_type_schar)
      free((char *)values[i]);

    rest = cdr(rest);
  }
}

//for handling FORMAT statements. Floats
//need to be converted to double to make
//varargs work in libffi, so we don't
//want to use call_foreign_function.
//with a little work, the existing code
//could have been shoehorned to handle this,
//but this is simpler and cleaner
int format(OBJECT_PTR args)
{
  int nof_args = cons_length(args);
  int i;

  ffi_type **arg_types = (ffi_type **)malloc(nof_args * sizeof(ffi_type *));
  if(!arg_types)
  {
    throw_generic_exception("Unable to allocate memory for argument types buffer");
    return -1;
  }

  void **arg_values = (void **)malloc(nof_args * sizeof(void *));
  if(!arg_values)
  {
    throw_generic_exception("Unable to allocate memory for argument values buffer");
    free(arg_types);
    return -1;
  }

  int    i_val, *i_val_ptr;
  double d_val, *d_val_ptr;
  char   c_val, *c_val_ptr;

  i=0;

  arg_types[i] = &ffi_type_sint;
  arg_values[i] = (int *)malloc(sizeof(int));

  if(car(args) == NIL)
    i_val = stdout;
  else
    //i_val = get_int_value(car(args));
    i_val = fdopen(get_int_value(car(args)), "w"); //append mode node supported for format

  *(int *)arg_values[i] = i_val;

  i++;

  OBJECT_PTR rest_args = cdr(args);

  while(rest_args != NIL)
  {
    OBJECT_PTR val = car(rest_args);

    if(IS_INTEGER_OBJECT(val))
    {
      arg_types[i] = &ffi_type_sint;
      i_val = get_int_value(val);
      arg_values[i] = (int *)malloc(sizeof(int));
      *(int *)arg_values[i] = i_val;
    }
    else if(IS_FLOAT_OBJECT(val))
    {
      arg_types[i] = &ffi_type_double;
      d_val = get_float_value(val);
      arg_values[i] = (double *)malloc(sizeof(double));
      *(double *)arg_values[i] = d_val;
    }
    else if(IS_CHAR_OBJECT(val))
    {
      arg_types[i] = &ffi_type_schar;
      c_val = (int)val >> OBJECT_SHIFT;
      arg_values[i] = (char *)malloc(sizeof(char));
      *(char *)arg_values[i] = c_val;
    }
    else if(IS_STRING_LITERAL_OBJECT(val) || is_string_object(val))
    {
      arg_types[i] = &ffi_type_pointer;
      c_val_ptr = IS_STRING_LITERAL_OBJECT(val) ? strdup(strings[(int)val >> OBJECT_SHIFT]) : get_string(val);
      arg_values[i] = (char **)malloc(sizeof(char *));
      *(char **)arg_values[i] = c_val_ptr;
    }

    i++;
    rest_args = cdr(rest_args);
  }
  
  ffi_cif cif;
  ffi_status status;

  status = ffi_prep_cif(&cif, FFI_DEFAULT_ABI, nof_args, &ffi_type_void, arg_types);

  if(status != FFI_OK)
  {
    throw_generic_exception("format(): ffi_prep_cif() failed");
    free_arg_values(arg_types, arg_values, args, nof_args);
    free(arg_values);
    free(arg_types);
    return -1;
  }

  ffi_arg ret_val;

  ffi_call(&cif, (void *)fprintf, &ret_val, arg_values);

  free_arg_values_for_format(arg_types, arg_values, args, nof_args);

  free(arg_values);
  free(arg_types);

  return 0;
}

void free_arg_values_for_format(ffi_type **types, void **values, OBJECT_PTR args, int nargs)
{
  OBJECT_PTR rest = args;

  int i;

  for(i=0; i<nargs; i++)
  {
    OBJECT_PTR val = car(rest);

    if(types[i] == &ffi_type_pointer)
    {
      if(IS_STRING_LITERAL_OBJECT(val) || is_string_object(val))
        free(*(char **)values[i]);
    }
    else if(types[i] == &ffi_type_sint)
      free((int *)values[i]);
    else if(types[i] == &ffi_type_float)
      free((float *)values[i]);
    else if(types[i] == &ffi_type_schar)
      free((char *)values[i]);

    rest = cdr(rest);
  }
}

int format_for_gui(OBJECT_PTR args)
{
  int nof_args = cons_length(args);
  int i;

  ffi_type **arg_types = (ffi_type **)malloc(nof_args * sizeof(ffi_type *));
  if(!arg_types)
  {
    throw_generic_exception("Unable to allocate memory for argument types buffer");
    return -1;
  }

  void **arg_values = (void **)malloc(nof_args * sizeof(void *));
  if(!arg_values)
  {
    throw_generic_exception("Unable to allocate memory for argument values buffer");
    free(arg_types);
    return -1;
  }

  int    i_val, *i_val_ptr;
  double d_val, *d_val_ptr;
  char   c_val, *c_val_ptr;

  char buf[MAX_STRING_LENGTH];
  memset(buf, '\0', MAX_STRING_LENGTH);

  if(car(args) == NIL)
  {
    arg_types[0] = &ffi_type_pointer;
    arg_values[0] = (char **)malloc(sizeof(char *));
    *(char **)arg_values[0] = buf;
  }
  else
  {
    arg_types[0] = &ffi_type_sint;
    arg_values[0] = (int *)malloc(sizeof(int));

    //i_val = get_int_value(car(args));
    i_val = fdopen(get_int_value(car(args)), "w"); //append mode node supported for format
    *(int *)arg_values[0] = i_val;
  }

  OBJECT_PTR rest_args = cdr(args);

  i=1;

  while(rest_args != NIL)
  {
    OBJECT_PTR val = car(rest_args);

    if(IS_INTEGER_OBJECT(val))
    {
      arg_types[i] = &ffi_type_sint;
      i_val = get_int_value(val);
      arg_values[i] = (int *)malloc(sizeof(int));
      *(int *)arg_values[i] = i_val;
    }
    else if(IS_FLOAT_OBJECT(val))
    {
      arg_types[i] = &ffi_type_double;
      d_val = get_float_value(val);
      arg_values[i] = (double *)malloc(sizeof(double));
      *(double *)arg_values[i] = d_val;
    }
    else if(IS_CHAR_OBJECT(val))
    {
      arg_types[i] = &ffi_type_schar;
      c_val = (int)val >> OBJECT_SHIFT;
      arg_values[i] = (char *)malloc(sizeof(char));
      *(char *)arg_values[i] = c_val;
    }
    else if(IS_STRING_LITERAL_OBJECT(val) || is_string_object(val))
    {
      arg_types[i] = &ffi_type_pointer;
      c_val_ptr = IS_STRING_LITERAL_OBJECT(val) ? strdup(strings[(int)val >> OBJECT_SHIFT]) : get_string(val);
      arg_values[i] = (char **)malloc(sizeof(char *));
      *(char **)arg_values[i] = c_val_ptr;
    }

    i++;
    rest_args = cdr(rest_args);
  }
 
  ffi_cif cif;
  ffi_status status;

  status = ffi_prep_cif(&cif, FFI_DEFAULT_ABI, nof_args, &ffi_type_void, arg_types);

  if(status != FFI_OK)
  {
    throw_generic_exception("format(): ffi_prep_cif() failed");
    free_arg_values(arg_types, arg_values, args, nof_args);
    free(arg_values);
    free(arg_types);
    return -1;
  }

  ffi_arg ret_val;

  if(car(args) == NIL)
    ffi_call(&cif, (void *)sprintf, &ret_val, arg_values);
  else
    ffi_call(&cif, (void *)fprintf, &ret_val, arg_values);

  free_arg_values_for_format(arg_types+1, arg_values+1, args, nof_args-1);

  free(arg_values);
  free(arg_types);

  if(car(args) == NIL)
    print_to_transcript(buf);

  return 0;
}

OBJECT_PTR apply_macro_or_fn(OBJECT_PTR macro_or_fn_obj, OBJECT_PTR args)
{
  assert(IS_MACRO2_OBJECT(macro_or_fn_obj) || IS_FUNCTION2_OBJECT(macro_or_fn_obj));
  assert(args == NIL || IS_CONS_OBJECT(args));

  int nof_args = cons_length(args) + 2; //since the closure (macro object) will be the first argument, and the id closure will be the last argument
  int i;

  ffi_type **arg_types = (ffi_type **)malloc(nof_args * sizeof(ffi_type *));
  if(!arg_types)
  {
    raise_error("Error in invoking macro - malloc() failed (1)");
    return NIL;
  }

  void **arg_values = (void **)malloc(nof_args * sizeof(void *));
  if(!arg_values)
  {
    raise_error("Error in invoking macro - malloc() failed (2)");
    free(arg_types);
    return NIL;
  }

  i=0;

  arg_types[i] = &ffi_type_uint;
  arg_values[i] = (unsigned int *)malloc(sizeof(unsigned int));

  //the first parameter to the native function is
  //the closure (macro object) itself
  *(int *)arg_values[i] = macro_or_fn_obj;

  i++;

  OBJECT_PTR rest_args = args;

  while(rest_args != NIL)
  {
    arg_types[i] = &ffi_type_uint;
    arg_values[i] = (unsigned int *)malloc(sizeof(unsigned int));
    *(int *)arg_values[i] = car(rest_args);

    i++;
    rest_args = cdr(rest_args);
  }

  arg_types[i] = &ffi_type_uint;
  arg_values[i] = (unsigned int *)malloc(sizeof(unsigned int));
  *(int *)arg_values[i] = idclo;
  
  ffi_cif cif;
  ffi_status status;

  status = ffi_prep_cif(&cif, FFI_DEFAULT_ABI, nof_args, &ffi_type_uint, arg_types);

  if(status != FFI_OK)
  {
    raise_error("Error in invoking macro - ffi_prep_cif() failed");

    for(i=0; i<nof_args; i++)
      free(arg_values[i]);

    free(arg_values);
    free(arg_types);

    return NIL;
  }

  ffi_arg ret_val;

  ffi_call(&cif, (void *)extract_native_fn(macro_or_fn_obj), &ret_val, arg_values);

  for(i=0; i<nof_args; i++)
    free(arg_values[i]);

  free(arg_values);
  free(arg_types);

  assert(is_valid_object((int)ret_val));

  return (int)ret_val;
}
