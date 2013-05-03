#include <dlfcn.h>
#include <ffi.h>

#include "plisp.h"

extern int nof_dl_handles;
extern void **dl_handles;
extern char err_buf[];

extern OBJECT_PTR NIL;

extern char **strings;
extern BOOLEAN in_exception;
extern RAW_PTR *heap;

extern OBJECT_PTR reg_current_env;

OBJECT_PTR call_foreign_function(OBJECT_PTR fn_name, OBJECT_PTR ret_type, OBJECT_PTR args)
{
  char *fn_name_str = strings[fn_name >> OBJECT_SHIFT];

  ffi_cif cif;

  int nof_args = length(args);

  ffi_type **arg_types = (ffi_type **)malloc(nof_args * sizeof(ffi_type *));
  if(!arg_types)
  {
    raise_error("Unable to allocate memory for argument types buffer");
    return NIL;
  }

  void **arg_values = (void **)malloc(nof_args * sizeof(void *));
  if(!arg_values)
  {
    raise_error("Unable to allocate memory for argument values buffer");
    free(arg_types);
    return NIL;
  }
  
  ffi_arg ret_val;
  float float_ret; //using ffi_arg for float return values results in junk data for some reason
  OBJECT_PTR ret;
  
  OBJECT_PTR rest_args = args;

  int i=0;

  while(rest_args != NIL)
  {
    OBJECT_PTR val = CAAR(rest_args);

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

    OBJECT_PTR type = CADAR(rest_args);

    if(IS_SYMBOL_OBJECT(val))
    {
      OBJECT_PTR res = get_symbol_value(val, reg_current_env);
      if(car(res) != NIL)
        val = cdr(res);
      else
      {
	char buf[SYMBOL_STRING_SIZE];
	print_symbol(val, buf);
	sprintf(err_buf, "Symbol not bound: %s", buf);
        raise_error(err_buf);
        return NIL;
      }
    }

    if(equal(type, get_symbol_object("INTEGER")))
    {
      if(!IS_INTEGER_OBJECT(val))
      {
	raise_error("Argument type mismatch: integer expected");

        //we get a 'sig_send: wait for sig_complete event failed, signal 6, rc 258' error if
        //we try to free arg_values elements that are strings
        /*
        int j=0;

        for(j=0; j<i; j++)
        {
	  if(arg_types[j] == &ffi_type_pointer)
	    free(arg_values[j]);
        }
        free(arg_values);
        free(arg_types);
        */
	
	return NIL;
      }
      arg_types[i] = &ffi_type_sint;
      int i_val = get_int_value(val);
      *(int *)arg_values[i] = i_val;
    }
    else if(equal(type, get_symbol_object("FLOAT")))
    {

      if(!IS_FLOAT_OBJECT(val))
      {
	raise_error("Argument type mismatch: float expected");

        /*
        int j=0;

        for(j=0; j<i; j++)
        {
	  if(arg_types[j] == &ffi_type_pointer)
	    free(arg_values[j]);
        }
        free(arg_values);
        free(arg_types);
        */
	
	return NIL;
      }

      arg_types[i] = &ffi_type_float;
      float f_val = get_float_value(val);
      *(float *)arg_values[i] = f_val;
    }
    else if(equal(type, get_symbol_object("CHARACTER")))
    {

      if(!IS_CHAR_OBJECT(val))
      {
	raise_error("Argument type mismatch: character expected");

        /*
        int j=0;

        for(j=0; j<i; j++)
        {
	  if(arg_types[j] == &ffi_type_pointer)
	    free(arg_values[j]);
        }
        free(arg_values);
        free(arg_types);
        */
	
	return NIL;
      }

      arg_types[i] = &ffi_type_schar;
      char c_val = val >> OBJECT_SHIFT;
      arg_values[i] = &c_val;
    }
    else if(equal(type, get_symbol_object("CHARACTER-POINTER")))
    {
      if(!IS_STRING_LITERAL_OBJECT(val) && !is_string_object(val))
      {
	raise_error("Argument type mismatch: string object/literal expected");

        /*
        int j=0;

        for(j=0; j<i; j++)
        {
	  if(arg_types[j] == &ffi_type_pointer)
	    free(arg_values[j]);
        }
        free(arg_values);
        free(arg_types);
        */
	
	return NIL;
      }

      arg_types[i] = &ffi_type_pointer;
      char *s_val = IS_STRING_LITERAL_OBJECT(val) ? strdup(strings[val >> OBJECT_SHIFT]) : get_string(val);
      arg_values[i] = &s_val;
    }
    else if(equal(type, get_symbol_object("INTEGER-POINTER")))
    {
      if(!IS_SYMBOL_OBJECT(CAAR(rest_args)) || !IS_INTEGER_OBJECT(val))
      {
	raise_error("Mapping a non-variable to INTEGER-POINTER / Argument type mismatch");

	/*
	int j=0;

	for(j=0; j<i; j++)
	{
          if(arg_types[j] == &ffi_type_pointer)
	    free(arg_values[j]);
	}
        free(arg_values);
        free(arg_types);
        */
	
	return NIL;
      }

      arg_types[i] = &ffi_type_pointer;
      int *i_val = (int *)malloc(sizeof(int));
      *i_val = get_int_value(val);
      arg_values[i] = &i_val;
    }
    else if(equal(type, get_symbol_object("FLOAT-POINTER")))
    {
      if(!IS_SYMBOL_OBJECT(CAAR(rest_args)) || !IS_FLOAT_OBJECT(val))
      {
	raise_error("Mapping a non-variable to FLOAT-POINTER / Argument type mismatch");

	/*
	int j=0;

	for(j=0; j<i; j++)
	{
          if(arg_types[j] == &ffi_type_pointer)
	    free(arg_values[j]);
	}
        free(arg_values);
        free(arg_types);
        */
	
	return NIL;
      }

      arg_types[i] = &ffi_type_pointer;
      float *f_val = (float *)malloc(sizeof(float));
      *f_val = get_float_value(val);
      arg_values[i] = &f_val;
    }
    else
    {
      raise_error("call_foreign_function(): non-primitive object type not handled");

      /*
      int j=0;

      for(j=0; j<i; j++)
      {
	if(arg_types[j] == &ffi_type_pointer)
	  free(arg_values[j]);
      }
      free(arg_values);
      free(arg_types);
      */

      return NIL;
    }

    i++;
    rest_args = cdr(rest_args);
  }

  ffi_status status;

  if(equal(ret_type, get_symbol_object("INTEGER")))
    status = ffi_prep_cif(&cif, FFI_DEFAULT_ABI, nof_args, &ffi_type_sint, arg_types);
  else if(equal(ret_type, get_symbol_object("CHARACTER")))
    status = ffi_prep_cif(&cif, FFI_DEFAULT_ABI, nof_args, &ffi_type_schar, arg_types);
  else if(equal(ret_type, get_symbol_object("FLOAT")))
    status = ffi_prep_cif(&cif, FFI_DEFAULT_ABI, nof_args, &ffi_type_float, arg_types);
  else if(equal(ret_type, get_symbol_object("CHARACTER-POINTER")))
    status = ffi_prep_cif(&cif, FFI_DEFAULT_ABI, nof_args, &ffi_type_pointer, arg_types);
  else if(equal(ret_type, get_symbol_object("VOID")))
    status = ffi_prep_cif(&cif, FFI_DEFAULT_ABI, nof_args, &ffi_type_void, arg_types);
  else
  {
    raise_error("Unknown return type");

    /*
    for(i=0; i<nof_args; i++)
    {
      if(arg_types[i] == &ffi_type_pointer)
	free(arg_values[i]);
    }
    free(arg_values);
    free(arg_types);
    */

    return NIL;
  }

  if(status != FFI_OK)
  {
    raise_error("call_foreign_function(): ffi_prep_cif() failed");

    /*
    for(i=0; i<nof_args; i++)
    {
      if(arg_types[i] == &ffi_type_pointer)
	free(arg_values[i]);
    }
    free(arg_values);
    free(arg_types);
    */

    return NIL;
  }

  void (*function_ptr)(void);

  for(i=0;i<nof_dl_handles; i++)
  {
    function_ptr = dlsym(dl_handles[i], fn_name_str);
    if(function_ptr != NULL)
      break;
  }
  
  if(function_ptr == NULL)
  {
    raise_error("Foreign function not found");

    /*
    for(i=0; i<nof_args; i++)
    {
      if(arg_types[i] == &ffi_type_pointer)
	free(arg_values[i]);
    }
    free(arg_values);
    free(arg_types);
    */

    return NIL;
  }

  if(equal(ret_type, get_symbol_object("FLOAT")))
    ffi_call(&cif, function_ptr, &float_ret, arg_values);
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

    if(equal(type, get_symbol_object("INTEGER-POINTER")))
    {
      value = convert_int_to_object(*((int *)*(int **)arg_values[i]));

      if(update_environment(reg_current_env, sym, value) == NIL)
      {
	raise_error("update_environment failed");

	/*
	for(i=0; i<nof_args; i++)
	{
	  if(arg_types[i] == &ffi_type_pointer)
	     free(arg_values[i]);
        }
        free(arg_values);
        free(arg_types);
        */
	return NIL;
      }
    }
    else if(equal(type, get_symbol_object("FLOAT-POINTER")))
    {
      value = convert_float_to_object(*((float *)*(float **)arg_values[i]));

      if(update_environment(reg_current_env, sym, value) == NIL)
      {
	raise_error("update_environment failed");

	/*
	for(i=0; i<nof_args; i++)
	{
	  if(arg_types[i] == &ffi_type_pointer)
	     free(arg_values[i]);
        }
        free(arg_values);
        free(arg_types);
        */
	return NIL;
      }
    }
    else if(equal(type, get_symbol_object("CHARACTER-POINTER"))
	    && IS_SYMBOL_OBJECT(sym))
      //&& is_string_object(eval(CAAR(rest_args), env_list)))
    {
      char *str = *(char **)arg_values[i];

      int sz = strlen(str);

      RAW_PTR ptr = object_alloc(sz+1);

      heap[ptr] = convert_int_to_object(sz);

      int j;
      for(j=0; j< sz; j++)
	heap[ptr + j + 1] = (str[j] << OBJECT_SHIFT) + CHAR_TAG;

      free(str);

      if(update_environment(reg_current_env, sym, (ptr << OBJECT_SHIFT) + ARRAY_TAG) == NIL)
      {
	raise_error("update_environment failed");

	/*
	for(i=0; i<nof_args; i++)
	{
	  if(arg_types[i] == &ffi_type_pointer)
	     free(arg_values[i]);
        }
        free(arg_values);
        free(arg_types);
        */
	return NIL;
      }
    }

    rest_args = cdr(rest_args);
    i++;
  }

  if(equal(ret_type, get_symbol_object("INTEGER")))
    ret = convert_int_to_object((int)ret_val);
  else if(equal(ret_type, get_symbol_object("CHARACTER")))
    ret = ((char)ret_val << OBJECT_SHIFT) + CHAR_TAG;
  else if(equal(ret_type, get_symbol_object("FLOAT")))
    ret = convert_float_to_object((float)float_ret);
  else if(equal(ret_type, get_symbol_object("CHARACTER-POINTER")))
  {

    char *str = (char *)ret_val;

    int sz = strlen(str);

    RAW_PTR ptr = object_alloc(sz+1);

    heap[ptr] = convert_int_to_object(sz);

    for(i=0; i< sz; i++)
      heap[ptr + i + 1] = (str[i] << OBJECT_SHIFT) + CHAR_TAG;

    free(str);

    ret = (ptr << OBJECT_SHIFT) + ARRAY_TAG;

  }
  else if(equal(ret_type, get_symbol_object("VOID")))
    ret = NIL;

  /*
  for(i=0; i<nof_args; i++)
  {
    if(arg_types[i] == &ffi_type_pointer)
      free(arg_values[i]);
  }
  free(arg_values);
  free(arg_types);
  */

  return ret;
}
