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

OBJECT_PTR load_foreign_library(OBJECT_PTR library_name, OBJECT_PTR env_list)
{
  nof_dl_handles++;
  
  void **temp = (void **)realloc(dl_handles, nof_dl_handles * sizeof(void *));

  if(temp == NULL)
  {
    sprintf(err_buf, "Unable to extend memory for dl_handles");
    raise_error();
    return NIL;
  }

  dl_handles = temp;

  void *ret = dlopen(strings[eval(library_name, env_list) >> STRING_LITERAL_SHIFT], RTLD_LAZY);

  if(!ret)
  {
    sprintf(err_buf, "dl_open() failed: %S", dlerror());
    raise_error();
    return NIL;
  }

  dl_handles[nof_dl_handles - 1] = ret;

  return NIL;

}

//TODO: things are quite unstable; the function
//runs successfully sometimes and fails at other times
//for some reason (for the same input).
//issue most probably with the 'ptrs' pointer -- need
//to investigate whether its role can be done by
//arg_values itself
OBJECT_PTR call_foreign_function(OBJECT_PTR fn_name, OBJECT_PTR ret_type, OBJECT_PTR args, OBJECT_PTR env_list)
{

  char *fn_name_str = strings[eval(fn_name, env_list) >> STRING_LITERAL_SHIFT];

  if(in_exception)
    return NIL;

  ffi_cif cif;

  int nof_args = length(args);

  ffi_type **arg_types = (ffi_type **)malloc(nof_args * sizeof(ffi_type *));
  if(!arg_types)
  {
    sprintf(err_buf, "Unable to allocate memory for argument types buffer");
    raise_error();
    return NIL;
  }

  void **arg_values = (void **)malloc(nof_args * sizeof(void *));
  if(!arg_values)
  {
    sprintf(err_buf, "Unable to allocate memory for argument values buffer");
    raise_error();
    free(arg_types);
    return NIL;
  }
  
  void ***ptrs = (void ***)malloc(nof_args * sizeof(void **));
  if(!ptrs)
  {
    sprintf(err_buf, "Unable to allocate memory for ptrs buffer");
    raise_error();
    free(arg_types);
    free(arg_values);
    return NIL;
  }

  ffi_arg ret_val;
  float float_ret; //using ffi_arg for float return values results in junk data for some reason
  OBJECT_PTR ret;
  
  OBJECT_PTR rest_args = args;

  int i=0;

  while(rest_args != NIL)
  {
    OBJECT_PTR val = eval(CAAR(rest_args), env_list);
    OBJECT_PTR type = CADAR(rest_args);

    if(equal(type, get_symbol_object(":INTEGER")))
    {
      if(!IS_INTEGER_OBJECT(val))
      {
	sprintf(err_buf, "Argument type mismatch: integer expected");
	raise_error();

        //we get a 'sig_send: wait for sig_complete event failed, signal 6, rc 258' error if
        //we try to free arg_values elements that are strings
        /*
        int j=0;

        for(j=0; j<i; j++)
        {
	  if(arg_types[j] == &ffi_type_pointer)
	  {
	    free(arg_values[j]);
	    free(ptrs[j]);
	  }
        }
        free(arg_values);
        free(arg_types);
	free(ptrs);
        */
	
	return NIL;
      }
      arg_types[i] = &ffi_type_sint;
      int i_val = get_int_value(val);
      arg_values[i] = &i_val;
    }
    else if(equal(type, get_symbol_object(":FLOAT")))
    {

      if(!IS_FLOAT_OBJECT(val))
      {
	sprintf(err_buf, "Argument type mismatch: float expected");
	raise_error();

        /*
        int j=0;

        for(j=0; j<i; j++)
        {
	  if(arg_types[j] == &ffi_type_pointer)
	  {
	    free(arg_values[j]);
	    free(ptrs[j]);
	  }
        }
        free(arg_values);
        free(arg_types);
	free(ptrs);
        */
	
	return NIL;
      }

      arg_types[i] = &ffi_type_float;
      float f_val = get_float_value(val);
      arg_values[i] = &f_val;
    }
    else if(equal(type, get_symbol_object(":CHARACTER")))
    {

      if(!IS_CHAR_OBJECT(val))
      {
	sprintf(err_buf, "Argument type mismatch: character expected");
	raise_error();

        /*
        int j=0;

        for(j=0; j<i; j++)
        {
	  if(arg_types[j] == &ffi_type_pointer)
	  {
	    free(arg_values[j]);
	    free(ptrs[j]);
	  }
        }
        free(arg_values);
        free(arg_types);
	free(ptrs);
        */
	
	return NIL;
      }

      arg_types[i] = &ffi_type_schar;
      char c_val = val >> CHAR_SHIFT;
      arg_values[i] = &c_val;
    }
    else if(equal(type, get_symbol_object(":CHARACTER-POINTER")))
    {
      if(!IS_STRING_LITERAL_OBJECT(val) && !is_string_object(val))
      {
	sprintf(err_buf, "Argument type mismatch: string object/literal expected");
	raise_error();

        /*
        int j=0;

        for(j=0; j<i; j++)
        {
	  if(arg_types[j] == &ffi_type_pointer)
	  {
	    free(arg_values[j]);
	    free(ptrs[j]);
	  }
        }
        free(arg_values);
        free(arg_types);
	free(ptrs);
        */
	
	return NIL;
      }

      if(IS_STRING_LITERAL_OBJECT(val))
      {
	arg_types[i] = &ffi_type_pointer;
	char *s_val = strdup(strings[val >> STRING_LITERAL_SHIFT]);
	arg_values[i] = &s_val;
      }
      else if(is_string_object(val))
      {
	arg_types[i] = &ffi_type_pointer;
	char *s_val = get_string(val);
	ptrs[i][0] = (void **)malloc(strlen(s_val) * sizeof(char *));
	memset(ptrs[i][0],'\0',strlen(s_val));
	strcpy(ptrs[i][0], s_val);
	arg_values[i] = ptrs[i];
      }
    }
    else if(equal(type, get_symbol_object(":INTEGER-POINTER")))
    {
      if(!IS_SYMBOL_OBJECT(CAAR(rest_args)) || !IS_INTEGER_OBJECT(val))
      {
	sprintf(err_buf, "Mapping a non-variable to :INTEGER-POINTER / Argument type mismatch");
	raise_error();

	/*
	int j=0;

	for(j=0; j<i; j++)
	{
          if(arg_types[j] == &ffi_type_pointer)
	  {
	    free(arg_values[j]);
	    free(ptrs[j]);
	  }
	}
        free(arg_values);
        free(arg_types);
	free(ptrs);
        */
	
	return NIL;
      }

      arg_types[i] = &ffi_type_pointer;
      ptrs[i] = (void **)malloc(sizeof(int *));
      *((int *)ptrs[i][0]) = get_int_value(val);
      arg_values[i] = ptrs[i];
    }
    else if(equal(type, get_symbol_object(":FLOAT-POINTER")))
    {
      if(!IS_SYMBOL_OBJECT(CAAR(rest_args)) || !IS_FLOAT_OBJECT(val))
      {
	sprintf(err_buf, "Mapping a non-variable to :FLOAT-POINTER / Argument type mismatch");
	raise_error();

	/*
	int j=0;

	for(j=0; j<i; j++)
	{
          if(arg_types[j] == &ffi_type_pointer)
	  {
	    free(arg_values[j]);
	    free(ptrs[j]);
	  }
	}
        free(arg_values);
        free(arg_types);
	free(ptrs);
        */
	
	return NIL;
      }

      arg_types[i] = &ffi_type_pointer;
      ptrs[i] = (void **)malloc(sizeof(float *));
      *((float *)ptrs[i][0]) = get_float_value(val);
      arg_values[i] = ptrs[i];
    }
    else
    {
      sprintf(err_buf, "call_foreign_function(): non-primitive object type not handled");
      raise_error();

      /*
      int j=0;

      for(j=0; j<i; j++)
      {
	if(arg_types[j] == &ffi_type_pointer)
	{
	  free(arg_values[j]);
	    free(ptrs[j]);
	  }
      }
      free(arg_values);
      free(arg_types);
      free(ptrs);
      */

      return NIL;
    }

    i++;
    rest_args = cdr(rest_args);
  }

  ffi_status status;

  if(equal(ret_type, get_symbol_object(":INTEGER")))
    status = ffi_prep_cif(&cif, FFI_DEFAULT_ABI, nof_args, &ffi_type_sint, arg_types);
  else if(equal(ret_type, get_symbol_object(":CHARACTER")))
    status = ffi_prep_cif(&cif, FFI_DEFAULT_ABI, nof_args, &ffi_type_schar, arg_types);
  else if(equal(ret_type, get_symbol_object(":FLOAT")))
    status = ffi_prep_cif(&cif, FFI_DEFAULT_ABI, nof_args, &ffi_type_float, arg_types);
  else if(equal(ret_type, get_symbol_object(":CHARACTER-POINTER")))
    status = ffi_prep_cif(&cif, FFI_DEFAULT_ABI, nof_args, &ffi_type_pointer, arg_types);
  else if(equal(ret_type, get_symbol_object(":VOID")))
    status = ffi_prep_cif(&cif, FFI_DEFAULT_ABI, nof_args, &ffi_type_void, arg_types);
  else
  {
    sprintf(err_buf, "Unknown return type: %s", strings[ret_type >> STRING_LITERAL_SHIFT]);
    raise_error();

    /*
    for(i=0; i<nof_args; i++)
    {
      if(arg_types[i] == &ffi_type_pointer)
      {
	free(arg_values[i]);
	free(ptrs[j]);
      }
    }
    free(arg_values);
    free(arg_types);
    free(ptrs);
    */

    return NIL;
  }

  if(status != FFI_OK)
  {
    sprintf(err_buf, "call_foreign_function(): ffi_prep_cif() failed");
    raise_error();

    /*
    for(i=0; i<nof_args; i++)
    {
      if(arg_types[i] == &ffi_type_pointer)
      {
	free(arg_values[i]);
	free(ptrs[j]);
      }
    }
    free(arg_values);
    free(arg_types);
    free(ptrs);
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
    sprintf(err_buf, "Foreign function %s not found", fn_name_str);
    raise_error();

    /*
    for(i=0; i<nof_args; i++)
    {
      if(arg_types[i] == &ffi_type_pointer)
      {
	free(arg_values[i]);
	free(ptrs[j]);
      }
    }
    free(arg_values);
    free(arg_types);
    free(ptrs);
    */

    return NIL;
  }

  if(equal(ret_type, get_symbol_object(":FLOAT")))
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

    if(equal(type, get_symbol_object(":INTEGER-POINTER")))
    {
      value = convert_int_to_object((int)(*((int *)ptrs[i][0])));
      if(update_environment(env_list, sym, value) == NIL)
      {
	sprintf(err_buf, "update_environment failed");
	raise_error();

	/*
	for(i=0; i<nof_args; i++)
	{
	  if(arg_types[i] == &ffi_type_pointer)
          {
	     free(arg_values[i]);
	     free(ptrs[j]);
          }
        }
        free(arg_values);
        free(arg_types);
        free(ptrs);
        */
	return NIL;
      }
    }
    else if(equal(type, get_symbol_object(":FLOAT-POINTER")))
    {
      value = convert_float_to_object((float)(*((float *)ptrs[i][0])));
      if(update_environment(env_list, sym, value) == NIL)
      {
	sprintf(err_buf, "update_environment failed");
	raise_error();

	/*
	for(i=0; i<nof_args; i++)
	{
	  if(arg_types[i] == &ffi_type_pointer)
          {
	     free(arg_values[i]);
	     free(ptrs[j]);
          }
        }
        free(arg_values);
        free(arg_types);
        free(ptrs);
        */
	return NIL;
      }
    }
    else if(equal(type, get_symbol_object(":CHARACTER-POINTER"))
	    && IS_SYMBOL_OBJECT(sym))
      //&& is_string_object(eval(CAAR(rest_args), env_list)))
    {
      char *str = (char *)ptrs[i][0];

      int sz = strlen(str);

      RAW_PTR ptr = object_alloc(sz+1);

      heap[ptr] = convert_int_to_object(sz);

      int j;
      for(j=0; j< sz; j++)
	heap[ptr + j + 1] = (str[j] << CHAR_SHIFT) + CHAR_TAG;

      free(str);

      if(update_environment(env_list, sym, (ptr << ARRAY_SHIFT) + ARRAY_TAG) == NIL)
      {
	sprintf(err_buf, "update_environment failed");
	raise_error();

	/*
	for(i=0; i<nof_args; i++)
	{
	  if(arg_types[i] == &ffi_type_pointer)
          {
	     free(arg_values[i]);
	     free(ptrs[j]);
          }
        }
        free(arg_values);
        free(arg_types);
        free(ptrs);
        */
	return NIL;
      }
    }

    rest_args = cdr(rest_args);
    i++;
  }

  if(equal(ret_type, get_symbol_object(":INTEGER")))
    ret = convert_int_to_object((int)ret_val);
  else if(equal(ret_type, get_symbol_object(":CHARACTER")))
    ret = ((char)ret_val << CHAR_SHIFT) + CHAR_TAG;
  else if(equal(ret_type, get_symbol_object(":FLOAT")))
    ret = convert_float_to_object((float)float_ret);
  else if(equal(ret_type, get_symbol_object(":CHARACTER-POINTER")))
  {

    char *str = (char *)ret_val;

    int sz = strlen(str);

    RAW_PTR ptr = object_alloc(sz+1);

    heap[ptr] = convert_int_to_object(sz);

    for(i=0; i< sz; i++)
      heap[ptr + i + 1] = (str[i] << CHAR_SHIFT) + CHAR_TAG;

    free(str);

    ret = (ptr << ARRAY_SHIFT) + ARRAY_TAG;

  }
  else if(equal(ret_type, get_symbol_object(":VOID")))
    ret = NIL;

  /*
  for(i=0; i<nof_args; i++)
  {
    if(arg_types[i] == &ffi_type_pointer)
    {
      free(arg_values[i]);
      free(ptrs[j]);
    }
  }
  free(arg_values);
  free(arg_types);
  free(ptrs);
  */

  return ret;
}
