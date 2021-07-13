/**
  Copyright 2011-2021 Rajesh Jayaprakash <rajesh.jayaprakash@pm.me>

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
#include <stdlib.h>
#include <assert.h>
#include <math.h>

#include "plisp.h"

#include "memory.h"

extern OBJECT_PTR top_level_env, NIL;
extern OBJECT_PTR CONS_NIL_NIL;
extern OBJECT_PTR CONS_APPLY_NIL;
extern OBJECT_PTR CONS_HALT_NIL;
extern OBJECT_PTR CONS_RETURN_NIL;

extern BOOLEAN IS_CONS_OBJECT(OBJECT_PTR);
extern BOOLEAN IS_CLOSURE_OBJECT(OBJECT_PTR);
extern BOOLEAN IS_MACRO_OBJECT(OBJECT_PTR);
extern BOOLEAN IS_ARRAY_OBJECT(OBJECT_PTR);
extern BOOLEAN IS_CONTINUATION_OBJECT(OBJECT_PTR);
extern BOOLEAN IS_INTEGER_OBJECT(OBJECT_PTR);
extern BOOLEAN IS_FLOAT_OBJECT(OBJECT_PTR);

extern BOOLEAN console_mode, single_expression_mode, pipe_mode;

extern unsigned int nof_global_vars;
extern global_var_mapping_t *top_level_symbols;

extern OBJECT_PTR saved_continuations;
extern OBJECT_PTR idclo;

extern OBJECT_PTR most_recent_closure;
extern OBJECT_PTR continuations_for_return;

extern OBJECT_PTR debug_window_dbg_stack;

extern OBJECT_PTR continuation_to_resume;

extern OBJECT_PTR exception_handlers;

extern void show_error_dialog(char *);

//forward declarations

void gc(BOOLEAN, BOOLEAN);
BOOLEAN is_dynamic_memory_object(OBJECT_PTR);

//an AND operation with POINTER_MASK will get
//the pointer created by posix_memalign.
//const unsigned int POINTER_MASK = 4294967280;
uintptr_t POINTER_MASK;

extern BOOLEAN IS_FUNCTION2_OBJECT(OBJECT_PTR);
extern BOOLEAN IS_MACRO2_OBJECT(OBJECT_PTR);
extern OBJECT_PTR cons_equivalent(OBJECT_PTR);
extern BOOLEAN IS_NATIVE_FN_OBJECT(OBJECT_PTR);

void gc(BOOLEAN force, BOOLEAN clear_black)
{
  //no op
}

BOOLEAN is_dynamic_memory_object(OBJECT_PTR obj)
{
   return IS_CONS_OBJECT(obj)         ||
          IS_ARRAY_OBJECT(obj)        ||
          IS_CLOSURE_OBJECT(obj)      ||
          IS_MACRO_OBJECT(obj)        ||
          IS_CONTINUATION_OBJECT(obj) ||
          IS_INTEGER_OBJECT(obj)      ||
          IS_FLOAT_OBJECT(obj)        ||
          IS_NATIVE_FN_OBJECT(obj)    ||
          IS_FUNCTION2_OBJECT(obj)    ||
          IS_MACRO2_OBJECT(obj);
}

void insert_node(unsigned int set_type, OBJECT_PTR val)
{
  //no op
}

void set_heap(uintptr_t ptr, unsigned int index, OBJECT_PTR val)
{
  if(!is_valid_object(val))
    assert(false);

  uintptr_t *ptr1 = (uintptr_t *)ptr;

  *(ptr1 + index) = val;
}

OBJECT_PTR get_heap(uintptr_t ptr, unsigned int index)
{
  //unsigned int *ptr1 = (unsigned int *)ptr;

  //OBJECT_PTR ret = *(ptr1 + index);

  /* if(!is_valid_object(ret)) */
  /* { */
  /*   printf("0x%x 0x%x\n", ptr, ret); */
  /*   assert(false); */
  /* } */

  //return ret;

  return (OBJECT_PTR)*((OBJECT_PTR *)ptr + index);

}

extern unsigned int words_alloc_for_profiled_exp;

uintptr_t object_alloc(int size, int tag)
{
  uintptr_t *ret;

  /* if(tag == FLOAT_TAG) */
  /*   ret = GC_MALLOC(sizeof(double)); */
  /* else */
  /*   ret = GC_MALLOC(size * sizeof(OBJECT_PTR)); */

  int err;

  if(tag == FLOAT_TAG)
    err = GC_posix_memalign((void **)&ret, 16, sizeof(double));
  else
    err = GC_posix_memalign((void **)&ret, 16, size * sizeof(OBJECT_PTR));

  if(err)
  {
    if(!console_mode && !single_expression_mode && !pipe_mode)
    {
      char buf[MAX_STRING_LENGTH];
      memset(buf, '\0', MAX_STRING_LENGTH);
      int len=0;
      len += sprintf(buf, "Unable to allocate memory\n");
      show_error_dialog(buf);
    }
    else
      printf("Unable to allocate memory\n");

    cleanup();
    exit(1);
  }

  assert(is_valid_object((OBJECT_PTR)((uintptr_t)ret+tag)));

  words_alloc_for_profiled_exp += size;
  
  return (uintptr_t)ret;
}

int initialize_memory()
{
  GC_INIT();
  POINTER_MASK = 0xFFFFFFFFFFFFFFF0;

  return 0;
}

void cleanup_memory()
{
  //no op
}

uintptr_t extract_ptr(OBJECT_PTR obj)
{
  return (obj >> OBJECT_SHIFT) << OBJECT_SHIFT;
}
