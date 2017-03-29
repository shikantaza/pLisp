/**
  Copyright 2011-2017 Rajesh Jayaprakash <rajesh.jayaprakash@gmail.com>

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

#include "rb/red_black_tree.h"

#include "hashtable.h"

#ifdef GC_USES_HASHTABLE
hashtable_t *white, *grey, *black;
#else
rb_red_blk_tree *white, *grey, *black;
#endif

extern OBJECT_PTR top_level_env, NIL;
extern OBJECT_PTR CONS_NIL_NIL;
extern OBJECT_PTR CONS_APPLY_NIL;
extern OBJECT_PTR CONS_HALT_NIL;
extern OBJECT_PTR CONS_RETURN_NIL;

extern OBJECT_PTR reg_accumulator;
extern OBJECT_PTR reg_next_expression;
extern OBJECT_PTR reg_current_env;
extern OBJECT_PTR reg_current_value_rib;
extern OBJECT_PTR reg_current_stack;
extern OBJECT_PTR continuations_map;
extern OBJECT_PTR debug_continuation;
extern OBJECT_PTR debug_env;
extern OBJECT_PTR debug_execution_stack;

extern BOOLEAN IS_CONS_OBJECT(OBJECT_PTR);
extern BOOLEAN IS_CLOSURE_OBJECT(OBJECT_PTR);
extern BOOLEAN IS_MACRO_OBJECT(OBJECT_PTR);
extern BOOLEAN IS_ARRAY_OBJECT(OBJECT_PTR);
extern BOOLEAN IS_CONTINUATION_OBJECT(OBJECT_PTR);
extern BOOLEAN IS_INTEGER_OBJECT(OBJECT_PTR);
extern BOOLEAN IS_FLOAT_OBJECT(OBJECT_PTR);

extern hashtable_t *ht;

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

//forward declarations

void dealloc(OBJECT_PTR);

void insert_node(unsigned int, OBJECT_PTR);

void remove_node(unsigned int, OBJECT_PTR);
BOOLEAN is_set_empty(unsigned int);
BOOLEAN value_exists(unsigned int, OBJECT_PTR);

void gc(BOOLEAN, BOOLEAN);
BOOLEAN is_dynamic_memory_object(OBJECT_PTR);
void build_grey_set();

void recreate_black();
//end forward declarations

enum {WHITE, GREY, BLACK};

unsigned int words_allocated;
unsigned int words_deallocated;

unsigned int memory_allocated() { return words_allocated; }
unsigned int memory_deallocated() { return words_deallocated; }

//an AND operation with POINTER_MASK will get
//the pointer created by posix_memalign.
//const unsigned int POINTER_MASK = 4294967280;
uintptr_t POINTER_MASK;

BOOLEAN can_do_gc;

extern BOOLEAN IS_FUNCTION2_OBJECT(OBJECT_PTR);
extern BOOLEAN IS_MACRO2_OBJECT(OBJECT_PTR);
extern OBJECT_PTR cons_equivalent(OBJECT_PTR);
extern BOOLEAN IS_NATIVE_FN_OBJECT(OBJECT_PTR);

void free_white_set_objects()
{
  while(!is_set_empty(WHITE))
  {

#ifdef GC_USES_HASHTABLE
    hashtable_entry_t *e = hashtable_get_any_element(white);
    assert(e);
    OBJECT_PTR obj = (OBJECT_PTR)(e->ptr);
    assert(is_valid_object(obj));
#else
    rb_red_blk_node *white_obj = white->root->left;
    OBJECT_PTR obj = *((OBJECT_PTR *)(white_obj->key));
#endif

    if(!value_exists(BLACK, obj))
      dealloc(obj);

    remove_node(WHITE, obj);
  }
}

void move_from_white_to_grey(OBJECT_PTR obj)
{
  if(is_dynamic_memory_object(obj))
  {
    if(value_exists(WHITE, obj))
    {
      remove_node(WHITE, obj);
      insert_node(GREY, obj);
    }
    else if(IS_FUNCTION2_OBJECT(obj) || IS_MACRO2_OBJECT(obj))
      insert_node(GREY, cons_equivalent(obj));
  }
}

OBJECT_PTR get_an_object_from_grey()
{
#ifdef GC_USES_HASHTABLE
  OBJECT_PTR obj = (OBJECT_PTR)(((hashtable_entry_t *)(hashtable_get_any_element(grey)))->ptr);
  assert(is_valid_object(obj));
#else
  //we can pick any  object,
  //picking the root for convenience
  OBJECT_PTR obj = (OBJECT_PTR)*(OBJECT_PTR *)grey->root->left->key;
#endif
  return obj;
}

OBJECT_PTR get_an_object_from_black()
{
#ifdef GC_USES_HASHTABLE
  OBJECT_PTR obj = (OBJECT_PTR)(((hashtable_entry_t *)(hashtable_get_any_element(black)))->ptr);
  assert(is_valid_object(obj));
#else
  //we can pick any object,
  //picking the root for convenience
  OBJECT_PTR obj = (OBJECT_PTR)*(OBJECT_PTR *)black->root->left->key;
#endif
  return obj;
}

void free_all_objects()
{
  gc(true, false);

  unsigned int count = memory_deallocated();

  //assert(!is_set_empty(BLACK));

  while(!is_set_empty(BLACK))
  {
    OBJECT_PTR obj = get_an_object_from_black();
    assert(is_valid_object(obj));
    dealloc(obj);
    remove_node(BLACK, obj);
  }

  //printf("%d words freed during cleanup\n", memory_deallocated() - count);
}

void gc(BOOLEAN force, BOOLEAN clear_black)
{
  //no op
}

void gc_orig(BOOLEAN force, BOOLEAN clear_black)
{
  static unsigned long count = 0;

  if(!can_do_gc)
    return;

  //no new objects were created since the
  //last GC cycle, so nothing to do.
  if(is_set_empty(WHITE))
    return;

  //do GC every GC_FREQUENCYth time called
  if((count % GC_FREQUENCY) != 0)
    return;

  //printf("Entering GC cycle... ");

  unsigned int dealloc_words = memory_deallocated();

  //assert(is_set_empty(GREY));

  build_grey_set();

  assert(!is_set_empty(GREY));

  while(!is_set_empty(GREY))
  {
    OBJECT_PTR obj = get_an_object_from_grey();

    assert(is_dynamic_memory_object(obj));

    //FUNCTION2 and MACRO2 objects are handled
    //by handling their undelying CONS objects
    if(!IS_FUNCTION2_OBJECT(obj) && !IS_MACRO2_OBJECT(obj))
      insert_node(BLACK, obj);

    remove_node(GREY, obj);

    if(IS_CONS_OBJECT(obj))
    {
      move_from_white_to_grey(car(obj));
      move_from_white_to_grey(cdr(obj));
    }
    else if(IS_CLOSURE_OBJECT(obj) || IS_MACRO_OBJECT(obj))
    {
      move_from_white_to_grey(get_env_list(obj));
      move_from_white_to_grey(get_params_object(obj));
      move_from_white_to_grey(get_body_object(obj));
      move_from_white_to_grey(get_source_object(obj));
    }
    else if(IS_ARRAY_OBJECT(obj))
    {
      uintptr_t ptr = extract_ptr(obj);

      //OBJECT_PTR length_obj = get_heap(ptr, 0);

      //move_from_white_to_grey(length_obj);

      //int len = get_int_value(length_obj);
      int len = *((OBJECT_PTR *)ptr);

      int i;

      for(i=1; i<=len; i++)
        move_from_white_to_grey(get_heap(ptr, i));
    }
    else if(IS_CONTINUATION_OBJECT(obj))
      move_from_white_to_grey(get_heap(extract_ptr(obj), 0));
    else if(IS_FUNCTION2_OBJECT(obj) || IS_MACRO2_OBJECT(obj))
    {
      OBJECT_PTR cons_equiv = cons_equivalent(obj);
      //move_from_white_to_grey(car(cons_equiv));
      //move_from_white_to_grey(cdr(cons_equiv));
      move_from_white_to_grey(cons_equiv);
    }
  } //end of while(!is_set_empty(GREY))

  free_white_set_objects();

  assert(is_set_empty(GREY));
  assert(is_set_empty(WHITE));

  assert(!is_set_empty(BLACK));

  /* if(clear_black) */
    /* recreate_black(); */

  /* if(clear_black) */
  /*   assert(is_set_empty(BLACK)); */

  //printf("%d words deallocated in current GC cycle\n", memory_deallocated() - dealloc_words);
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

void pin_globals()
{
  /* if(is_dynamic_memory_object(reg_accumulator)) */
  /*   insert_node(GREY, reg_accumulator); */

  /* if(is_dynamic_memory_object(reg_next_expression)) */
  /*   insert_node(GREY, reg_next_expression); */

  /* if(is_dynamic_memory_object(reg_current_value_rib)) */
  /*   insert_node(GREY, reg_current_value_rib); */

  /* if(is_dynamic_memory_object(reg_current_env)) */
  /*   insert_node(GREY, reg_current_env); */

  /* if(is_dynamic_memory_object(reg_current_stack)) */
  /*   insert_node(GREY, reg_current_stack); */

  /* if(is_dynamic_memory_object(debug_env)) */
  /*   insert_node(GREY, debug_env); */

  /* if(is_dynamic_memory_object(debug_continuation)) */
  /*   insert_node(GREY, debug_continuation); */

  /* if(is_dynamic_memory_object(debug_execution_stack)) */
  /*   insert_node(GREY, debug_execution_stack); */

  /* if(is_dynamic_memory_object(continuations_map)) */
  /*   insert_node(GREY, continuations_map); */

  if(is_dynamic_memory_object(saved_continuations))
    insert_node(GREY, saved_continuations);

  if(is_dynamic_memory_object(idclo))
    insert_node(GREY, idclo);

  if(is_dynamic_memory_object(most_recent_closure))
    insert_node(GREY, most_recent_closure);

  if(is_dynamic_memory_object(continuations_for_return))
    insert_node(GREY, continuations_for_return);

  if(is_dynamic_memory_object(continuation_to_resume))
    insert_node(GREY, continuation_to_resume);

  if(is_dynamic_memory_object(exception_handlers))
    insert_node(GREY, exception_handlers);

}

void build_grey_set()
{
  /* assert(top_level_env != NIL); */

  /* insert_node(GREY, top_level_env); */

  int i;
  for(i=0; i<nof_global_vars; i++)
  {
    if(top_level_symbols[i].delete_flag)
      continue;

    if(is_dynamic_memory_object(top_level_symbols[i].val))
      insert_node(GREY, top_level_symbols[i].val);
  }

  /* insert_node(GREY, CONS_NIL_NIL); */
  /* insert_node(GREY, CONS_APPLY_NIL); */
  /* insert_node(GREY, CONS_HALT_NIL); */
  /* insert_node(GREY, CONS_RETURN_NIL); */

  if(debug_window_dbg_stack != NIL)
    insert_node(GREY, debug_window_dbg_stack);

  pin_globals();
}

#ifndef GC_USES_HASHTABLE

void IntDest(void* a)
{
  free((OBJECT_PTR*)a);
}

int IntComp(const void* a,const void* b)
{
  if( *(OBJECT_PTR *)a > *(OBJECT_PTR *)b) return(1);
  if( *(OBJECT_PTR *)a < *(OBJECT_PTR *)b) return(-1);
  return(0);
}

void IntPrint(const void* a)
{
  printf("%i",*(OBJECT_PTR *)a);
}

void InfoPrint(void* a)
{
  ;
}

void InfoDest(void *a)
{
  ;
}

#endif

/* void insert_node(unsigned int set_type, OBJECT_PTR val) */
/* { */
/*   if(!is_dynamic_memory_object(val)) */
/*     assert(false); */

/*   if(!is_valid_object(val)) */
/*     assert(false); */

/*   if(value_exists(set_type, val)) */
/*     return; */

/* #ifdef GC_USES_HASHTABLE */

/*   if(set_type == WHITE) */
/*     hashtable_put(white, (void *)val, 0); */
/*   else if(set_type == GREY) */
/*     hashtable_put(grey, (void *)val, 0); */
/*   else if(set_type == BLACK) */
/*     hashtable_put(black, (void *)val, 0); */
/*   else */
/*     assert(false); */

/* #else */

/*   rb_red_blk_tree *tree; */

/*   if(set_type == WHITE) */
/*     tree = white; */
/*   else if(set_type == GREY) */
/*     tree = grey; */
/*   else if(set_type == BLACK) */
/*     tree = black; */
/*   else */
/*     assert(false); */

/*   OBJECT_PTR *newInt=(OBJECT_PTR *) malloc(sizeof(OBJECT_PTR)); */
/*   *newInt = (OBJECT_PTR)val; */

/*   RBTreeInsert(tree, newInt, 0); */

/* #endif */
/* } */

void insert_node(unsigned int set_type, OBJECT_PTR val)
{
  //no op
}

void remove_node(unsigned int set_type, OBJECT_PTR val)
{

#ifdef GC_USES_HASHTABLE

  if(set_type == WHITE)
    hashtable_remove(white, (void *)val);
  else if(set_type == GREY)
    hashtable_remove(grey, (void *)val);
  else if(set_type == BLACK)
    hashtable_remove(black, (void *)val);
  else
    assert(false);

#else

  rb_red_blk_tree *tree;

  if(set_type == WHITE)
    tree = white;
  else if(set_type == GREY)
    tree = grey;
  else if(set_type == BLACK)
    tree = black;
  else
    assert(false);

  if(!tree)
    return;

  rb_red_blk_node* newNode;
  if((newNode=RBExactQuery(tree,&val))) 
    RBDelete(tree,newNode);  

#endif
}

BOOLEAN is_set_empty(unsigned int set_type)
{

#ifdef GC_USES_HASHTABLE

  if(set_type == WHITE)
    return(hashtable_count(white) == 0);
  else if(set_type == GREY)
    return(hashtable_count(grey) == 0);
  else if(set_type == BLACK)
    return(hashtable_count(black) == 0);
  else
    assert(false);

#else

  rb_red_blk_tree *tree;

  if(set_type == WHITE)
    tree = white;
  else if(set_type == GREY)
    tree = grey;
  else if(set_type == BLACK)
    tree = black;
  else
    assert(false);

  if(!tree)
    return true;

  return tree->root->left == tree->nil;

#endif
}

BOOLEAN value_exists(unsigned int set_type, OBJECT_PTR val)
{

#ifdef GC_USES_HASHTABLE

  if(set_type == WHITE)
    return(hashtable_get(white, (void *)val) != NULL);
  else if(set_type == GREY)
    return(hashtable_get(grey, (void *)val) != NULL);
  else if(set_type == BLACK)
    return(hashtable_get(black, (void *)val) != NULL);
  else
    assert(false);

#else

  rb_red_blk_tree *tree;

  if(set_type == WHITE)
    tree = white;
  else if(set_type == GREY)
    tree = grey;
  else if(set_type == BLACK)
    tree = black;
  else
    assert(false);

  if(!tree)
    return false;

  rb_red_blk_node* newNode;
  if((newNode=RBExactQuery(tree,&val)))
    return true;
  else
    return false;

#endif
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

/* uintptr_t object_alloc(int size, int tag) */
/* { */
/*   uintptr_t *ret; */

/* #ifdef WIN32 */

/*   if(tag == FLOAT_TAG) */
/*     ret = __mingw_aligned_malloc(sizeof(double), 16); */
/*   else */
/*     ret = __mingw_aligned_malloc(size * sizeof(OBJECT_PTR), 16); */

/*   if(!ret) */
/*   { */
/*     if(!console_mode && !single_expression_mode && !pipe_mode) */
/*     { */
/*       char buf[MAX_STRING_LENGTH]; */
/*       memset(buf, '\0', MAX_STRING_LENGTH); */
/*       int len=0; */
/*       len += sprintf(buf, "Unable to allocate memory\n"); */
/*       show_error_dialog(buf); */
/*     } */
/*     else */
/*       printf("Unable to allocate memory\n"); */

/*     cleanup(); */
/*     exit(1); */
/*   } */
/* #else */
/*   int err; */

/*   if(tag == FLOAT_TAG) */
/*     err = posix_memalign((void **)&ret, 16, sizeof(double)); */
/*   else */
/*     err = posix_memalign((void **)&ret, 16, size * sizeof(OBJECT_PTR)); */

/*   if(err) */
/*   { */
/*     if(!console_mode && !single_expression_mode && !pipe_mode) */
/*     { */
/*       char buf[MAX_STRING_LENGTH]; */
/*       memset(buf, '\0', MAX_STRING_LENGTH); */
/*       int len=0; */
/*       len += sprintf(buf, "Unable to allocate memory; posix_memalign error = %d\n", err); */
/*       show_error_dialog(buf); */
/*     } */
/*     else */
/*       printf("Unable to allocate memory; posix_memalign error = %d\n", err); */

/*     //printf("Unable to allocate memory; posix_memalign error = %d\n", err); */

/*     cleanup(); */
/*     exit(1); */
/*   } */
/* #endif */

/*   //for arrays, the array size object is allocated outside the call to object_alloc */
/*   //but we include it here for accounting */
/*   words_allocated += size; */

/*   assert(is_valid_object((OBJECT_PTR)((uintptr_t)ret+tag))); */

/*   insert_node(WHITE, (OBJECT_PTR)((uintptr_t)ret+tag)); */

/*   return (uintptr_t)ret; */
/* } */

uintptr_t object_alloc(int size, int tag)
{
  uintptr_t *ret;

  if(tag == FLOAT_TAG)
    ret = GC_MALLOC(sizeof(double));
  else
    ret = GC_MALLOC(size * sizeof(OBJECT_PTR));

  if(!ret)
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

  return (uintptr_t)ret;
}

void dealloc(OBJECT_PTR ptr)
{
  unsigned int prev_words_deallocated = words_deallocated;

  if(!is_valid_object(ptr))
  {
    printf("%d\n", ptr);
    assert(false);
  }

  unsigned int tag = ptr & BIT_MASK;

  OBJECT_PTR array_size;

  switch(tag)
  {
    case CONS_TAG:
      words_deallocated += 2;
      break;
    case ARRAY_TAG:
      array_size = *((OBJECT_PTR *)extract_ptr(ptr));
      words_deallocated += (array_size + 1);
      break;
    case CLOSURE_TAG:
      words_deallocated += 4;
      break;
    case MACRO_TAG:
      words_deallocated += 4;
      break;
    case CONTINUATION_TAG:
      words_deallocated += 1;
      break;
    case INTEGER_TAG:
      words_deallocated += 1;
      break;
    case FLOAT_TAG:
      words_deallocated += 1;
      break;
    /* case FUNCTION2_TAG: */
    /*   words_allocated += 2; */
    /*   break; */
    /* case MACRO2_TAG: */
    /*   words_allocated += 2; */
    /*   break; */
    case NATIVE_FN_TAG:
      words_deallocated += 1;
      break;
    default:
      assert(false);
  }

  uintptr_t p = extract_ptr(ptr);
  int i;
  for(i=0; i < words_deallocated - prev_words_deallocated; i++)
    *((OBJECT_PTR *)p+i) = 0xFEEEFEEE;

#ifdef WIN32
  __mingw_aligned_free((void *)extract_ptr(ptr));
#else
  free((void *)extract_ptr(ptr));
#endif
}

/* int initialize_memory() */
/* { */
/* #ifdef GC_USES_HASHTABLE */

/*   white = hashtable_create(1001); */
/*   grey  = hashtable_create(1001); */
/*   black = hashtable_create(1001); */

/* #else */

/*   white = RBTreeCreate(IntComp,IntDest,InfoDest,IntPrint,InfoPrint); */
/*   grey  = RBTreeCreate(IntComp,IntDest,InfoDest,IntPrint,InfoPrint); */
/*   black = RBTreeCreate(IntComp,IntDest,InfoDest,IntPrint,InfoPrint); */

/* #endif */

/*   words_allocated = 0; */
/*   words_deallocated = 0; */

/*   //POINTER_MASK = ((uintptr_t)(pow(2, 8 * sizeof(uintptr_t))) >> OBJECT_SHIFT) << OBJECT_SHIFT; */
/*   POINTER_MASK = 0xFFFFFFFFFFFFFFF0; */
  
/*   can_do_gc = true; */

/*   return 0; */
/* } */

/* void cleanup_memory() */
/* { */
/*   can_do_gc = true; */
/*   free_all_objects(); */

/* #ifdef GC_USES_HASHTABLE */
/*   hashtable_delete(white); */
/*   hashtable_delete(grey); */
/*   hashtable_delete(black); */
/* #else */
/*   if(white)RBTreeDestroy(white); */
/*   if(grey)RBTreeDestroy(grey); */
/*   if(black)RBTreeDestroy(black); */
/* #endif */

/*   white = NULL; */
/*   grey  = NULL; */
/*   black = NULL; */

/*   //note: there will be a discrepancy between the allocated memory and deallocated memory */
/*   //if calls are made to any foreign function calls that modify pLisp strings */
/*   //printf("%d words allocated, %d words deallocated\n", memory_allocated(), memory_deallocated()); */
/* } */

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

void recreate_black()
{
  RBTreeDestroy(black);
  black = RBTreeCreate(IntComp,IntDest,InfoDest,IntPrint,InfoPrint);
}

uintptr_t extract_ptr(OBJECT_PTR obj)
{
  return (obj >> OBJECT_SHIFT) << OBJECT_SHIFT;
}
