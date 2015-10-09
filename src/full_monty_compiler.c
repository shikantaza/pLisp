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
#include <string.h>
#include <stdarg.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>

#include "plisp.h"
#include "util.h"
#include "libtcc.h"

typedef struct binding
{
  OBJECT_PTR key;
  OBJECT_PTR val;
} binding_t;

typedef struct binding_env
{
  unsigned int count;
  binding_t *bindings;
} binding_env_t;

//global variables
unsigned int nof_global_vars = 0;
global_var_mapping_t *top_level_symbols = NULL;

OBJECT_PTR saved_continuations;
OBJECT_PTR idclo;

unsigned int nof_unmet_dependencies = 0;
unmet_dependency_t *global_unmet_dependencies = NULL;

unsigned int nof_and_rest_mappings = 0;
and_rest_mapping_t *and_rest_mappings = NULL;

unsigned int nof_native_fns = 0;
native_fn_src_mapping_t *native_fn_sources = NULL;

OBJECT_PTR most_recent_closure;
OBJECT_PTR continuations_for_return;
//end of global variables

//external variables
extern  OBJECT_PTR first(OBJECT_PTR);
extern  OBJECT_PTR second(OBJECT_PTR);
extern  OBJECT_PTR third(OBJECT_PTR);
extern  OBJECT_PTR fourth(OBJECT_PTR);

extern OBJECT_PTR CADR(OBJECT_PTR);

extern BOOLEAN is_atom(OBJECT_PTR);

extern OBJECT_PTR NIL;
extern OBJECT_PTR ERROR;
extern OBJECT_PTR IF;
extern OBJECT_PTR SET;
extern OBJECT_PTR LAMBDA;
extern OBJECT_PTR LET;
extern OBJECT_PTR LETREC;
extern OBJECT_PTR CONS;

extern OBJECT_PTR CAR;
extern OBJECT_PTR SETCAR;
extern OBJECT_PTR LET1;
extern OBJECT_PTR DEFINE;
extern OBJECT_PTR CALL_CC;
extern OBJECT_PTR SAVE_CONTINUATION;
extern OBJECT_PTR NTH;
extern OBJECT_PTR CALL_CC1;
extern OBJECT_PTR MY_CONT_VAR;
extern OBJECT_PTR ADD;
extern OBJECT_PTR SUB;
extern OBJECT_PTR MULT;
extern OBJECT_PTR DIV;
extern OBJECT_PTR GT;
extern OBJECT_PTR LT;
extern OBJECT_PTR LEQ;
extern OBJECT_PTR GEQ;
extern OBJECT_PTR ATOM;
extern OBJECT_PTR EQ;
extern OBJECT_PTR CDR;
extern OBJECT_PTR PRINT;
extern OBJECT_PTR SYMBOL_VALUE;
extern OBJECT_PTR BACKQUOTE;
extern OBJECT_PTR GENSYM;
extern OBJECT_PTR SETCDR;
extern OBJECT_PTR COMMA;
extern OBJECT_PTR COMMA_AT;
extern OBJECT_PTR APPLY;
extern OBJECT_PTR SYMBL;
extern OBJECT_PTR SYMBOL_NAME;
extern OBJECT_PTR FORMAT;
extern OBJECT_PTR CLONE;
extern OBJECT_PTR RETURN_FROM;
extern OBJECT_PTR RETURN;
extern OBJECT_PTR UNBIND;
extern OBJECT_PTR NEWLINE;
extern OBJECT_PTR NOT;
extern OBJECT_PTR PROGN;
extern OBJECT_PTR STRING;
extern OBJECT_PTR MAKE_ARRAY;
extern OBJECT_PTR ARRAY_GET;
extern OBJECT_PTR ARRAY_SET;
extern OBJECT_PTR SUB_ARRAY;
extern OBJECT_PTR ARRAY_LENGTH;
extern OBJECT_PTR PRINT_STRING;
extern OBJECT_PTR CONSP;
extern OBJECT_PTR LISTP;
extern OBJECT_PTR INTEGERP;
extern OBJECT_PTR FLOATP;
extern OBJECT_PTR CHARACTERP;
extern OBJECT_PTR SYMBOLP;
extern OBJECT_PTR STRINGP;
extern OBJECT_PTR ARRAYP;
extern OBJECT_PTR CLOSUREP;
extern OBJECT_PTR MACROP;
extern OBJECT_PTR CONTINUATIONP;
extern OBJECT_PTR LOAD_FOREIGN_LIBRARY;
extern OBJECT_PTR CALL_FOREIGN_FUNCTION;
extern OBJECT_PTR CREATE_PACKAGE;
extern OBJECT_PTR IN_PACKAGE;
extern OBJECT_PTR EXPORT_PACKAGE;
extern OBJECT_PTR CREATE_IMAGE;
extern OBJECT_PTR SAVE_OBJECT;
extern OBJECT_PTR LOAD_OBJECT;
extern OBJECT_PTR LOAD_FILE;
extern OBJECT_PTR PROFILE;
extern OBJECT_PTR TIME;
extern OBJECT_PTR BREAK;
extern OBJECT_PTR RESUME;
extern OBJECT_PTR ENV;
extern OBJECT_PTR EXPAND_MACRO;
extern OBJECT_PTR EVAL;

extern OBJECT_PTR LST;
extern OBJECT_PTR EXTRACT_NATIVE_FN;
extern OBJECT_PTR CREATE_FN_CLOSURE;
extern OBJECT_PTR MACRO;
extern OBJECT_PTR QUOTE;

extern OBJECT_PTR TRUE;

extern OBJECT_PTR CONCAT;

extern OBJECT_PTR GET_CONTINUATION;

extern unsigned int POINTER_MASK;

extern expression_t *g_expr;

extern BOOLEAN console_mode;
extern BOOLEAN image_mode;
extern BOOLEAN single_expression_mode;
extern BOOLEAN interpreter_mode;
extern BOOLEAN pipe_mode;

extern FILE *yyin;
extern BOOLEAN core_library_loaded;

extern BOOLEAN in_error;

extern package_t *packages;

extern int current_package;
//end of external variables

//external functions
extern OBJECT_PTR quote(OBJECT_PTR, OBJECT_PTR);
extern OBJECT_PTR primitive_add(OBJECT_PTR, ...);
extern OBJECT_PTR primitive_sub(OBJECT_PTR, ...);
extern OBJECT_PTR primitive_lt(OBJECT_PTR, OBJECT_PTR);
extern OBJECT_PTR primitive_gt(OBJECT_PTR, OBJECT_PTR);
extern OBJECT_PTR primitive_leq(OBJECT_PTR, OBJECT_PTR);
extern OBJECT_PTR primitive_geq(OBJECT_PTR, OBJECT_PTR);
extern OBJECT_PTR primitive_if(OBJECT_PTR, OBJECT_PTR, OBJECT_PTR);
extern OBJECT_PTR primitive_error(OBJECT_PTR);
extern OBJECT_PTR primitive_print(OBJECT_PTR);
extern OBJECT_PTR primitive_setcar(OBJECT_PTR);
extern OBJECT_PTR primitive_setcdr(OBJECT_PTR);
extern OBJECT_PTR primitive_mult(OBJECT_PTR);
extern OBJECT_PTR primitive_equal(OBJECT_PTR);

extern OBJECT_PTR apply_macro_or_fn(OBJECT_PTR, OBJECT_PTR);

extern OBJECT_PTR primitive_list(OBJECT_PTR, ...);
extern OBJECT_PTR primitive_concat(OBJECT_PTR, ...);

extern OBJECT_PTR primitive_not(OBJECT_PTR);
extern OBJECT_PTR primitive_car(OBJECT_PTR);

extern OBJECT_PTR primitive_atom(OBJECT_PTR);
extern OBJECT_PTR prim_symbol_value(OBJECT_PTR);
extern OBJECT_PTR primitive_apply(OBJECT_PTR);
extern OBJECT_PTR primitive_symbol(OBJECT_PTR);
extern OBJECT_PTR prim_symbol_name(OBJECT_PTR);
extern OBJECT_PTR primitive_format(OBJECT_PTR, OBJECT_PTR, OBJECT_PTR, ...);
extern OBJECT_PTR primitive_clone(OBJECT_PTR);
extern OBJECT_PTR primitive_unbind(OBJECT_PTR);
extern OBJECT_PTR primitive_newline(OBJECT_PTR);

extern OBJECT_PTR primitive_consp(OBJECT_PTR);
extern OBJECT_PTR primitive_listp(OBJECT_PTR);
extern OBJECT_PTR primitive_integerp(OBJECT_PTR);
extern OBJECT_PTR primitive_floatp(OBJECT_PTR);
extern OBJECT_PTR prim_characterp(OBJECT_PTR);
extern OBJECT_PTR primitive_symbolp(OBJECT_PTR);
extern OBJECT_PTR primitive_stringp(OBJECT_PTR);
extern OBJECT_PTR primitive_arrayp(OBJECT_PTR);
extern OBJECT_PTR primitive_closurep(OBJECT_PTR);
extern OBJECT_PTR primitive_macrop(OBJECT_PTR);

extern OBJECT_PTR primitive_string(OBJECT_PTR);
extern OBJECT_PTR prim_make_array(OBJECT_PTR, ...);
extern OBJECT_PTR prim_array_set(OBJECT_PTR, OBJECT_PTR, OBJECT_PTR);
extern OBJECT_PTR prim_array_get(OBJECT_PTR, OBJECT_PTR);
extern OBJECT_PTR prim_sub_array(OBJECT_PTR, OBJECT_PTR, OBJECT_PTR);
extern OBJECT_PTR prim_array_length(OBJECT_PTR);
extern OBJECT_PTR prim_print_string(OBJECT_PTR);

extern OBJECT_PTR prim_load_fgn_lib(OBJECT_PTR);
extern OBJECT_PTR prim_call_fgn_func(OBJECT_PTR, OBJECT_PTR, OBJECT_PTR);

extern OBJECT_PTR prim_create_pkg(OBJECT_PTR);
extern OBJECT_PTR prim_in_package(OBJECT_PTR);
extern OBJECT_PTR prim_export_pkg(OBJECT_PTR, OBJECT_PTR);

extern OBJECT_PTR prim_create_image(OBJECT_PTR);
extern OBJECT_PTR prim_serialize(OBJECT_PTR, OBJECT_PTR);
extern OBJECT_PTR prim_deserialize(OBJECT_PTR);

extern OBJECT_PTR prim_load_file(OBJECT_PTR);

extern OBJECT_PTR primitive_time(OBJECT_PTR);

extern OBJECT_PTR primitive_env(OBJECT_PTR);
extern OBJECT_PTR prim_expand_macro(OBJECT_PTR);
//end of external functions

//forward declarations
OBJECT_PTR assignment_conversion(OBJECT_PTR, OBJECT_PTR);
OBJECT_PTR translate_to_il(OBJECT_PTR);
OBJECT_PTR ren_transform(OBJECT_PTR, binding_env_t *);
OBJECT_PTR cps_transform_var_literal(OBJECT_PTR);
OBJECT_PTR cps_transform_abstraction(OBJECT_PTR);
OBJECT_PTR cps_transform_application(OBJECT_PTR);
OBJECT_PTR cps_transform_primop(OBJECT_PTR);
OBJECT_PTR cps_transform_error(OBJECT_PTR);
OBJECT_PTR cps_transform_let(OBJECT_PTR);
OBJECT_PTR cps_transform_if(OBJECT_PTR, OBJECT_PTR, OBJECT_PTR);
OBJECT_PTR cps_transform_return_from(OBJECT_PTR);
OBJECT_PTR desugar_il(OBJECT_PTR);
OBJECT_PTR closure_conv_transform(OBJECT_PTR);
OBJECT_PTR range(int, int, int);
OBJECT_PTR closure_conv_transform_abs_cont(OBJECT_PTR);
OBJECT_PTR closure_conv_transform_abs_no_cont(OBJECT_PTR);
OBJECT_PTR backquote2(OBJECT_PTR);
OBJECT_PTR process_backquote(OBJECT_PTR);
unsigned int build_c_fragment(OBJECT_PTR, char *, BOOLEAN, BOOLEAN);
unsigned int build_c_string(OBJECT_PTR, char *, BOOLEAN);
OBJECT_PTR convert_native_fn_to_object(nativefn);
void add_top_level_sym(OBJECT_PTR, OBJECT_PTR);
int get_top_level_sym_value(OBJECT_PTR, OBJECT_PTR *);
void save_continuation(OBJECT_PTR);
OBJECT_PTR identity_function(OBJECT_PTR, OBJECT_PTR);
OBJECT_PTR create_closure(unsigned int, BOOLEAN, OBJECT_PTR, ...);
nativefn extract_native_fn(OBJECT_PTR);
OBJECT_PTR nth(OBJECT_PTR, OBJECT_PTR);
nativefn get_nativefn_value(OBJECT_PTR);
OBJECT_PTR reverse(OBJECT_PTR);
TCCState *create_tcc_state1();
TCCState *compile_functions(OBJECT_PTR);
char *extract_variable_string(OBJECT_PTR, BOOLEAN);
OBJECT_PTR call_cc1(OBJECT_PTR);
OBJECT_PTR create_fn_closure(OBJECT_PTR, nativefn, ...);
OBJECT_PTR expand_macro_full(OBJECT_PTR, BOOLEAN);
OBJECT_PTR expand_bodies(OBJECT_PTR);
OBJECT_PTR get_top_level_symbols();
int add_reference_to_top_level_sym(OBJECT_PTR, int, OBJECT_PTR);
int update_references(OBJECT_PTR, OBJECT_PTR);

void add_unmet_dependency(OBJECT_PTR, OBJECT_PTR, int);
BOOLEAN unmet_dependencies_exist(OBJECT_PTR);
void update_dependencies(OBJECT_PTR, OBJECT_PTR);

int location_of_and_rest(OBJECT_PTR);
OBJECT_PTR strip_and_rest(OBJECT_PTR);
void record_and_rest_closure(OBJECT_PTR, int);
OBJECT_PTR handle_and_rest_applications(OBJECT_PTR, OBJECT_PTR);

OBJECT_PTR get_free_variables(OBJECT_PTR);

OBJECT_PTR rewrite_zero_arg_applications(OBJECT_PTR);

OBJECT_PTR symbol_to_use(OBJECT_PTR);

OBJECT_PTR full_monty_eval(OBJECT_PTR);

void add_native_fn_source(nativefn, char *);

OBJECT_PTR cons_equivalent(OBJECT_PTR);
//end of forward declarations

binding_env_t *create_binding_env()
{
  binding_env_t *env = (binding_env_t *)malloc(sizeof(binding_env_t));
  env->count = 0;
  env->bindings = NULL;

  return env;
}

OBJECT_PTR get_binding_val(binding_env_t *env, OBJECT_PTR key)
{
  int i;
  for(i=0; i<env->count; i++)
    if(env->bindings[i].key == key)
      return env->bindings[i].val;

  return key;
}

void put_binding_val(binding_env_t *env, OBJECT_PTR key, OBJECT_PTR val)
{
  int i;

  BOOLEAN found = false;

  for(i=0;i<env->count;i++)
  {
    if(env->bindings[i].key == key)
    {
      env->bindings[i].val = val;
      found = true;
      break;
    }
  }

  if(!found)
  {
    env->count++;

    binding_t *temp = (binding_t *)realloc(env->bindings, env->count * sizeof(binding_t));

    assert(temp);

    env->bindings = temp;

    env->bindings[env->count-1].key = key;
    env->bindings[env->count-1].val = val;
  }
}

BOOLEAN exists(OBJECT_PTR obj, OBJECT_PTR lst)
{
  OBJECT_PTR rest = lst;

  while(rest != NIL)
  {
    if(equal(obj, car(rest)))
      return true;

    rest = cdr(rest);
  }
  return false;
}

OBJECT_PTR union1(unsigned int count, ...)
{
  va_list ap;
  OBJECT_PTR ret, rest;
  int i;

  if(!count)
    return NIL;

  ret = NIL;

  va_start(ap, count);

  for(i=0; i<count; i++)
  {
    rest = (OBJECT_PTR)va_arg(ap, int);

    while(rest != NIL)
    {
      OBJECT_PTR obj = car(rest);
      if(!exists(obj, ret))
      {
        if(ret == NIL)
          ret = cons(clone_object(obj), NIL);
        else
        {
          uintptr_t ptr = last_cell(ret) & POINTER_MASK;
          set_heap(ptr, 1, cons(clone_object(obj), NIL));
        }
      }

      rest = cdr(rest);
    }
  }

  va_end(ap);

  return ret;
}

OBJECT_PTR difference(OBJECT_PTR lst1, OBJECT_PTR lst2)
{
  OBJECT_PTR ret = NIL, rest = lst1;

  while(rest != NIL)
  {
    OBJECT_PTR obj = car(rest);
    if(!exists(obj, lst2))
    {
      if(ret == NIL)
        ret = cons(clone_object(obj), NIL);
      else
      {
        uintptr_t ptr = last_cell(ret) & POINTER_MASK;
        set_heap(ptr, 1, cons(clone_object(obj), NIL));        
      }
    }

    rest = cdr(rest);
  }

  return ret;
}

OBJECT_PTR map(OBJECT_PTR (*f)(OBJECT_PTR), OBJECT_PTR lst)
{
  assert(lst == NIL || IS_CONS_OBJECT(lst));

  OBJECT_PTR ret = NIL, rest = lst;

  while(rest != NIL)
  {
    OBJECT_PTR val = f(car(rest));

    if(ret == NIL)
      ret = cons(val, NIL);
    else
    {
      uintptr_t ptr = last_cell(ret) & POINTER_MASK;
      set_heap(ptr, 1, cons(val, NIL));        
    }

    rest = cdr(rest);
  }

  return ret;  
}

//version of map to work around absense of the free variables
//available when the lambda is defined in the call to map.
//works for two free variables (second and third parameters)
OBJECT_PTR map2(OBJECT_PTR (*f)(OBJECT_PTR, OBJECT_PTR, OBJECT_PTR), 
                OBJECT_PTR v1, 
                OBJECT_PTR v2, 
                OBJECT_PTR lst)
{
  OBJECT_PTR ret = NIL, rest = lst;

  while(rest != NIL)
  {
    OBJECT_PTR val = f(car(rest), v1, v2);

    if(ret == NIL)
      ret = cons(val, NIL);
    else
    {
      uintptr_t ptr = last_cell(ret) & POINTER_MASK;
      set_heap(ptr, 1, cons(val, NIL));        
    }

    rest = cdr(rest);
  }

  return ret;  
}

OBJECT_PTR map2_fn(OBJECT_PTR (*f)(OBJECT_PTR, 
                                   OBJECT_PTR (*)(OBJECT_PTR), 
                                   OBJECT_PTR), 
                   OBJECT_PTR (*f1)(OBJECT_PTR), 
                   OBJECT_PTR v2, 
                   OBJECT_PTR lst)
{
  OBJECT_PTR ret = NIL, rest = lst;

  while(rest != NIL)
  {
    OBJECT_PTR val = f(car(rest), f1, v2);

    if(ret == NIL)
      ret = cons(val, NIL);
    else
    {
      uintptr_t ptr = last_cell(ret) & POINTER_MASK;
      set_heap(ptr, 1, cons(val, NIL));        
    }

    rest = cdr(rest);
  }

  return ret;  
}

OBJECT_PTR concat(unsigned int count, ...)
{
  va_list ap;
  OBJECT_PTR lst, ret, rest;
  int i, start = 1;

  if(!count)
    return NIL;

  va_start(ap, count);

  lst = (OBJECT_PTR)va_arg(ap, int);

  if(!IS_CONS_OBJECT(lst) && lst != NIL)
    assert(false);

  //to skip NILs
  while(lst == NIL)
  {
    start++;

    if(start > count)
      return NIL;

    lst = (OBJECT_PTR)va_arg(ap, int);

    if(!IS_CONS_OBJECT(lst) && lst != NIL)
    {
      print_object(lst);
      assert(false);
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
      //print_object(lst);
      assert(false);
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

OBJECT_PTR subexps(OBJECT_PTR exp)
{
  OBJECT_PTR car_exp;

  if(IS_CONS_OBJECT(exp))
    car_exp = car(exp);

  if(is_atom(exp) || car_exp == ERROR)
    return NIL;
  else if(car_exp == IF)
    return list(3, second(exp), third(exp), fourth(exp));
  else if(car_exp == SET || car_exp == LAMBDA)
    return list(1, third(exp));
  else if(primop(car_exp))
    return cdr(exp);
  else if(car_exp == LET || car_exp == LETREC)
  {
    //OBJECT_PTR t1 = map(CADR, second(exp));
    //set_heap(last_cell(t1) & POINTER_MASK, 1, cons(third(exp), NIL));        
    return concat(2,
                  map(CADR, second(exp)),
                  list(1, third(exp)));
  }
  else
    return exp;
}

OBJECT_PTR temp1(OBJECT_PTR x)
{
  return list(1, car(x));
}

OBJECT_PTR mutating_ids(OBJECT_PTR exp)
{
  OBJECT_PTR car_exp;

  if(IS_CONS_OBJECT(exp))
    car_exp = car(exp);

  if(is_atom(exp) || car_exp == ERROR)
    return NIL;
  else if(car_exp == SET)
    return union1(2,
                  list(1, second(exp)),
                  mutating_ids(third(exp)));
  else if(car_exp == LAMBDA)
    return difference(mutating_ids(third(exp)),
                      second(exp));
  else if(car_exp == LET)
  {
    return union1(2,
                  union1(1,map(mutating_ids,
                               map(CADR, second(exp)))),
                  difference(mutating_ids(third(exp)),
                             union1(1, mutating_ids(map(temp1, second(exp))))));
  }
  else if(car_exp == LETREC)
  {
    return difference(union1(2,
                             mutating_ids(third(exp)),
                             union1(1,map(mutating_ids,
                                          map(CADR, second(exp))))),
                      union1(1, mutating_ids(map(temp1, second(exp)))));
  }
  else
    return union1(1, map(mutating_ids, subexps(exp)));
}

OBJECT_PTR intersection(OBJECT_PTR lst1, OBJECT_PTR lst2)
{
  OBJECT_PTR ret = NIL, rest = lst1;

  while(rest != NIL)
  {
    OBJECT_PTR obj = car(rest);
    if(exists(obj, lst2))
    {
      if(ret == NIL)
        ret = cons(clone_object(obj), NIL);
      else
      {
        uintptr_t ptr = last_cell(ret) & POINTER_MASK;
        set_heap(ptr, 1, cons(clone_object(obj), NIL));        
      }
    }

    rest = cdr(rest);
  }

  return ret;
}

OBJECT_PTR partition(OBJECT_PTR ids, OBJECT_PTR exps)
{
  OBJECT_PTR mids = union1(1, map(mutating_ids, exps));
  return cons(intersection(ids, mids),
              difference(ids, mids));
}

OBJECT_PTR temp2(OBJECT_PTR x)
{
  return list(2, x, list(3, CONS, x, NIL));
}

OBJECT_PTR wrap_cells(OBJECT_PTR ids, OBJECT_PTR exp)
{
  if(ids == NIL)
    return exp;
  else
    return list(3, LET, map(temp2, ids), exp);
}

OBJECT_PTR maybe_cell(OBJECT_PTR id, OBJECT_PTR ids, OBJECT_PTR exp)
{
  if(exists(id, ids))
     return list(3, CONS, exp, NIL);
  else
    return exp;
}

OBJECT_PTR temp3(OBJECT_PTR x, OBJECT_PTR v1, OBJECT_PTR v2)
{
  return list(2, 
              first(x),
              maybe_cell(first(x),
                         v1,
                         assignment_conversion(second(x), v2)));
}

OBJECT_PTR temp4(OBJECT_PTR x, OBJECT_PTR v1, OBJECT_PTR v2)
{
  return list(2,
              first(x),
              maybe_cell(first(x),
                         v1,
                         assignment_conversion(second(x), v2)));
}

OBJECT_PTR temp6(OBJECT_PTR x, 
                 OBJECT_PTR (*f)(OBJECT_PTR, OBJECT_PTR, OBJECT_PTR),
                 OBJECT_PTR v)
{
  return list(2,
              first(x),
              f(second(x), v, NIL));
}

OBJECT_PTR temp7(OBJECT_PTR x, OBJECT_PTR v, OBJECT_PTR dummy)
{
  return assignment_conversion(x, v);
}

OBJECT_PTR map2_fn1(OBJECT_PTR (*f)(OBJECT_PTR, 
                                    OBJECT_PTR (*)(OBJECT_PTR, OBJECT_PTR, OBJECT_PTR), 
                                   OBJECT_PTR), 
                    OBJECT_PTR (*f1)(OBJECT_PTR, OBJECT_PTR, OBJECT_PTR), 
                    OBJECT_PTR v2, 
                    OBJECT_PTR lst)
{
  OBJECT_PTR ret = NIL, rest = lst;

  while(rest != NIL)
  {
    OBJECT_PTR val = f(car(rest), f1, v2);

    if(ret == NIL)
      ret = cons(val, NIL);
    else
    {
      uintptr_t ptr = last_cell(ret) & POINTER_MASK;
      set_heap(ptr, 1, cons(val, NIL));        
    }

    rest = cdr(rest);
  }

  return ret;  
}

OBJECT_PTR mapsub1(OBJECT_PTR exp, 
                   OBJECT_PTR (*tf)(OBJECT_PTR, OBJECT_PTR, OBJECT_PTR),
                   OBJECT_PTR v)
{
  OBJECT_PTR car_exp;

  if(IS_CONS_OBJECT(exp))
    car_exp = car(exp);

  if(is_atom(exp) || car_exp == ERROR)
    return exp;
  else if(car_exp == IF)
    return list(4, IF, tf(second(exp),v,NIL), tf(third(exp),v,NIL), tf(fourth(exp),v,NIL));
  else if(car_exp == SET)
    return list(3, SET, second(exp), tf(third(exp),v,NIL));
  else if(car_exp == LAMBDA)
    return list(3, LAMBDA, second(exp), tf(third(exp),v,NIL));
  else if(primop(car_exp))
    return cons(first(exp),
                map2(tf, v, NIL, cdr(exp)));
  else if(car_exp == LET || car_exp == LETREC)
    return list(3,
                car_exp,
                map2_fn1(temp6, tf, v, second(exp)),
                tf(third(exp),v,NIL));
  else
    return map2(tf, v, NIL, exp);
}

BOOLEAN is_quoted_expression(OBJECT_PTR exp)
{
  if(IS_CONS_OBJECT(exp) && car(exp) == QUOTE)
    return true;
  else
    return false;
}

BOOLEAN is_backquoted_expression(OBJECT_PTR exp)
{
  if(IS_CONS_OBJECT(exp) && car(exp) == BACKQUOTE)
    return true;
  else
    return false;
}

OBJECT_PTR assignment_conversion(OBJECT_PTR exp, OBJECT_PTR ids)
{
  OBJECT_PTR first_exp;

  if(IS_CONS_OBJECT(exp))
    first_exp = first(exp);

  if(IS_SYMBOL_OBJECT(exp))
  {
    if(exists(exp, ids))
      return list(2, CAR, exp);
    else
      return exp;
  }
  else if(is_atom(exp) || is_quoted_expression(exp) || is_backquoted_expression(exp))
    return exp;
  else if(first_exp == SET)
    return list(3, SETCAR, second(exp), assignment_conversion(third(exp),
                                                              ids));
  else if(first_exp == LAMBDA)
  {
    OBJECT_PTR pids = partition(second(exp),
                                list(1, third(exp)));
    OBJECT_PTR mids = car(pids);
    OBJECT_PTR uids = cdr(pids);
    
    return list(3, 
                LAMBDA, 
                second(exp),
                wrap_cells(mids,
                           assignment_conversion(third(exp),
                                                 difference(union1(2, ids, mids),
                                                            uids))));
  }
  else if(first_exp == LET)
  {
    OBJECT_PTR pids = partition(map(car, second(exp)),
                                list(1, third(exp)));

    OBJECT_PTR mids = car(pids);
    OBJECT_PTR uids = cdr(pids);

    return list(3, 
                LET, 
                map2(temp3, mids, ids, second(exp)),
                assignment_conversion(third(exp),
                                      difference(union1(2, ids, mids),
                                                 uids)));
  }

  else if(first_exp == LETREC)
  {
    OBJECT_PTR pids = partition(map(car,
                                    second(exp)),
                                concat(2,
                                       map(CADR,
                                           second(exp)),
                                       list(1, third(exp))));

    OBJECT_PTR mids = car(pids);
    OBJECT_PTR uids = cdr(pids);

    OBJECT_PTR ids1 = difference(union1(2, ids, mids),
                                 uids);

    return list(3,
                LETREC,
                map2(temp4, mids, ids1, second(exp)),
                assignment_conversion(third(exp), ids1));
      
  }
  else
    return mapsub1(exp, temp7, ids);
}

OBJECT_PTR temp5(OBJECT_PTR x, 
                 OBJECT_PTR (*f)(OBJECT_PTR),
                 OBJECT_PTR v)
{
  return list(2,
              first(x),
              f(second(x)));
}

OBJECT_PTR mapsub(OBJECT_PTR exp, 
                  OBJECT_PTR (*tf)(OBJECT_PTR))
{
  OBJECT_PTR car_exp;

  if(IS_CONS_OBJECT(exp))
    car_exp = car(exp);

  if(is_atom(exp) || car_exp == ERROR)
    return exp;
  else if(car_exp == IF)
    return list(4, IF, tf(second(exp)), tf(third(exp)), tf(fourth(exp)));
  else if(car_exp == SET)
    return list(3, SET, second(exp), tf(third(exp)));
  else if(car_exp == LAMBDA)
    return list(3, LAMBDA, second(exp), tf(third(exp)));
  else if(primop(car_exp))
    return cons(first(exp),
                map(tf, cdr(exp)));
  else if(car_exp == LET || car_exp == LETREC)
    return list(3,
                car_exp,
                map2_fn(temp5, tf, NIL, second(exp)),
                tf(third(exp)));
  else
    return map(tf, exp);
}

OBJECT_PTR subst(OBJECT_PTR exp1,
                 OBJECT_PTR exp2,
                 OBJECT_PTR exp)
{
  if(exp == NIL)
    return NIL;
  else if(is_atom(exp))
  {
    if(equal(exp,exp2))
      return exp1;
    else
      return exp;
  }
  else
  {
    return cons(subst(exp1, exp2, car(exp)),
                subst(exp1, exp2, cdr(exp)));
  }
}

OBJECT_PTR msubst(OBJECT_PTR ids, OBJECT_PTR exp)
{
  OBJECT_PTR res = clone_object(exp);

  OBJECT_PTR rest = ids;

  while(rest != NIL)
  {
    res = subst(list(2, CAR, car(rest)),
                car(rest),
                res);

    rest = cdr(rest);
  }

  return res;
}

OBJECT_PTR temp8(OBJECT_PTR x)
{
  return list(2, first(x), list(3, CONS, NIL, NIL));
}

OBJECT_PTR temp9(OBJECT_PTR x,
                 OBJECT_PTR v1,
                 OBJECT_PTR v2)
{
  return list(2,
              gensym(),
              list(3, 
                   SETCAR, 
                   first(x),
                   msubst(map(car, second(v1)),
                          translate_to_il(second(x)))));
}

OBJECT_PTR translate_to_il(OBJECT_PTR exp)
{
  if(is_atom(exp) || is_quoted_expression(exp) || is_backquoted_expression(exp))
    return exp;
  else if(car(exp) == LETREC)
  {
    return list(3,
                LET,
                map(temp8, second(exp)),
                desugar_il(list(3,
                                LET1,
                                map2(temp9, exp, NIL,second(exp)),
                                msubst(map(car, second(exp)),
                                       translate_to_il(third(exp))))));
  }
  else
    return cons(translate_to_il(car(exp)),
                translate_to_il(cdr(exp)));
}

OBJECT_PTR desugar_il(OBJECT_PTR exp)
{
  if(second(exp) == NIL)
    return third(exp);
  else
    return list(3,
                LET,
                list(1, first(second(exp))),
                desugar_il(list(3,
                                LET1,
                                cdr(second(exp)),
                                third(exp))));
}

OBJECT_PTR temp13(OBJECT_PTR x)
{
  return gensym();
}

/* OBJECT_PTR *generate_fresh_ids(unsigned int count) */
/* { */
/*   OBJECT_PTR *syms = (OBJECT_PTR *)malloc(count * sizeof(OBJECT_PTR)); */

/*   assert(syms); */

/*   int i; */

/*   for(i=0; i<count; i++) */
/*     syms[i] = gensym(); */

/*   return syms; */
/* } */
OBJECT_PTR generate_fresh_ids(unsigned int count)
{
  return map(temp13, range(1,count,1));
}

//restricted version of mapcar that works for two lists
OBJECT_PTR mapcar(OBJECT_PTR (*f)(OBJECT_PTR,OBJECT_PTR),
                  OBJECT_PTR list1,
                  OBJECT_PTR list2)
{
  OBJECT_PTR ret = NIL, rest1 = list1, rest2 = list2;

  while(rest1 != NIL || rest2 != NIL)
  {
    OBJECT_PTR val = f(car(rest1), car(rest2));

    if(ret == NIL)
      ret = cons(val, NIL);
    else
    {
      uintptr_t ptr = last_cell(ret) & POINTER_MASK;
      set_heap(ptr, 1, cons(val, NIL));        
    }

    rest1 = cdr(rest1);
    rest2 = cdr(rest2);
  }

  return ret;    
}

OBJECT_PTR temp91(OBJECT_PTR x, binding_env_t *env, OBJECT_PTR v)
{
  return list(2,
              first(x),
              ren_transform(second(second(x)), env));
}

OBJECT_PTR map2_for_ren_transform(OBJECT_PTR (*f)(OBJECT_PTR, binding_env_t *, OBJECT_PTR),
                                  binding_env_t *env,
                                  OBJECT_PTR v2,
                                  OBJECT_PTR lst)
{
  OBJECT_PTR ret = NIL, rest = lst;

  while(rest != NIL)
  {
    OBJECT_PTR val = f(car(rest), env, v2);

    if(ret == NIL)
      ret = cons(val, NIL);
    else
    {
      uintptr_t ptr = last_cell(ret) & POINTER_MASK;
      set_heap(ptr, 1, cons(val, NIL));        
    }

    rest = cdr(rest);
  }

  return ret;    
}

OBJECT_PTR pair( OBJECT_PTR list1,
                 OBJECT_PTR list2)
{
  OBJECT_PTR ret = NIL, rest1 = list1, rest2 = list2;

  while(rest1 != NIL || rest2 != NIL)
  {
    OBJECT_PTR val = list(2, car(rest1), car(rest2));

    if(ret == NIL)
      ret = cons(val, NIL);
    else
    {
      uintptr_t ptr = last_cell(ret) & POINTER_MASK;
      set_heap(ptr, 1, cons(val, NIL));        
    }

    rest1 = cdr(rest1);
    rest2 = cdr(rest2);
  }

  return ret;    
}

OBJECT_PTR ren_transform(OBJECT_PTR exp, binding_env_t *env)
{
  if(exp == NIL)
    return NIL;
  else if(IS_SYMBOL_OBJECT(exp))
    return get_binding_val(env, exp);
  else if(is_atom(exp) || is_quoted_expression(exp) || is_backquoted_expression(exp))
    return exp;
  else if(car(exp) == LAMBDA)
  {
    OBJECT_PTR fresh_ids = generate_fresh_ids(cons_length(second(exp)));

    OBJECT_PTR new_bindings = mapcar(cons, second(exp), fresh_ids);

    OBJECT_PTR rest = new_bindings;

    while(rest != NIL)
    {
      put_binding_val(env, car(car(rest)), cdr(car(rest)));
      rest = cdr(rest);
    }

    return list(3,
                LAMBDA,
                fresh_ids,
                ren_transform(third(exp), env));
  }
  else if(car(exp) == LET)
  {
    OBJECT_PTR fresh_ids = generate_fresh_ids(cons_length(second(exp)));

    OBJECT_PTR new_bindings = mapcar(cons,
                                     map(car, second(exp)),
                                     fresh_ids);

    OBJECT_PTR rest = new_bindings;

    while(rest != NIL)
    {
      put_binding_val(env, car(car(rest)), cdr(car(rest)));
      rest = cdr(rest);
    }

    return list(3,
                LET,
                map2_for_ren_transform(temp91, env, NIL, pair(fresh_ids, second(exp))),
                ren_transform(third(exp), env));
  }
  else
    return cons(ren_transform(car(exp), env),
                ren_transform(cdr(exp), env));
}

OBJECT_PTR temp10(OBJECT_PTR x)
{
  if(IS_CONS_OBJECT(x))
    return x;
  else return list(1, x);
}

OBJECT_PTR flatten(OBJECT_PTR lst)
{
  OBJECT_PTR ret = NIL, rest = lst;

  while(rest != NIL)
  {
    if(IS_CONS_OBJECT(car(rest)) || car(rest) == NIL)
    {
      OBJECT_PTR rest1 = car(rest);

      while(rest1 != NIL)
      {
        if(ret == NIL)
          ret = cons(car(rest1), NIL);
        else
        {
          uintptr_t ptr = last_cell(ret) & POINTER_MASK;
          set_heap(ptr, 1, cons(car(rest1), NIL));        
        }

        rest1 = cdr(rest1);
      }
    }
    else
    {
      if(ret == NIL)
        ret = cons(car(rest), NIL);
      else
      {
        uintptr_t ptr = last_cell(ret) & POINTER_MASK;
        set_heap(ptr, 1, cons(car(rest), NIL));        
      }
    }

    rest = cdr(rest);
  }

  return ret;  
}

OBJECT_PTR free_ids_il(OBJECT_PTR exp)
{
  OBJECT_PTR car_exp;

  if(IS_CONS_OBJECT(exp))
    car_exp = car(exp);

  if(exp == NIL)
    return NIL;
  else if(IS_SYMBOL_OBJECT(exp))
  {
    if(primop(exp))
      return NIL;
    else
      return list(1, exp);
  }
  else if(is_atom(exp))
    return NIL;
  else if(car_exp == QUOTE)
    return NIL;
  else if(car_exp == IF)
    return union1(3,
                  free_ids_il(second(exp)),
                  free_ids_il(third(exp)),
                  free_ids_il(fourth(exp)));
  else if(car_exp == LAMBDA)
    return difference(union1(2, free_ids_il(third(exp)), free_ids_il(fourth(exp))),
                      second(exp));
  else if(car_exp == DEFINE)
    return difference(free_ids_il(third(exp)), list(1, second(exp)));
  else if(car_exp == LET)
    return union1(2,
                  flatten(map(free_ids_il,
                              map(CADR, second(exp)))),
                  difference(free_ids_il(third(exp)),
                             map(car, second(exp))));
  else if(car_exp == LETREC)
    return difference(union1(2,
                             flatten(map(free_ids_il,
                                         map(CADR, second(exp)))),
                             free_ids_il(third(exp))),
                      map(car, second(exp)));
  else if(car_exp == ERROR || car_exp == CALL_CC)
    return free_ids_il(second(exp));
  else
    return flatten(cons(free_ids_il(car(exp)),
                        free_ids_il(cdr(exp))));
}

OBJECT_PTR simplify_il_empty_let(OBJECT_PTR exp)
{
  if(is_atom(exp) || is_quoted_expression(exp) || is_backquoted_expression(exp))
    return exp;
  else if(first(exp) == LET && second(exp) == NIL)
    return third(exp);
  else
    return cons(simplify_il_empty_let(car(exp)),
                simplify_il_empty_let(cdr(exp)));
}

OBJECT_PTR temp11(OBJECT_PTR v1, OBJECT_PTR v2)
{
  return list(2, v1, v2);
}

OBJECT_PTR simplify_il_implicit_let(OBJECT_PTR exp)
{
  if(IS_CONS_OBJECT(exp) &&
     IS_CONS_OBJECT(car(exp)) &&
     CAAR(exp) == LAMBDA)
    return list(3,
                LET,
                mapcar(temp11, second(first(exp)), cdr(exp)),
                third(first(exp)));
  else
    return exp;
}

OBJECT_PTR simplify_il_eta(OBJECT_PTR exp)
{
  if(is_atom(exp) || is_quoted_expression(exp) || is_backquoted_expression(exp))
    return exp;
  else if(car(exp) == LAMBDA &&
          IS_CONS_OBJECT(third(exp)) &&
          !primop(first(third(exp))) &&
          first(third(exp)) != LET &&
          (IS_SYMBOL_OBJECT(first(third(exp))) ||
           (IS_CONS_OBJECT(first(third(exp))) &&
            first(first(third(exp)) == LAMBDA))) &&
          second(exp) == CDDR(third(exp)) &&
          intersection(free_ids_il(first(third(exp))),
                       second(exp)) == NIL)
    return first(third(exp));
  else
    return cons(simplify_il_eta(car(exp)),
                simplify_il_eta(cdr(exp)));
}

OBJECT_PTR simplify_il_copy_prop(OBJECT_PTR exp)
{
  if(is_atom(exp) || is_quoted_expression(exp) || is_backquoted_expression(exp))
    return exp;
  else if(car(exp) == LET &&
          cons_length(second(exp)) == 2 &&
          IS_SYMBOL_OBJECT(first(first(second(exp)))) &&
          IS_SYMBOL_OBJECT(second(first(second(exp)))))
    return subst(second(second(first(exp))),
                 first(second(first(exp))),
                 third(exp));
  else
    return cons(simplify_il_copy_prop(car(exp)),
                simplify_il_copy_prop(cdr(exp)));
}

OBJECT_PTR cps_transform(OBJECT_PTR exp)
{
  OBJECT_PTR car_exp;

  if(IS_CONS_OBJECT(exp))
    car_exp = car(exp);

  if(is_atom(exp) || is_quoted_expression(exp) || is_backquoted_expression(exp))
    return cps_transform_var_literal(exp);
  else if(car_exp == LAMBDA)
    return cps_transform_abstraction(exp);
  else if(car_exp == LET)
    return cps_transform_let(exp);
  else if(primop(car_exp))
  {
    if(car_exp == RETURN_FROM)
      return cps_transform_return_from(exp);
    else
      return cps_transform_primop(exp);
  }
  else if(car_exp == IF)
    return cps_transform_if(second(exp),
                            third(exp),
                            fourth(exp));
  else if(car_exp == ERROR)
    return cps_transform_error(second(exp));
  else
    return cps_transform_application(exp);
}

OBJECT_PTR cps_transform_var_literal(OBJECT_PTR exp)
{
  OBJECT_PTR ik = gensym();

  return list(4,
              LAMBDA,
              list(1, ik),
              list(2, SAVE_CONTINUATION, ik),
              list(2, ik, exp));
}

OBJECT_PTR cps_transform_abstraction(OBJECT_PTR exp)
{
  OBJECT_PTR ik = gensym();
  OBJECT_PTR iabs = gensym();
  OBJECT_PTR ikcall = gensym();

  return list(4,
              LAMBDA,
              list(1, ik),
              list(2, SAVE_CONTINUATION, ik),
              list(3,
                   LET,
                   list(1,list(2,
                               iabs,
                               list(4,
                                    LAMBDA,
                                    concat(2, second(exp), list(1, ikcall)),
                                    list(2, SAVE_CONTINUATION, ikcall),
                                    list(2, cps_transform(third(exp)), ikcall)))),
                   list(2, ik, iabs)));
}

OBJECT_PTR cps_trans_app_internal(OBJECT_PTR exp,
                                  OBJECT_PTR syms,
                                  OBJECT_PTR first_sym)
{
  if(exp == NIL)
    return concat(2, syms, list(1, first_sym));
  else
  {
    OBJECT_PTR i = gensym();

    return list(2,
                cps_transform(car(exp)),
                list(3,
                     LAMBDA,
                     list(1,i),
                     cps_trans_app_internal(cdr(exp),
                                            concat(2, syms, list(1,i)),
                                            first_sym)));
  }
}

OBJECT_PTR cps_transform_application(OBJECT_PTR exp)
{
  OBJECT_PTR ik = gensym();

  return list(4,
              LAMBDA,
              list(1, ik),
              list(2, SAVE_CONTINUATION, ik),
              cps_trans_app_internal(exp, NIL, ik));
}

OBJECT_PTR cps_trans_let_internal(OBJECT_PTR body,
                                  OBJECT_PTR bindings,
                                  OBJECT_PTR first_sym)
{
  if(bindings == NIL)
    return list(2, cps_transform(body), first_sym);
  else
    return list(2,
                cps_transform(second(first(bindings))),
                list(3,
                     LAMBDA,
                     list(1, first(first(bindings))),
                     cps_trans_let_internal(body,
                                            cdr(bindings),
                                            first_sym)));
}

OBJECT_PTR cps_transform_let(OBJECT_PTR exp)
{
  OBJECT_PTR ik = gensym();

  return list(4,
              LAMBDA,
              list(1, ik),
              list(2, SAVE_CONTINUATION, ik),
              cps_trans_let_internal(third(exp),
                                     second(exp),
                                     ik));
}

OBJECT_PTR cps_trans_primop_internal(OBJECT_PTR primop1,
                                     OBJECT_PTR args,
                                     OBJECT_PTR syms,
                                     OBJECT_PTR first_sym)
{
  if(args == NIL)
  {
    OBJECT_PTR ians = gensym();

    return list(3,
                LET,
                list(1, list(2, 
                             ians, 
                             concat(2, 
                                    is_vararg_primop(primop1) ? list(2, primop1, convert_int_to_object(cons_length(syms))) :
                                                                list(1, primop1),
                                    syms))),
                list(2, first_sym, ians));
  }
  else
  {
    OBJECT_PTR i = gensym();

    return list(2,
                cps_transform(car(args)),
                list(3,
                     LAMBDA,
                     list(1, i),
                     cps_trans_primop_internal(primop1,
                                               cdr(args),
                                               concat(2, 
                                                      syms, 
                                                      list(1,i)),
                                               first_sym)));
  }
}

OBJECT_PTR cps_transform_primop(OBJECT_PTR exp)
{
  OBJECT_PTR ik = gensym();

  return list(4,
              LAMBDA,
              list(1, ik),
              list(2, SAVE_CONTINUATION, ik),
              cps_trans_primop_internal(car(exp),
                                        cdr(exp),
                                        NIL,
                                        ik));
}

OBJECT_PTR cps_transform_return_from(OBJECT_PTR exp)
{
  OBJECT_PTR ik = gensym();
  OBJECT_PTR i1 = gensym();
  OBJECT_PTR i2 = gensym();

  return list(4,
              LAMBDA,
              list(1, ik),
              list(2, SAVE_CONTINUATION, ik),
              list(2,
                   cps_transform(second(exp)),
                   list(3,
                        LAMBDA,
                        list(1, i1),
                        list(2,
                             cps_transform(third(exp)),
                             list(3,
                                  LAMBDA,
                                  list(1, i2),
                                  list(2,
                                       list(2, GET_CONTINUATION, i1),
                                       i2))))));
}

BOOLEAN primop(OBJECT_PTR sym)
{
  return arithop(sym)            ||
    core_op(sym)                 ||
    string_array_op(sym)         ||
    predicate_op(sym)            ||
    ffi_op(sym)                  ||
    package_op(sym)              ||
    serialization_op(sym)        ||
    interpreter_specific_op(sym) ||
    debug_op(sym)                ||
    perf_op(sym);
}

OBJECT_PTR cps_transform_if(OBJECT_PTR test,
                            OBJECT_PTR then,
                            OBJECT_PTR else1)
{
  OBJECT_PTR ik = gensym();
  OBJECT_PTR itest = gensym();

  return list(4,
              LAMBDA,
              list(1, ik),
              list(2, SAVE_CONTINUATION, ik),
              list(2,
                   cps_transform(test),
                   list(3,
                        LAMBDA,
                        list(1, itest),
                        list(4,
                             IF,
                             itest,
                             list(2, cps_transform(then), ik),
                             list(2, cps_transform(else1), ik)))));
}

OBJECT_PTR cps_transform_error(OBJECT_PTR exp)
{
  OBJECT_PTR ik = gensym();
  OBJECT_PTR ians = gensym();

  return list(4,
              LAMBDA,
              list(1, ik),
              list(2, SAVE_CONTINUATION, ik),
              list(2,
                   cps_transform(exp),
                   list(3,
                        LAMBDA,
                        list(1, ians),
                        list(2, ERROR, ians))));
}

OBJECT_PTR closure_conv_transform_let(OBJECT_PTR exp)
{
  OBJECT_PTR exp1 = closure_conv_transform(second(first(second(exp))));
  OBJECT_PTR icode = gensym();

  return list(3,
              LET1,
              list(2,
                   list(2, icode, third(exp1)),
                   list(2, 
                        first(first(second(exp))),
                        concat(2,
                               list(3, CREATE_FN_CLOSURE, convert_int_to_object(cons_length(CDDDR(exp1))), icode),
                               CDDDR(exp1)))),
              closure_conv_transform(third(exp)));
}

OBJECT_PTR closure_conv_transform_app(OBJECT_PTR exp)
{
  OBJECT_PTR iclo = gensym();
  OBJECT_PTR icode = gensym();

  return list(3,
              LET1,
              list(2,
                   list(2, iclo, closure_conv_transform(first(exp))),
                   //list(2, icode, list(3, NTH, convert_int_to_object(0), iclo))),
                   list(2, icode, list(2, EXTRACT_NATIVE_FN, iclo))),
              concat(2,
                     list(2, icode, iclo),
                     map(closure_conv_transform, cdr(exp))));
}

OBJECT_PTR closure_conv_transform(OBJECT_PTR exp)
{
  OBJECT_PTR car_exp;

  if(IS_CONS_OBJECT(exp))
    car_exp = car(exp);

  if(exp == NIL)
    return NIL;
  else if(is_atom(exp) || is_quoted_expression(exp) || is_backquoted_expression(exp))
    return exp;
  else if(car_exp == LAMBDA)
  {
    if(cons_length(exp) == 3)
      return closure_conv_transform_abs_no_cont(exp);
    else
      return closure_conv_transform_abs_cont(exp);
  }
  else if(car_exp == LET)
  {
    OBJECT_PTR rval = second(first(second(exp)));

    if(IS_CONS_OBJECT(rval) &&
       first(rval) == LAMBDA)
      return closure_conv_transform_let(exp);
    else
      return mapsub(exp, closure_conv_transform);
  }
  else if(primop(car_exp) ||
          car_exp == IF   ||
          car_exp == ERROR)
    return mapsub(exp, closure_conv_transform);
  else
    return closure_conv_transform_app(exp);
}

OBJECT_PTR lift_transform(OBJECT_PTR exp, OBJECT_PTR bindings)
{
  if(is_atom(exp) || is_quoted_expression(exp) || is_backquoted_expression(exp))
    return cons(exp, bindings);
  else if(car(exp) == LAMBDA)
  {
    OBJECT_PTR sym = gensym();
    OBJECT_PTR res = lift_transform(cons_length(exp) == 3 ? third(exp) : fourth(exp),
                                    bindings);

    return cons(sym,
                concat(2,
                       list(1,
                            list(2,
                                 sym,
                                 cons_length(exp) == 3 ? list(3, LAMBDA, second(exp), car(res)) : list(4, LAMBDA, second(exp), third(exp), car(res)))),
                       cdr(res)));
  }
  else
  {
    OBJECT_PTR car_res = lift_transform(car(exp), bindings);
    OBJECT_PTR cdr_res = lift_transform(cdr(exp), cdr(car_res));

    return cons(cons(car(car_res), car(cdr_res)),
                cdr(cdr_res));
  }
}

OBJECT_PTR simplify_il(OBJECT_PTR exp)
{
  return simplify_il_copy_prop(
    simplify_il_eta(
      simplify_il_implicit_let(
        simplify_il_empty_let(exp))));
}

OBJECT_PTR replace_macros(OBJECT_PTR exp)
{
  if(exp == MACRO)
    return LAMBDA;
  else if(is_atom(exp) || is_quoted_expression(exp) || is_backquoted_expression(exp))
    return exp;
  else
    return cons(replace_macros(car(exp)),
                replace_macros(cdr(exp)));
}

OBJECT_PTR get_top_level_symbols()
{
  OBJECT_PTR ret = NIL;

  int i;

  for(i=0; i < nof_global_vars; i++)
  {
    if(top_level_symbols[i].delete_flag)
      continue;

    OBJECT_PTR val = top_level_symbols[i].sym;

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

OBJECT_PTR replace_t(OBJECT_PTR exp)
{
  if(is_atom(exp))
  {
    if(exp == TRUE)
      return list(2, QUOTE, TRUE);
    else
      return exp;
  }
  else if(cons_length(exp) == 2 &&
          car(exp) == QUOTE     &&
          second(exp) == TRUE)
    return exp;
  else
    return map(replace_t, exp);
}

OBJECT_PTR compile_and_evaluate(OBJECT_PTR exp)
{
  OBJECT_PTR res = clone_object(exp);

  res = replace_t(res);
//print_object(res);printf("\n");getchar();

  BOOLEAN macro_flag = false;

  if(!is_valid_expression(exp))
    return NIL; //error would have been raised in is_valid_expression()

  if(IS_CONS_OBJECT(res) && car(res) == MACRO)
  {
    macro_flag = true;
    res = list(3, LAMBDA, second(res), third(res));
  }

  //this is not needed since
  //we're disallowing internal macros
  res = replace_macros(res);
//print_object(res);printf("\n");getchar();

  //res = handle_and_rest_applications(res, get_free_variables(res));
//print_object(res);printf("\n");getchar();

  //TODO: only free ids that correspond
  //to top level macro objects should be
  //considered for the macro expansion
  OBJECT_PTR prev_res = res;
  res = expand_macro_full(res, true);
//print_object(res);printf("\n");getchar();

  res = replace_t(res);
  //res = replace_macros(res);
  //res = handle_and_rest_applications(res, get_free_variables(res));

  if(res != prev_res && IS_CONS_OBJECT(res) && car(res) == DEFINE)
  {
    if(!IS_SYMBOL_OBJECT(second(res)))
    {
      raise_error("Second argument to DEFINE should be a symbol");
      return NIL;
    }

    OBJECT_PTR symbol_to_be_used = symbol_to_use(second(res));

    printf("%d\n", symbol_to_be_used);

    //this is to prevent unmet dependecy error
    //for recursive definitions
    add_top_level_sym(symbol_to_be_used, cons(NIL, NIL));

    OBJECT_PTR res1 = compile_and_evaluate(third(res));

    add_top_level_sym(symbol_to_be_used, cons(res1, NIL));

    update_dependencies(symbol_to_be_used, cons(res1, NIL));

    if(update_references(symbol_to_be_used, res1))
    {
      raise_error("Update of reference to top level symbol failed");
      return NIL;
    }

    return res1;
  }

  res = expand_bodies(res);
//print_object(res);getchar();

  res = process_backquote(res);
//print_object(res);printf("\n");getchar();

  res = assignment_conversion(res, concat(2, 
                                          get_top_level_symbols(),
                                          //free_ids_il(exp)));
                                          get_free_variables(exp)));
//print_object(res);printf("\n");getchar();

  res = translate_to_il(res);
//print_object(res);printf("\n");getchar();

  binding_env_t *env = create_binding_env();
  res = ren_transform(res, env);
//print_object(res);printf("\n");getchar();
  free(env->bindings);
  env->bindings = NULL;
  free(env);

  res = simplify_il(res);
//print_object(res);printf("\n");getchar();

  res = cps_transform(res);
//print_object(res);printf("\n");getchar();

  res = closure_conv_transform(res);
//print_object(res);printf("\n");getchar();

  res = lift_transform(res, NIL);
//print_object(res);printf("\n");getchar();

  OBJECT_PTR lambdas = reverse(cdr(res));

  TCCState *tcc_state1 = compile_functions(lambdas);

  while(lambdas != NIL)
  {
    OBJECT_PTR lambda = car(lambdas);

    char *fname = extract_variable_string(first(lambda), true);

    add_top_level_sym(first(lambda),
                      convert_native_fn_to_object((nativefn)tcc_get_symbol(tcc_state1, fname)));

    char source[1000];
    memset(source, 1000, '\0');
    build_c_string(lambda, source, true);

    assert(strlen(source)<=1000);

    add_native_fn_source((nativefn)tcc_get_symbol(tcc_state1, fname),
                         source);

    lambdas = cdr(lambdas);
    free(fname);
  }

  OBJECT_PTR closure_components = CDDR(first(res));
  OBJECT_PTR out;
  int retval = get_top_level_sym_value(car(closure_components), &out);

  assert(retval == 0);

  OBJECT_PTR ret = cons(out, NIL);
  OBJECT_PTR rest = cdr(closure_components);

  //OBJECT_PTR undefined_syms = NIL;
  int pos = 1;

  while(rest != NIL)
  {
    OBJECT_PTR out1;

    OBJECT_PTR symbol_to_be_used = symbol_to_use(car(rest));

    int retval = get_top_level_sym_value(symbol_to_be_used, &out1);

    if(retval && symbol_to_be_used != SAVE_CONTINUATION && symbol_to_be_used != TRUE)
    {
      char buf[200];
      memset(buf, 200, '\0');
      sprintf(buf, "Undefined symbol: %s", get_symbol_name(symbol_to_be_used));
      //raise_error(buf);
      //return NIL;
      printf("%s\n", buf);
      /* add_unmet_dependency(((ret >> OBJECT_SHIFT) << OBJECT_SHIFT) + FUNCTION2_TAG, */
      /*                      car(rest), */
      /*                      pos); */
      /* undefined_syms = cons(cons(car(rest),  */
      /*                            convert_int_to_object(pos)), */
      /*                       undefined_syms); */
      uintptr_t ptr = last_cell(ret) & POINTER_MASK;
      set_heap(ptr, 1, cons(cons(NIL, NIL), NIL));        
    }
    else
    {
      uintptr_t ptr = last_cell(ret) & POINTER_MASK;
      set_heap(ptr, 1, cons(out1, NIL));        
    }

    rest = cdr(rest);
    pos++;
  }

  ret = ((ret >> OBJECT_SHIFT) << OBJECT_SHIFT) + FUNCTION2_TAG;

  nativefn tt = extract_native_fn(ret);

  if(!tt)
    return NIL;

  OBJECT_PTR ret1 = tt(ret, idclo);

  if(macro_flag)
    ret1 = ((ret1 >> OBJECT_SHIFT) << OBJECT_SHIFT) + MACRO2_TAG;

  if(!IS_FUNCTION2_OBJECT(ret1) && !IS_MACRO2_OBJECT(ret1))
    return ret1;

  //record which top-level symbols
  //are referred by the closure.
  //this is necessary to update the
  //closure object when any of these
  //top level symbols are updated
  int i=1;
  rest = cdr(closure_components);

  while(rest != NIL)
  {
    OBJECT_PTR symbol_to_be_used = symbol_to_use(car(rest));

    /* if(add_reference_to_top_level_sym(car(rest), i, ret1)) */
    /* { */
    /*   raise_error("Unable to add reference to top level"); */
    /*   return NIL; */
    /* } */
    add_reference_to_top_level_sym(symbol_to_be_used, i, ret1);

    //record any unmet dependencies for the closure
    OBJECT_PTR out1;
    int retval = get_top_level_sym_value(symbol_to_be_used, &out1);

    if(retval && symbol_to_be_used != SAVE_CONTINUATION && symbol_to_be_used != TRUE)
      add_unmet_dependency(ret1, symbol_to_be_used, i);

    rest = cdr(rest);
    i++;
  }

  return ret1;
}

BOOLEAN arithop(OBJECT_PTR sym)
{
  return sym == ADD ||
    sym == SUB      ||
    sym == MULT     ||
    sym == DIV      ||
    sym == GT       ||
    sym == LT       ||
    sym == LEQ      ||
    sym == GEQ;
}

BOOLEAN core_op(OBJECT_PTR sym)
{
  return sym == ATOM    ||
    sym == CONCAT       ||
    sym == DEFINE       ||
    sym == QUOTE        ||
    sym == EQ           ||
    sym == CALL_CC1     ||
    sym == SAVE_CONTINUATION ||
    sym == SET          ||
    sym == ERROR        ||
    sym == LST          ||
    sym == CONS         ||
    sym == CAR          ||
    sym == CDR          ||
    sym == PRINT        ||
    sym == SYMBOL_VALUE ||
    sym == BACKQUOTE    ||
    sym == GENSYM       ||
    sym == SETCAR       ||
    sym == SETCDR       ||
    sym == COMMA        ||
    sym == COMMA_AT     ||
    sym == APPLY        ||
    sym == SYMBL        ||
    sym == SYMBOL_NAME  ||
    sym == FORMAT       ||
    sym == CLONE        ||
    sym == UNBIND       ||
    sym == NEWLINE      ||
    sym == NOT          ||
    sym == RETURN_FROM  ||
    sym == GET_CONTINUATION;
}

BOOLEAN string_array_op(OBJECT_PTR sym)
{
  return sym == STRING  ||
    sym == MAKE_ARRAY   ||
    sym == ARRAY_GET    ||
    sym == ARRAY_SET    ||
    sym == SUB_ARRAY    ||
    sym == ARRAY_LENGTH ||
    sym == PRINT_STRING;
}

BOOLEAN predicate_op(OBJECT_PTR sym)
{
  return sym == CONSP ||
    sym == LISTP      ||
    sym == INTEGERP   ||
    sym == FLOATP     ||
    sym == CHARACTERP ||
    sym == SYMBOLP    ||
    sym == STRINGP    ||
    sym == ARRAYP     ||
    sym == CLOSUREP   ||
    sym == MACROP     ||
    sym == CONTINUATIONP;
}

BOOLEAN ffi_op(OBJECT_PTR sym)
{
  return sym == LOAD_FOREIGN_LIBRARY ||
    sym == CALL_FOREIGN_FUNCTION;
}

BOOLEAN package_op(OBJECT_PTR sym)
{
  return sym == CREATE_PACKAGE ||
    sym == IN_PACKAGE          ||
    sym == EXPORT_PACKAGE;
}

BOOLEAN serialization_op(OBJECT_PTR sym)
{
  return sym == CREATE_IMAGE ||
    sym == SAVE_OBJECT ||
    sym == LOAD_OBJECT ||
    sym == LOAD_FILE;
}

BOOLEAN perf_op(OBJECT_PTR sym)
{
  return sym == PROFILE || sym == TIME;
}

BOOLEAN debug_op(OBJECT_PTR sym)
{
  return sym == BREAK ||
    sym == RESUME     ||
    sym == ENV        ||
    sym == EXPAND_MACRO;
}

BOOLEAN interpreter_specific_op(OBJECT_PTR sym)
{
  return sym == EVAL;
}

OBJECT_PTR range(int start, int end, int step)
{
  OBJECT_PTR ret = NIL;

  int i = start;

  while(i <= end)
  {
    OBJECT_PTR val = convert_int_to_object(i);

    if(ret == NIL)
      ret = cons(val, NIL);
    else
    {
      uintptr_t ptr = last_cell(ret) & POINTER_MASK;
      set_heap(ptr, 1, cons(val, NIL));        
    }

    i = i + step;
  }

  return ret;    
}

OBJECT_PTR nth(OBJECT_PTR n, OBJECT_PTR lst)
{
  assert(IS_CONS_OBJECT(lst) || IS_FUNCTION2_OBJECT(lst) || IS_MACRO2_OBJECT(lst));

  assert(IS_INTEGER_OBJECT(n));

  int lst1 = IS_CONS_OBJECT(lst) ? lst : ((lst >> OBJECT_SHIFT) << OBJECT_SHIFT) + CONS_TAG;

  int i_val = get_int_value(n);

  if(i_val < 0 || i_val >= cons_length(lst1))
    return NIL;
  else
  {
    if(i_val == 0)
      return car(lst1);
    else
      return nth(convert_int_to_object(i_val-1), cdr(lst1));
  }
}

OBJECT_PTR temp12(OBJECT_PTR x, OBJECT_PTR v1, OBJECT_PTR v2)
{
  return list(2,
              nth(x, v1),
              list(3,
                   NTH,
                   primitive_add(convert_int_to_object(2), x, convert_int_to_object(1)),
                   v2));
}

OBJECT_PTR closure_conv_transform_abs_cont(OBJECT_PTR exp)
{
  //OBJECT_PTR free_ids = difference(free_ids_il(exp), list(1, SAVE_CONTINUATION));
  OBJECT_PTR free_ids = free_ids_il(exp);
  OBJECT_PTR iclo = gensym();

  if(free_ids == NIL)
    return concat(3,
                  list(1, CREATE_FN_CLOSURE),
                  list(1, convert_int_to_object(0)),
                  list(1,list(4,
                              LAMBDA,
                              concat(2,
                                     list(1, iclo),
                                     second(exp)),
                              third(exp),
                              closure_conv_transform(fourth(exp)))));
  else
  {
    return concat(4,
                  list(1, CREATE_FN_CLOSURE),
                  list(1, convert_int_to_object(cons_length(free_ids))),
                  list(1,
                       list(4,
                            LAMBDA,
                            concat(2, list(1, iclo), second(exp)),
                            third(exp),
                            list(3,
                                 LET1,
                                 map2(temp12, free_ids, iclo,
                                      range(0, cons_length(free_ids)-1, 1)),
                                 closure_conv_transform(fourth(exp))))),
                  free_ids);
  }
}

OBJECT_PTR closure_conv_transform_abs_no_cont(OBJECT_PTR exp)
{
  OBJECT_PTR free_ids = free_ids_il(exp);
  OBJECT_PTR iclo = gensym();

  if(free_ids == NIL)
    return concat(3,
                  list(1, CREATE_FN_CLOSURE),
                  list(1, convert_int_to_object(0)),
                  list(1,list(3,
                              LAMBDA,
                              concat(2,
                                     list(1, iclo),
                                     second(exp)),
                              closure_conv_transform(third(exp)))));
  else
  {
    return concat(4,
                  list(1, CREATE_FN_CLOSURE),
                  list(1, convert_int_to_object(cons_length(free_ids))),
                  list(1,
                       list(3,
                            LAMBDA,
                            concat(2, list(1, iclo), second(exp)),
                            list(3,
                                 LET1,
                                 map2(temp12, free_ids, iclo,
                                      range(0, cons_length(free_ids)-1, 1)),
                                 closure_conv_transform(third(exp))))),
                  free_ids);
  }
}

OBJECT_PTR process_backquote(OBJECT_PTR exp)
{
  if(is_atom(exp))
    return exp;
  else if(car(exp) == BACKQUOTE)
    return backquote2(CADR(exp));
  else
    return cons(process_backquote(car(exp)),
                process_backquote(cdr(exp)));
}

OBJECT_PTR butlast(OBJECT_PTR lst)
{
  assert(lst == NIL || IS_CONS_OBJECT(lst));

  int len = cons_length(lst);

  int i=0;

  OBJECT_PTR ret = NIL, rest = lst;

  while(i < len-1)
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

  return ret;
}

OBJECT_PTR backquote2(OBJECT_PTR exp)
{
  //print_object(exp);printf("\n"); getchar();

  OBJECT_PTR car_exp;

  if(IS_CONS_OBJECT(exp))
    car_exp = car(exp);

  if(is_atom(exp))
    return exp;
  else if(car_exp == COMMA || car_exp == COMMA_AT)
    return CADR(exp);
  else
  {
    OBJECT_PTR res = NIL, rest = exp;

    while(rest != NIL)
    {
      OBJECT_PTR x = car(rest);

      if(is_atom(x))
      {
        if(res == NIL)
          res = list(2, LST, list(2, QUOTE, x));
        else
          res = concat(2, res, list(1,list(2, QUOTE, x)));
      }
      else if(car(x) == COMMA)
      {
        //OBJECT_PTR sym = gensym();
        //bindings = cons(list(2, sym, CADR(x)),
        //                bindings);
        if(res == NIL)
          res = list(2, LST, CADR(x));
        else
          res = concat(2, res, list(1,CADR(x)));
      }
      else if(car(x) == COMMA_AT)
      {
        //OBJECT_PTR sym = gensym();
        //bindings = cons(list(2, sym, CADR(x)),
        //                bindings);
        if(res == NIL)
          res = CADR(x);
        else
        {
          //uintptr_t ptr = last_cell(res) & POINTER_MASK;
          //set_heap(ptr, 1, sym);
          //print_object(butlast(res));printf("\n");getchar();
          //print_object(car(last_cell(res)));printf("\n");getchar();

          OBJECT_PTR last_cell1 = last_cell(res);

          if(cons_length(butlast(res)) > 1)
            /* res = concat(2, butlast(res), list(1, list(3, */
            /*                                            CONS, */
            /*                                            car(last_cell1), */
            /*                                            CADR(x)))); */
          {
            res = list(3, CONCAT, butlast(res), list(3,
                                                     CONS,
                                                     car(last_cell1),
                                                     CADR(x)));
            //print_object(res);
          }
          else
            res = list(3, 
                       CONS, 
                       car(last_cell1), 
                       CADR(x));
        }
      }
      else
      {
        if(res == NIL)
          res = list(2, LST, backquote2(x));
        else
          res = concat(2, res, list(1, backquote2(x)));
      }

      rest = cdr(rest);
    }

    //print_object(res); printf("\n"); getchar();

    //if(bindings == NIL)
      return res;
    //else
    //return list(3, LET, bindings, res);
  }
}

char *replace_hyphens(char *s)
{
  int i, len = strlen(s);
  
  for(i=0; i<len; i++)
    if(s[i] == '-')
      s[i] = '_';

  return s;
}

char *extract_variable_string(OBJECT_PTR var, BOOLEAN serialize_flag)
{
  if(IS_SYMBOL_OBJECT(var))
  {
    char *raw_name = strdup(get_symbol_name(var));

    if(raw_name[0] == '#')
    {
      //char *name = substring(raw_name, 2, strlen(raw_name)-2);
      char *name = strdup(raw_name);
      free(raw_name);
      name[0] = '_';
      name[1] = '_';
      return convert_to_lower_case(replace_hyphens(name));
    }
    else if(primop(var))
    {
      char *s = (char *)malloc(20*sizeof(char));
      if(var == ADD)
        sprintf(s,"primitive_add");
      else if(var == SUB)
        sprintf(s,"primitive_sub");
      else if(var == CAR)
        sprintf(s,"primitive_car");
      else if(var == CDR)
        sprintf(s,"cdr");
      else if(var == QUOTE)
        sprintf(s,"quote");
      else if(var == LT)
        sprintf(s,"primitive_lt");
      else if(var == GT)
        sprintf(s, "primitive_gt");
      else if(var == LEQ)
        sprintf(s, "primitive_leq");
      else if(var == GEQ)
        sprintf(s, "primitive_geq");
      else if(var == ERROR)
        sprintf(s,"primitive_error");
      else if(var == IF)
        sprintf(s, "primitive_if");
      else if(var == PRINT)
        sprintf(s, "primitive_print");
      else if(var == CONS)
        sprintf(s, "cons");
      else if(var == SETCAR)
        sprintf(s, "primitive_setcar");
      else if(var == SETCDR)
        sprintf(s, "primitive_setcdr");
      else if(var == CALL_CC1)
        sprintf(s, "call_cc1");
      else if(var == SAVE_CONTINUATION)
        sprintf(s, "save_continuation");
      else if(var == LST)
        sprintf(s, "primitive_list");
      else if(var == MULT)
        sprintf(s, "primitive_mult");
      else if(var == EQ)
        sprintf(s, "primitive_equal");
      else if(var == CONCAT)
        sprintf(s, "primitive_concat");
      else if(var == NOT)
        sprintf(s, "primitive_not");
      else if(var == GENSYM)
        sprintf(s, "gensym");
      else if(var == ATOM)
        sprintf(s, "primitive_atom");
      else if(var == SYMBOL_VALUE)
        sprintf(s, "prim_symbol_value");
      else if(var == APPLY)
        sprintf(s, "primitive_apply");
      else if(var == SYMBL)
        sprintf(s, "primitive_symbol");
      else if(var == SYMBOL_NAME)
        sprintf(s, "prim_symbol_name");
      else if(var == FORMAT)
        sprintf(s, "primitive_format");
      else if(var == CLONE)
        sprintf(s, "primitive_clone");
      else if(var == UNBIND)
        sprintf(s, "primitive_unbind");
      else if(var == NEWLINE)
        sprintf(s, "primitive_newline");
      else if(var == CONSP)
        sprintf(s, "primitive_consp");
      else if(var == LISTP)
        sprintf(s, "primitive_listp");
      else if(var == INTEGERP)
        sprintf(s, "primitive_integerp");
      else if(var == FLOATP)
        sprintf(s, "primitive_floatp");
      else if(var == CHARACTERP)
        sprintf(s, "prim_characterp");
      else if(var == SYMBOLP)
        sprintf(s, "primitive_symbolp");
      else if(var == STRINGP)
        sprintf(s, "primitive_stringp");
      else if(var == ARRAYP)
        sprintf(s, "primitive_arrayp");
      else if(var == CLOSUREP)
        sprintf(s, "primitive_closurep");
      else if(var == MACROP)
        sprintf(s, "primitive_macrop");
      else if(var == STRING)
        sprintf(s, "primitive_string");
      else if(var == MAKE_ARRAY)
        sprintf(s, "prim_make_array");
      else if(var == ARRAY_SET)
        sprintf(s, "prim_array_set");
      else if(var == ARRAY_GET)
        sprintf(s, "prim_array_get");
      else if(var == SUB_ARRAY)
        sprintf(s, "prim_sub_array");
      else if(var == ARRAY_LENGTH)
        sprintf(s, "prim_array_length");
      else if(var == PRINT_STRING)
        sprintf(s, "prim_print_string");
      else if(var == LOAD_FOREIGN_LIBRARY)
        sprintf(s, "prim_load_fgn_lib");
      else if(var == CALL_FOREIGN_FUNCTION)
        sprintf(s, "prim_call_fgn_func");
      else if(var == CREATE_PACKAGE)
        sprintf(s, "prim_create_pkg");
      else if(var == IN_PACKAGE)
        sprintf(s, "prim_in_package");
      else if(var == EXPORT_PACKAGE)
        sprintf(s, "prim_export_pkg");
      else if(var == CREATE_IMAGE)
        sprintf(s, "prim_create_image");
      else if(var == SAVE_OBJECT)
        sprintf(s, "prim_serialize");
      else if(var == LOAD_OBJECT)
        sprintf(s, "prim_deserialize");
      else if(var == LOAD_FILE)
        sprintf(s, "prim_load_file");
      else if(var == TIME)
        sprintf(s, "primitive_time");
      else if(var == ENV)
        sprintf(s, "primitive_env");
      else if(var == EXPAND_MACRO)
        sprintf(s, "prim_expand_macro");
      else if(var == EVAL)
        sprintf(s, "primitive_eval");
      else if(var == GET_CONTINUATION)
        sprintf(s, "get_continuation");
      else
      {
        print_object(var);
        assert(false);
      }
      return s;
    }
    return convert_to_lower_case(replace_hyphens(raw_name));
  }
  else
  {
    /* if(serialize_flag) */
    /* { */
      char *s = (char *)malloc(50*sizeof(char));
      memset(s,50,'\0');

      if(IS_INTEGER_OBJECT(var))
        sprintf(s, "convert_int_to_object(%d)", get_int_value(var));
      else if(IS_FLOAT_OBJECT(var))
        sprintf(s, "convert_float_to_object(%f)", get_float_value(var));
      else
        sprintf(s, "%d", var);

      return s;
    /* } */
    /* else */
    /* { */
    /*   char *s = (char *)malloc(10*sizeof(char)); */
    /*   memset(s,10,'\0'); */
    /*   sprintf(s, "%d", var); */
    /*   return s; */
    /* } */
  }
}

unsigned int build_c_string(OBJECT_PTR lambda_form, char *buf, BOOLEAN serialize_flag)
{
  char *fname = extract_variable_string(car(lambda_form), serialize_flag);

  unsigned int len = 0;

  len += sprintf(buf+len, "unsigned int %s(", fname);

  OBJECT_PTR params = second(second(lambda_form));

  OBJECT_PTR rest = params;

  BOOLEAN first_time = true;

  while(rest != NIL)
  {
    char *pname = extract_variable_string(car(rest), serialize_flag);

    if(!first_time)
      len += sprintf(buf+len, ", ");

    len += sprintf(buf+len, "unsigned int %s", pname);

    rest = cdr(rest);
    first_time = false;

    free(pname);
  }

  len += sprintf(buf+len, ")\n{\n");

  len += sprintf(buf+len, "set_most_recent_closure(%s);\n", extract_variable_string(first(params), serialize_flag));
  //len += sprintf(buf+len, "printf(\"%s\\n\");\n", fname);
  len += sprintf(buf+len, "unsigned int nil = 17;\n");

  OBJECT_PTR body = CDDR(second(lambda_form));

  assert(cons_length(body) == 1 || cons_length(body) == 2);

  if(cons_length(body) == 2)
  {
    len += build_c_fragment(car(body), buf+len, false, serialize_flag);
    //len += sprintf(buf+len, ";\n");
    len += build_c_fragment(CADR(body), buf+len, false, serialize_flag);
  }
  else
  {
    len += build_c_fragment(car(body), buf+len, false, serialize_flag);
  }

  len += sprintf(buf+len, "\n}\n");

  free(fname);

  return len;
}

unsigned int build_c_fragment(OBJECT_PTR exp, char *buf, BOOLEAN nested_call, BOOLEAN serialize_flag)
{
  unsigned int len = 0;

  BOOLEAN primitive_call = false;

  if(is_atom(exp))
  {
    char *var = extract_variable_string(exp, serialize_flag);
    len += sprintf(buf+len, "%s;\n", var);
    free(var);
  }
  else if(car(exp) == IF)
  {
    char *test_var = extract_variable_string(second(exp), serialize_flag);
    len += sprintf(buf+len, "if(%s != nil)", test_var);
    len += build_c_fragment(third(exp), buf+len, false, serialize_flag);
    len += sprintf(buf+len, "else\n");
    len += build_c_fragment(fourth(exp), buf+len, false, serialize_flag);
    free(test_var);
  }
  else if(car(exp) == LET || car(exp) == LET1)
  {
    len += sprintf(buf+len, "{\n");

    OBJECT_PTR rest = second(exp);

    while(rest != NIL)
    {
      char *var = extract_variable_string(car(car(rest)), serialize_flag);

      if(IS_CONS_OBJECT(second(car(rest))) && first(second(car(rest))) == EXTRACT_NATIVE_FN)
      {
        len += sprintf(buf+len, "typedef unsigned int (*nativefn)(unsigned int, ...);\n");
        len += sprintf(buf+len, "nativefn %s = (nativefn)", var);
        len += build_c_fragment(CADR(car(rest)), buf+len, false, serialize_flag);
      }
      else
      {
        len += sprintf(buf+len, "unsigned int %s = (unsigned int)", var);
        len += build_c_fragment(CADR(car(rest)), buf+len, false, serialize_flag);
      }

      rest = cdr(rest);
      free(var);
    }

    if(first(third(exp)) != LET && first(third(exp)) != LET1 && first(third(exp)) != IF)
      len += sprintf(buf+len, "return ");

    len += build_c_fragment(third(exp), buf+len, false, serialize_flag);

    len += sprintf(buf+len, "\n}");

  }
  else //primitive or user-defined function/macro application
  {

    if(car(exp) == QUOTE)
    {
      len += sprintf(buf+len, "%d", second(exp));
    }
    /* else if(car(exp) == IF) */
    /* { */
    /*   char *test_var = extract_variable_string(second(exp)); */
    /*   char *then_var = extract_variable_string(third(exp)); */
    /*   char *else_var = extract_variable_string(fourth(exp)); */

    /*   len += sprintf(buf+len, "(%s != nil) ? %s : %s", test_var, then_var, else_var); */

    /*   free(test_var); */
    /*   free(then_var); */
    /*   free(else_var); */
    /* } */
    else
    {
      if(primop(car(exp)))
        primitive_call = true;

      char *var = extract_variable_string(car(exp), serialize_flag);
      len += sprintf(buf+len, "%s(", var);
      free(var);

      OBJECT_PTR rest = cdr(exp);

      if(primitive_call && rest == NIL)
      {
        len += sprintf(buf+len, "17)");
      }
      else
      {
        BOOLEAN first_time = true;

        while(rest != NIL)
        {
          if(!first_time)
            len += sprintf(buf+len, ", ");

          if(is_atom(car(rest)))
          {
            char *arg_name = extract_variable_string(car(rest), serialize_flag);
            len += sprintf(buf+len, "%s", arg_name);
            free(arg_name);
          }
          else
            len += build_c_fragment(car(rest), buf+len, true, serialize_flag);

          rest = cdr(rest);
          first_time = false;
        }

        len += sprintf(buf+len, ")");
      }
    }

    if(!nested_call)
    {
      len += sprintf(buf+len, ";\n");
      if(primitive_call || car(exp) == EXTRACT_NATIVE_FN)
        len += sprintf(buf+len, "if(in_error_condition()==1)return 17;\n");
    }
  }

  return len;
}

nativefn extract_native_fn(OBJECT_PTR closure)
{
  if(!IS_FUNCTION2_OBJECT(closure) && !IS_MACRO2_OBJECT(closure))
  {
    print_object(closure);
    assert(false);
  }

  //TODO: replace assert by a raise_error; also,
  //this check should not be done when
  //invoked for the top-level closure
  //assert(!unmet_dependencies_exist(closure));
  if(unmet_dependencies_exist(closure))
  {
    raise_error("Unmet dependencies exist for function/macro");
    return NULL;
  }

  OBJECT_PTR nativefn_obj = get_heap(closure & POINTER_MASK, 0);

  if(!IS_NATIVE_FN_OBJECT(nativefn_obj))
  {
    print_object(nativefn_obj);
    assert(false);
  }

  return get_nativefn_value(nativefn_obj);
}

OBJECT_PTR get_continuation(OBJECT_PTR clo)
{
  OBJECT_PTR rest = continuations_for_return;

  while(rest != NIL)
  {
    OBJECT_PTR obj = car(rest);

    if(car(obj)== clo)
      return cdr(obj);

    rest = cdr(rest);
  }

  raise_error("Unable to get continuation for closure");
  return NIL;
}

void set_most_recent_closure(OBJECT_PTR clo)
{
  most_recent_closure = clo;
}

void save_continuation(OBJECT_PTR cont)
{
  saved_continuations = cons(cont, saved_continuations);
  if(most_recent_closure != NIL)
    continuations_for_return = cons(cons(most_recent_closure, cont), continuations_for_return);
}

int in_error_condition()
{
  return in_error;
}

TCCState *create_tcc_state1()
{
  TCCState *tcc_state = tcc_new();

  if (!tcc_state)
  {
    fprintf(stderr, "Could not create tcc state\n");
    return NULL;
  }

  tcc_set_output_type(tcc_state, TCC_OUTPUT_MEMORY);

  tcc_add_symbol(tcc_state, "nth",                nth);
  tcc_add_symbol(tcc_state, "save_continuation",  save_continuation);
  tcc_add_symbol(tcc_state, "extract_native_fn",  extract_native_fn);
  tcc_add_symbol(tcc_state, "call_cc1",           call_cc1);
  tcc_add_symbol(tcc_state, "create_fn_closure",  create_fn_closure);
  tcc_add_symbol(tcc_state, "primitive_add",      primitive_add);
  tcc_add_symbol(tcc_state, "primitive_sub",      primitive_sub);
  tcc_add_symbol(tcc_state, "primitive_car",      primitive_car);
  tcc_add_symbol(tcc_state, "cdr",                cdr);
  tcc_add_symbol(tcc_state, "quote",              quote);
  tcc_add_symbol(tcc_state, "primitive_error",    primitive_error);
  tcc_add_symbol(tcc_state, "primitive_lt",       primitive_lt);
  tcc_add_symbol(tcc_state, "primitive_gt",       primitive_gt);
  tcc_add_symbol(tcc_state, "primitive_leq",      primitive_leq);
  tcc_add_symbol(tcc_state, "primitive_geq",      primitive_geq);
  tcc_add_symbol(tcc_state, "primitive_if",       primitive_if);
  tcc_add_symbol(tcc_state, "in_error_condition", in_error_condition);
  tcc_add_symbol(tcc_state, "primitive_print",    primitive_print);
  tcc_add_symbol(tcc_state, "cons",               cons);
  tcc_add_symbol(tcc_state, "primitive_setcar",   primitive_setcar);
  tcc_add_symbol(tcc_state, "primitive_setcdr",   primitive_setcdr);
  tcc_add_symbol(tcc_state, "primitive_list",     primitive_list);
  tcc_add_symbol(tcc_state, "primitive_mult",     primitive_mult);
  tcc_add_symbol(tcc_state, "primitive_equal",    primitive_equal);
  tcc_add_symbol(tcc_state, "primitive_concat",   primitive_concat);

  tcc_add_symbol(tcc_state, "primitive_not",      primitive_not);

  tcc_add_symbol(tcc_state, "gensym",             gensym);
  tcc_add_symbol(tcc_state, "primitive_atom",     primitive_atom);
  tcc_add_symbol(tcc_state, "prim_symbol_value",  prim_symbol_value);
  tcc_add_symbol(tcc_state, "primitive_apply",    primitive_apply);
  tcc_add_symbol(tcc_state, "primitive_symbol",   primitive_symbol);
  tcc_add_symbol(tcc_state, "prim_symbol_name",   prim_symbol_name);
  tcc_add_symbol(tcc_state, "primitive_format",   primitive_format);
  tcc_add_symbol(tcc_state, "primitive_clone",    primitive_clone);
  tcc_add_symbol(tcc_state, "primitive_unbind",   primitive_unbind);
  tcc_add_symbol(tcc_state, "primitive_newline",  primitive_newline);

  tcc_add_symbol(tcc_state, "primitive_consp",    primitive_consp);
  tcc_add_symbol(tcc_state, "primitive_listp",    primitive_listp);
  tcc_add_symbol(tcc_state, "primitive_integerp", primitive_integerp);
  tcc_add_symbol(tcc_state, "primitive_floatp",   primitive_floatp);
  tcc_add_symbol(tcc_state, "prim_characterp",    prim_characterp);
  tcc_add_symbol(tcc_state, "primitive_symbolp",  primitive_symbolp);
  tcc_add_symbol(tcc_state, "primitive_stringp",  primitive_stringp);
  tcc_add_symbol(tcc_state, "primitive_arrayp",   primitive_arrayp);
  tcc_add_symbol(tcc_state, "primitive_closurep", primitive_closurep);
  tcc_add_symbol(tcc_state, "primitive_macrop",   primitive_macrop);

  tcc_add_symbol(tcc_state, "primitive_string",   primitive_string);
  tcc_add_symbol(tcc_state, "prim_make_array",    prim_make_array);
  tcc_add_symbol(tcc_state, "prim_array_set",     prim_array_set);
  tcc_add_symbol(tcc_state, "prim_array_get",     prim_array_get);
  tcc_add_symbol(tcc_state, "prim_sub_array",     prim_sub_array);
  tcc_add_symbol(tcc_state, "prim_array_length",  prim_array_length);
  tcc_add_symbol(tcc_state, "prim_print_string",  prim_print_string);

  tcc_add_symbol(tcc_state, "prim_load_fgn_lib",  prim_load_fgn_lib);
  tcc_add_symbol(tcc_state, "prim_call_fgn_func", prim_call_fgn_func);

  tcc_add_symbol(tcc_state, "prim_create_pkg",    prim_create_pkg);
  tcc_add_symbol(tcc_state, "prim_in_package",    prim_in_package);
  tcc_add_symbol(tcc_state, "prim_export_pkg",    prim_export_pkg);

  tcc_add_symbol(tcc_state, "prim_create_image",  prim_create_image);
  tcc_add_symbol(tcc_state, "prim_serialize",     prim_serialize);
  tcc_add_symbol(tcc_state, "prim_deserialize",   prim_deserialize);

  tcc_add_symbol(tcc_state, "prim_load_file",     prim_load_file);

  tcc_add_symbol(tcc_state, "primitive_time",     primitive_time);

  tcc_add_symbol(tcc_state, "primitive_env",      primitive_env);
  tcc_add_symbol(tcc_state, "prim_expand_macro",  prim_expand_macro);

  tcc_add_symbol(tcc_state, "primitive_eval",     full_monty_eval);

  tcc_add_symbol(tcc_state, "convert_int_to_object",    convert_int_to_object);
  tcc_add_symbol(tcc_state, "convert_float_to_object",  convert_float_to_object);

  tcc_add_symbol(tcc_state, "set_most_recent_closure",  set_most_recent_closure);
  tcc_add_symbol(tcc_state, "get_continuation",         get_continuation);

  return tcc_state;
}

OBJECT_PTR identity_function(OBJECT_PTR closure, OBJECT_PTR x)
{
  return x;
}

OBJECT_PTR convert_native_fn_to_object(nativefn fn)
{
  uintptr_t ptr = object_alloc(1, NATIVE_FN_TAG);

  *((nativefn *)ptr) = fn;

  return ptr + NATIVE_FN_TAG;  
}

nativefn get_nativefn_value(OBJECT_PTR obj)
{
  if(!IS_NATIVE_FN_OBJECT(obj))
    assert(false);

  return *((nativefn *)(obj & POINTER_MASK));
  
}

OBJECT_PTR create_closure(unsigned int count, BOOLEAN function, OBJECT_PTR fnobj, ...)
{
  va_list ap;
  int i;
  OBJECT_PTR closed_object;
  OBJECT_PTR ret;

  //since count can be zero,
  //disabling this
  //if(!count)
  //  return NIL;

  ret = cons(fnobj, NIL);

  va_start(ap, fnobj);

  for(i=0; i<count; i++)
  {
    closed_object = (OBJECT_PTR)va_arg(ap, int);
    uintptr_t ptr = last_cell(ret) & POINTER_MASK;
    set_heap(ptr, 1, cons(closed_object, NIL));
  }

  ret = ((ret >> OBJECT_SHIFT) << OBJECT_SHIFT) + (function ? FUNCTION2_TAG : MACRO2_TAG);

  return ret;
}

//identical to create_closure(), except that this is
//hardwired to produce only function closures.
//not making a call to create_closure() internally
//since this will involve mucking around with va_list
//etc. to pass the variable arguments to create_closure().
//also, the count parameter is an OBJECT_PTR so that
//create_fn_closure can be invoked from within the generated code.
OBJECT_PTR create_fn_closure(OBJECT_PTR count1, nativefn fn, ...)
{
  OBJECT_PTR fnobj = convert_native_fn_to_object(fn);

  va_list ap;
  int i;
  OBJECT_PTR closed_object;
  OBJECT_PTR ret;

  assert(IS_INTEGER_OBJECT(count1));

  int count = get_int_value(count1);

  //since count can be zero,
  //disabling this
  //if(!count)
  //  return NIL;

  ret = cons(fnobj, NIL);

  va_start(ap, fn);

  for(i=0; i<count; i++)
  {
    closed_object = (OBJECT_PTR)va_arg(ap, int);
    uintptr_t ptr = last_cell(ret) & POINTER_MASK;
    set_heap(ptr, 1, cons(closed_object, NIL));
  }

  ret = ((ret >> OBJECT_SHIFT) << OBJECT_SHIFT) + FUNCTION2_TAG;

  assert(IS_FUNCTION2_OBJECT(ret));

  return ret;  
}

TCCState *compile_functions(OBJECT_PTR lambda_forms)
{
  char str[65536];
  memset(str, 32768, '\0');

  unsigned int len = 0;

  TCCState *tcc_state1 = create_tcc_state1();
  assert(tcc_state1);

  OBJECT_PTR rest = lambda_forms;

  while(rest != NIL)
  {
    len += build_c_string(car(rest), str+len, false);
    rest = cdr(rest);
  }

  assert(len <= 65536);

  //FILE *out = fopen("debug.c", "a");
  //fprintf(out, "%s\n", str);
  //fclose(out);

  if(tcc_compile_string(tcc_state1, str) == -1)
    assert(false);

  if(tcc_relocate(tcc_state1, TCC_RELOCATE_AUTO) < 0)
    assert(false);

  return tcc_state1;
}

int get_top_level_sym_value(OBJECT_PTR sym, OBJECT_PTR *out)
{
  int i;

  for(i=0; i<nof_global_vars; i++)
  {
    if(top_level_symbols[i].delete_flag)
      continue;
    else if(top_level_symbols[i].sym == sym)
    {
      *out = top_level_symbols[i].val;
      return 0;
    }
  }
  return 1;
}

void add_top_level_sym(OBJECT_PTR sym, OBJECT_PTR val)
{
  int i;

  BOOLEAN found = false;

  for(i=0; i<nof_global_vars; i++)
  {
    if(top_level_symbols[i].sym == sym)
    {
      top_level_symbols[i].val = val;
      top_level_symbols[i].delete_flag = false;
      found = true;
    }
  }

  if(!found)
  {
    nof_global_vars++;

    global_var_mapping_t *temp = (global_var_mapping_t *)realloc(top_level_symbols, nof_global_vars*sizeof(global_var_mapping_t));

    assert(temp);

    top_level_symbols = temp;
    top_level_symbols[nof_global_vars-1].sym = sym;
    top_level_symbols[nof_global_vars-1].val = val;
    top_level_symbols[nof_global_vars-1].delete_flag = false;

    top_level_symbols[nof_global_vars-1].ref_count = 0;
    top_level_symbols[nof_global_vars-1].references = NULL;
  }
}

int remove_top_level_sym(OBJECT_PTR sym)
{
  int i,j,k,p;
  BOOLEAN found;

  for(i=0; i<nof_global_vars; i++)
  {
    if(top_level_symbols[i].delete_flag)
      continue;

    if(top_level_symbols[i].sym == sym)
    {
      top_level_symbols[i].delete_flag = true;

      for(j=0; j<top_level_symbols[i].ref_count; j++)
      {
        OBJECT_PTR clo = top_level_symbols[i].references[j].referrer;
        unsigned int pos = top_level_symbols[i].references[j].pos;

        found = false;

        //to reuse existing unmet dependecy records
        for(k=0; k<nof_unmet_dependencies; k++)
        {
          if(global_unmet_dependencies[k].delete_flag)
            continue;

          if(global_unmet_dependencies[k].clo == clo &&
             global_unmet_dependencies[k].top_level_sym == sym)
          {
            global_unmet_dependencies[k].delete_flag = false;
            global_unmet_dependencies[k].pos = pos;
            found = true;
            break;
          }
        }

        if(!found)
        {
          nof_unmet_dependencies++;

          unmet_dependency_t *temp = (unmet_dependency_t *)realloc(global_unmet_dependencies,
                                                                   nof_unmet_dependencies * sizeof(unmet_dependency_t));

          assert(temp);

          global_unmet_dependencies = temp;

          global_unmet_dependencies[nof_unmet_dependencies - 1].clo = clo;
          global_unmet_dependencies[nof_unmet_dependencies - 1].top_level_sym = sym;
          global_unmet_dependencies[nof_unmet_dependencies - 1].pos = pos;
          global_unmet_dependencies[nof_unmet_dependencies - 1].delete_flag = false;
        }
      }
    }
    return OK;
  }
  return NOT_OK;
}

OBJECT_PTR reverse(OBJECT_PTR lst)
{
  OBJECT_PTR ret = NIL, rest = lst;

  while(rest != NIL)
  {
    ret = cons(car(rest), ret);

    rest = cdr(rest);
  }

  return ret;
}

/* (defun call-cc1 (clo) */
/*   ((car clo) clo (cadr saved-continuations) (list (lambda (y x) x)))) */

OBJECT_PTR call_cc1(OBJECT_PTR clo)
{
  nativefn fn = extract_native_fn(clo);
  if(!fn)
    return NIL;
  return fn(clo, CADDR(saved_continuations), idclo);
}

/* OBJECT_PTR compile_and_evaluate(OBJECT_PTR exp) */
/* { */
/*   //print_object(exp);getchar(); */

/*   OBJECT_PTR compiled_form; */

/*   BOOLEAN macro_flag = false; */

/*   if(IS_CONS_OBJECT(exp) && car(exp) == MACRO) */
/*   { */
/*     macro_flag = true; */
/*     compiled_form = compile_exp(replace_macros(exp)); */
/*   } */
/*   else */
/*     compiled_form = compile_exp(exp); */

/*   if(compiled_form == NIL) */
/*     return NIL; */

/*   nativefn tt = extract_native_fn(compiled_form); */

/*   OBJECT_PTR ret = tt(compiled_form, idclo); */

/*   if(macro_flag) */
/*     return ((ret >> OBJECT_SHIFT) << OBJECT_SHIFT) + MACRO2_TAG; */

/*   return ret; */
/* } */

int repl2()
{
  if(!g_expr)
    return 0;

  if(g_expr->type == LIST && g_expr->elements[0]->type == SYMBOL && !strcmp(g_expr->elements[0]->atom_value,"QUIT"))
  {
    if(console_mode)
    {
      fprintf(stdout, "Bye.\n");
      cleanup();
      exit(0);
    }
    else
      quit_application();
  }
  else
  {
    OBJECT_PTR exp;
    int val = convert_expression_to_object(g_expr, &exp);

    assert(is_valid_object(exp));

    if(val != 0)
      return 1;

    saved_continuations = NIL;
    continuations_for_return = NIL;
    most_recent_closure = NIL;

    //idclo = create_closure(0, true, convert_native_fn_to_object((nativefn)identity_function));

    in_error = false;

    OBJECT_PTR res = full_monty_eval(exp);

    if(in_error)
      return 1;

    if((console_mode || single_expression_mode || pipe_mode) && core_library_loaded)
    {
      /* char buf[500]; */
      /* memset(buf, 500, '\0'); */

      /* print_object_to_string(res, buf, 0); */

      /* fprintf(stdout, "%s\n", buf); */
      /* fflush(stdout); */

      print_object(res);
    }
    else
    {
      if(!console_mode && !single_expression_mode && !pipe_mode)
      {
	if(core_library_loaded)
	{
	  print_object(res);
	  print_to_transcript("\n");
	}
      }
      else
      {
	if(yyin == stdin)
	  print_object(res);
      }
    }
  }

  delete_expression(g_expr);
  g_expr = NULL;

  /* OBJECT_PTR cons_equiv; */
  /* OBJECT_PTR fnn; */

  /* print_object(idclo); printf(" --- "); */
  /* cons_equiv = ((idclo >> OBJECT_SHIFT) << OBJECT_SHIFT) + CONS_TAG; */
  /* fnn = car(cons_equiv); */
  /* print_object(fnn);printf("\n"); */

  gc(false, true);

  /* print_object(idclo); printf(" --- "); */
  /* cons_equiv = ((idclo >> OBJECT_SHIFT) << OBJECT_SHIFT) + CONS_TAG; */
  /* print_object(car(cons_equiv));printf("\n"); */

  return 0;
}

OBJECT_PTR temp14(OBJECT_PTR x)
{
  return list(2, QUOTE, x);
}

OBJECT_PTR quote_all_arguments(OBJECT_PTR exp)
{
  //return cons(car(exp), map(temp14, cdr(exp)));
  return map(temp14, exp);
}

OBJECT_PTR expand_macro_full_no_rest_handling(OBJECT_PTR exp)
{
  return expand_macro_full(exp, false);
}

OBJECT_PTR expand_macro_full(OBJECT_PTR exp1, BOOLEAN handle_and_rest)
{
  //print_object(exp1);printf("^^^\n");getchar();

  OBJECT_PTR exp = handle_and_rest ? handle_and_rest_applications(exp1, get_free_variables(exp1)) : exp1;

  //print_object(exp);printf("^^^\n");getchar();

  OBJECT_PTR ret;

  OBJECT_PTR free_variables = get_free_variables(exp);

  if(is_atom(exp) || is_quoted_expression(exp) || is_backquoted_expression(exp))
    ret = exp;
  else if(IS_SYMBOL_OBJECT(car(exp)) && exists(car(exp), free_variables))
  {
    OBJECT_PTR sym_to_use = symbol_to_use(car(exp));

    OBJECT_PTR out;
    int retval = get_top_level_sym_value(sym_to_use, &out);

    if(!retval && IS_MACRO2_OBJECT(car(out)))
    {
      //print_object(apply_macro_or_fn(car(out), cdr(exp)));printf("&&&\n"); getchar();
      ret = expand_macro_full(apply_macro_or_fn(car(out), cdr(exp)), true); }
    else
      ret = map(expand_macro_full_no_rest_handling, exp);
  }
  else
    ret = map(expand_macro_full_no_rest_handling, exp);

  return ret;
}

BOOLEAN is_vararg_primop(OBJECT_PTR sym)
{
  return sym == ADD  ||
         sym == SUB  ||
         sym == MULT ||
         sym == DIV  ||
         sym == LST  ||
         sym == CONCAT ||
         sym == FORMAT ||
         sym == MAKE_ARRAY;
}

/*
(defmacro begin (&rest body)
  (if (null body)
      nil
    (if (eq (length body) 1)
        (car body)
      `(let ((,(gensym) ,(car body)))
         (begin ,@(cdr body))))))
*/
OBJECT_PTR expand_body(OBJECT_PTR body)
{
  if(body == NIL)
    return NIL;
  else if(cons_length(body) == 1)
    return car(body);
  else
    return list(3,
                LET,
                list(1, list(2, gensym(), car(body))),
                expand_body(cdr(body)));
}

OBJECT_PTR expand_bodies(OBJECT_PTR exp)
{
  if(is_atom(exp) || is_quoted_expression(exp) || is_backquoted_expression(exp))
    return exp;
  else if(car(exp) == LAMBDA || car(exp) == MACRO)
    return list(3, first(exp), second(exp), expand_body(CDDR(exp)));
  else
    return cons(expand_bodies(car(exp)),
                expand_bodies(cdr(exp)));
}

int add_reference_to_top_level_sym(OBJECT_PTR sym, int pos, OBJECT_PTR clo)
{
  if(!IS_FUNCTION2_OBJECT(clo) && !IS_MACRO2_OBJECT(clo))
  {
    print_object(clo);
    assert(false);
  }

  int i;

  for(i=0; i<nof_global_vars; i++)
  {
    if(top_level_symbols[i].delete_flag)
      continue;

    if(top_level_symbols[i].sym == sym)
    {
      if(top_level_symbols[i].ref_count == 0)
      {
        top_level_symbols[i].ref_count++;
        top_level_symbols[i].references = (global_var_ref_detail_t *)malloc(sizeof(global_var_ref_detail_t));
        assert(top_level_symbols[i].references);
        top_level_symbols[i].references[top_level_symbols[i].ref_count-1].referrer = clo;
        top_level_symbols[i].references[top_level_symbols[i].ref_count-1].pos = pos;
      }
      else
      {
        global_var_ref_detail_t *temp;
        top_level_symbols[i].ref_count++;
        temp = (global_var_ref_detail_t *)realloc(top_level_symbols[i].references, 
                                                  top_level_symbols[i].ref_count * sizeof(global_var_ref_detail_t));
        assert(temp);

        top_level_symbols[i].references = temp;
        top_level_symbols[i].references[top_level_symbols[i].ref_count-1].referrer = clo;
        top_level_symbols[i].references[top_level_symbols[i].ref_count-1].pos = pos;
      }
      return 0;
    }
  }
  return 1;
}

int update_references(OBJECT_PTR sym, OBJECT_PTR new_val)
{
  int i;

  for(i=0; i<nof_global_vars; i++)
  {
    if(top_level_symbols[i].delete_flag)
      continue;

    if(top_level_symbols[i].sym == sym)
    {
      int j;

      for(j=0; j<top_level_symbols[i].ref_count; j++)
      {
        OBJECT_PTR clo = top_level_symbols[i].references[j].referrer;

        assert(IS_FUNCTION2_OBJECT(clo) || IS_MACRO2_OBJECT(clo));

        //print_object(clo);printf("\n");
        int pos = top_level_symbols[i].references[j].pos;

        OBJECT_PTR cons_equiv = ((clo >> OBJECT_SHIFT) << OBJECT_SHIFT) + CONS_TAG;

        OBJECT_PTR rest = cons_equiv;
        int k=0;

        for(k=0; k<pos; k++)
          rest = cdr(rest);

        set_heap(car(rest) & POINTER_MASK, 0, new_val);
      }

      return 0;
    }
  }

  return 0;
}

void add_unmet_dependency(OBJECT_PTR clo, OBJECT_PTR sym, int pos)
{
  assert(IS_FUNCTION2_OBJECT(clo) || IS_MACRO2_OBJECT(clo));
  assert(IS_SYMBOL_OBJECT(sym));
  //assert(pos >= 1 && 
  //       pos <  cons_length(((clo >> OBJECT_SHIFT) << OBJECT_SHIFT) + CONS_TAG));

  int i;

  //to reuse existing record if available
  for(i=0; i<nof_unmet_dependencies; i++)
  {
    if(global_unmet_dependencies[i].clo == clo &&
       global_unmet_dependencies[i].top_level_sym == sym)
    {
      global_unmet_dependencies[i].pos = pos;
      global_unmet_dependencies[i].delete_flag = false;
      return;
    }
  }

  //create fresh unmet dependecy record
  //if not able to reuse

  nof_unmet_dependencies++;

  unmet_dependency_t *temp = (unmet_dependency_t *)realloc(global_unmet_dependencies,
                                                           nof_unmet_dependencies * sizeof(unmet_dependency_t));

  assert(temp);

  global_unmet_dependencies = temp;

  global_unmet_dependencies[nof_unmet_dependencies - 1].clo = clo;
  global_unmet_dependencies[nof_unmet_dependencies - 1].top_level_sym = sym;
  global_unmet_dependencies[nof_unmet_dependencies - 1].pos = pos;
  global_unmet_dependencies[nof_unmet_dependencies - 1].delete_flag = false;
}

BOOLEAN unmet_dependencies_exist(OBJECT_PTR clo)
{
  assert(IS_FUNCTION2_OBJECT(clo) || IS_MACRO2_OBJECT(clo));

  int i;

  for(i=0; i<nof_unmet_dependencies; i++)
  {
    if(global_unmet_dependencies[i].delete_flag)
      continue;

    if(global_unmet_dependencies[i].clo == clo)
        return true;
  }

  return false;
}

void update_dependencies(OBJECT_PTR sym, OBJECT_PTR val)
{
  int i;

  for(i=0; i<nof_unmet_dependencies; i++)
  {
    if(global_unmet_dependencies[i].delete_flag)
      continue;

    if(global_unmet_dependencies[i].top_level_sym == sym)
    {
      OBJECT_PTR clo = global_unmet_dependencies[i].clo;

      assert(IS_FUNCTION2_OBJECT(clo) || IS_MACRO2_OBJECT(clo));

      int pos = global_unmet_dependencies[i].pos;

      OBJECT_PTR cons_eqiv = ((clo >> OBJECT_SHIFT) << OBJECT_SHIFT) + CONS_TAG;

      OBJECT_PTR rest = cons_eqiv;
      int k=0;

      for(k=0; k<pos; k++)
        rest = cdr(rest);

      set_heap(car(rest) & POINTER_MASK, 0, car(val));

      add_reference_to_top_level_sym(sym, pos, clo);

      global_unmet_dependencies[i].delete_flag = true;

      break;
    }
  }
}

void debug_print_closure(OBJECT_PTR clo)
{
  print_object(((clo >> OBJECT_SHIFT) << OBJECT_SHIFT) + CONS_TAG);
}

int location_of_and_rest(OBJECT_PTR lst)
{
  OBJECT_PTR rest = lst;
  int ret = 0;

  while(rest != NIL)
  {
    int package_index = (int)car(rest) >> (SYMBOL_BITS + OBJECT_SHIFT);
    int symbol_index =  ((int)car(rest) >> OBJECT_SHIFT) & TWO_RAISED_TO_SYMBOL_BITS_MINUS_1;

    char *param_name = packages[package_index].symbols[symbol_index];

    if(!strcmp(param_name, "&REST"))
      return ret;

    ret++;
    rest = cdr(rest);
  }

  return -1;
}

OBJECT_PTR strip_and_rest(OBJECT_PTR lst)
{
  OBJECT_PTR ret = NIL, rest = lst;

  while(rest != NIL)
  {
    OBJECT_PTR val = car(rest);

    int package_index = (int)val >> (SYMBOL_BITS + OBJECT_SHIFT);
    int symbol_index =  ((int)val >> OBJECT_SHIFT) & TWO_RAISED_TO_SYMBOL_BITS_MINUS_1;

    char *param_name = packages[package_index].symbols[symbol_index];

    if(strcmp(param_name, "&REST"))
    {
      if(ret == NIL)
        ret = cons(val, NIL);
      else
      {
        uintptr_t ptr = last_cell(ret) & POINTER_MASK;
        set_heap(ptr, 1, cons(val, NIL));        
      }      
    }

    rest = cdr(rest);
  }

  return ret;
}

void record_and_rest_closure(OBJECT_PTR sym, int pos_of_and_rest)
{
  //assert(IS_FUNCTION2_OBJECT(clo) || IS_MACRO2_OBJECT(clo));
  assert(pos_of_and_rest >= 0);

  nof_and_rest_mappings++;

  and_rest_mapping_t *temp = (and_rest_mapping_t *)realloc(and_rest_mappings, nof_and_rest_mappings * sizeof(and_rest_mapping_t));

  assert(temp);

  and_rest_mappings = temp;

  and_rest_mappings[nof_and_rest_mappings-1].delete_flag = false;
  and_rest_mappings[nof_and_rest_mappings-1].sym = sym;
  and_rest_mappings[nof_and_rest_mappings-1].pos = pos_of_and_rest;
}

int and_rest_closure_pos(OBJECT_PTR sym)
{
  //assert(IS_FUNCTION2_OBJECT(clo) || IS_MACRO2_OBJECT(clo));

  int i;

  for(i=0; i<nof_and_rest_mappings; i++)
  {
    if(and_rest_mappings[i].delete_flag)
      continue;

    if(and_rest_mappings[i].sym == sym)
      return and_rest_mappings[i].pos;
  }

  return -1;
}

OBJECT_PTR get_free_variables(OBJECT_PTR exp)
{
  return difference(free_ids_il(exp),
                    list(10, LET, LETREC, IF, SET, LAMBDA, MACRO, ERROR, DEFINE, TRUE, NIL));
}

OBJECT_PTR temp15(OBJECT_PTR x)
{
  return list(2, QUOTE, x);
}

OBJECT_PTR temp16(OBJECT_PTR x, OBJECT_PTR v1, OBJECT_PTR v2)
{
  return handle_and_rest_applications(x, v1);
}

OBJECT_PTR handle_and_rest_applications(OBJECT_PTR exp, OBJECT_PTR free_variables)
{
  if(is_atom(exp) || is_quoted_expression(exp) || is_backquoted_expression(exp))
    return exp;
  else if(exists(car(exp), free_variables))
  {
    OBJECT_PTR out;

    OBJECT_PTR symbol_to_be_used = symbol_to_use(car(exp));

    int retval = get_top_level_sym_value(symbol_to_be_used, &out);

    if(retval)
      return exp;

    OBJECT_PTR clo = car(out);

    BOOLEAN macro_flag = IS_MACRO2_OBJECT(clo);

    int pos = and_rest_closure_pos(symbol_to_be_used);

    if(pos != -1)
    {
      OBJECT_PTR bindings = NIL;
      OBJECT_PTR rest = cdr(exp);
      int i = 0;

      OBJECT_PTR ret = cons(symbol_to_be_used, NIL);

      assert(cons_length(rest) >= pos);

      while(rest != NIL && i < pos)
      {
        OBJECT_PTR val = car(rest);

        uintptr_t ptr = last_cell(ret) & POINTER_MASK;
        set_heap(ptr, 1, cons(val, NIL));

        i++;
        rest = cdr(rest);
      }

      OBJECT_PTR val;

      if(rest != NIL)
      {
        val = macro_flag ? rest : concat(2, list(1, LST), rest);

        uintptr_t ptr = last_cell(ret) & POINTER_MASK;
        set_heap(ptr, 1, cons(val, NIL));
      }
      else
      {
        uintptr_t ptr = last_cell(ret) & POINTER_MASK;
        set_heap(ptr, 1, cons(NIL, NIL));
      }

      return ret;
    }
    else
      return map2(temp16, free_variables, NIL, exp);
  }
  else
    return map2(temp16, free_variables, NIL, exp);
}

OBJECT_PTR handle_and_rest_applications_old(OBJECT_PTR exp, OBJECT_PTR free_variables)
{
  if(is_atom(exp) || is_quoted_expression(exp) || is_backquoted_expression(exp))
    return exp;
  else if(exists(car(exp), free_variables))
  {
    OBJECT_PTR out;

    int retval = get_top_level_sym_value(car(exp), &out);

    if(retval)
      return exp;

    OBJECT_PTR clo = car(out);

    BOOLEAN macro_flag = IS_MACRO2_OBJECT(clo);

    int pos = and_rest_closure_pos(car(exp));

    OBJECT_PTR ret;

    if(macro_flag)
      ret = cons(car(exp), NIL);

    if(pos != -1)
    {
      OBJECT_PTR bindings = NIL;
      OBJECT_PTR rest = cdr(exp);
      int i = 0;

      assert(cons_length(rest) >= pos);

      while(rest != NIL && i < pos)
      {
        //OBJECT_PTR val = macro_flag ? list(2, QUOTE, car(rest)) : car(rest);
        OBJECT_PTR val = car(rest);

        if(!macro_flag)
        {
          OBJECT_PTR binding = list(2, gensym(), val);

          if(bindings == NIL)
            bindings = cons(binding, NIL);
          else
          {
            uintptr_t ptr = last_cell(bindings) & POINTER_MASK;
            set_heap(ptr, 1, cons(binding, NIL));
          }
        }
        else
        {
          uintptr_t ptr = last_cell(ret) & POINTER_MASK;
          set_heap(ptr, 1, cons(val, NIL));
        }

        i++;
        rest = cdr(rest);
      }

      //OBJECT_PTR val = macro_flag ? concat(2, list(1, LST), map(temp15,rest)) : concat(2, list(1, LST), rest);
      OBJECT_PTR val;

      if(rest == NIL)
        val = NIL;
      else
        val = macro_flag ? rest : concat(2, list(1, LST), rest);

      if(!macro_flag)
      {
        OBJECT_PTR binding = list(2, gensym(), val);

        if(bindings == NIL)
          bindings = cons(binding, NIL);
        else
        {
          uintptr_t ptr = last_cell(bindings) & POINTER_MASK;
          set_heap(ptr, 1, cons(binding, NIL));
        }
      }
      else
      {
        uintptr_t ptr = last_cell(ret) & POINTER_MASK;
        set_heap(ptr, 1, rest == NIL ? NIL : cons(val, NIL));
      }

      if(!macro_flag)
        return list(3, LET, bindings, concat(2, list(1, car(exp)), map(car, bindings)));
      else
        return ret;
    }
    else
      return map2(temp16, free_variables, NIL, exp);
  }
  else
    return map2(temp16, free_variables, NIL, exp);
}

BOOLEAN contains_internal_macros(OBJECT_PTR exp)
{
  if(is_atom(exp))
    return false;
  else if(car(exp) == MACRO)
    return true;
  else
    return contains_internal_macros(car(exp) || contains_internal_macros(cdr(exp)));
}

BOOLEAN is_valid_expression(OBJECT_PTR exp)
{
  if(IS_CONS_OBJECT(exp) &&
     ((car(exp) == MACRO && contains_internal_macros(third(exp))) ||
      (car(exp) != MACRO && contains_internal_macros(exp))))
  {
    raise_error("Expression contains internal macros");
    return false;
  }
   
  return true;
}

OBJECT_PTR rewrite_zero_arg_applications(OBJECT_PTR exp)
{
  if(is_atom(exp))
    return exp;
  else if(car(exp) == LAMBDA)
    return list(3, LAMBDA, second(exp), rewrite_zero_arg_applications(third(exp)));
  else if(exists(car(exp), get_free_variables(exp)) && cons_length(exp) == 1)
    return list(2, car(exp), NIL);
  else
    return map(rewrite_zero_arg_applications, exp);
}

//given an object, returns the top-level
//symbol that is mapped to this object (if
//such a symbol exists)
OBJECT_PTR reverse_sym_lookup(OBJECT_PTR obj)
{
  int i;

  for(i=0; i<nof_global_vars; i++)
  {
    if(top_level_symbols[i].delete_flag)
      continue;
    else if(car(top_level_symbols[i].val) == obj)
      return top_level_symbols[i].sym;
  }

  return NIL;  
}

OBJECT_PTR symbol_to_use(OBJECT_PTR sym)
{
  OBJECT_PTR symbol_given = sym;
  OBJECT_PTR symbol_to_be_used;

  int package_index = (int)symbol_given >> (SYMBOL_BITS + OBJECT_SHIFT);

  if(package_index != current_package)
  {
    if(package_index == CORE_PACKAGE_INDEX)
      symbol_to_be_used = cdr(get_qualified_symbol_object(packages[current_package].name, get_symbol_name(symbol_given)));
    else
      symbol_to_be_used = symbol_given;
  }
  else
    symbol_to_be_used = symbol_given;  

  return symbol_to_be_used;
}

OBJECT_PTR full_monty_eval(OBJECT_PTR exp)
{
  OBJECT_PTR res;

  if(car(exp) == DEFINE)
  {
    if(!IS_SYMBOL_OBJECT(second(exp)))
    {
      raise_error("Second argument to DEFINE should be a symbol");
      return NIL;
    }

    OBJECT_PTR symbol_to_be_used = symbol_to_use(second(exp));

    OBJECT_PTR t1 = cons(NIL, NIL);

    if(IS_CONS_OBJECT(third(exp)))
    {
      if(first(third(exp)) == LAMBDA)
        t1 = cons(((NIL >> OBJECT_SHIFT) << OBJECT_SHIFT) + FUNCTION2_TAG,
                  NIL);
      else if(first(third(exp)) == MACRO)
        t1 = cons(((NIL >> OBJECT_SHIFT) << OBJECT_SHIFT) + MACRO2_TAG,
                  NIL);
    }      

    //to prevent unmet dependency error
    //for recursive definitions
    add_top_level_sym(symbol_to_be_used, t1);

    res = third(exp);

    //to handle &rest params for recursive definitions
    if(IS_CONS_OBJECT(third(exp)) && 
       (car(third(exp)) == LAMBDA || car(third(exp)) == MACRO))
    {
      int pos_of_and_rest = location_of_and_rest(second(third(exp)));
      if(pos_of_and_rest != -1)
      {
        res = list(3, car(third(exp)), strip_and_rest(second(third(exp))), third(third(exp)));
        record_and_rest_closure(symbol_to_be_used, pos_of_and_rest);
      }
    }

    res = compile_and_evaluate(res);

    //store the source of the function/macro as the
    //last cell in the closure
    if(IS_FUNCTION2_OBJECT(res) || IS_MACRO2_OBJECT(res))
    {
      BOOLEAN macro_flag = IS_MACRO2_OBJECT(res);
      OBJECT_PTR cons_equiv = ((res >> OBJECT_SHIFT) << OBJECT_SHIFT) + CONS_TAG;
      uintptr_t ptr = last_cell(cons_equiv) & POINTER_MASK;
      set_heap(ptr, 1, cons(third(exp), NIL));
      res = ((cons_equiv >> OBJECT_SHIFT) << OBJECT_SHIFT) + (macro_flag ? MACRO2_TAG : FUNCTION2_TAG);
    }

    add_top_level_sym(symbol_to_be_used, cons(res, NIL));

    update_dependencies(symbol_to_be_used, cons(res, NIL));

    if(update_references(symbol_to_be_used, res))
    {
      raise_error("Update of reference to top level symbol failed");
      return NIL;
    }
  }
  else if(car(exp) == SET)
  {
    if(!IS_SYMBOL_OBJECT(second(exp)))
    {
      raise_error("Second argument to SET should be a symbol");
      return NIL;
    } 

    OBJECT_PTR symbol_to_be_used = symbol_to_use(second(exp));

    OBJECT_PTR out;
    int retval = get_top_level_sym_value(symbol_to_be_used, &out);

    if(retval)
    {
      char buf[200];
      memset(buf, 200, '\0');
      sprintf(buf, "Undefined symbol: %s", get_symbol_name(symbol_to_be_used));
      raise_error(buf);
      return NIL;
    }

    res = compile_and_evaluate(third(exp));

    //store the source of the function/macro as the
    //last cell in the closure
    if(IS_FUNCTION2_OBJECT(res) || IS_MACRO2_OBJECT(res))
    {
      BOOLEAN macro_flag = IS_MACRO2_OBJECT(res);
      OBJECT_PTR cons_equiv = ((res >> OBJECT_SHIFT) << OBJECT_SHIFT) + CONS_TAG;
      uintptr_t ptr = last_cell(cons_equiv) & POINTER_MASK;
      set_heap(ptr, 1, cons(third(exp), NIL));
      res = ((cons_equiv >> OBJECT_SHIFT) << OBJECT_SHIFT) + (macro_flag ? MACRO2_TAG : FUNCTION2_TAG);
    }

    add_top_level_sym(symbol_to_be_used, cons(res, NIL));     

    update_dependencies(symbol_to_be_used, cons(res, NIL));

    if(update_references(symbol_to_be_used, res))
    {
      raise_error("Update of reference to top level symbol failed");
      return NIL;
    }
  }
  else if(IS_SYMBOL_OBJECT(exp))
  {
    OBJECT_PTR out;
    int retval = get_top_level_sym_value(exp, &out);

    if(retval)
    {
      char buf[200];
      memset(buf, 200, '\0');
      sprintf(buf, "Undefined symbol: %s", get_symbol_name(exp));
      raise_error(buf);
      return NIL;
    }
        
    res = out;
  }
  else
  {
    res = compile_and_evaluate(exp);
  }

  return res;
}

void cleanup_full_monty_global_vars()
{
  int i;

  for(i=0; i<nof_global_vars; i++)
    free(top_level_symbols[i].references);

  free(top_level_symbols);

  free(global_unmet_dependencies);

  free(and_rest_mappings);

  for(i=0; i<nof_native_fns; i++)
    free(native_fn_sources[i].source);

  free(native_fn_sources);
}

void add_native_fn_source(nativefn fn, char *source)
{
  nof_native_fns++;

  native_fn_src_mapping_t *temp = (native_fn_src_mapping_t *)realloc(native_fn_sources, nof_native_fns * sizeof(native_fn_src_mapping_t));

  assert(temp);

  native_fn_sources = temp;

  native_fn_sources[nof_native_fns-1].fn = fn;
  native_fn_sources[nof_native_fns-1].source = strdup(source);
}

char *get_native_fn_source(nativefn fn)
{
  if(fn == (nativefn)identity_function)
    return "unsigned int identity_function(unsigned int closure, unsigned int x) {  return x; }";

  int i;
  for(i=0; i<nof_native_fns; i++)
  {
    if(native_fn_sources[i].fn == fn)
      return native_fn_sources[i].source;
  }

  return NULL;
}

inline OBJECT_PTR cons_equivalent(OBJECT_PTR obj)
{
  assert(IS_FUNCTION2_OBJECT(obj) || IS_MACRO2_OBJECT(obj));

  return ((obj >> OBJECT_SHIFT) << OBJECT_SHIFT) + CONS_TAG;
}
