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

typedef OBJECT_PTR (*nativefn)(OBJECT_PTR, ...);

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

extern unsigned int POINTER_MASK;

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
OBJECT_PTR desugar_il(OBJECT_PTR);
OBJECT_PTR closure_conv_transform(OBJECT_PTR);
OBJECT_PTR range(int, int, int);
OBJECT_PTR closure_conv_transform_abs_cont(OBJECT_PTR);
OBJECT_PTR closure_conv_transform_abs_no_cont(OBJECT_PTR);
OBJECT_PTR backquote2(OBJECT_PTR);
OBJECT_PTR process_backquote(OBJECT_PTR);
unsigned int build_c_fragment(OBJECT_PTR, char *);
void build_c_string(OBJECT_PTR, char *);
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

  while(lst == NIL)
  {
    start++;
    lst = (OBJECT_PTR)va_arg(ap, int);

    if(!IS_CONS_OBJECT(lst) && lst != NIL)
      assert(false);
  }

  ret = clone_object(lst);

  for(i=start; i<count; i++)
  {
    lst = (OBJECT_PTR)va_arg(ap, int);

    if(lst == NIL)
      continue;

    if(!IS_CONS_OBJECT(lst))
      assert(false);

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
  else if(car_exp == LET || car_exp == LETREC)
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
  else if(is_atom(exp))
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
                ids,
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
  if(is_atom(exp))
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
  else if(is_atom(exp))
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
  else if(car_exp == ERROR || car_exp == CALL_CC)
    return free_ids_il(second(exp));
  else
    return flatten(cons(free_ids_il(car(exp)),
                        free_ids_il(cdr(exp))));
}

OBJECT_PTR simplify_il_empty_let(OBJECT_PTR exp)
{
  if(is_atom(exp))
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
  if(is_atom(exp))
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
  if(is_atom(exp))
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

  if(is_atom(exp))
    return cps_transform_var_literal(exp);
  else if(car_exp == LAMBDA)
    return cps_transform_abstraction(exp);
  else if(car_exp == LET)
    return cps_transform_let(exp);
  else if(primop(car_exp))
    return cps_transform_primop(exp);
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

BOOLEAN primop(OBJECT_PTR sym)
{
  return arithop(sym)    ||
    core_op(sym)         ||
    string_array_op(sym) ||
    predicate_op(sym)    ||
    ffi_op(sym)          ||
    package_op(sym)      ||
    serialization_op(sym);
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
                   list(2, icode, second(exp1)),
                   list(2, 
                        first(first(second(exp))),
                        concat(2,
                               list(2, LST, icode),
                               CDDR(exp1)))),
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
  else if(is_atom(exp))
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
  if(is_atom(exp))
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

OBJECT_PTR compile_exp(OBJECT_PTR exp)
{
  OBJECT_PTR res = clone_object(exp);

  //TODO: macro expansion via invoking native function pointer
  //that implements macros (calling primitive/native version of backquote2 internally)

  res = process_backquote(exp);

  res = assignment_conversion(res, list(2, CALL_CC1, MY_CONT_VAR)); //TODO
  res = translate_to_il(res);

  binding_env_t *env = create_binding_env();
  res = ren_transform(res, env);

  free(env->bindings);
  env->bindings = NULL;
  free(env);

  res = simplify_il(res);
  res = cps_transform(res);
  res = closure_conv_transform(res);
  res = lift_transform(res, NIL);

  char str[1024];
  memset(str, 1024, '\0');

  build_c_string(second(res), str);

  printf("%s\n", str);

  return res;
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
    sym == EQ           ||
    sym == CALL_CC1     ||
    sym == SET          ||
    sym == ERROR        ||
    sym == LIST         ||
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
    sym == RETURN       ||
    sym == RETURN_FROM  ||
    sym == UNBIND       ||
    sym == NEWLINE      ||
    sym == NOT          ||
    sym == PROGN;
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

OBJECT_PTR primitive_add(unsigned int count, ...)
{
  va_list ap;
  OBJECT_PTR arg;
  int i, sum=0;

  assert(count >= 2); //TODO: handle this via error()

  va_start(ap, count);

  for(i=0; i<count; i++)
  {
    arg = (OBJECT_PTR)va_arg(ap, int);
    sum += get_int_value(arg);
  }

  va_end(ap);

  return convert_int_to_object(sum);  
}

OBJECT_PTR nth(OBJECT_PTR n, OBJECT_PTR lst)
{
  int i_val = get_int_value(n);

  if(i_val < 0 || i_val >= cons_length(lst))
    return NIL;
  else
  {
    if(i_val == 0)
      return car(lst);
    else
      return nth(convert_int_to_object(i_val-1), cdr(lst));
  }
}

OBJECT_PTR temp12(OBJECT_PTR x, OBJECT_PTR v1, OBJECT_PTR v2)
{
  return list(2,
              nth(x, v1),
              list(3,
                   NTH,
                   primitive_add(2, x, convert_int_to_object(1)),
                   v2));
}

OBJECT_PTR closure_conv_transform_abs_cont(OBJECT_PTR exp)
{
  OBJECT_PTR free_ids = free_ids_il(exp);
  OBJECT_PTR iclo = gensym();

  if(free_ids == NIL)
    return concat(2,
                  list(1, LST),
                  list(1,list(4,
                              LAMBDA,
                              concat(2,
                                     list(1, iclo),
                                     second(exp)),
                              third(exp),
                              closure_conv_transform(fourth(exp)))));
  else
  {
    return concat(3,
                  list(1, LST),
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
    return concat(2,
                  list(1, LST),
                  list(1,list(3,
                              LAMBDA,
                              concat(2,
                                     list(1, iclo),
                                     second(exp)),
                              closure_conv_transform(third(exp)))));
  else
  {
    return concat(3,
                  list(1, LST),
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

OBJECT_PTR backquote2(OBJECT_PTR exp)
{
  OBJECT_PTR car_exp;

  if(IS_CONS_OBJECT(exp))
    car_exp = car(exp);

  if(is_atom(exp))
    return exp;
  else if(car_exp == COMMA || car_exp == COMMA_AT)
    return CADR(exp);
  else
  {
    OBJECT_PTR bindings = NIL, res = NIL, rest = exp;

    while(rest != NIL)
    {
      OBJECT_PTR x = car(rest);

      if(is_atom(x))
      {
        if(res == NIL)
          res = list(1, x);
        else
          res = concat(2, res, list(1,x));
      }
      else if(car(x) == COMMA)
      {
        OBJECT_PTR sym = gensym();
        bindings = cons(list(2, sym, CADR(x)),
                        bindings);
        if(res == NIL)
          res = list(1, sym);
        else
          res = concat(2, res, list(1,sym));
      }
      else if(car(x) == COMMA_AT)
      {
        OBJECT_PTR sym = gensym();
        bindings = cons(list(2, sym, CADR(x)),
                        bindings);
        if(res == NIL)
          res = sym;
        else
        {
          uintptr_t ptr = last_cell(res) & POINTER_MASK;
          set_heap(ptr, 1, sym);        
        }
      }
      rest = cdr(rest);
    }

    if(bindings == NIL)
      return res;
    else
      return list(3, LET, bindings, res);
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

char *extract_variable_string(OBJECT_PTR var)
{
  //TODO: prefix underscore to the gensym variable names
  //to avoid name clash with user-defined variables

  //TODO: convert any hyphens to undescores

  if(IS_SYMBOL_OBJECT(var))
  {
    char *raw_name = strdup(get_symbol_name(var));

    if(raw_name[0] == '#')
    {
      char *name = substring(raw_name, 2, strlen(raw_name)-2);
      free(raw_name);
      return convert_to_lower_case(replace_hyphens(name));
    }
    else
      return convert_to_lower_case(replace_hyphens(raw_name));
  }
  else
  {
    char *s = (char *)malloc(10*sizeof(char));
    memset(s,10,'\0');
    sprintf(s, "%d", var);
    return s;
  }
}

void build_c_string(OBJECT_PTR lambda_form, char *buf)
{
  char *fname = extract_variable_string(car(lambda_form));

  unsigned int len = 0;

  len += sprintf(buf+len, "OBJECT_PTR %s(", fname);

  OBJECT_PTR params = second(second(lambda_form));

  OBJECT_PTR rest = params;

  BOOLEAN first_time = true;

  while(rest != NIL)
  {
    char *pname = extract_variable_string(car(rest));

    if(!first_time)
      len += sprintf(buf+len, ", ");

    len += sprintf(buf+len, "OBJECT_PTR %s", pname);

    rest = cdr(rest);
    first_time = false;

    free(pname);
  }

  len += sprintf(buf+len, ")\n{\n");

  OBJECT_PTR body = CDDR(second(lambda_form));

  assert(cons_length(body) == 1 || cons_length(body) == 2);

  if(cons_length(body) == 2)
  {
    len += build_c_fragment(car(body), buf+len);
    //len += sprintf(buf+len, ";\n");
    len += build_c_fragment(CADR(body), buf+len);
  }
  else
  {
    len += build_c_fragment(car(body), buf+len);
  }

  len += sprintf(buf+len, "\n}\n");

  free(fname);
}

unsigned int build_c_fragment(OBJECT_PTR exp, char *buf)
{
  unsigned int len = 0;

  if(is_atom(exp))
  {
    char *var = extract_variable_string(exp);
    len += sprintf(buf+len, "%s;\n", var);
    free(var);
  }
  else if(car(exp) == LET || car(exp) == LET1)
  {
    len += sprintf(buf+len, "{\n");

    OBJECT_PTR rest = second(exp);

    while(rest != NIL)
    {
      char *var = extract_variable_string(car(car(rest)));

      if(IS_CONS_OBJECT(second(car(rest))) && first(second(car(rest))) == EXTRACT_NATIVE_FN)
      {
        len += sprintf(buf+len, "nativefn %s = ", var);
        len += build_c_fragment(CADR(car(rest)), buf+len);
      }
      else
      {
        len += sprintf(buf+len, "OBJECT_PTR %s = ", var);
        len += build_c_fragment(CADR(car(rest)), buf+len);
      }

      rest = cdr(rest);
      free(var);
    }

    len += build_c_fragment(third(exp), buf+len);

    len += sprintf(buf+len, "\n}");

  }
  else //primitive or user-defined function/macro application
  {

    char *var = extract_variable_string(car(exp));
    len += sprintf(buf+len, "%s(", var);
    free(var);

    OBJECT_PTR rest = cdr(exp);

    BOOLEAN first_time = true;

    while(rest != NIL)
    {
      char *arg_name = extract_variable_string(car(rest));

      if(!first_time)
        len += sprintf(buf+len, ", ");

      len += sprintf(buf+len, "%s", arg_name);

      rest = cdr(rest);
      first_time = false;

      free(arg_name);
    }

    len += sprintf(buf+len, ");\n");
  }

  return len;
}
