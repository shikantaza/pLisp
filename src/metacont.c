/**
  Copyright 2011-2023 Rajesh Jayaprakash <rajesh.jayaprakash@pm.me>

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
#include <stdint.h>
#include <assert.h>

#include "gc.h"

#include "plisp.h"

//forward declarations
struct reg_closure;
struct metacont_closure;
                         
typedef OBJECT_PTR (*reg_cont_fn)(struct reg_closure *, OBJECT_PTR);

typedef struct reg_closure
{
  reg_cont_fn fn;
  unsigned int nof_closed_vals;
  OBJECT_PTR *closed_vals;
  void *data;
} reg_closure_t;

typedef OBJECT_PTR (*metacont_fn)(struct metacont_closure *, struct reg_closure *);

typedef struct metacont_closure
{
  metacont_fn mfn;
  unsigned int nof_closed_vals;
  OBJECT_PTR *closed_vals;  
} metacont_closure_t;

extern OBJECT_PTR LAMBDA;
extern OBJECT_PTR IF;
extern OBJECT_PTR LET;
extern OBJECT_PTR LET1;
extern OBJECT_PTR NIL;

#ifdef WIN32
extern OBJECT_PTR ERROR1;
#else
extern OBJECT_PTR ERROR;
#endif

extern OBJECT_PTR RETURN_FROM;
extern OBJECT_PTR THROW;
extern OBJECT_PTR CALL_CC;
extern OBJECT_PTR BREAK;
extern OBJECT_PTR SAVE_CONTINUATION;
extern OBJECT_PTR GET_CONTINUATION;
extern OBJECT_PTR SAVE_CONTINUATION_TO_RESUME;

extern OBJECT_PTR first(OBJECT_PTR);
extern OBJECT_PTR second(OBJECT_PTR);
extern OBJECT_PTR third(OBJECT_PTR);
extern OBJECT_PTR fourth(OBJECT_PTR);
extern int cons_length(OBJECT_PTR);
extern OBJECT_PTR reverse(OBJECT_PTR);
extern OBJECT_PTR concat(unsigned int, ...);
extern BOOLEAN primop(OBJECT_PTR);
extern BOOLEAN IS_CONS_OBJECT(OBJECT_PTR);
extern BOOLEAN is_quoted_expression(OBJECT_PTR);

extern BOOLEAN is_vararg_primop(OBJECT_PTR);

metacont_closure_t *mcps(OBJECT_PTR);

reg_closure_t *create_reg_let_closure(OBJECT_PTR,
                                      OBJECT_PTR,
                                      OBJECT_PTR,
                                      unsigned int,
                                      OBJECT_PTR *,
                                      reg_closure_t *);

reg_closure_t *create_reg_primop_closure(OBJECT_PTR,
                                         OBJECT_PTR,
                                         unsigned int,
                                         OBJECT_PTR *,
                                         reg_closure_t *);

reg_closure_t *create_reg_app_closure(OBJECT_PTR,
                                      unsigned int,
                                      OBJECT_PTR *,
                                      reg_closure_t *);

OBJECT_PTR mcps_transform(OBJECT_PTR);

OBJECT_PTR id_to_mc_fn(reg_closure_t *cls, OBJECT_PTR val)
{
  return list(2, cls->closed_vals[0], val);
}

reg_closure_t *id_to_mc(OBJECT_PTR id)
{
  reg_closure_t *cls = (reg_closure_t *)GC_MALLOC(sizeof(reg_closure_t));

  cls->fn              = id_to_mc_fn;
  cls->nof_closed_vals = 1;
  cls->closed_vals     = (OBJECT_PTR *)GC_MALLOC(cls->nof_closed_vals * sizeof(OBJECT_PTR));
  cls->closed_vals[0]  = id;
  cls->data            = NULL;

  return cls;
}

OBJECT_PTR mc_to_exp(reg_closure_t *cls)
{
  OBJECT_PTR i_temp = gensym();

  return list(3,
              LAMBDA,
              list(1, i_temp),
              //list(2, SAVE_CONTINUATION, i_temp),
              cls->fn(cls, i_temp));
}

OBJECT_PTR if_reg_cont_fn(reg_closure_t *cls, OBJECT_PTR test_val)
{
  OBJECT_PTR i_kif = gensym();  

  reg_closure_t *cls1 = (reg_closure_t *)cls->data;

  OBJECT_PTR then_exp = cls->closed_vals[0];
  OBJECT_PTR else_exp = cls->closed_vals[1];
  
  metacont_closure_t *then_mcls = mcps(then_exp);
  metacont_closure_t *else_mcls = mcps(else_exp);

  reg_closure_t *kif_cls = id_to_mc(i_kif);
  
  return list(3,
              LET,
              list(1, list(2, i_kif, mc_to_exp(cls1))),
              list(4,
                   IF,
                   test_val,
                   then_mcls->mfn(then_mcls, kif_cls),
                   else_mcls->mfn(else_mcls, kif_cls)));
}

OBJECT_PTR if_metacont_fn(metacont_closure_t *mcls, reg_closure_t *cls1)
{
  OBJECT_PTR test_exp = mcls->closed_vals[0];
  OBJECT_PTR then_exp = mcls->closed_vals[1];
  OBJECT_PTR else_exp = mcls->closed_vals[2];

  metacont_closure_t *test_mcls = mcps(test_exp);

  reg_closure_t *cls = (reg_closure_t *)GC_MALLOC(sizeof(reg_closure_t));

  cls->fn              = if_reg_cont_fn;
  cls->nof_closed_vals = 2;
  cls->closed_vals     = (OBJECT_PTR *)GC_MALLOC(cls->nof_closed_vals * sizeof(OBJECT_PTR));

  cls->closed_vals[0]  = then_exp;
  cls->closed_vals[1]  = else_exp;

  cls->data = cls1;
  
  return test_mcls->mfn(test_mcls, cls);
}

OBJECT_PTR let_cont_fn_recur(reg_closure_t *cls, OBJECT_PTR val)
{
  OBJECT_PTR bindings      = cls->closed_vals[0];
  OBJECT_PTR full_bindings = cls->closed_vals[1];
  OBJECT_PTR body          = cls->closed_vals[2];

  unsigned int nof_vals = cls->nof_closed_vals - 3;
  
  OBJECT_PTR *new_vals = (OBJECT_PTR *)GC_MALLOC((nof_vals + 1) * sizeof(OBJECT_PTR));

  int i;
  for(i=0; i<nof_vals; i++)
    new_vals[i] = cls->closed_vals[i+3];

  new_vals[nof_vals] = val;

  metacont_closure_t *mcls = mcps(second(first(bindings)));
  
  return mcls->mfn(mcls,
                   create_reg_let_closure(cdr(bindings), full_bindings, body, nof_vals+1, new_vals, (reg_closure_t *)cls->data));
}

OBJECT_PTR let_cont_fn_non_recur(reg_closure_t *cls, OBJECT_PTR val)
{
  OBJECT_PTR bindings      = cls->closed_vals[0];
  OBJECT_PTR full_bindings = cls->closed_vals[1];
  OBJECT_PTR body          = cls->closed_vals[2];

  //assert(cons_length(full_bindings) == cls->nof_closed_vals - 3);

  OBJECT_PTR reversed_full_bindings = reverse(full_bindings);
  
  OBJECT_PTR rest = cdr(reversed_full_bindings);
  OBJECT_PTR ret = NIL;
  
  int i = cls->nof_closed_vals - 1; //since we're starting from the end
  
  while(rest != NIL)
  {
    ret = cons(list(2, car(car(rest)), cls->closed_vals[i]), ret);

    i--;
    rest = cdr(rest);
  }

  ret = concat(2, ret, list(1, list(2, car(car(reversed_full_bindings)), val)));

  metacont_closure_t *mcls = mcps(body);
  
  return list(3,
              LET,
              ret,
              mcls->mfn(mcls, (reg_closure_t *)cls->data));
}

reg_closure_t *create_reg_let_closure(OBJECT_PTR    bindings,
                                      OBJECT_PTR    full_bindings,
                                      OBJECT_PTR    body,
                                      unsigned int  nof_vals,
                                      OBJECT_PTR    *vals,
                                      reg_closure_t *cls)
{  
  reg_closure_t *let_closure = (reg_closure_t *)GC_MALLOC(sizeof(reg_closure_t));

  if(cons_length(bindings) == 0) //last binding
    let_closure->fn = let_cont_fn_non_recur;
  else
    let_closure->fn = let_cont_fn_recur;

  let_closure->nof_closed_vals = nof_vals + 3;
  let_closure->closed_vals     = (OBJECT_PTR *)GC_MALLOC(let_closure->nof_closed_vals * sizeof(OBJECT_PTR));

  let_closure->closed_vals[0]  = bindings;
  let_closure->closed_vals[1]  = full_bindings;
  let_closure->closed_vals[2]  = body;

  int i;
  for(i=3; i<let_closure->nof_closed_vals; i++)
    let_closure->closed_vals[i] = vals[i-3];

  let_closure->data = cls;
  
  return let_closure;
}

OBJECT_PTR lit_id_metacont_fn(metacont_closure_t *mcls, reg_closure_t *cls)
{
  if(is_quoted_expression(mcls->closed_vals[0]))
  {
    OBJECT_PTR i_sym = gensym();
    
    return list(3,
                LET,
                list(1, list(2, i_sym, mcls->closed_vals[0])),
                cls->fn(cls, i_sym));
  }
  else
    return cls->fn(cls, mcls->closed_vals[0]);
}

OBJECT_PTR lambda_metacont_fn(metacont_closure_t *mcls, reg_closure_t *cls)
{
  OBJECT_PTR i_abs   = gensym();
  OBJECT_PTR i_kcall = gensym();

  OBJECT_PTR params = mcls->closed_vals[0];
  OBJECT_PTR body   = mcls->closed_vals[1];

  metacont_closure_t *mcls1 = mcps(body);
  
  return list(3,
              LET,
              list(1,
                   list(2,
                        i_abs,
                        list(4,
                             LAMBDA,
                             concat(2, params, list(1, i_kcall)),
                             list(2, SAVE_CONTINUATION, i_kcall),
                             mcls1->mfn(mcls1, id_to_mc(i_kcall))))),
              cls->fn(cls, i_abs));  
}

OBJECT_PTR error_metacont_fn(metacont_closure_t *mcls, reg_closure_t *cls)
{
#ifdef WIN32
  return list(2, ERROR1, mcls->closed_vals[0]);
#else  
  return list(2, ERROR, mcls->closed_vals[0]);
#endif
}

OBJECT_PTR let_metacont_fn(metacont_closure_t *mcls, reg_closure_t *cls)
{
  OBJECT_PTR bindings      = mcls->closed_vals[0];
  OBJECT_PTR full_bindings = mcls->closed_vals[0];
  OBJECT_PTR body          = mcls->closed_vals[1];

  OBJECT_PTR e = second(first(bindings));
  
  metacont_closure_t *mcls1 = mcps(e);
  
  return mcls1->mfn(mcls1, create_reg_let_closure(cdr(bindings), full_bindings, body, 0, NULL, cls));  
}

OBJECT_PTR primop_cont_fn_recur(reg_closure_t *cls, OBJECT_PTR val)
{
  OBJECT_PTR operator      = cls->closed_vals[0];
  OBJECT_PTR operands      = cls->closed_vals[1];

  unsigned int nof_vals = cls->nof_closed_vals - 2;
  
  OBJECT_PTR *new_vals = (OBJECT_PTR *)GC_MALLOC((nof_vals + 1) * sizeof(OBJECT_PTR));

  int i;
  for(i=0; i<nof_vals; i++)
    new_vals[i] = cls->closed_vals[i+2];

  new_vals[nof_vals] = val;

  metacont_closure_t *mcls = mcps(first(operands));
  
  return mcls->mfn(mcls,
                   create_reg_primop_closure(operator, cdr(operands), nof_vals+1, new_vals, (reg_closure_t *)cls->data));
}

OBJECT_PTR primop_cont_fn_non_recur(reg_closure_t *cls, OBJECT_PTR val)
{
  OBJECT_PTR operator      = cls->closed_vals[0];
  OBJECT_PTR operands      = cls->closed_vals[1];

  OBJECT_PTR i_ans = gensym();

  OBJECT_PTR ret = cons(operator, NIL);

  if(is_vararg_primop(operator))
    ret = cons(convert_int_to_object(cls->nof_closed_vals - 1), ret);
  
  int i;
  for(i=2; i<cls->nof_closed_vals; i++)
    ret = cons(cls->closed_vals[i], ret);

  ret = cons(val, ret);
  
  reg_closure_t *cls1 = (reg_closure_t *)cls->data;
  
  return list(3,
              LET,
              list(1, list(2, i_ans, reverse(ret))),
              cls1->fn(cls1, i_ans));
}

reg_closure_t *create_reg_primop_closure(OBJECT_PTR    operator,
                                         OBJECT_PTR    operands,
                                         unsigned int  nof_vals,
                                         OBJECT_PTR    *vals,
                                         reg_closure_t *cls)
{  
  reg_closure_t *primop_closure = (reg_closure_t *)GC_MALLOC(sizeof(reg_closure_t));

  if(cons_length(operands) == 0) //last binding
    primop_closure->fn = primop_cont_fn_non_recur;
  else
    primop_closure->fn = primop_cont_fn_recur;

  primop_closure->nof_closed_vals = nof_vals + 2;
  primop_closure->closed_vals     = (OBJECT_PTR *)GC_MALLOC(primop_closure->nof_closed_vals * sizeof(OBJECT_PTR));

  primop_closure->closed_vals[0]  = operator;
  primop_closure->closed_vals[1]  = operands;

  int i;
  for(i=2; i<primop_closure->nof_closed_vals; i++)
    primop_closure->closed_vals[i] = vals[i-2];

  primop_closure->data = cls;
  
  return primop_closure;
}

OBJECT_PTR primop_metacont_fn(metacont_closure_t *mcls, reg_closure_t *cls)
{
  OBJECT_PTR operator      = mcls->closed_vals[0];
  OBJECT_PTR operands      = mcls->closed_vals[1];

  OBJECT_PTR e = first(operands);
  
  metacont_closure_t *mcls1 = mcps(e);
  
  return mcls1->mfn(mcls1, create_reg_primop_closure(operator, cdr(operands), 0, NULL, cls));  
}

OBJECT_PTR app_cont_fn_recur(reg_closure_t *cls, OBJECT_PTR val)
{
  OBJECT_PTR exp      = cls->closed_vals[0];

  unsigned int nof_vals = cls->nof_closed_vals - 1;
  
  OBJECT_PTR *new_vals = (OBJECT_PTR *)GC_MALLOC((nof_vals + 1) * sizeof(OBJECT_PTR));

  int i;
  for(i=0; i<nof_vals; i++)
    new_vals[i] = cls->closed_vals[i+1];

  new_vals[nof_vals] = val;

  metacont_closure_t *mcls = mcps(first(exp));
  
  return mcls->mfn(mcls,
                   create_reg_app_closure(cdr(exp), nof_vals+1, new_vals, (reg_closure_t *)cls->data));
}

OBJECT_PTR app_cont_fn_non_recur(reg_closure_t *cls, OBJECT_PTR val)
{
  OBJECT_PTR exp = cls->closed_vals[0];

  OBJECT_PTR i_k = gensym();

  OBJECT_PTR ret = NIL;

  int i;
  for(i=1; i<cls->nof_closed_vals; i++)
    ret = cons(cls->closed_vals[i], ret);

  ret = cons(val, ret);
  
  reg_closure_t *cls1 = (reg_closure_t *)cls->data;
  
  return list(3,
              LET,
              list(1, list(2, i_k, mc_to_exp(cls1))),
              concat(2, reverse(ret), list(1, i_k)));
}

reg_closure_t *create_reg_app_closure(OBJECT_PTR    exp,
                                      unsigned int  nof_vals,
                                      OBJECT_PTR    *vals,
                                      reg_closure_t *cls)
{  
  reg_closure_t *app_closure = (reg_closure_t *)GC_MALLOC(sizeof(reg_closure_t));

  if(cons_length(exp) == 0) //last binding
    app_closure->fn = app_cont_fn_non_recur;
  else
    app_closure->fn = app_cont_fn_recur;

  app_closure->nof_closed_vals = nof_vals + 1;
  app_closure->closed_vals     = (OBJECT_PTR *)GC_MALLOC(app_closure->nof_closed_vals * sizeof(OBJECT_PTR));

  app_closure->closed_vals[0]  = exp;

  int i;
  for(i=1; i<app_closure->nof_closed_vals; i++)
    app_closure->closed_vals[i] = vals[i-1];

  app_closure->data = cls;
  
  return app_closure;
}

OBJECT_PTR app_metacont_fn(metacont_closure_t *mcls, reg_closure_t *cls)
{
  OBJECT_PTR exp = mcls->closed_vals[0];

  OBJECT_PTR e = first(exp);
  
  metacont_closure_t *mcls1 = mcps(e);
  
  return mcls1->mfn(mcls1, create_reg_app_closure(cdr(exp), 0, NULL, cls));  
}

OBJECT_PTR ret_from_fn2(reg_closure_t *cls, OBJECT_PTR val)
{
  OBJECT_PTR v1 = cls->closed_vals[0];

  return list(2,
              list(2, GET_CONTINUATION, v1),
              val);
}

reg_closure_t *create_reg_ret_from_closure2(OBJECT_PTR val)
{
  reg_closure_t *ret_from_cls = (reg_closure_t *)GC_MALLOC(sizeof(reg_closure_t));

  ret_from_cls->fn = ret_from_fn2;

  ret_from_cls->nof_closed_vals = 1;
  ret_from_cls->closed_vals     = (OBJECT_PTR *)GC_MALLOC(ret_from_cls->nof_closed_vals * sizeof(OBJECT_PTR));
  ret_from_cls->closed_vals[0]  = val;
  //ret_from_cls->data            = cls;

  return ret_from_cls;  
}

OBJECT_PTR ret_from_fn1(reg_closure_t *cls, OBJECT_PTR val)
{
  OBJECT_PTR ret_exp = cls->closed_vals[0];

  metacont_closure_t *mcls = mcps(ret_exp);

  return mcls->mfn(mcls, create_reg_ret_from_closure2(val));
}

reg_closure_t *create_reg_ret_from_closure1(OBJECT_PTR ret_exp, reg_closure_t *cls)
{
  reg_closure_t *ret_from_cls = (reg_closure_t *)GC_MALLOC(sizeof(reg_closure_t));

  ret_from_cls->fn = ret_from_fn1;

  ret_from_cls->nof_closed_vals = 1;
  ret_from_cls->closed_vals     = (OBJECT_PTR *)GC_MALLOC(ret_from_cls->nof_closed_vals * sizeof(OBJECT_PTR));
  ret_from_cls->closed_vals[0]  = ret_exp;
  ret_from_cls->data            = cls;

  return ret_from_cls;
}

OBJECT_PTR return_from_metacont_fn(metacont_closure_t *mcls, reg_closure_t *cls)
{
  OBJECT_PTR exp = mcls->closed_vals[0];

  OBJECT_PTR fn_exp  = second(exp);
  OBJECT_PTR ret_exp = third(exp);
  
  metacont_closure_t *mcls1 = mcps(fn_exp);
  
  return mcls1->mfn(mcls1, create_reg_ret_from_closure1(ret_exp, cls));  
}

OBJECT_PTR throw_reg_fn(reg_closure_t *cls, OBJECT_PTR val)
{
  return list(2, THROW, val);
}

reg_closure_t *create_throw_reg_closure()
{
  reg_closure_t *cls = (reg_closure_t *)GC_MALLOC(sizeof(reg_closure_t));

  cls->fn              = throw_reg_fn;
  cls->nof_closed_vals = 0;
  cls->closed_vals     = NULL;
  cls->data            = NULL;

  return cls;
}

OBJECT_PTR throw_metacont_fn(metacont_closure_t *mcls, reg_closure_t *cls)
{
  OBJECT_PTR throw_exp = mcls->closed_vals[0];

  metacont_closure_t *mcls1 = mcps(throw_exp);

  return mcls1->mfn(mcls1, create_throw_reg_closure());
}

OBJECT_PTR call_cc_reg_fn(reg_closure_t *cls, OBJECT_PTR val)
{
  OBJECT_PTR mc_exp = mc_to_exp((reg_closure_t *)cls->data);
  
  return list(3, val, mc_exp, mc_exp);
}

reg_closure_t *create_call_cc_reg_closure(reg_closure_t *cls)
{
  reg_closure_t *cls1 = (reg_closure_t *)GC_MALLOC(sizeof(reg_closure_t));

  cls1->fn              = call_cc_reg_fn;
  cls1->nof_closed_vals = 0;
  cls1->closed_vals     = NULL;
  cls1->data            = cls;

  return cls1;
}

OBJECT_PTR call_cc_metacont_fn(metacont_closure_t *mcls, reg_closure_t *cls)
{
  OBJECT_PTR call_cc_exp = mcls->closed_vals[0];

  metacont_closure_t *mcls1 = mcps(call_cc_exp);

  return mcls1->mfn(mcls1, create_call_cc_reg_closure(cls));
}

OBJECT_PTR break_metacont_fn(metacont_closure_t *mcls, reg_closure_t *cls)
{
  return list(2, SAVE_CONTINUATION_TO_RESUME, mc_to_exp(cls));
}

metacont_closure_t *mcps(OBJECT_PTR exp)
{
  if(is_atom(exp) || is_quoted_expression(exp))
  {
    metacont_closure_t *mcls = (metacont_closure_t *)GC_MALLOC(sizeof(metacont_closure_t));

    mcls->mfn             = lit_id_metacont_fn;

    mcls->nof_closed_vals = 1;
    mcls->closed_vals     = (OBJECT_PTR *)GC_MALLOC(mcls->nof_closed_vals * sizeof(OBJECT_PTR));

    mcls->closed_vals[0]  = exp;
    
    return mcls;
  }
    
  OBJECT_PTR car_exp = car(exp);
  
  if(car_exp == LAMBDA)
  {
    metacont_closure_t *mcls = (metacont_closure_t *)GC_MALLOC(sizeof(metacont_closure_t));

    mcls->mfn             = lambda_metacont_fn;

    mcls->nof_closed_vals = 2;
    mcls->closed_vals     = (OBJECT_PTR *)GC_MALLOC(mcls->nof_closed_vals * sizeof(OBJECT_PTR));

    mcls->closed_vals[0]  = second(exp);
    mcls->closed_vals[1]  = third(exp);
    
    return mcls;    
  }

  if(car_exp == LET)
  {
    metacont_closure_t *mcls = (metacont_closure_t *)GC_MALLOC(sizeof(metacont_closure_t));

    mcls->mfn             = let_metacont_fn;

    mcls->nof_closed_vals = 2;
    mcls->closed_vals     = (OBJECT_PTR *)GC_MALLOC(mcls->nof_closed_vals * sizeof(OBJECT_PTR));

    mcls->closed_vals[0]  = second(exp);
    mcls->closed_vals[1]  = third(exp);
    
    return mcls;
  }

  if(primop(car_exp))
  {
    if(car_exp == RETURN_FROM)
    {
      metacont_closure_t *mcls = (metacont_closure_t *)GC_MALLOC(sizeof(metacont_closure_t));

      mcls->mfn             = return_from_metacont_fn;

      mcls->nof_closed_vals = 1;
      mcls->closed_vals     = (OBJECT_PTR *)GC_MALLOC(mcls->nof_closed_vals * sizeof(OBJECT_PTR));

      mcls->closed_vals[0]  = exp;
    
      return mcls;      
    }
    else if(car_exp == THROW)
    {
      metacont_closure_t *mcls = (metacont_closure_t *)GC_MALLOC(sizeof(metacont_closure_t));

      mcls->mfn             = throw_metacont_fn;

      mcls->nof_closed_vals = 1;
      mcls->closed_vals     = (OBJECT_PTR *)GC_MALLOC(mcls->nof_closed_vals * sizeof(OBJECT_PTR));

      mcls->closed_vals[0]  = second(exp);
    
      return mcls;      
    }
    else if(car_exp == CALL_CC)
    {
      metacont_closure_t *mcls = (metacont_closure_t *)GC_MALLOC(sizeof(metacont_closure_t));

      mcls->mfn             = call_cc_metacont_fn;

      mcls->nof_closed_vals = 1;
      mcls->closed_vals     = (OBJECT_PTR *)GC_MALLOC(mcls->nof_closed_vals * sizeof(OBJECT_PTR));

      mcls->closed_vals[0]  = second(exp);
    
      return mcls;      
    }
    else if(car_exp == BREAK)
    {
      metacont_closure_t *mcls = (metacont_closure_t *)GC_MALLOC(sizeof(metacont_closure_t));

      mcls->mfn             = break_metacont_fn;

      mcls->nof_closed_vals = 0;
      mcls->closed_vals     = NULL;
    
      return mcls;      
    }    
    else
    {
      metacont_closure_t *mcls = (metacont_closure_t *)GC_MALLOC(sizeof(metacont_closure_t));

      mcls->mfn             = primop_metacont_fn;

      mcls->nof_closed_vals = 2;
      mcls->closed_vals     = (OBJECT_PTR *)GC_MALLOC(mcls->nof_closed_vals * sizeof(OBJECT_PTR));

      mcls->closed_vals[0]  = car_exp;  //operator
      mcls->closed_vals[1]  = cdr(exp); //operands
    
      return mcls;
    }
  }
  
  if(car_exp == IF)
  {
    metacont_closure_t *mcls = (metacont_closure_t *)GC_MALLOC(sizeof(metacont_closure_t));

    mcls->mfn             = if_metacont_fn;

    mcls->nof_closed_vals = 3;
    mcls->closed_vals     = (OBJECT_PTR *)GC_MALLOC(mcls->nof_closed_vals * sizeof(OBJECT_PTR));

    mcls->closed_vals[0]  = second(exp);
    mcls->closed_vals[1]  = third(exp);
    mcls->closed_vals[2]  = fourth(exp);
    
    return mcls;    
  }

#ifdef WIN32
  if(car_exp == ERROR1)
#else
  if(car_exp == ERROR)
#endif    
  {
    metacont_closure_t *mcls = (metacont_closure_t *)GC_MALLOC(sizeof(metacont_closure_t));

    mcls->mfn             = error_metacont_fn;

    mcls->nof_closed_vals = 1;
    mcls->closed_vals     = (OBJECT_PTR *)GC_MALLOC(mcls->nof_closed_vals * sizeof(OBJECT_PTR));

    mcls->closed_vals[0]  = second(exp);
    
    return mcls;
  }

  //it is an application
  metacont_closure_t *mcls = (metacont_closure_t *)GC_MALLOC(sizeof(metacont_closure_t));

  mcls->mfn             = app_metacont_fn;

  mcls->nof_closed_vals = 1;
  mcls->closed_vals     = (OBJECT_PTR *)GC_MALLOC(mcls->nof_closed_vals * sizeof(OBJECT_PTR));

  mcls->closed_vals[0]  = exp;
    
  return mcls;
}

OBJECT_PTR identity_fn(reg_closure_t *cls, OBJECT_PTR val)
{
  return val;
}

reg_closure_t *create_identity_closure()
{
  reg_closure_t *cls = (reg_closure_t *)GC_MALLOC(sizeof(reg_closure_t));

  cls->fn =identity_fn;
  cls->nof_closed_vals = 0;
  cls->closed_vals= NULL;
  cls->data = NULL;

  return cls;
}

OBJECT_PTR mcps_transform(OBJECT_PTR exp)
{
  OBJECT_PTR i_k = gensym();
  
  metacont_closure_t *mcls = mcps(exp);

  return list(4,
              LAMBDA,
              list(1, i_k),
              list(2, SAVE_CONTINUATION, i_k),
              mcls->mfn(mcls, id_to_mc(i_k)));
}
