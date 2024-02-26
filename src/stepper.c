/**
  Copyright 2011-2024 Rajesh Jayaprakash <rajesh.jayaprakash@pm.me>

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

#include "stack.h"

typedef struct exception_handler
{
  OBJECT_PTR catch_clause;
  OBJECT_PTR env;
  continuation_t *k;
} exception_handler_t;

typedef struct fn_cont
{
  OBJECT_PTR fn;
  continuation_t *k;
} fn_cont_t;

//forward declarations
OBJECT_PTR step_if(OBJECT_PTR, OBJECT_PTR);
OBJECT_PTR step_let(OBJECT_PTR, OBJECT_PTR);
OBJECT_PTR step_letrec(OBJECT_PTR, OBJECT_PTR);
OBJECT_PTR step_define(OBJECT_PTR, OBJECT_PTR);
OBJECT_PTR step_set(OBJECT_PTR, OBJECT_PTR);
OBJECT_PTR step_lambda(OBJECT_PTR, OBJECT_PTR);
OBJECT_PTR step_app(OBJECT_PTR, OBJECT_PTR);
void stepper_error(char *, char *);

continuation_t *make_id_cont(OBJECT_PTR);
continuation_t *make_if_cont(OBJECT_PTR, OBJECT_PTR, OBJECT_PTR, continuation_t *, OBJECT_PTR);
OBJECT_PTR step_cont(OBJECT_PTR, OBJECT_PTR, continuation_t *);
OBJECT_PTR resume_cont(continuation_t *, OBJECT_PTR);
OBJECT_PTR expand_macro_stepper(OBJECT_PTR);
OBJECT_PTR preprocess_stepper(OBJECT_PTR);

BOOLEAN is_primitive_fn(OBJECT_PTR);
OBJECT_PTR primitive_apply(OBJECT_PTR, OBJECT_PTR);
//end of forward declarations

//external functions
char *get_symbol_name(OBJECT_PTR);
OBJECT_PTR concat(unsigned int, ...);
int get_top_level_sym_value(OBJECT_PTR, OBJECT_PTR *);
void add_top_level_sym(OBJECT_PTR, OBJECT_PTR);     
void update_dependencies(OBJECT_PTR, OBJECT_PTR);
int update_references(OBJECT_PTR, OBJECT_PTR);

OBJECT_PTR mapcar(OBJECT_PTR (*f)(OBJECT_PTR,OBJECT_PTR),
                  OBJECT_PTR list1,
                  OBJECT_PTR list2);

OBJECT_PTR first(OBJECT_PTR);
OBJECT_PTR second(OBJECT_PTR);
OBJECT_PTR third(OBJECT_PTR);
OBJECT_PTR fourth(OBJECT_PTR);

OBJECT_PTR butlast(OBJECT_PTR);
int cons_length(OBJECT_PTR);

OBJECT_PTR primitive_setcdr(OBJECT_PTR, OBJECT_PTR);
OBJECT_PTR CDDR(OBJECT_PTR);

OBJECT_PTR full_monty_eval(OBJECT_PTR);
OBJECT_PTR prim_get_source(OBJECT_PTR);

OBJECT_PTR get_params_object(OBJECT_PTR);

OBJECT_PTR compile_and_evaluate(OBJECT_PTR, OBJECT_PTR);
void print_object(OBJECT_PTR);

int convert_expression_to_object(expression_t *, OBJECT_PTR *);
BOOLEAN is_valid_object(OBJECT_PTR);
void delete_expression(expression_t *);
void cleanup();
void quit_application();

BOOLEAN IS_SYMBOL_OBJECT(OBJECT_PTR);
BOOLEAN IS_CONS_OBJECT(OBJECT_PTR);
BOOLEAN primop(OBJECT_PTR);
BOOLEAN IS_FUNCTION2_OBJECT(OBJECT_PTR);
BOOLEAN IS_MACRO2_OBJECT(OBJECT_PTR);

OBJECT_PTR reverse(OBJECT_PTR);

uintptr_t extract_ptr(OBJECT_PTR);
BOOLEAN is_special_form(OBJECT_PTR);

OBJECT_PTR cons_equivalent(OBJECT_PTR);
OBJECT_PTR last_cell(OBJECT_PTR);
void set_heap(uintptr_t, unsigned int, OBJECT_PTR);

OBJECT_PTR quote_all_arguments(OBJECT_PTR);

OBJECT_PTR exp_macro_full(OBJECT_PTR);

void print_qualified_symbol(OBJECT_PTR, char *);
OBJECT_PTR get_qualified_symbol_object(char *, char *);

OBJECT_PTR rewrite_symbols(OBJECT_PTR);

OBJECT_PTR get_symbol_object(char *);
OBJECT_PTR get_string_object(char *);

OBJECT_PTR handle_exception();
OBJECT_PTR expand_bodies(OBJECT_PTR);
OBJECT_PTR replace_t(OBJECT_PTR);

BOOLEAN is_core_package_op(OBJECT_PTR);

OBJECT_PTR reverse_sym_lookup(OBJECT_PTR);

void create_stepper_window();
void close_stepper_window();

void show_stepper_window(OBJECT_PTR, OBJECT_PTR, continuation_t *, OBJECT_PTR);
//end of external functions

//external variables
extern OBJECT_PTR NIL;
extern OBJECT_PTR IF;
extern OBJECT_PTR LET;
extern OBJECT_PTR LETREC;
extern OBJECT_PTR LAMBDA;
extern OBJECT_PTR DEFINE;
extern OBJECT_PTR SET;
extern OBJECT_PTR QUOTE;
extern OBJECT_PTR RETURN_FROM;
extern OBJECT_PTR CALL_CC;

extern OBJECT_PTR BREAK;
extern OBJECT_PTR DEFUN;

extern BOOLEAN system_changed;
extern expression_t *g_expr;

extern BOOLEAN console_mode;
extern OBJECT_PTR saved_continuations;
extern OBJECT_PTR continuations_for_return;
extern OBJECT_PTR most_recent_closure;

extern OBJECT_PTR exception_object;
extern OBJECT_PTR exception_handlers;

extern BOOLEAN debug_mode;
extern OBJECT_PTR debug_stack;

extern unsigned int current_package;
extern package_t *packages;

extern char **strings;

extern BOOLEAN in_error;

extern BOOLEAN run_to_completion;
extern BOOLEAN abrt_stepper;
extern BOOLEAN step_over;
continuation_t *step_over_continuation;
//end of external variables

//globals
OBJECT_PTR lambda_closures;

OBJECT_PTR stepper_exception;
BOOLEAN in_stepper_error;

BOOLEAN stepper_mode;

BOOLEAN disable_stepper_exception_handling;

stack_type *stepper_exception_handlers;

stack_type *conts_for_return;

OBJECT_PTR fn_source;
//end globals


void stepper_error(char *excp_name, char *excp_str)
{
  in_stepper_error = true;
  //stepper_exception = cons(get_symbol_object(excp_name), get_string_object(excp_str));
  stepper_exception = list(2, get_symbol_object(excp_name), get_string_object(excp_str));
  exception_object = stepper_exception;
}

OBJECT_PTR stepper_map(OBJECT_PTR (*f)(OBJECT_PTR), OBJECT_PTR lst)
{
  assert(lst == NIL || IS_CONS_OBJECT(lst));

  OBJECT_PTR ret = NIL, rest = lst;

  while(rest != NIL)
  {
    OBJECT_PTR val = f(car(rest));

    if(in_stepper_error)
      return NIL;
    
    if(ret == NIL)
      ret = cons(val, NIL);
    else
    {
      uintptr_t ptr = extract_ptr(last_cell(ret));
      set_heap(ptr, 1, cons(val, NIL));        
    }

    rest = cdr(rest);
  }

  return ret;  
}

OBJECT_PTR stepper_map2(OBJECT_PTR (*f)(OBJECT_PTR, OBJECT_PTR, OBJECT_PTR), 
                        OBJECT_PTR v1, 
                        OBJECT_PTR v2, 
                        OBJECT_PTR lst)
{
  OBJECT_PTR ret = NIL, rest = lst;

  while(rest != NIL)
  {
    OBJECT_PTR val = f(car(rest), v1, v2);

    if(in_stepper_error)
      return NIL;
    
    if(ret == NIL)
      ret = cons(val, NIL);
    else
    {
      uintptr_t ptr = extract_ptr(last_cell(ret));
      set_heap(ptr, 1, cons(val, NIL));        
    }

    rest = cdr(rest);
  }

  return ret;  
}

BOOLEAN symbols_equal(OBJECT_PTR sym1, OBJECT_PTR sym2)
{
  /* char buf1[100], buf2[100]; */

  /* print_qualified_symbol(sym1, buf1); */
  /* print_qualified_symbol(sym2, buf2); */

  /* return !strcmp(buf1, buf2); */
  return sym1 == sym2;
}

OBJECT_PTR last_n(OBJECT_PTR lst, int n)
{
  OBJECT_PTR rest = lst;
  int i;
  int len = cons_length(lst);

  for(i=0; i<len-n; i++)
    rest = cdr(rest);

  return clone_object(rest);
}

OBJECT_PTR last(OBJECT_PTR lst)
{
  return car(last_n(lst,1));
}

OBJECT_PTR lookup(OBJECT_PTR sym, OBJECT_PTR env)
{
  if(primop(sym) || is_special_form(sym))
    return sym;

  OBJECT_PTR rest = env;

  while(rest != NIL)
  {
    OBJECT_PTR e = car(rest);

    if(symbols_equal(sym, first(e)))  
      return second(e);

    rest = cdr(rest);
  }

  OBJECT_PTR val;

  int retval = get_top_level_sym_value(sym, &val);

  if(!retval)
    return car(val);
  
  stepper_error("EXCEPTION", "Unable to look up object");

  return NIL;
}

OBJECT_PTR concat_stepper(unsigned int count, ...)
{
  va_list ap;
  OBJECT_PTR lst, ret, rest;
  int i, start = 1;

  if(!count)
    return NIL;

  va_start(ap, count);

  lst = (OBJECT_PTR)va_arg(ap, OBJECT_PTR);

  if(!IS_CONS_OBJECT(lst) && lst != NIL)
  {
    print_object(lst);printf("\n");
    assert(false);
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
      print_object(lst);
      assert(false);
    }
  }

  //ret = clone_object(lst);
  ret = lst;
  
  for(i=start; i<count; i++)
  {
    lst = (OBJECT_PTR)va_arg(ap, OBJECT_PTR);

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
      uintptr_t ptr = extract_ptr(last_cell(ret));
      //set_heap(ptr, 1, cons(clone_object(car(rest)), NIL));
      set_heap(ptr, 1, cons(car(rest), NIL));
      
      rest = cdr(rest);
    }
  }

  va_end(ap);

  return ret;
}

OBJECT_PTR extend_env(OBJECT_PTR env, OBJECT_PTR new_env)
{
  if(env == NIL)
    return new_env;
  else if(new_env == NIL)
    return env;
  else
    return concat_stepper(2, new_env, env);
}

void print_environment(OBJECT_PTR env)
{
  if(env == NIL)
  {
    printf("<Empty>");
    return;
  }
  
  OBJECT_PTR rest = env;

  while(rest != NIL)
  {
    OBJECT_PTR e = car(rest);
    print_object(first(e));
    printf(" : ");
    print_object(second(e));
    printf("\n");
    
    rest = cdr(rest);
  }
}

OBJECT_PTR step_internal(OBJECT_PTR exp, OBJECT_PTR env)
{
  printf("Stepping into\n");

  if(IS_SYMBOL_OBJECT(exp))
  {
    char buf[100];
    print_qualified_symbol(exp, buf);
    printf("%s", buf);
  }
  else
    print_object(exp);
  
  printf("\n");
  printf("Environment:\n");
  //print_environment(env);
  print_object(env);
  printf("\n");

  OBJECT_PTR ret;
  
  if(is_atom(exp))
  {
    if(exp == NIL)
      ret = NIL;
    else
    {
      if(IS_SYMBOL_OBJECT(exp))
      {
        if(primop(exp)) //TODO: add check for special form too?
          ret = exp;
        else
          ret = lookup(exp, env);
      }
      else
        ret = exp;
    }
  }
  else if(IS_CONS_OBJECT(exp))
  {
    OBJECT_PTR obj = car(exp);

    if(obj == IF)
      ret = step_if(exp, env);
    else if(obj == LET)
      ret = step_let(exp, env);
    else if(obj == LETREC)
      ret = step_letrec(exp, env);
    else if(obj == DEFINE)
      ret = step_define(exp, env);
    else if(obj == SET)
      ret = step_set(exp, env);
    else if(obj == LAMBDA)
      ret = step_lambda(exp, env);
    else if(obj == QUOTE)
      ret = second(exp);
    else if(!strcmp(get_symbol_name(obj), "ERROR"))
    {
      OBJECT_PTR desc_obj = second(exp);
      stepper_error("EXCEPTION", is_string_object(desc_obj) ? get_string(desc_obj) : strings[(int)desc_obj >> OBJECT_SHIFT]);
      ret = NIL;
    }
    else
      ret = step_app(exp, env);
  }
  else
    assert(false);

  if(in_stepper_error)
    return NIL;
  
  printf("Return: ");
  print_object(ret);
  printf("\n");

  return ret;
}

OBJECT_PTR step_if(OBJECT_PTR exp, OBJECT_PTR env)
{
  OBJECT_PTR cond = step_internal(second(exp), env);

  if(in_stepper_error)
    return NIL;
  
  if(cond != NIL)
  {
    OBJECT_PTR then = step_internal(third(exp), env);
    
    if(in_stepper_error)
      return NIL;
    
    return then;
  }
  else
  {
    OBJECT_PTR else1 = step_internal(fourth(exp), env);

    if(in_stepper_error)
      return NIL;

    return else1;
  }
}

OBJECT_PTR temp_fn1(OBJECT_PTR x, OBJECT_PTR v1, OBJECT_PTR v2)
{
  OBJECT_PTR val = step_internal(second(x), v1);

  if(in_stepper_error)
    return NIL;
  
  return list(2, first(x), val);
}

OBJECT_PTR step_let(OBJECT_PTR exp, OBJECT_PTR env)
{
  OBJECT_PTR lst = stepper_map2(temp_fn1, env, NIL, second(exp));

  if(in_stepper_error)
    return NIL;

  OBJECT_PTR ext_env = extend_env(env, lst);

  OBJECT_PTR rest = butlast(CDDR(exp));

  while(rest != NIL)
  {
    OBJECT_PTR e = car(rest);

    step_internal(e, ext_env);

    if(in_stepper_error)
      return NIL;
    
    rest = cdr(rest);
  }

  OBJECT_PTR val = step_internal(last(CDDR(exp)), ext_env);

  if(in_stepper_error)
    return NIL;

  return val;
}

OBJECT_PTR temp_fn2(OBJECT_PTR x)
{
  return list(2, first(x), NIL);
}

OBJECT_PTR nth(int i_val, OBJECT_PTR lst)
{
  assert(IS_CONS_OBJECT(lst) || IS_FUNCTION2_OBJECT(lst) || IS_MACRO2_OBJECT(lst));

  OBJECT_PTR lst1 = IS_CONS_OBJECT(lst) ? lst : extract_ptr(lst) + CONS_TAG;

  if(i_val < 0 || i_val >= cons_length(lst1))
    return NIL;
  else
  {
    int i = 0;

    OBJECT_PTR rest = lst1;
    
    while(i<i_val)
    {
      rest = cdr(rest);
      i++;
    }

    return car(rest);
  }
}

OBJECT_PTR step_letrec(OBJECT_PTR exp, OBJECT_PTR env)
{
  OBJECT_PTR let_env = stepper_map(temp_fn2, second(exp));

  if(in_stepper_error)
    return NIL;
  
  int n = cons_length(let_env);
  OBJECT_PTR ext_env = extend_env(env, let_env);

  int i;

  for(i=0; i<n; i++)
  {
    OBJECT_PTR val = step_internal(second(nth(i, second(exp))), ext_env);

    if(in_stepper_error)
      return NIL;
    
    primitive_setcdr(nth(i, ext_env),
                     cons(val, NIL));
  }

  OBJECT_PTR rest = butlast(CDDR(exp));

  while(rest != NIL)
  {
    OBJECT_PTR e = car(rest);

    step_internal(e, ext_env);

    if(in_stepper_error)
      return NIL;
    
    rest = cdr(rest);
  }

  OBJECT_PTR val = step_internal(last(CDDR(exp)), ext_env);

  if(in_stepper_error)
    return NIL;

  return val;
}

OBJECT_PTR step_lambda(OBJECT_PTR exp, OBJECT_PTR env)
{
  OBJECT_PTR closure_obj = compile_and_evaluate(exp, exp);

  assert(IS_FUNCTION2_OBJECT(closure_obj));

  //to append the source for the closure
  OBJECT_PTR cons_equiv = cons_equivalent(closure_obj);
  uintptr_t ptr = extract_ptr(last_cell(cons_equiv));
  set_heap(ptr, 1, cons(exp, NIL));
  closure_obj = (extract_ptr(cons_equiv)) + FUNCTION2_TAG;
  
  lambda_closures = cons(cons(closure_obj, env), lambda_closures);
  
  return closure_obj;
}

OBJECT_PTR step_define(OBJECT_PTR exp, OBJECT_PTR env)
{
  OBJECT_PTR sym = cdr(get_qualified_symbol_object(packages[current_package].name, get_symbol_name(second(exp))));
  
  OBJECT_PTR val = step_internal(third(exp), env);

  if(in_stepper_error)
    return NIL;
  
  add_top_level_sym(sym, cons(val, NIL));     

  update_dependencies(sym, cons(val, NIL));

  if(update_references(sym, val))
  {
    stepper_error("EXCEPTION", "Update of reference to top level symbol failed");
    return NIL;
  }

  system_changed = true;
  
  return val;
}

OBJECT_PTR step_set(OBJECT_PTR exp, OBJECT_PTR env)
{
  OBJECT_PTR rest = env;

  OBJECT_PTR sym = second(exp);

  OBJECT_PTR val = step_internal(third(exp), env);

  if(in_stepper_error)
    return NIL;
  
  while(rest != NIL)
  {
    OBJECT_PTR x = car(rest);

    if(symbols_equal(sym, first(x)))
    {
      primitive_setcdr(x, cons(val, NIL));
      return val;
    }
    
    rest = cdr(rest);
  }

  //check if the symbol is a top-level symbol
  OBJECT_PTR out;
  int retval = get_top_level_sym_value(sym, &out);

  if(retval)
  {
    char msg[100];
    sprintf(msg, "Symbol %s not bound", get_symbol_name(sym));
    stepper_error("EXCEPTION", msg);
    return NIL;
  }
  
  add_top_level_sym(sym, cons(val, NIL));     

  update_dependencies(sym, cons(val, NIL));

  if(update_references(sym, val))
  {
    char msg[100];
    sprintf(msg, "Update of reference to top level symbol failed");
    stepper_error("EXCEPTION", msg);
    return NIL;
  }

  system_changed = true;
  //end of check for top-level symbol
  
  return val;
}

OBJECT_PTR get_env_for_lambda(OBJECT_PTR closure_obj)
{
  assert(IS_FUNCTION2_OBJECT(closure_obj));
  
  OBJECT_PTR rest = lambda_closures;

  while(rest != NIL)
  {
    OBJECT_PTR x = car(rest);
    if(closure_obj == car(x))
      return cdr(x);
    rest = cdr(rest);
  }

  //stepper_error("EXCEPTION", "Unable to get environment for closure object");
  
  return NIL;
}

BOOLEAN contains_rest(OBJECT_PTR lst)
{
  if(lst == NIL)
    return false;
  else
  {
    if(!strcmp(get_symbol_name(car(lst)), "&REST"))
      return true;
    else
      return contains_rest(cdr(lst));
  }
}

OBJECT_PTR temp_fn3(OBJECT_PTR x, OBJECT_PTR v1, OBJECT_PTR v2)
{
  OBJECT_PTR val = step_internal(x,v1);

  if(in_stepper_error)
    return NIL;

  return val;
}

OBJECT_PTR butlast_n(OBJECT_PTR lst, int n)
{
  int i;

  OBJECT_PTR ret = lst;
  
  for(i=0; i<n; i++)
    ret = butlast(ret);

  return ret;
}

OBJECT_PTR temp_fn4(OBJECT_PTR x, OBJECT_PTR y)
{
  return list(2, x, y);
}

OBJECT_PTR create_bindings(OBJECT_PTR syms, OBJECT_PTR vals)
{
  if(contains_rest(syms))
  {
    int n = cons_length(syms);
    OBJECT_PTR res = mapcar(temp_fn4,
                            butlast_n(syms, 2),
                            butlast_n(vals, cons_length(vals) - (n-2) ));
    
    res = cons(cons(last(syms),
                    list(1, last_n(vals, cons_length(vals) - (n-2)))),
               res);
    
    return res;
  }
  else
    return mapcar(temp_fn4, syms, vals);
}

OBJECT_PTR step_app(OBJECT_PTR exp, OBJECT_PTR env)
{
  OBJECT_PTR rator = step_internal(car(exp), env);

  if(in_stepper_error)
    return NIL;
  
  OBJECT_PTR rands = stepper_map2(temp_fn3, env, NIL, cdr(exp));

  if(in_stepper_error)
    return NIL;  
  
  if(primop(rator))
  {
    OBJECT_PTR ret = full_monty_eval(concat(2, list(1, rator), quote_all_arguments(rands)));

    if(in_error)
    {
      OBJECT_PTR exception_name = car(exception_object);
      OBJECT_PTR desc_obj = cdr(exception_object);
      stepper_error(get_symbol_name(exception_name), is_string_object(desc_obj) ? get_string(desc_obj) : strings[(int)desc_obj >> OBJECT_SHIFT]);
      return NIL;
    }
    else
      return ret;
  }
  else
  {
    assert(IS_FUNCTION2_OBJECT(rator));

    OBJECT_PTR arg_syms = get_params_object(rator);
    OBJECT_PTR new_env = create_bindings(arg_syms, rands);

    OBJECT_PTR source = get_source_object(rator);
    
    OBJECT_PTR rest = butlast(source);

    OBJECT_PTR extended_env;
    OBJECT_PTR env_lambda = get_env_for_lambda(rator);

    if(in_stepper_error)
      return NIL;
    
    if(env_lambda != NIL)
      extended_env = extend_env(env_lambda, reverse(new_env));
    else
      extended_env = reverse(new_env);

    while(rest != NIL)
    {
      OBJECT_PTR e = car(rest);

      step_internal(e, extended_env);

      if(in_stepper_error)
        return NIL;
      
      rest = cdr(rest);
    }

    OBJECT_PTR val = step_internal(last(source), extended_env);

    if(in_stepper_error)
      return NIL;

    return val;
  }
}

OBJECT_PTR step1(OBJECT_PTR exp)
{
  lambda_closures = NIL;
  //TODO: handle '(try ...)
  OBJECT_PTR val = step_internal(exp_macro_full(exp), NIL);

  if(in_stepper_error)
    return NIL;

  return val;
}

void handle_stepper_exception()
{
  char buf[200];
  memset(buf, '\0', 200);

  OBJECT_PTR desc_obj = second(stepper_exception);
  
  sprintf(buf, "Uncaught exception %s: %s", get_symbol_name(car(stepper_exception)), 
          is_string_object(desc_obj) ? get_string(desc_obj) : strings[(int)desc_obj >> OBJECT_SHIFT]);
  raise_error(buf);

  in_error = false;
  
  //this is actually redundant
  in_stepper_error = false;
  stepper_exception = NIL;
}

void cleanup_stepper_env()
{
  lambda_closures = NIL;
  
  saved_continuations = NIL;
  continuations_for_return = NIL;
  most_recent_closure = NIL;

  exception_object = NIL;
  exception_handlers = NIL;

  if(!debug_mode)
    debug_stack = NIL;

  exception_object = NIL;

  in_error = false;
  in_stepper_error = false;
  stepper_exception = NIL;
  disable_stepper_exception_handling = false;

  stepper_exception_handlers = stack_create();

  conts_for_return = stack_create();  

  abrt_stepper = false;
}

int repl_step()
{
  stepper_mode = true;
  
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
    {
      handle_exception();
      return 1;
    }

    cleanup_stepper_env();
    
    OBJECT_PTR exp1 = rewrite_symbols(exp);

    OBJECT_PTR exp2 = preprocess_stepper(exp1);

    cleanup_stepper_env();
    
    //OBJECT_PTR res = step(exp1);
    continuation_t *id_continuation = make_id_cont(NIL); //TODO: do this initialization outside the repl loop
    OBJECT_PTR res = step_cont(exp2, NIL, id_continuation);
    
    if(in_stepper_error)
      handle_stepper_exception();
    else
      print_object(res);
  }

  delete_expression(g_expr);
  g_expr = NULL;

  return 0;
}

// ---begin continuation-based interpreter code---

void print_cont_object(continuation_t *k)
{
  //printf("Type: ");
  if(k->type == ID_CONT)
    printf("Identity");
  else if(k->type == IF_CONT)
  {
    printf("IF");
    //print_object(k->exp1);
    //printf("\n");
    //print_object(k->exp2);
    //printf("\n");
  }
  else if(k->type == TRY_CONT)
    printf("TRY");
  else if(k->type == FINALLY_CONT)
    printf("FINALLY");
  else if(k->type == THROW_CONT)
    printf("THROW");
  else if(k->type == PRIMOP_CONT)
    printf("PRIMOP");
  else if(k->type == FN_APP_CONT)
    printf("FN_APP");
  else if(k->type == PROGN_CONT)
    printf("PROGN");
  else if(k->type == DEFINE_CONT)
    printf("DEFINE");
  else if(k->type == SET_CONT)
    printf("SET");
  else if(k->type == LET_CONT)
    printf("LET");
  else if(k->type == LETREC_CONT)
    printf("LETREC");
  else if(k->type == RETURN_FROM_CONT)
    printf("RETURN_FROM_CONT");
  else
    assert(false);
}

continuation_t *make_id_cont(OBJECT_PTR fn_source)
{
  continuation_t *ret = GC_MALLOC(sizeof(continuation_t));

  ret->type = ID_CONT;

  ret->fn_source = fn_source;
  
  return ret;  
}

continuation_t *make_if_cont(OBJECT_PTR then_exp, OBJECT_PTR else_exp, OBJECT_PTR env, continuation_t *k, OBJECT_PTR fn_source)
{
  /* printf("making if cont\n"); */
  /* print_object(env); */
  /* printf("\n"); */
  /* getchar(); */
  
  continuation_t *ret = GC_MALLOC(sizeof(continuation_t));

  ret->type = IF_CONT;
  ret->exp1 = then_exp;
  ret->exp2 = else_exp;
  ret->env = env;
  ret->k = k;

  ret->fn_source = fn_source;
  return ret;
}

continuation_t *make_finally_cont(OBJECT_PTR finally_clause, OBJECT_PTR env, continuation_t *k, OBJECT_PTR fn_source)
{
  continuation_t *ret = GC_MALLOC(sizeof(continuation_t));
  
  ret->type = FINALLY_CONT;
  ret->exp1 = finally_clause;
  ret->env = env;
  ret->k = k;

  ret->fn_source = fn_source;
  
  return ret;
}

continuation_t *make_try_cont(OBJECT_PTR catch_clause, OBJECT_PTR finally_clause, OBJECT_PTR env, continuation_t *k, OBJECT_PTR fn_source)
{
  continuation_t *ret = GC_MALLOC(sizeof(continuation_t));

  continuation_t *finally_cont = make_finally_cont(finally_clause, env, k, fn_source);
  
  ret->type = TRY_CONT;
  ret->env = env;
  ret->k = finally_cont;

  ret->fn_source = fn_source;
  
  exception_handler_t *excp_handler = (exception_handler_t *)GC_MALLOC(sizeof(exception_handler_t));
  excp_handler->catch_clause = catch_clause;
  excp_handler->env = env;
  excp_handler->k = finally_cont;
  
  stack_push(stepper_exception_handlers, excp_handler);

  assert(!stack_is_empty(stepper_exception_handlers));
  
  return ret;  
}

continuation_t *make_throw_cont(continuation_t *k, OBJECT_PTR fn_source)
{
  continuation_t *ret = GC_MALLOC(sizeof(continuation_t));  
  ret->type = THROW_CONT;
  ret->k = k;

  ret->fn_source = fn_source;

  return ret;
}

continuation_t *make_error_cont(OBJECT_PTR fn_source)
{
  continuation_t *ret = GC_MALLOC(sizeof(continuation_t));
  ret->type = ERROR_CONT;

  ret->fn_source = fn_source;

  return ret;
}

continuation_t *make_primop_cont(OBJECT_PTR operator, OBJECT_PTR rands_to_be_evaled, OBJECT_PTR rands_evaled, OBJECT_PTR env, continuation_t *k, OBJECT_PTR fn_source)
{
  continuation_t *ret = GC_MALLOC(sizeof(continuation_t));

  ret->type = PRIMOP_CONT;
  ret->exp1 = operator;
  ret->exp2 = rands_to_be_evaled;
  ret->exp3 = rands_evaled;
  ret->env = env;
  ret->k = k;

  ret->fn_source = fn_source;
  
  return ret;  
}

continuation_t *make_fn_app_cont(OBJECT_PTR operator, OBJECT_PTR rands_to_be_evaled, OBJECT_PTR rands_evaled, OBJECT_PTR env, continuation_t *k, OBJECT_PTR fn_source)
{
  continuation_t *ret = GC_MALLOC(sizeof(continuation_t));

  ret->type = FN_APP_CONT;
  ret->exp1 = operator;
  ret->exp2 = rands_to_be_evaled;
  ret->exp3 = rands_evaled;
  ret->env = env;
  ret->k = k;

  ret->fn_source = fn_source;
  
  return ret;  
}

continuation_t *make_progn_cont(OBJECT_PTR exps, OBJECT_PTR env, continuation_t *k, OBJECT_PTR fn_source)
{
  continuation_t *ret = GC_MALLOC(sizeof(continuation_t));

  ret->type = PROGN_CONT;
  ret->exp1 = exps;
  ret->env = env;
  ret->k = k;

  ret->fn_source = fn_source;

  return ret;  
}

continuation_t *make_define_cont(OBJECT_PTR sym, continuation_t *k, OBJECT_PTR fn_source)
{
  continuation_t *ret = GC_MALLOC(sizeof(continuation_t));

  ret->type = DEFINE_CONT;
  ret->exp1 = sym;
  ret->k = k;

  ret->fn_source = fn_source;

  return ret;  
}

continuation_t *make_set_cont(OBJECT_PTR sym, OBJECT_PTR env, continuation_t *k, OBJECT_PTR fn_source)
{
  /* printf("making set cont\n"); */
  /* print_object(env); */
  /* printf("\n"); */
  /* getchar(); */
  
  continuation_t *ret = GC_MALLOC(sizeof(continuation_t));

  ret->type = SET_CONT;
  ret->exp1 = sym;
  ret->env = env;
  ret->k = k;

  ret->fn_source = fn_source;
  
  return ret;  
}

continuation_t *make_let_cont(OBJECT_PTR full_bindings,
                              OBJECT_PTR let_body,
                              OBJECT_PTR to_be_evaled_bindings,
                              OBJECT_PTR evaluated_vals,
                              OBJECT_PTR env,
                              continuation_t *k,
                              OBJECT_PTR fn_source)
{
  continuation_t *ret = GC_MALLOC(sizeof(continuation_t));

  ret->type = LET_CONT;
  ret->exp1 = full_bindings;
  ret->exp2 = let_body;
  ret->exp3 = to_be_evaled_bindings;
  ret->exp4 = evaluated_vals;
  ret->env = env;
  ret->k = k;

  ret->fn_source = fn_source;

  return ret;  
}

continuation_t *make_letrec_cont(OBJECT_PTR full_bindings,
                                 OBJECT_PTR let_body,
                                 OBJECT_PTR to_be_evaled_bindings,
                                 OBJECT_PTR evaluated_vals,
                                 OBJECT_PTR env,
                                 continuation_t *k,
                                 OBJECT_PTR fn_source)
{
  continuation_t *ret = GC_MALLOC(sizeof(continuation_t));

  ret->type = LETREC_CONT;
  ret->exp1 = full_bindings;
  ret->exp2 = let_body;
  ret->exp3 = to_be_evaled_bindings;
  ret->exp4 = evaluated_vals;
  ret->env = env;
  ret->k = k;

  ret->fn_source = fn_source;
  return ret;  
}

continuation_t *make_return_from_cont(OBJECT_PTR ret_exp, OBJECT_PTR fn_source)
{
  continuation_t *ret = GC_MALLOC(sizeof(continuation_t));

  ret->type = RETURN_FROM_CONT;
  ret->exp1 = ret_exp;

  ret->fn_source = fn_source;
  return ret;
}

OBJECT_PTR step_cont(OBJECT_PTR exp, OBJECT_PTR env, continuation_t *k)
{
  /* printf("Evaluating ");print_object(exp); */
  /* printf(" in environment ");print_object(env); */
  /* printf(" with continuation "); */
  /* print_cont_object(k); */
  /* printf("\n"); */

  if(abrt_stepper)
    return NIL;

  if(is_atom(exp))
  {
    if(exp == NIL)
      return resume_cont(k, NIL);
    else
    {
      if(IS_SYMBOL_OBJECT(exp))
      {
        if(primop(exp)) //TODO: add check for special form too?
        {
          printf("primops yet to be implemented\n");
          exit(1);
        }
        else
          return resume_cont(k, lookup(exp, env));
      }
      else
        return resume_cont(k, exp);
    }
  }
  else if(IS_CONS_OBJECT(exp))
  {
    OBJECT_PTR obj = car(exp);

    if((!run_to_completion && !step_over) || obj == BREAK)
      show_stepper_window(exp, env, k, fn_source);

    if(obj == BREAK)
      return resume_cont(k, NIL);
    else if(obj == DEFINE)
      return step_cont(third(exp), env, make_define_cont(second(exp), k, fn_source));
    else if(obj == SET)
      return step_cont(third(exp), env, make_set_cont(second(exp), env, k, fn_source));
    else if(obj == LET)
      return step_cont(second(first(second(exp))), env, make_let_cont(second(exp), CDDR(exp), cdr(second(exp)), NIL, env, k, fn_source));
    else if(obj == LETREC)
    {
      OBJECT_PTR let_env = stepper_map(temp_fn2, second(exp));
      OBJECT_PTR extended_env = extend_env(env, let_env);
      return step_cont(second(first(second(exp))), extended_env, make_letrec_cont(second(exp), CDDR(exp), cdr(second(exp)), NIL, extended_env, k, fn_source));
    }
    else if(obj == RETURN_FROM)
    {
      OBJECT_PTR fn = step_cont(second(exp), env, make_id_cont(fn_source));
      assert(IS_FUNCTION2_OBJECT(fn));
      return step_cont(third(exp), env, make_return_from_cont(fn, fn_source));
    }
    else if(obj == CALL_CC)
    {
      stepper_error("EXCEPTION", "CALL/CC not implemented in stepper");
      return resume_cont(k, NIL);
    }
    else if(obj == QUOTE)
      return resume_cont(k, second(exp));
    else if(obj == IF)
      return step_cont(second(exp), env, make_if_cont(third(exp), fourth(exp), env, k, fn_source));
    else if(IS_SYMBOL_OBJECT(obj) && !strcmp(get_symbol_name(obj), "TRY"))
    {
      disable_stepper_exception_handling = true;
      return step_cont(second(exp), env, make_try_cont(third(exp), fourth(exp), env, k, fn_source));
    }
    else if(IS_SYMBOL_OBJECT(obj) && !strcmp(get_symbol_name(obj), "THROW"))
    {
      return step_cont(second(exp), env, make_throw_cont(k, fn_source));
    }
    else if(IS_SYMBOL_OBJECT(obj) && !strcmp(get_symbol_name(obj), "ERROR"))
    {
      return step_cont(second(exp), env, make_error_cont(fn_source));
    }
    else if(obj == LAMBDA)
    {
      OBJECT_PTR closure_obj = compile_and_evaluate(exp, exp);

      if(in_error)
      {
        OBJECT_PTR exception_name = car(exception_object);
        //OBJECT_PTR desc_obj = cdr(exception_object);
        OBJECT_PTR desc_obj = second(exception_object);
        stepper_error(get_symbol_name(exception_name), is_string_object(desc_obj) ? get_string(desc_obj) : strings[(int)desc_obj >> OBJECT_SHIFT]);
        return resume_cont(k, NIL);
      }
      
      assert(IS_FUNCTION2_OBJECT(closure_obj));

      //to append the source for the closure
      OBJECT_PTR cons_equiv = cons_equivalent(closure_obj);
      uintptr_t ptr = extract_ptr(last_cell(cons_equiv));
      set_heap(ptr, 1, cons(exp, NIL));
      closure_obj = (extract_ptr(cons_equiv)) + FUNCTION2_TAG;
  
      lambda_closures = cons(cons(closure_obj, env), lambda_closures);
  
      return resume_cont(k, closure_obj);
    }
    else if(primop(obj))
    {
      return step_cont(second(exp), env, make_primop_cont(obj, CDDR(exp), NIL, env, k, fn_source));
    }
    else //it's a function application
    {
      return step_cont(first(exp), env, make_fn_app_cont(NIL, cdr(exp), NIL, env, k, fn_source));
    }
  }
  else
  {
    printf("Warning: invalid object type passed to step_cont()\n");
    return NIL;
  }
}

OBJECT_PTR resume_cont(continuation_t *k, OBJECT_PTR v)
{
  /* printf("Resuming "); */
  /* print_cont_object(k); */
  /* printf(" with value "); */
  /* print_object(v); */
  /* printf("\n"); */

  if(step_over && k->k == step_over_continuation)
    step_over = false;
  
  fn_source = k->fn_source;
  
  if(in_stepper_error && k->type != FINALLY_CONT)
  {
    if(stack_is_empty(stepper_exception_handlers))
    {
      //to handle the case where a throw in the code
      //generates the expression
      if(exception_object == NIL)
        exception_object = stepper_exception;
      handle_exception();
      return NIL;      
    }
    else
    {
      exception_handler_t *excp_handler = (exception_handler_t *)stack_pop(stepper_exception_handlers);

      OBJECT_PTR catch_clause = excp_handler->catch_clause;
      OBJECT_PTR catch_env = excp_handler->env;
      continuation_t *k1 = excp_handler->k;
      
      // TO DO: this assumes the catch body is a single expression, needs to be modified to handle multiple expressions
      in_stepper_error = false;
      OBJECT_PTR ret = step_cont(third(catch_clause), extend_env(catch_env, list(1, list(2, first(second(catch_clause)), stepper_exception))), k1);
      disable_stepper_exception_handling = false;

      return ret;
    }
  }
  
  if(k->type == ID_CONT)
    return v;
  else if(k->type == IF_CONT)
  {
    OBJECT_PTR then_exp = k->exp1;
    OBJECT_PTR else_exp = k->exp2;

    if(v != NIL)
      return step_cont(then_exp, k->env, k->k);
    else
      return step_cont(else_exp, k->env, k->k);
  }
  else if(k->type == TRY_CONT)
  {
    
    if(!in_stepper_error)
    {
      //pop the exception handler
      stack_pop(stepper_exception_handlers);

      return resume_cont(k->k, v);
    }
    else
    {
      //the exception handler created by the TRY excpression should be there
      assert(!stack_is_empty(stepper_exception_handlers));

      exception_handler_t *excp_handler = (exception_handler_t *)stack_pop(stepper_exception_handlers);
      OBJECT_PTR catch_clause = excp_handler->catch_clause;
      continuation_t *k1 = excp_handler->k;
      
      // TO DO: this assumes the catch body is a single expression, needs to be modified to handle multiple expressions
      OBJECT_PTR ret = step_cont(third(catch_clause), extend_env(k->env, list(1, list(2, first(second(catch_clause)), stepper_exception))), k1);
      disable_stepper_exception_handling = false;
      in_stepper_error = false;
      return ret;
    }
  }
  else if(k->type == FINALLY_CONT)
  {
    in_stepper_error = false;
    in_error = false;
    OBJECT_PTR ret = step_cont(k->exp1, k->env, make_id_cont(k->fn_source));

    return resume_cont(k->k, v);
  }
  else if(k->type == ERROR_CONT)
  {
    stepper_error("EXCEPTION", is_string_object(v) ? get_string(v) : strings[(int)v >> OBJECT_SHIFT]);
    return resume_cont(k, NIL);
  }
  else if(k->type == PRIMOP_CONT)
  {
    if(k->exp2 == NIL) //all operands evaluated
    {
      OBJECT_PTR ret = full_monty_eval(concat(2, list(1, k->exp1), quote_all_arguments(reverse(cons(v, k->exp3)))));
      
      if(in_error)
      {
        OBJECT_PTR exception_name = car(exception_object);
        //OBJECT_PTR desc_obj = cdr(exception_object);
        OBJECT_PTR desc_obj = second(exception_object);
        stepper_error(get_symbol_name(exception_name), is_string_object(desc_obj) ? get_string(desc_obj) : strings[(int)desc_obj >> OBJECT_SHIFT]);
        return resume_cont(k->k, NIL);
      }
      else
        return resume_cont(k->k, ret);  
    }
    else
    {
      return step_cont(car(k->exp2), k->env, make_primop_cont(k->exp1, cdr(k->exp2), cons(v, k->exp3), k->env, k->k, k->fn_source));
    }
  }
  else if(k->type == FN_APP_CONT)
  {
    if(k->exp1 == NIL) //first call
    {
      assert(IS_FUNCTION2_OBJECT(v));

      return step_cont(car(k->exp2), k->env, make_fn_app_cont(v, cdr(k->exp2), k->exp3, k->env, k->k, k->fn_source));
    }
    else
    {
      if(k->exp2 == NIL) //all operands evaluated
      {
        OBJECT_PTR rator = k->exp1;
        
        OBJECT_PTR arg_syms = get_params_object(rator);

        OBJECT_PTR new_env;

        if(arg_syms == NIL)
          new_env = NIL;
        else
          new_env = create_bindings(arg_syms, reverse(cons(v, k->exp3)));

        OBJECT_PTR source = preprocess_stepper(get_source_object(rator));

        OBJECT_PTR fn_name = reverse_sym_lookup(rator);

        if(is_primitive_fn(fn_name))
        {
          OBJECT_PTR ret = primitive_apply(rator, reverse(cons(v, k->exp3)));

          if(in_error)
          {
            OBJECT_PTR exception_name = car(exception_object);
            OBJECT_PTR desc_obj = second(exception_object);
            stepper_error(get_symbol_name(exception_name), is_string_object(desc_obj) ? get_string(desc_obj) : strings[(int)desc_obj >> OBJECT_SHIFT]);
            return resume_cont(k->k, NIL);
          }
          else
            return resume_cont(k->k, ret);
        }
        else
        {
          if(fn_name == NIL)
            fn_source = concat(2, list(2, LAMBDA, arg_syms), source);
          else
            fn_source = concat(2, list (3, DEFUN, fn_name, arg_syms), source);

          OBJECT_PTR rest = source;
        
          OBJECT_PTR extended_env;

          OBJECT_PTR env_lambda = get_env_for_lambda(rator);

          /* if(k->env != NIL) */
          /*   extended_env = extend_env(k->env, reverse(new_env)); */
          /* else */
          /*   extended_env = reverse(new_env); */
          if(env_lambda != NIL)
            extended_env = extend_env(env_lambda, reverse(new_env));
          else
            extended_env = reverse(new_env);
        
          fn_cont_t *fc = (fn_cont_t *)GC_MALLOC(sizeof(fn_cont_t));

          fc->fn = rator;
          fc->k = k->k;
        
          stack_push(conts_for_return, fc);
        
          return step_cont(car(rest), extended_env, make_progn_cont(cdr(rest), extended_env, k->k, fn_source));
        }
      }
      else
      {
        return step_cont(car(k->exp2), k->env, make_fn_app_cont(k->exp1, cdr(k->exp2), cons(v, k->exp3), k->env, k->k, k->fn_source));
      }
    }
  }
  else if(k->type == THROW_CONT)
  {
    stepper_exception = v;
    in_stepper_error = true;
    disable_stepper_exception_handling = true;
    return resume_cont(k->k, NIL);
  }
  else if(k->type == PROGN_CONT)
  {
    if(k->exp1 == NIL) //all expressions evaluated
    {
      return resume_cont(k->k, v);
    }
    else
    {
      //v is not passed to the progn continuation
      return step_cont(car(k->exp1), k->env, make_progn_cont(cdr(k->exp1), k->env, k->k, k->fn_source));
    }
  }
  else if(k->type == DEFINE_CONT)
  {
    OBJECT_PTR sym = cdr(get_qualified_symbol_object(packages[current_package].name, get_symbol_name(k->exp1)));
  
    add_top_level_sym(sym, cons(v, NIL));     

    update_dependencies(sym, cons(v, NIL));

    if(update_references(sym, v))
    {
      stepper_error("EXCEPTION", "Update of reference to top level symbol failed");
      return resume_cont(k->k, NIL);
    }

    system_changed = true;
  
    return resume_cont(k->k, v);
  }
  else if(k->type == SET_CONT)
  {
    OBJECT_PTR rest = k->env;
    OBJECT_PTR sym = k->exp1;

    while(rest != NIL)
    {
      OBJECT_PTR x = car(rest);

      if(symbols_equal(sym, first(x)))
      {
        primitive_setcdr(x, cons(v, NIL));
        return resume_cont(k->k, v);
      }
    
      rest = cdr(rest);
    }

    //check if the symbol is a top-level symbol
    OBJECT_PTR out;
    int retval = get_top_level_sym_value(sym, &out);

    if(retval)
    {
      char msg[100];
      sprintf(msg, "Symbol %s not bound", get_symbol_name(sym));
      stepper_error("EXCEPTION", msg);
      return resume_cont(k->k, NIL);
    }
  
    add_top_level_sym(sym, cons(v, NIL));     

    update_dependencies(sym, cons(v, NIL));

    if(update_references(sym, v))
    {
      char msg[100];
      sprintf(msg, "Update of reference to top level symbol failed");
      stepper_error("EXCEPTION", msg);
      return resume_cont(k->k, NIL);
    }

    system_changed = true;
    //end of check for top-level symbol
  
    return resume_cont(k->k, v);
  }
  else if(k->type == LET_CONT)
  {
    if(cons_length(k->exp3) == 0) //all binding expressions evaluated
    {
      OBJECT_PTR syms = stepper_map(car, k->exp1);
      
      OBJECT_PTR new_env = mapcar(temp_fn4, syms, reverse(cons(v,k->exp4)));

      OBJECT_PTR extended_env = extend_env(k->env, new_env);
      
      return step_cont(car(k->exp2), extended_env, make_progn_cont(cdr(k->exp2), extended_env, k->k, k->fn_source));
    }
    else
    {
      return step_cont(second(first(k->exp3)), k->env, make_let_cont(k->exp1, k->exp2, cdr(k->exp3), cons(v, k->exp4), k->env, k->k, k->fn_source));
    }
  }
  else if(k->type == LETREC_CONT)
  {
    int n = cons_length(k->exp1) - cons_length(k->exp3) - 1;
    primitive_setcdr(nth(n, k->env), cons(v, NIL));

    if(cons_length(k->exp3) == 0) //all binding expressions evaluated
      return step_cont(car(k->exp2), k->env, make_progn_cont(cdr(k->exp2), k->env, k->k, k->fn_source));
    else
      return step_cont(second(first(k->exp3)), k->env, make_letrec_cont(k->exp1, k->exp2, cdr(k->exp3), cons(v, k->exp4), k->env, k->k, k->fn_source));
  }
  else if(k->type == RETURN_FROM_CONT)
  {
    unsigned n = stack_count(conts_for_return);
    unsigned int i;

    fn_cont_t **data = (fn_cont_t **)stack_data(conts_for_return);
    
    for(i=0; i<n; i++)
    {
      fn_cont_t *fc = data[i];

      if(fc->fn == k->exp1)
        return resume_cont(fc->k, v);
    }

    stepper_error("EXCEPTION", "Unable to get destination for RETURN-FROM");
    return resume_cont(k, NIL);
  }
  else
  {
    printf("Other continuation types yet to be implemented\n");
    exit(1);
  }
}

OBJECT_PTR preprocess_stepper(OBJECT_PTR exp)
{
  OBJECT_PTR ret = expand_macro_stepper(exp);
  return ret;
}

OBJECT_PTR expand_macro_stepper(OBJECT_PTR exp)
{
  OBJECT_PTR ret;
  if(IS_CONS_OBJECT(exp) &&
     IS_SYMBOL_OBJECT(car(exp)) &&
     !strcmp(get_symbol_name(car(exp)), "TRY"))
    ret = list(4,
                cdr(get_qualified_symbol_object("CORE", "TRY")),
                expand_macro_stepper(second(exp)),
                expand_macro_stepper(third(exp)),
                expand_macro_stepper(fourth(exp)));
  else
    ret = exp_macro_full(exp);

  return ret;
}

OBJECT_PTR step(OBJECT_PTR exp)
{
  stepper_mode = true;
  cleanup_stepper_env();
    
  OBJECT_PTR exp1 = rewrite_symbols(exp);

  OBJECT_PTR exp2 = preprocess_stepper(exp1);

  cleanup_stepper_env();

  run_to_completion = false;
  step_over = false;
  
  continuation_t *id_continuation = make_id_cont(exp);

  fn_source = exp2;

  create_stepper_window();
  
  OBJECT_PTR res = step_cont(exp2, NIL, id_continuation);
    
  if(in_stepper_error)
  {
    handle_stepper_exception();
    stepper_mode = false;
    close_stepper_window();
    return NIL;
  }
  else
  {
    stepper_mode = false;
    close_stepper_window();
    return res;
  }
}


//this considers as primitive functions
//only those that will occur in user
//code, so it's a subset of the full list 
//covered in is_primop() in compiler.c
BOOLEAN is_primitive_fn(OBJECT_PTR fn_name)
{
  if(!IS_SYMBOL_OBJECT(fn_name))
    return false;

  char *fn_name_str = get_symbol_name(fn_name);

  return !strcmp(fn_name_str, "+")                ||
    !strcmp(fn_name_str, "-")                     ||
    !strcmp(fn_name_str, "*")                     ||
    !strcmp(fn_name_str, "/")                     ||
    !strcmp(fn_name_str, ">")                     ||
    !strcmp(fn_name_str, "<")                     ||
    !strcmp(fn_name_str, "<=")                    ||
    !strcmp(fn_name_str, ">=")                    ||
    !strcmp(fn_name_str, "ATOM")                  ||
    !strcmp(fn_name_str, "CONCAT")                ||
    !strcmp(fn_name_str, "EQ")                    ||
    !strcmp(fn_name_str, "LIST")                  ||
    !strcmp(fn_name_str, "CONS")                  ||
    !strcmp(fn_name_str, "CAR")                   ||
    !strcmp(fn_name_str, "CDR")                   ||
    !strcmp(fn_name_str, "PRINT")                 ||
    !strcmp(fn_name_str, "SYMBOL-VALUE")          ||
    !strcmp(fn_name_str, "GENSYM")                ||
    !strcmp(fn_name_str, "SETCAR")                ||
    !strcmp(fn_name_str, "SETCDR")                ||
    !strcmp(fn_name_str, "APPLY")                 ||
    !strcmp(fn_name_str, "SYMBL")                 ||
    !strcmp(fn_name_str, "SYMBOL-NAME")           ||
    !strcmp(fn_name_str, "FORMAT")                ||
    !strcmp(fn_name_str, "CLONE")                 ||
    !strcmp(fn_name_str, "UNBIND")                ||
    !strcmp(fn_name_str, "NEWLINE")               ||
    !strcmp(fn_name_str, "NOT")                   ||
    !strcmp(fn_name_str, "STRING")                ||
    !strcmp(fn_name_str, "MAKE-ARRAY")            ||
    !strcmp(fn_name_str, "ARRAY-GET")             ||
    !strcmp(fn_name_str, "ARRAY-SET")             ||
    !strcmp(fn_name_str, "SUB-ARRAY")             ||
    !strcmp(fn_name_str, "ARRAY-LENGTH")          ||
    !strcmp(fn_name_str, "PRINT-STRING")          ||
    !strcmp(fn_name_str, "CONSP")                 ||
    !strcmp(fn_name_str, "LISTP")                 ||
    !strcmp(fn_name_str, "INTEGERP")              ||
    !strcmp(fn_name_str, "FLOATP")                ||
    !strcmp(fn_name_str, "CHARACTERP")            ||
    !strcmp(fn_name_str, "SYMBOLP")               ||
    !strcmp(fn_name_str, "STRINGP")               ||
    !strcmp(fn_name_str, "ARRAYP")                ||
    !strcmp(fn_name_str, "CLOSUREP")              ||
    !strcmp(fn_name_str, "MACROP")                ||
    !strcmp(fn_name_str, "CONTINUATIONP")         ||
    !strcmp(fn_name_str, "LOAD-FOREIGN-LIBRARY")  ||
    !strcmp(fn_name_str, "CALL-FF-INTERNAL") ||
    !strcmp(fn_name_str, "CREATE-PACKAGE")        ||
    !strcmp(fn_name_str, "IN-PACKAGE")            ||
    !strcmp(fn_name_str, "EXPORT-PACKAGE")        ||
    !strcmp(fn_name_str, "IMPORT-PACKAGE")        ||
    !strcmp(fn_name_str, "CREATE-IMAGE")          ||
    !strcmp(fn_name_str, "SAVE-OBJECT")           ||
    !strcmp(fn_name_str, "LOAD-OBJECT")           ||
    !strcmp(fn_name_str, "LOAD-FILE")             ||
    !strcmp(fn_name_str, "EVAL");  
}


/*
TODO: 

1. Handling call/cc may be tricky since continuations in the stepper are not
   OBJECT_PTRs (may need to do some coercing) - maybe not implement it for this
   iteration of the stepper.

2. Assertion failed: (IS_FUNCTION2_OBJECT(closure_obj)) in step_cont() for letrec
   (occurs randomly)

3. How to handle throw statements within try (in any location in the try expression)?

*/

// ---end of continuation-based interpreter code--
