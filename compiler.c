#include <stdio.h>

#include "plisp.h"

extern OBJECT_PTR NIL;
extern OBJECT_PTR QUOTE;
extern OBJECT_PTR LAMBDA;
extern OBJECT_PTR IF;
extern OBJECT_PTR SET;
extern OBJECT_PTR CALL_CC;
extern OBJECT_PTR PROGN;
extern OBJECT_PTR LOAD_FILE;

extern OBJECT_PTR HALT;                  
extern OBJECT_PTR REFER;
extern OBJECT_PTR CONSTANT;
extern OBJECT_PTR CLOSE;
extern OBJECT_PTR MACRO;
extern OBJECT_PTR TEST;
extern OBJECT_PTR ASSIGN;         
extern OBJECT_PTR DEFINE;
extern OBJECT_PTR CONTI;
extern OBJECT_PTR NUATE;
extern OBJECT_PTR FRAME;
extern OBJECT_PTR ARGUMENT;
extern OBJECT_PTR APPLY;
extern OBJECT_PTR RETURN;
extern OBJECT_PTR BACKQUOTE;

extern expression_t *g_expr;

extern OBJECT_PTR reg_accumulator;
extern OBJECT_PTR reg_next_expression;
extern OBJECT_PTR reg_current_env;
extern OBJECT_PTR reg_current_value_rib;
extern OBJECT_PTR reg_current_stack;

extern OBJECT_PTR top_level_env;

extern FILE *yyin;

extern BOOLEAN debug_mode;

OBJECT_PTR execution_stack;
OBJECT_PTR debug_execution_stack;

BOOLEAN in_error;

OBJECT_PTR compile_loop(OBJECT_PTR args, OBJECT_PTR c, OBJECT_PTR next)
{
  if(args == NIL)
  {
    if(car(next) == RETURN)
      return c;
    else
      return cons(FRAME, cons(next ,cons(c, NIL)));
  }
  else
    return compile_loop(cdr(args), compile(car(args), cons(ARGUMENT, cons(c, NIL))), next);
}

OBJECT_PTR compile(OBJECT_PTR exp, OBJECT_PTR next)
{
  if(IS_SYMBOL_OBJECT(exp))
  {
    return cons(REFER, cons (exp, cons(next, NIL)));
  }

  if(IS_CONS_OBJECT(exp))
  {
    OBJECT_PTR car_obj = car(exp);

    if(car_obj == QUOTE)
      return cons(CONSTANT, cons(CADR(exp), cons(next, NIL)));

    if(car_obj == BACKQUOTE)
    {
      return cons(FRAME,
                  cons(next,
                       cons(cons(CONSTANT,
                                 cons(CADR(exp),
                                      cons(cons(ARGUMENT,
                                                cons(cons(CONSTANT,
                                                          cons(BACKQUOTE,
                                                               cons(cons(APPLY, NIL),
                                                                    NIL))),
                                                     NIL)),
                                           NIL))),
                            NIL)));
    }

    if(car_obj == LAMBDA)
      return cons(CLOSE, cons(CADR(exp), cons(compile(cons(PROGN, CDDR(exp)), cons(RETURN, NIL)), cons(next, NIL))));

    if(car_obj == MACRO)
      return cons(MACRO, cons(CADR(exp), cons(compile(cons(PROGN, CDDR(exp)), cons(RETURN, NIL)), cons(next, NIL))));

    if(car_obj == IF)
    {
      OBJECT_PTR thenc = compile(CADDR(exp), next);
      OBJECT_PTR elsec = compile(CADDDR(exp), next);

      return compile(CADR(exp), cons(TEST, cons(thenc, cons(elsec, NIL))));
    }

    if(car_obj == SET)
      return compile(CADDR(exp), cons(ASSIGN, cons(CADR(exp), cons(next, NIL))));

    if(car_obj == DEFINE)
      return compile(CADDR(exp), cons(DEFINE, cons(CADR(exp), cons(next, NIL))));

    if(car_obj == CALL_CC)
    {
      OBJECT_PTR c = cons(CONTI,
                          cons(cons(ARGUMENT, 
                                    cons(compile(CADR(exp), cons(APPLY, NIL)), 
                                         NIL)),
                               NIL));

      if(car(next) == RETURN)
        return c;
      else
        return cons(FRAME, cons(next, cons(c, NIL)));
    }

    if(car_obj == PROGN)
      return compile_progn(cdr(exp), next);

    //macros
    if(IS_SYMBOL_OBJECT(car_obj))
    {
      OBJECT_PTR res = get_symbol_value_from_env(car_obj, top_level_env);

      if(car(res) != NIL)
      {
        if(IS_MACRO_OBJECT(cdr(res)))
        {
          reg_next_expression = cons(FRAME,
                                     cons(cons(HALT, NIL),
                                          cons(cons(APPLY, NIL),
                                               NIL)));

          eval();

          reg_current_value_rib = NIL;

          OBJECT_PTR args = cdr(exp);

          //build the value rib with the unevaluated
          //arguments
          while(args != NIL)
          {
            if(reg_current_value_rib == NIL)
              reg_current_value_rib = cons(car(args), NIL);
            else
              set_heap((last_cell(reg_current_value_rib) >> OBJECT_SHIFT) + 1, 
                       cons(car(args), NIL));         
            args = cdr(args);
          }

          //place the macro object in the accumulator (to invoke APPLY)
          reg_accumulator = cdr(res);
          /* reg_next_expression = cons(APPLY, NIL); */
          
          //evaluate the macro invocation
          while(reg_next_expression != NIL)
            eval();

          //compile the output of the macro invocation
          return compile(reg_accumulator, next);

        } /* end of if(IS_MACRO_OBJECT(cdr(res))) */
      }
    } /* end of if(IS_SYMBOL_OBJECT(car_obj)) */

    //it's an application
    //TODO: do we need a closure here (bundling 'next' with compile_loop
    //instead of passing it as a parameter?)
    return compile_loop(cdr(exp), compile(car(exp), cons(APPLY, NIL)), next);
  }

  //it's a constant object (integer, float, char, string, array, closure, continuation, macro) 
  return cons(CONSTANT, cons (exp, cons(next, NIL)));
}

OBJECT_PTR compile_progn(OBJECT_PTR exps, OBJECT_PTR next)
{
  if(cdr(exps) == NIL)
    return compile(car(exps), next);
  else
    return compile(car(exps), compile_progn(cdr(exps), next));
}

int main(int argc, char **argv)
{
  if(argc == 1)
  {
    initialize();
    load_core_library();
    if(in_error)
    {
      cleanup();
      exit(1);
    }
  }
  else
  {
    load_from_image(argv[1]);
  }

  welcome();

  while(1)
  {
    prompt();
    yyparse();
    repl();
  }
}

int repl()
{
  if(!g_expr)
    return 0;

  if(g_expr->type == SYMBOL && ( !strcmp(g_expr->atom_value,"QUIT") ||
				 !strcmp(g_expr->atom_value,"EXIT") ||
				 !strcmp(g_expr->atom_value,"Q") ))
  {
    fprintf(stdout, "Bye.\n");
    cleanup();
    exit(0);
  }
  else if(debug_mode)
  {
    OBJECT_PTR out;
    int val = convert_expression_to_object(g_expr, &out);

    if(val != 0)
      return 1;

    if(!is_permitted_in_debug_mode(out))
    {
      fprintf(stdout, "Expression not permitted in debug mode\n");
      return 1;
    }

    reg_next_expression = compile(out, cons(HALT, NIL));

    reg_current_env = NIL;

    reg_current_value_rib = NIL;
    reg_current_stack = NIL;

    execution_stack = NIL;    

    while(reg_next_expression != NIL)
      eval();

    if(yyin == stdin && !in_error)
      print_object(reg_accumulator);

    delete_expression(g_expr);
    g_expr = NULL;
  }
  else
  {
    reg_accumulator = NIL;

    OBJECT_PTR exp;
    int val = convert_expression_to_object(g_expr, &exp);

    if(val != 0)
      return 1;

    reg_next_expression = compile(exp, cons(HALT, NIL));

    reg_current_env = NIL;

    reg_current_value_rib = NIL;
    reg_current_stack = NIL;

    execution_stack = NIL;

    in_error = false;

    while(reg_next_expression != NIL)
      eval();

    if(yyin == stdin && !in_error)
      print_object(reg_accumulator);

    delete_expression(g_expr);
    g_expr = NULL;

    if(!debug_mode)
      gc();

    /* if(yyin == stdin) */
    /*   prompt(); */

    return 0;
  }
}

void load_core_library()
{
  reg_accumulator       = NIL;
  reg_next_expression   = compile(cons(LOAD_FILE, cons(get_string_object("plisp.lisp"),
                                                       NIL)), 
                                  cons(HALT, NIL));

  reg_current_env = NIL;

  reg_current_value_rib = NIL;
  reg_current_stack     = NIL;

  execution_stack = NIL;

  in_error = false;

  while(reg_next_expression != NIL)
    eval();

  if(in_error)
    return;

  gc();

  return;  
}
