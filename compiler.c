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
extern OBJECT_PTR WHILE;

extern OBJECT_PTR ERROR;

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

OBJECT_PTR continuations_map;

OBJECT_PTR compile_loop(OBJECT_PTR args, OBJECT_PTR c, OBJECT_PTR next)
{
  if(args == NIL)
  {
    if(car(next) == RETURN)
      return c;
    else
      return cons(FRAME, 
                       cons(next,
                            cons(c, NIL)));
  }
  else
  {
    OBJECT_PTR temp = compile(car(args), 
                              cons(cons(ARGUMENT, 
                                        cons(c, NIL)),
                                   cdr(c)));

    if(temp == ERROR)
    {
      printf("compile_loop() failed\n");
      return ERROR;
    }
    else
      return compile_loop(cdr(args), temp, next);
  }
}

OBJECT_PTR compile(OBJECT_PTR exp, OBJECT_PTR next)
{
  if(IS_SYMBOL_OBJECT(exp))
  {
    return cons(cons(REFER,
                     cons(exp, 
                          cons(next, NIL))),
                exp);
  }

  if(IS_CONS_OBJECT(exp))
  {
    OBJECT_PTR car_obj = car(exp);

    if(car_obj == QUOTE)
      return cons(cons(CONSTANT,
                       cons(CADR(exp), 
                            cons(next, NIL))),
                  exp);

    if(car_obj == BACKQUOTE)
    {

      //basically (FRAME next (CONSTANT cadr(exp) (ARGUMENT (CONSTANT BACKQUOTE (APPLY)))))
      //decorated with exp (cons'd to all the instructions recursively)

      return cons(cons(FRAME, cons(next, cons(cons(cons(CONSTANT, cons(CADR(exp), cons(cons(cons(ARGUMENT, cons(cons(cons(CONSTANT, cons(BACKQUOTE, cons(cons(cons(APPLY, NIL), NIL), exp))), NIL), exp)), NIL), exp))), NIL), exp))), exp);

    }

    if(car_obj == LAMBDA)
    {
      OBJECT_PTR temp = compile(cons(PROGN, CDDR(exp)), cons(cons(RETURN, NIL), exp));

      if(temp == ERROR)
      {
        printf("Compiling lambda expression failed\n");
        return ERROR;
      }
      else
        return cons(cons(CLOSE, cons(CADR(exp), cons(temp, cons(next, NIL)))),
                    exp);
    }

    if(car_obj == MACRO)
    {
      OBJECT_PTR temp = compile(cons(PROGN, CDDR(exp)), cons(cons(RETURN, NIL), exp));

      if(temp == ERROR)
      {
        printf("Compiling macro expression failed\n");
        return ERROR;
      }
      else
        return cons(cons(MACRO, cons(CADR(exp), cons(temp, cons(next, NIL)))),
                    exp);
    }

    if(car_obj == IF)
    {
      OBJECT_PTR thenc = compile(CADDR(exp), next);
      if(thenc == ERROR)
      {
        printf("Compiling then clause of IF expression failed\n");
        return ERROR;
      }

      OBJECT_PTR elsec = compile(CADDDR(exp), next);
      if(elsec == ERROR)
      {
        printf("Compiling else clause of IF expression failed\n");
        return ERROR;
      }

      return compile(CADR(exp), cons(cons(TEST, cons(thenc, cons(elsec, NIL))), exp));
    }

    if(car_obj == WHILE)
    {
      OBJECT_PTR cond = compile(CADR(exp), cons(cons(HALT,NIL), exp));
      if(cond == ERROR)
      {
        printf("Compiling condition in WHILE failed\n");
        return ERROR;
      }

      OBJECT_PTR body = compile(cons(PROGN, CDDR(exp)), 
                                cons(cons(HALT, NIL), exp));
      if(body == ERROR)
      {
        printf("Compiling body of WHILE failed\n");
        return ERROR;
      }

      return cons(cons(WHILE, cons(cond, cons(body, cons(next, NIL)))),
                  exp);
    }

    if(car_obj == SET)
      return compile(CADDR(exp), cons(cons(ASSIGN, cons(CADR(exp), cons(next, NIL))), exp));

    if(car_obj == DEFINE)
      return compile(CADDR(exp), cons(cons(DEFINE, cons(CADR(exp), cons(next, NIL))), exp) );

    if(car_obj == CALL_CC)
    {
      OBJECT_PTR temp = compile(CADR(exp), cons(cons(APPLY, NIL), exp));
      if(temp == ERROR)
      {
        printf("Compiling CALL-CC failed\n");
        return ERROR;
      }

      OBJECT_PTR c = cons(CONTI,
                          cons(cons(cons(ARGUMENT, 
                                         cons(temp, 
                                              NIL)),
                                    exp),
                               NIL));

      if(car(next) == RETURN)
        return cons(c,exp);
      else
        return cons(cons(FRAME, cons(next, cons(cons(c, NIL), exp))),
                    exp);
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
          reg_next_expression = cons(cons(FRAME,
                                          cons(cons(cons(HALT, NIL), car_obj),
                                               cons(cons(cons(APPLY, NIL), car_obj),
                                                    NIL))),
                                     car_obj);

          eval();

          if(in_error)
          {
            printf("Compiling macro failed(1)\n");
            return ERROR;
          }

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
          while(car(reg_next_expression) != NIL)
          {
            eval();
            if(in_error)
            {
              printf("Compiling macro failed (2)\n");
              return ERROR;
            }
          }

          //compile the output of the macro invocation
          return compile(reg_accumulator, next);

        } /* end of if(IS_MACRO_OBJECT(cdr(res))) */
      }
    } /* end of if(IS_SYMBOL_OBJECT(car_obj)) */

    //it's an application
    return cons(compile_loop(cdr(exp), compile(car(exp), cons(cons(APPLY, NIL), exp)), next), exp);
  }

  //it's a constant object (integer, float, char, string, array) 
  return cons(cons(CONSTANT, cons (exp, cons(next, NIL))),
              exp);
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
  print_copyright_notice();

  if(argc == 1)
  {
    initialize();

    if(load_core_library())
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

    reg_next_expression = compile(out, cons(cons(HALT, NIL), out));

    if(reg_next_expression == ERROR)
    {
      fprintf(stdout, "Compilation of expression failed\n");
      return 1;
    }

    reg_current_env = NIL;

    reg_current_value_rib = NIL;
    reg_current_stack = NIL;

    execution_stack = NIL;    

    continuations_map = NIL;

    while(car(reg_next_expression) != NIL)
    {
      eval();
      if(in_error)
      {
        fprintf(stdout, "Eval failed\n");
        return 1;
      }
    }

    if(yyin == stdin && !in_error)
      print_object(reg_accumulator);

    //delete_expression(g_expr);
    //g_expr = NULL;
  }
  else
  {
    reg_accumulator = NIL;

    OBJECT_PTR exp;
    int val = convert_expression_to_object(g_expr, &exp);

    if(val != 0)
      return 1;

    reg_next_expression = compile(exp, cons(cons(HALT, NIL), exp));

    reg_current_env = NIL;

    reg_current_value_rib = NIL;
    reg_current_stack = NIL;

    execution_stack = NIL;
    continuations_map = NIL;

    in_error = false;

    while(car(reg_next_expression) != NIL)
    {
      eval();
      if(in_error)
        return 1;
    }

    if(yyin == stdin && !in_error)
      print_object(reg_accumulator);

    //reset the EXCEPTION-HANDLERS variable
    update_environment(cons(top_level_env, NIL),
                       get_symbol_object("EXCEPTION-HANDLERS"),
                       NIL);

    //delete_expression(g_expr);
    //g_expr = NULL;

    if(!debug_mode)
      gc();

    /* if(yyin == stdin) */
    /*   prompt(); */

  }

  delete_expression(g_expr);
  g_expr = NULL;

  return 0;
}

int load_core_library()
{
  fprintf(stdout, "Loading core library...");
  fflush(stdout);
  
  reg_accumulator       = NIL;

  OBJECT_PTR src = cons(LOAD_FILE, 
                        cons(get_string_object("plisp.lisp"),
                             NIL));

  OBJECT_PTR temp = compile(src, cons(cons(HALT, NIL), src));

  if(temp == ERROR)
  {
    fprintf(stdout, "Compile failed while loading core library\n");
    return 1;
  }

  reg_next_expression   = temp;

  reg_current_env = NIL;

  reg_current_value_rib = NIL;
  reg_current_stack     = NIL;

  execution_stack = NIL;

  continuations_map = NIL;

  in_error = false;

  while(car(reg_next_expression) != NIL)
  {
    eval();
    if(in_error)
    {
      fprintf(stdout, "Eval failed while loading core library\n");
      return 1;
    }
  }

  //gc();

  //hack to prevent message being overwritten
  fprintf(stdout, "Loading core library... done\n");

  return 0;  
}
