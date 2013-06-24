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
#include <assert.h>
#include <dlfcn.h>

#include "plisp.h"

extern OBJECT_PTR reg_accumulator;
extern OBJECT_PTR reg_next_expression;
extern OBJECT_PTR reg_current_env;
extern OBJECT_PTR reg_current_value_rib;
extern OBJECT_PTR reg_current_stack;

extern OBJECT_PTR TRUE;
extern OBJECT_PTR NIL;

extern OBJECT_PTR CONS;
extern OBJECT_PTR EQ;
extern OBJECT_PTR ATOM;
extern OBJECT_PTR CAR;
extern OBJECT_PTR CDR;

extern OBJECT_PTR ADD;
extern OBJECT_PTR SUB;
extern OBJECT_PTR MULT;
extern OBJECT_PTR DIV;

extern OBJECT_PTR PRINT;
extern OBJECT_PTR DEFVAR;
extern OBJECT_PTR LST;
extern OBJECT_PTR LISTP;
extern OBJECT_PTR SYMBOL_VALUE;
extern OBJECT_PTR DEFMACRO;

extern OBJECT_PTR GT;
extern OBJECT_PTR GENSYM;
extern OBJECT_PTR SETCAR;
extern OBJECT_PTR SETCDR;
extern OBJECT_PTR ERROR;
extern OBJECT_PTR CREATE_PACKAGE;
extern OBJECT_PTR IN_PACKAGE;
extern OBJECT_PTR COMMA;
extern OBJECT_PTR COMMA_AT;
extern OBJECT_PTR EXPAND_MACRO;

extern OBJECT_PTR STRING;
extern OBJECT_PTR MAKE_ARRAY;
extern OBJECT_PTR ARRAY_GET;
extern OBJECT_PTR ARRAY_SET;
extern OBJECT_PTR SUB_ARRAY;
extern OBJECT_PTR ARRAY_LENGTH;
extern OBJECT_PTR PRINT_STRING;
extern OBJECT_PTR LABELS;
extern OBJECT_PTR CREATE_IMAGE;
extern OBJECT_PTR BREAK;
extern OBJECT_PTR LOAD_FOREIGN_LIBRARY;
extern OBJECT_PTR CALL_FOREIGN_FUNCTION;
extern OBJECT_PTR ENV;
extern OBJECT_PTR EVAL;

extern OBJECT_PTR RESUME;

extern OBJECT_PTR BACKTRACE;
extern OBJECT_PTR LOAD_FILE;

/*symbols corresponding to assembler mnemonics */
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
/* end symbols corresponding to assembler mnemonics */

extern OBJECT_PTR INTEGR;
extern OBJECT_PTR FLOT;
extern OBJECT_PTR CHAR;
extern OBJECT_PTR VOID;
extern OBJECT_PTR INT_POINTER;
extern OBJECT_PTR FLOAT_POINTER;
extern OBJECT_PTR CHAR_POINTER;

extern OBJECT_PTR CONSP;
extern OBJECT_PTR INTEGERP;
extern OBJECT_PTR FLOATP;
extern OBJECT_PTR CHARACTERP;
extern OBJECT_PTR SYMBOLP;
extern OBJECT_PTR STRINGP;
extern OBJECT_PTR ARRAYP;
extern OBJECT_PTR CLOSUREP;
extern OBJECT_PTR MACROP;
extern OBJECT_PTR CONTINUATIONP;
extern OBJECT_PTR LAMBDA_EXPRESSION;
extern OBJECT_PTR WHILE;
extern OBJECT_PTR FORMAT;
extern OBJECT_PTR CLONE;
extern OBJECT_PTR COMPILE;
extern OBJECT_PTR RETURN_FROM;

extern OBJECT_PTR top_level_env;

extern struct node *white;

extern char **strings;

extern unsigned int current_package;
extern package_t *packages;

extern int nof_dl_handles;
extern void **dl_handles;

extern char err_buf[500];

extern BOOLEAN debug_mode;
OBJECT_PTR debug_continuation;
OBJECT_PTR debug_env;

extern OBJECT_PTR debug_execution_stack;

extern BOOLEAN in_error;

extern BOOLEAN yyin_popped;

extern FILE *yyin;

extern OBJECT_PTR continuations_map;

void eval()
{
  //print_object(car(reg_next_expression)); fprintf(stdout, "\n");
  //print_state();

  OBJECT_PTR exp = car(reg_next_expression);

  OBJECT_PTR opcode = car(exp);

  if(opcode == HALT)
  {
    reg_next_expression = NIL;
  }
  else if(opcode == REFER)
  {
    if(is_special_form(CADR(exp)))
      reg_accumulator = CADR(exp);
    else
    {
      OBJECT_PTR res = get_symbol_value(CADR(exp), debug_mode ? debug_env : reg_current_env);
      if(car(res) != NIL)
        reg_accumulator = cdr(res);
      else
      {
	char buf[SYMBOL_STRING_SIZE];
	print_symbol(CADR(exp), buf);
	sprintf(err_buf, "Symbol not bound(1): %s", buf);
        raise_error(err_buf);
        return;
      }
    }
    reg_next_expression = CADDR(exp);
  }
  else if(opcode == CONSTANT)
  {
    reg_accumulator = CADR(exp);
    reg_next_expression = CADDR(exp);
  }
  else if(opcode == CLOSE)
  {
    reg_accumulator = create_closure_object(reg_current_env, CADR(exp), CADDR(exp));
    reg_next_expression = CADDDR(exp);
  }
  else if(opcode == MACRO)
  {
    reg_accumulator = create_macro_object(reg_current_env, CADR(exp), CADDR(exp));
    reg_next_expression = CADDDR(exp);
  }
  else if(opcode == TEST)
  {
    if(reg_accumulator != NIL)
      reg_next_expression = CADR(exp);
    else
      reg_next_expression = CADDR(exp);
  }
  else if(opcode == WHILE)
  {
    OBJECT_PTR cond = CADR(exp);
    OBJECT_PTR body  = CADDR(exp);

    OBJECT_PTR ret = NIL;

    while(1)
    {
      reg_next_expression = cond;

      //print_object(create_current_continuation()); printf("\n");
      OBJECT_PTR temp = reg_current_stack;

      while(car(reg_next_expression) != NIL)
      {
        eval();
        if(in_error)
          return;
      }

      if(reg_accumulator == NIL)
        break;

      reg_next_expression = body;

      while(car(reg_next_expression) != NIL)
      {
        eval();
        if(in_error)
          return;
      }

      //to handle premature exits
      //via RETURN-FROM
      if(reg_current_stack != temp)
        return;

      ret = reg_accumulator;
    }

    reg_accumulator = ret;
    reg_next_expression = CADDDR(exp);
  }
  else if(opcode == ASSIGN)
  {
    if( update_environment(reg_current_env, CADR(exp), reg_accumulator) == NIL)
    {
      char buf[SYMBOL_STRING_SIZE];
      print_symbol(CADR(exp), buf);
      sprintf(err_buf, "Symbol not bound(2): %s", buf);
      raise_error(err_buf);
      return;
    }
    reg_next_expression = CADDR(exp);
  }
  else if(opcode == DEFINE)
  {
    add_to_top_level_environment(CADR(exp), reg_accumulator);
    reg_accumulator = CADR(exp);
    reg_next_expression = CADDR(exp);
  }
  else if(opcode == CONTI)
  {
    reg_accumulator = create_current_continuation();
    reg_current_value_rib = NIL;
    reg_next_expression = CADR(exp);
  }
  else if(opcode == NUATE) //this never gets called
  {
    reg_current_stack = CADR(exp);
    reg_accumulator = CADDR(exp);
    reg_current_value_rib = NIL;
    reg_next_expression =  cons(cons(RETURN, NIL), cdr(reg_next_expression));
  }
  else if(opcode == FRAME)
  {
    reg_current_stack = cons(create_call_frame(CADR(exp),
                                               reg_current_env,
                                               reg_current_value_rib),
                             reg_current_stack);

    reg_current_value_rib = NIL;
    reg_next_expression = CADDR(exp);
  }
  else if(opcode == ARGUMENT)
  {
    reg_current_value_rib = cons(reg_accumulator, reg_current_value_rib);
    reg_next_expression = CADR(exp);
  }
  else if(opcode == APPLY)
  {
    OBJECT_PTR operator = reg_accumulator;

    OBJECT_PTR res = get_symbol_from_value(reg_accumulator, reg_current_env);

    /* continuations_map = cons(cons(operator, create_current_continuation()), */
    /*                          continuations_map); */

    //primitive operators
    if(IS_SYMBOL_OBJECT(operator))
    {
      char val[SYMBOL_STRING_SIZE];
      print_symbol(operator, val);
        
      if(operator == CONS)
      {
        reg_accumulator = cons(car(reg_current_value_rib), CADR(reg_current_value_rib));
        reg_current_value_rib = NIL;
        reg_next_expression = cons(cons(RETURN, NIL), cdr(reg_next_expression));
      }
      else if(operator == EQ)
      {
        if(length(reg_current_value_rib) != 2)
        {
          raise_error("EQ expects two arguments");
          return;
        }

        OBJECT_PTR v1 = car(reg_current_value_rib);
        OBJECT_PTR v2 = CADR(reg_current_value_rib);

        reg_accumulator = equal(v1, v2) ? TRUE : NIL;
        reg_current_value_rib = NIL;
        reg_next_expression = cons(cons(RETURN, NIL), cdr(reg_next_expression));
      }
      else if(operator == ATOM)
      {
        if(length(reg_current_value_rib) != 1)
        {
          raise_error("ATOM expects one argument");
          return;
        }

        OBJECT_PTR v = car(reg_current_value_rib);

        reg_accumulator = is_atom(v) ? TRUE : NIL;
        reg_current_value_rib = NIL;
        reg_next_expression = cons(cons(RETURN, NIL), cdr(reg_next_expression));
      }
      else if(operator == CAR)
      {
        if(length(reg_current_value_rib) != 1)
        {
          raise_error("CAR expects one argument, a CONS object");
          return;
        }

        OBJECT_PTR car_obj = car(reg_current_value_rib);
        
        if(car_obj == NIL)
          reg_accumulator = NIL;
        else
        {
          if(!IS_CONS_OBJECT(car(reg_current_value_rib)))
          {
            raise_error("Argument to CAR should be a CONS object");
            return;
          }
          reg_accumulator = CAAR(reg_current_value_rib);
        }

        reg_current_value_rib = NIL;
        reg_next_expression = cons(cons(RETURN, NIL), cdr(reg_next_expression));
      }
      else if(operator == CDR)
      {
        if(length(reg_current_value_rib) != 1)
        {
          raise_error("CDR expects one argument, a CONS object");
          return;
        }

        OBJECT_PTR car_obj = car(reg_current_value_rib);
        
        if(car_obj == NIL)
          reg_accumulator = NIL;
        else
        {
          if(!IS_CONS_OBJECT(car(reg_current_value_rib)))
          {
            raise_error("Argument to CDR should be a CONS object");
            return;
          }

          reg_accumulator = CDAR(reg_current_value_rib);
        }

        reg_current_value_rib = NIL;
        reg_next_expression = cons(cons(RETURN, NIL), cdr(reg_next_expression));        
      }
      else if(operator == ADD)
      {

        if(length(reg_current_value_rib) < 2)
        {
          raise_error("Operator '+' requires at leaset two arguments");
          return;
        }

        float sum = 0;
        OBJECT_PTR rest = reg_current_value_rib;
        BOOLEAN is_float = false;

        while(rest != NIL)
	{
          OBJECT_PTR val = car(rest);
          if(IS_FLOAT_OBJECT(val))
	  {
            is_float = true;
            sum += get_float_value(val);
          }
          else
            sum += get_int_value(val);

          rest = cdr(rest);
        }

        if(is_float)
          reg_accumulator = convert_float_to_object(sum);
        else
          reg_accumulator = convert_int_to_object((int)sum);

          reg_current_value_rib = NIL;
          reg_next_expression = cons(cons(RETURN, NIL), cdr(reg_next_expression));        
      }
      else if(operator == SUB)
      {
        if(length(reg_current_value_rib) < 2)
        {
          raise_error("Operator '-' requires at leaset two arguments");
          return;
        }

        float val;
        BOOLEAN is_float = false;
        
        OBJECT_PTR first = car(reg_current_value_rib);

        if(IS_FLOAT_OBJECT(first))
	{
          is_float = true;
          val = get_float_value(first);
        }
        else
          val = get_int_value(first);

        OBJECT_PTR rest = cdr(reg_current_value_rib);

        float sum = 0;

        while(rest != NIL)
	{
          OBJECT_PTR val = car(rest);

          if(IS_FLOAT_OBJECT(val))
	  {
            is_float = true;
            sum += get_float_value(val);
          }
          else
            sum += get_int_value(val);

          rest = cdr(rest);
        }

        if(is_float)
          reg_accumulator = convert_float_to_object(val - sum);
        else
          reg_accumulator = convert_int_to_object((int)(val - sum));

        reg_current_value_rib = NIL;
        reg_next_expression = cons(cons(RETURN, NIL), cdr(reg_next_expression));        
      }
      else if(operator == MULT)
      {

        if(length(reg_current_value_rib) < 2)
        {
          raise_error("Operator '*' requires at leaset two arguments");
          return;
        }

        float sum = 1;
        OBJECT_PTR rest = reg_current_value_rib;
        BOOLEAN is_float = false;

        while(rest != NIL)
	{
          OBJECT_PTR val = car(rest);

          if(IS_FLOAT_OBJECT(val))
	  {
            is_float = true;
            sum *= get_float_value(val);
          }
          else
            sum *= get_int_value(val);

          rest = cdr(rest);
        }

        if(is_float)
          reg_accumulator = convert_float_to_object(sum);
        else
          reg_accumulator = convert_int_to_object((int)sum);

        reg_current_value_rib = NIL;
        reg_next_expression = cons(cons(RETURN, NIL), cdr(reg_next_expression));        
      }
      else if(operator == DIV)
      {

        if(length(reg_current_value_rib) < 2)
        {
          raise_error("Operator '/' requires at leaset two arguments");
          return;
        }

        float val;
        BOOLEAN is_float = false;

        OBJECT_PTR first = car(reg_current_value_rib);

        if(IS_FLOAT_OBJECT(first))
	{
          is_float = true;
          val = get_float_value(first);
        }
        else
          val = get_int_value(first);

        OBJECT_PTR rest = cdr(reg_current_value_rib);

        float sum = 1;

        while(rest != NIL)
	{
          OBJECT_PTR val = car(rest);

          if(IS_FLOAT_OBJECT(val))
	  {
            is_float = true;
            sum *= get_float_value(val);
          }
          else
            sum *= get_int_value(val);

	    rest = cdr(rest);
        }

        if(sum == 0)
        {
          raise_error("Division by zero");
          return;
        }

        if(is_float)
          reg_accumulator = convert_float_to_object(val / sum);
        else
          reg_accumulator = convert_int_to_object((int)(val / sum));

        reg_current_value_rib = NIL;
        reg_next_expression = cons(cons(RETURN, NIL), cdr(reg_next_expression));        
      }
      else if(operator == ERROR)
      {
        OBJECT_PTR error_string_obj = car(reg_current_value_rib);

        if(IS_STRING_LITERAL_OBJECT(error_string_obj))
          raise_error(strdup(strings[error_string_obj >> OBJECT_SHIFT]));
        else if(is_string_object(error_string_obj))
        {
          char msg[500];

          memset(msg, 500, '\0');

          RAW_PTR ptr = error_string_obj >> OBJECT_SHIFT;

          int len = get_int_value(get_heap(ptr));

          int i;

          for(i=1; i <= len; i++)
            sprintf(msg, "%c", get_heap(ptr + i) >> OBJECT_SHIFT);

          raise_error(msg);
        }
      }
      else if(operator == PRINT)
      {
        print_object(car(reg_current_value_rib));
        fprintf(stdout, "\n");
        reg_accumulator = car(reg_current_value_rib);
        reg_current_value_rib = NIL;
        reg_next_expression = cons(cons(RETURN, NIL), cdr(reg_next_expression));
      }
      else if(operator == LST)
      {
        OBJECT_PTR rest = reg_current_value_rib;

        reg_accumulator = NIL;

        while(rest != NIL)
        {
          if(reg_accumulator == NIL)
            reg_accumulator = cons(car(rest), NIL);
          else
            set_heap((last_cell(reg_accumulator) >> OBJECT_SHIFT) + 1, 
                     cons(car(rest), NIL));         

          rest = cdr(rest);
        }

        reg_current_value_rib = NIL;
        reg_next_expression = cons(cons(RETURN, NIL), cdr(reg_next_expression));

      }
      else if(operator == BACKQUOTE)
      {
        reg_accumulator = eval_backquote(car(reg_current_value_rib));

        reg_current_value_rib = NIL;
        reg_next_expression = cons(cons(RETURN, NIL), cdr(reg_next_expression));
      }
      else if(operator == LISTP) //can actually do this with a macro (not (atom) ...)
      {
        if(length(reg_current_value_rib) != 1)
        {
          raise_error("LISTP requires exactly one argument");
          return;
        }

        reg_accumulator = is_atom(car(reg_current_value_rib)) ? NIL : TRUE;

        reg_current_value_rib = NIL;
        reg_next_expression = cons(cons(RETURN, NIL), cdr(reg_next_expression));
      }
      else if(operator == SYMBOL_VALUE)
      {

        if(length(reg_current_value_rib) != 1)
        {
          raise_error("SYMBOL-VALUE requires exactly one argument");
          return;
        }

        OBJECT_PTR sym = car(reg_current_value_rib);

        if(!IS_SYMBOL_OBJECT(sym))
        {
          raise_error("Argument to SYMBOL-VALUE should be a symbol object");
          return;
        }

        OBJECT_PTR res = get_symbol_value(sym, reg_current_env);

        if(car(res) == NIL)
        {
          raise_error("Symbol not bound");
          return;
        }

        reg_accumulator = cdr(res);

        reg_current_value_rib = NIL;
        reg_next_expression = cons(cons(RETURN, NIL), cdr(reg_next_expression));
      }
      else if(operator == GT)
      {
        if(length(reg_current_value_rib) != 2)
        {
          raise_error("> requires exactly two arguments");
          return;
        }

        OBJECT_PTR v1 = car(reg_current_value_rib);
        OBJECT_PTR v2 = CADR(reg_current_value_rib);

        if((!(IS_INTEGER_OBJECT(v1)) && !(IS_FLOAT_OBJECT(v1))) ||
           (!(IS_INTEGER_OBJECT(v2)) && !(IS_FLOAT_OBJECT(v2))))
        {
          raise_error("Arguments to > should be numbers (integer or float)");
          return;
        }

        float val1, val2;
	  
        if(IS_FLOAT_OBJECT(v1))
          val1 = get_float_value(v1);
        else
          val1 = get_int_value(v1);

        if(IS_FLOAT_OBJECT(v2))
          val2 = get_float_value(v2);
        else
          val2 = get_int_value(v2);
	    
        reg_accumulator = (val1 > val2) ? TRUE : NIL;        

        reg_current_value_rib = NIL;
        reg_next_expression = cons(cons(RETURN, NIL), cdr(reg_next_expression));
      }
      else if(operator == GENSYM)
      {
        if(reg_current_value_rib != NIL)
        {
          raise_error("GEMSYM requires no argument");
          return;
        }

        reg_accumulator = gensym();
        reg_current_value_rib = NIL;
        reg_next_expression = cons(cons(RETURN, NIL), cdr(reg_next_expression));
      }
      else if(operator == SETCAR)
      {
        if(length(reg_current_value_rib) != 2)
        {
          raise_error("SETCAR requires two arguments");
          return;
        }

        OBJECT_PTR car_obj = car(reg_current_value_rib);

        if((!(IS_CONS_OBJECT(car_obj))))
        {
          raise_error("First argument to SETCAR should be a CONS object");
          return;
        }

        set_heap((car_obj >> OBJECT_SHIFT), CADR(reg_current_value_rib));

        reg_accumulator = CADR(reg_current_value_rib);

        reg_current_value_rib = NIL;
        reg_next_expression = cons(cons(RETURN, NIL), cdr(reg_next_expression));
      }
      else if(operator == SETCDR)
      {
        if(length(reg_current_value_rib) != 2)
        {
          raise_error("SETCDR requires two arguments");
          return;
        }

        OBJECT_PTR car_obj = car(reg_current_value_rib);

        if((!(IS_CONS_OBJECT(car_obj))))
        {
          raise_error("First argument to SETCDR should be a CONS object");
          return;
        }

        set_heap((car_obj >> OBJECT_SHIFT) + 1, CADR(reg_current_value_rib));

        reg_accumulator = CADR(reg_current_value_rib);

        reg_current_value_rib = NIL;
        reg_next_expression = cons(cons(RETURN, NIL), cdr(reg_next_expression));
      }
      else if(operator == CREATE_PACKAGE)
      {
        if(length(reg_current_value_rib) != 1)
        {
          raise_error("CREATE-PACKAGE requires exactly one argument");
          return;
        }

        OBJECT_PTR package = car(reg_current_value_rib);

        char *package_name = (char *)convert_to_upper_case(strings[package >> OBJECT_SHIFT]);

        if(!strcmp(package_name,"CORE"))
	{
          raise_error("Core package already exists");
          return;
        }

        create_package(package_name);

        reg_accumulator = package;

        reg_current_value_rib = NIL;
        reg_next_expression = cons(cons(RETURN, NIL), cdr(reg_next_expression));
      }
      else if(operator == IN_PACKAGE)
      {
        if(length(reg_current_value_rib) != 1)
        {
          raise_error("IN-PACKAGE requires exactly one argument");
          return;
        }

        OBJECT_PTR package = car(reg_current_value_rib);
        char *package_name = (char *)convert_to_upper_case(strings[package >> OBJECT_SHIFT]);

        if(!strcmp(package_name,"CORE"))
	{
          raise_error("Core package cannot be updated");
          return;
        }
        else
	{
          int index = find_package(package_name);
          if(index == NOT_FOUND)
	  {
            raise_error("Package does not exist");
            return;
          }
          else
          {
            current_package = index;
            reg_accumulator = NIL;
          }
        }

        reg_current_value_rib = NIL;
        reg_next_expression = cons(cons(RETURN, NIL), cdr(reg_next_expression));
      }
      else if(operator == EXPAND_MACRO)
      {
        if(length(reg_current_value_rib) != 1)
        {
          raise_error("EXPAND-MACRO requires exactly one argument");
          return;
        }

        OBJECT_PTR macro_body = car(reg_current_value_rib);
        OBJECT_PTR res = get_symbol_value(car(macro_body), reg_current_env);

        if(car(res) == NIL)
	{
          raise_error("Macro undefined");
          return;
	}
        else
        {
          OBJECT_PTR obj = cdr(res);
          OBJECT_PTR args = cdr(macro_body);

          reg_next_expression = cons(cons(FRAME,
                                          cons(cons(HALT, NIL),
                                               cons(cons(APPLY, NIL),
                                                    NIL))),
                                     car(macro_body));

          eval();
          if(in_error)
            return;

          reg_current_value_rib = NIL;

          //build the value rib
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
          reg_accumulator = obj;
          //reg_next_expression = cons(APPLY, NIL);
          
          //evaluate the macro invocation
          while(car(reg_next_expression) != NIL)
          {
            eval();
            if(in_error)
              return;
          }
        }        

        reg_current_value_rib = NIL;
        reg_next_expression = cons(cons(RETURN, NIL), cdr(reg_next_expression));
      }
      else if(operator == APPLY)
      {
        if(length(reg_current_value_rib) != 2)
        {
          raise_error("APPLY requires exactly two arguments");
          return;
        }        

        OBJECT_PTR obj = car(reg_current_value_rib);

        if((!(IS_SYMBOL_OBJECT(obj))) &&
           (!(IS_CLOSURE_OBJECT(obj)))     &&
           (!(IS_CONTINUATION_OBJECT(obj))))
        {
          raise_error("First argument to APPLY should be a special form, a closure or a continuation");
          return;
        }

        OBJECT_PTR args = CADR(reg_current_value_rib);
        if(args != NIL && (!(IS_CONS_OBJECT(args))))
        {
          raise_error("Second argument to APPLY should be a list of arguments");
          return;
        }

        reg_accumulator = obj;
        reg_current_value_rib = args;

        reg_next_expression = cons(cons(APPLY, NIL), cdr(reg_next_expression));
      }
      else if(operator == STRING)
      {
        if(length(reg_current_value_rib) != 1)
        {
          raise_error("STRING requires exactly one argument, a literal string");
          return;
        }        

        OBJECT_PTR string_literal = car(reg_current_value_rib);

        if((!(IS_STRING_LITERAL_OBJECT(string_literal))))
        {
          raise_error("Argument to STRING should be a literal string");
          return;
        }        
 
        reg_accumulator = eval_string(string_literal);

        reg_current_value_rib = NIL;
        reg_next_expression = cons(cons(RETURN, NIL), cdr(reg_next_expression));
      }
      else if(operator == MAKE_ARRAY)
      {
        if(length(reg_current_value_rib) != 2)
        {
          raise_error("MAKE-ARRAY requires exactly two arguments");
          return;
        }        
        
        OBJECT_PTR size = car(reg_current_value_rib);

        if((!(IS_INTEGER_OBJECT(size))))
        {
          raise_error("First argument to MAKE-ARRAY should be the size of the array (integer)");
          return;
        }        

        reg_accumulator = eval_make_array(size, CADR(reg_current_value_rib));

        reg_current_value_rib = NIL;
        reg_next_expression = cons(cons(RETURN, NIL), cdr(reg_next_expression));
      }
      else if(operator == ARRAY_SET)
      {
        if(length(reg_current_value_rib) != 3)
        {
          raise_error("ARRAY-SET requires exactly three arguments");
          return;
        }        

        OBJECT_PTR array_obj = car(reg_current_value_rib);

        if((!(IS_ARRAY_OBJECT(array_obj))))
        {
          raise_error("First argument to ARRAY-SET should be an array");
          return;
        }        

        OBJECT_PTR idx = CADR(reg_current_value_rib);

        if((!(IS_INTEGER_OBJECT(idx))))
        {
          raise_error("Second argument to ARRAY-SET should be an integer (index into the array)");
          return;
        }        

        int array_len = get_int_value(get_heap(array_obj >> OBJECT_SHIFT));

        int index = get_int_value(idx);

        if(index < 0 || (index >= array_len))
        {
          raise_error("Array index out of bounds");
          return;
        }        
 
        set_heap((array_obj >> OBJECT_SHIFT) + index + 1, CADDR(reg_current_value_rib));

        reg_accumulator = CADDR(reg_current_value_rib);

        reg_current_value_rib = NIL;
        reg_next_expression = cons(cons(RETURN, NIL), cdr(reg_next_expression));
      }
      else if(operator == ARRAY_GET)
      {
        if(length(reg_current_value_rib) != 2)
        {
          raise_error("ARRAY-GET requires exactly two arguments");
          return;
        }        

        OBJECT_PTR array_obj = car(reg_current_value_rib);

        if((!(IS_ARRAY_OBJECT(array_obj))))
        {
          raise_error("First argument to ARRAY-GET should be an array");
          return;
        }        

        OBJECT_PTR idx = CADR(reg_current_value_rib);

        if((!(IS_INTEGER_OBJECT(idx))))
        {
          raise_error("Second argument to ARRAY-GET should be an integer (index into the array)");
          return;
        }        

        int array_len = get_int_value(get_heap(array_obj >> OBJECT_SHIFT));

        int index = get_int_value(idx);

        if(index < 0 || (index >= array_len))
        {
          raise_error("Array index out of bounds");
          return;
        }        

        reg_accumulator = get_heap((array_obj >> OBJECT_SHIFT) + index + 1);

        reg_current_value_rib = NIL;
        reg_next_expression = cons(cons(RETURN, NIL), cdr(reg_next_expression));
      }
      else if(operator == SUB_ARRAY)
      {
        if(length(reg_current_value_rib) != 3)
        {
          raise_error("SUB-ARRAY requires exactly three arguments");
          return;
        }        

        OBJECT_PTR array = car(reg_current_value_rib);

        if(!(IS_ARRAY_OBJECT(array)))
        {
          raise_error("First argument to SUB-ARRAY should be an ARRAY object");
          return;
        }

        OBJECT_PTR start = CADR(reg_current_value_rib);

        if(!(IS_INTEGER_OBJECT(start)))
        {
          raise_error("Second argument to SUB-ARRAY should be an integer (start index)");
          return;
        }

        if(!(get_int_value(start) >= 0))
        {
          raise_error("Second argument to SUB-ARRAY should be a non-negative integer");
          return;
        }

        OBJECT_PTR array_length = CADDR(reg_current_value_rib);

        if(!(IS_INTEGER_OBJECT(array_length)))
        {
          raise_error("Third argument to SUB-ARRAY should be an integer (length of the sub-array)");
          return;
        }

        if(!(get_int_value(array_length) >= 0))
        {
          raise_error("Third argument to SUB-ARRAY should be a non-negative integer");
          return;
        }

        if((get_int_value(start) + get_int_value(array_length)) > get_int_value(get_heap(array >> OBJECT_SHIFT)))
        {
          raise_error("Range (start, length) for SUB-ARRAY out of bounds of the array");
          return;
        }

        reg_accumulator = eval_sub_array(array, start, array_length);

        reg_current_value_rib = NIL;
        reg_next_expression = cons(cons(RETURN, NIL), cdr(reg_next_expression));
      }
      else if(operator == ARRAY_LENGTH)
      {
        if(length(reg_current_value_rib) != 1)
        {
          raise_error("ARRAY-LENGTH requires exactly one argument, an array object");
          return;
        }        

        OBJECT_PTR array = car(reg_current_value_rib);

        if(array == NIL)
        {
          reg_accumulator = convert_int_to_object(0);
          reg_current_value_rib = NIL;
          reg_next_expression = cons(cons(RETURN, NIL), cdr(reg_next_expression));
          return;
        }

        if(!(IS_ARRAY_OBJECT(array)))
        {
          raise_error("Argument to ARRAY-LENGTH should be an ARRAY object");
          return;
        }

        reg_accumulator = get_heap(array >> OBJECT_SHIFT);

        reg_current_value_rib = NIL;
        reg_next_expression = cons(cons(RETURN, NIL), cdr(reg_next_expression));
      }
      else if(operator == PRINT_STRING)
      {
        if(length(reg_current_value_rib) != 1)
        {
          raise_error("PRINT-STRING requires exactly one argument, a string object");
          return;
        }        

        OBJECT_PTR str = car(reg_current_value_rib);

        if(!(is_string_object(str)) && (!(IS_STRING_LITERAL_OBJECT(str))))
        {
          raise_error("Argument to PRINT_STRING should be a string object");
          return;
        }

        print_object(str);
        fprintf(stdout, "\n");

        reg_accumulator = NIL;

        reg_current_value_rib = NIL;
        reg_next_expression = cons(cons(RETURN, NIL), cdr(reg_next_expression));
      }
      else if(operator == CREATE_IMAGE)
      {
        if(length(reg_current_value_rib) != 1)
        {
          raise_error("CREATE-IMAGE requires exactly one argument, a string object denoting the file name of the image");
          return;
        }        

        OBJECT_PTR file_name = car(reg_current_value_rib);

        if(!(is_string_object(file_name)) && (!(IS_STRING_LITERAL_OBJECT(file_name))))
        {
          raise_error("Argument to PRINT_STRING should be a string object denoting the file name of the image");
          return;
        }

        create_image(strings[file_name >> OBJECT_SHIFT]);

        reg_accumulator = NIL;

        reg_current_value_rib = NIL;
        reg_next_expression = cons(cons(RETURN, NIL), cdr(reg_next_expression));
      }
      else if(operator == LOAD_FOREIGN_LIBRARY)
      {
        if(length(reg_current_value_rib) != 1)
        {
          raise_error("LOAD_FOREIGN_LIBRARY requires exactly one argument, a string object denoting the library name");
          return;
        }        

        OBJECT_PTR file_name = car(reg_current_value_rib);

        if(!(is_string_object(file_name)) && (!(IS_STRING_LITERAL_OBJECT(file_name))))
        {
          raise_error("Argument to LOAD_FOREIGN_LIBRARY should be a string object denoting the library name");
          return;
        }

        nof_dl_handles++;
  
        void **temp = (void **)realloc(dl_handles, nof_dl_handles * sizeof(void *));

        if(temp == NULL)
        {
          raise_error("Unable to extend memory for dl_handles");
          return;
        }

        dl_handles = temp;

        void *ret = dlopen(strings[file_name >> OBJECT_SHIFT], RTLD_LAZY);

        if(!ret)
        {
          raise_error("dl_open() failed");
          return;
        }

        dl_handles[nof_dl_handles - 1] = ret;

        reg_accumulator = NIL;

        reg_current_value_rib = NIL;
        reg_next_expression = cons(cons(RETURN, NIL), cdr(reg_next_expression));
      }
      else if(operator == CALL_FOREIGN_FUNCTION)
      {
        if(length(reg_current_value_rib) != 3)
        {
          raise_error("CALL-FOREIGN-FUNCTION requires exactly three arguments");
          return;
        }        

        OBJECT_PTR fn_name = car(reg_current_value_rib);

        if(!IS_STRING_LITERAL_OBJECT(fn_name) && !is_string_object(fn_name))
        {
          raise_error("First argument to CALL-FOREIGN-FUNCTION should be the funtion name (string)");
          return;
        }

        OBJECT_PTR ret_type = CADR(reg_current_value_rib);

        if(!(ret_type == INTEGR        ||
             ret_type == FLOT          ||
             ret_type == CHAR          ||
             ret_type == VOID          ||
             /* ret_type == INT_POINTER   || */ //not handling int and float pointer return values
             /* ret_type == FLOAT_POINTER || */
             ret_type == CHAR_POINTER))
        {
          raise_error("Second parameter to CALL-FOREIGN-FUNCTION should be a valid return type");
          return;
        }

        OBJECT_PTR args = CADDR(reg_current_value_rib);

        if(args != NIL && !IS_CONS_OBJECT(args))
        {
          raise_error("Arguments should be a list of two-element lists (argument, type) -- 1");
          return;
        }

        OBJECT_PTR rest_args = args;

        while(rest_args != NIL)
        {
          OBJECT_PTR car_rest_args = car(rest_args);

          if(!IS_CONS_OBJECT(car_rest_args))
          {
            raise_error("Arguments should be a list of two-element lists (argument, type) -- 2");
            return;
          }

          if(length(car_rest_args) != 2)
          {
            raise_error("Arguments should be a list of two-element lists (argument, type) -- 3");
            return;
          }

          OBJECT_PTR val = CAAR(rest_args);

          if(!(IS_INTEGER_OBJECT(val)        ||
               IS_FLOAT_OBJECT(val)          ||
               IS_CHAR_OBJECT(val)           ||
               IS_STRING_LITERAL_OBJECT(val) ||
               is_string_object(val)         ||
               IS_SYMBOL_OBJECT(val)))
          {
            raise_error("Parameter should be integer-, float-, charcter-, or string constant, or a symbol");
            return;
          }

          if(IS_SYMBOL_OBJECT(val))
          {
            OBJECT_PTR res = get_symbol_value(val, reg_current_env);
            if(car(res) == NIL)
            {
              char buf[SYMBOL_STRING_SIZE];
              print_symbol(val, buf);
              sprintf(err_buf, "Symbol not bound(3): %s", buf);
              raise_error(err_buf);
              return;
            }
            val = cdr(res);
          }

          OBJECT_PTR type = CADAR(rest_args);

          if(type == INTEGR)
          {
            if(!IS_INTEGER_OBJECT(val))
            {
              raise_error("Argument type mismatch: integer expected");
              return;
            }
          }          
          else if(type == FLOT)
          {
            if(!IS_FLOAT_OBJECT(val))
            {
              raise_error("Argument type mismatch: float expected");
              return;
            }
          }
          else if(type == CHAR)
          {
            if(!IS_CHAR_OBJECT(val))
            {
              raise_error("Argument type mismatch: character expected");
              return;
            }
          }
          else if(type == CHAR_POINTER)
          {
            if(!IS_STRING_LITERAL_OBJECT(val) && !is_string_object(val))
            {
              raise_error("Argument type mismatch: string object/literal expected");
              return;
            }
          }
          else if(type == INT_POINTER)
          {
            if(!IS_SYMBOL_OBJECT(CAAR(rest_args)) || !IS_INTEGER_OBJECT(val))
            {
              raise_error("Mapping a non-variable to INTEGER-POINTER / Argument type mismatch");
              return;
            }
          }
          else if(type == FLOAT_POINTER)
          {
            if(!IS_SYMBOL_OBJECT(CAAR(rest_args)) || !IS_FLOAT_OBJECT(val))
            {
              raise_error("Mapping a non-variable to FLOAT-POINTER / Argument type mismatch");
              return;
            }
          }
          else
          {
            raise_error("call_foreign_function(): non-primitive object type not handled");
            return;
          }

          rest_args = cdr(rest_args);
        }

        reg_accumulator = call_foreign_function(fn_name, ret_type, args);

        reg_current_value_rib = NIL;
        reg_next_expression = cons(cons(RETURN, NIL), cdr(reg_next_expression));
      }
      else if(operator == ENV)
      {
        reg_accumulator = cons(top_level_env, debug_mode ? debug_env : reg_current_env);

        reg_current_value_rib = NIL;
        reg_next_expression = cons(cons(RETURN, NIL), cdr(reg_next_expression));
      }
      else if(operator == EVAL)
      {

        OBJECT_PTR temp = compile(car(reg_current_value_rib), NIL);

        if(temp == ERROR)
        {
          raise_error("EVAL: Compilation failed");
          return;
        }

        reg_next_expression = cons(cons(FRAME, cons(cons(cons(HALT, NIL), car(reg_current_value_rib)),
                                                    cons(cons(temp, NIL), car(reg_current_value_rib)))),
                                   car(reg_current_value_rib));

        while(car(reg_next_expression) != NIL)
        {
          eval();
          if(in_error)
          {
            raise_error("EVAL failed");
            return;
          }
        }

        reg_current_value_rib = NIL;
        reg_next_expression = cons(cons(RETURN, NIL), cdr(reg_next_expression));
      }
      else if(operator == BREAK)
      {
        debug_mode = true;
        debug_continuation = create_current_continuation();
        debug_env = reg_current_env;
        reg_next_expression = NIL;

        debug_execution_stack = reg_current_stack;

      }
      else if(operator == RESUME)
      {
        debug_mode = false;

        reg_current_stack = ((debug_continuation >> OBJECT_SHIFT) << OBJECT_SHIFT) + CONS_TAG;

        reg_current_value_rib = NIL;
        reg_next_expression = cons(cons(RETURN, NIL), cdr(reg_next_expression));

        debug_execution_stack = NIL;

        return;
      }
      else if(operator == BACKTRACE)
      {
        print_backtrace();
        reg_next_expression = cons(cons(RETURN, NIL), cdr(reg_next_expression));
        return;
      }
      else if(operator == LOAD_FILE)
      {
        if(length(reg_current_value_rib) != 1)
        {
          raise_error("LOAD-FILE requires exactly one argument");
          return;
        }        

        OBJECT_PTR arg = car(reg_current_value_rib);

        if(!is_string_object(arg) && (!IS_STRING_LITERAL_OBJECT(arg)))
        {
          raise_error("Argument to LOAD-FILE should be a string");
          return;
        }

        FILE *temp = fopen(is_string_object(arg) ?  get_string(arg) : strings[arg >> OBJECT_SHIFT], "r");

        if(!temp)
        {
          raise_error("LOAD-FILE unable to open file");
          return;
        }

        if(set_up_new_yyin(temp))
        {
          raise_error("Unable to read from file");
          return;
        }

        int ret;

        while(!yyparse())
        {
          repl();
        }
        pop_yyin();

        reg_accumulator = NIL;

        reg_current_value_rib = NIL;
      }
      else if(operator == CONSP)
      {
        reg_accumulator = IS_CONS_OBJECT(car(reg_current_value_rib)) ? TRUE : NIL;
        reg_current_value_rib = NIL;
        reg_next_expression = cons(cons(RETURN, NIL), cdr(reg_next_expression));
      }
      else if(operator ==  INTEGERP)
      {
        reg_accumulator = IS_INTEGER_OBJECT(car(reg_current_value_rib)) ? TRUE : NIL;
        reg_current_value_rib = NIL;
        reg_next_expression = cons(cons(RETURN, NIL), cdr(reg_next_expression));
      }
      else if(operator ==  FLOATP)
      {
        reg_accumulator = IS_FLOAT_OBJECT(car(reg_current_value_rib)) ? TRUE : NIL;
        reg_current_value_rib = NIL;
        reg_next_expression = cons(cons(RETURN, NIL), cdr(reg_next_expression));
      }
      else if(operator ==  CHARACTERP)
      {
        reg_accumulator = IS_CHAR_OBJECT(car(reg_current_value_rib)) ? TRUE : NIL;
        reg_current_value_rib = NIL;
        reg_next_expression = cons(cons(RETURN, NIL), cdr(reg_next_expression));
      }
      else if(operator ==  SYMBOLP)
      {
        reg_accumulator = IS_SYMBOL_OBJECT(car(reg_current_value_rib)) ? TRUE : NIL;
        reg_current_value_rib = NIL;
        reg_next_expression = cons(cons(RETURN, NIL), cdr(reg_next_expression));
      }
      else if(operator ==  STRINGP)
      {
        reg_accumulator = (IS_STRING_LITERAL_OBJECT(car(reg_current_value_rib)) || is_string_object(car(reg_current_value_rib))) ? TRUE : NIL;
        reg_current_value_rib = NIL;
        reg_next_expression = cons(cons(RETURN, NIL), cdr(reg_next_expression));
      }
      else if(operator ==  ARRAYP)
      {
        reg_accumulator = IS_ARRAY_OBJECT(car(reg_current_value_rib)) ? TRUE : NIL;
        reg_current_value_rib = NIL;
        reg_next_expression = cons(cons(RETURN, NIL), cdr(reg_next_expression));
      }
      else if(operator ==  CLOSUREP)
      {
        reg_accumulator = IS_CLOSURE_OBJECT(car(reg_current_value_rib)) ? TRUE : NIL;
        reg_current_value_rib = NIL;
        reg_next_expression = cons(cons(RETURN, NIL), cdr(reg_next_expression));
      }
      else if(operator ==  MACROP)
      {
        reg_accumulator = IS_MACRO_OBJECT(car(reg_current_value_rib)) ? TRUE : NIL;
        reg_current_value_rib = NIL;
        reg_next_expression = cons(cons(RETURN, NIL), cdr(reg_next_expression));
      }
      else if(operator ==  CONTINUATIONP)
      {
        reg_accumulator = IS_CONTINUATION_OBJECT(car(reg_current_value_rib)) ? TRUE : NIL;
        reg_current_value_rib = NIL;
        reg_next_expression = cons(cons(RETURN, NIL), cdr(reg_next_expression));
      }
      else if(operator == LAMBDA_EXPRESSION)
      {
        if(length(reg_current_value_rib) != 1)
        {
          raise_error("LAMBDA-EXPRESSION requires exactly one argument, a closure or macro object");
          return;
        }        

        OBJECT_PTR obj = car(reg_current_value_rib);        

        if(!IS_CLOSURE_OBJECT(obj) && !IS_MACRO_OBJECT(obj))
        {
          raise_error("Argument to LAMBDA-EXPRESSION should be a closure or macro object");
          return;
        }

        reg_accumulator = cons(get_params_object(obj),
                               get_body_object(obj));
        reg_next_expression = cons(cons(RETURN, NIL), cdr(reg_next_expression));
      }
      else if(operator == FORMAT)
      {
        if(length(reg_current_value_rib) < 1)
        {
          raise_error("FORMAT requires at least one argument, a format specification string");
          return;
        }

        if(!(IS_STRING_LITERAL_OBJECT(car(reg_current_value_rib))))
        {
          raise_error("First parameter to FORMAT must be a format specification string");
          return;
        }
        
        OBJECT_PTR rest = reg_current_value_rib;

        while(rest != NIL)
        {
          OBJECT_PTR val = car(rest);

          if(!(IS_INTEGER_OBJECT(val)        ||
               IS_FLOAT_OBJECT(val)          ||
               IS_CHAR_OBJECT(val)           ||
               IS_STRING_LITERAL_OBJECT(val) ||
               is_string_object(val)))
          {
            raise_error("Parameters to FORMAT should be integers, floats, characters or strings");
            return;
          }

          rest = cdr(rest);
        }

        if(format(reg_current_value_rib) == -1)
        {
          //error message would have been set in format()
          return;
        }

        reg_accumulator = NIL;
        reg_next_expression = cons(cons(RETURN, NIL), cdr(reg_next_expression));
      }
      else if(operator == CLONE)
      {
        if(length(reg_current_value_rib) != 1)
        {
          raise_error("CLONE takes exactly one parameter, the object to be cloned");
          return;
        }

        reg_accumulator  = clone_object(car(reg_current_value_rib));
        reg_current_value_rib = NIL;
        reg_next_expression = cons(cons(RETURN, NIL), cdr(reg_next_expression));
      }
      else if(operator == RETURN_FROM)
      {
        if(length(reg_current_value_rib) != 2)
        {
          raise_error("RETURN-FROM requires two parameters: the closure/macro from which to return, and the value to be returned");
          return;
        }

        OBJECT_PTR cont = get_continuation_for_return(car(reg_current_value_rib));

        if(cont == NIL)
        {
          raise_error("RETURN-FROM passed non-existent closure/macro object");
          return;
        }

        reg_current_stack = ((cont >> OBJECT_SHIFT) << OBJECT_SHIFT) + CONS_TAG;

        reg_accumulator = CADR(reg_current_value_rib);
        reg_current_value_rib = NIL;
        reg_next_expression = cons(cons(RETURN, NIL), cdr(reg_next_expression));
      }
      else if(operator == COMPILE)
      {
        if(length(reg_current_value_rib) != 2)
        {
          raise_error("COMPILE needs two arguments, an expression to be compiled and a 'next' expression");
          return;
        }

        OBJECT_PTR temp = compile(car(reg_current_value_rib), CADR(reg_current_value_rib));

        if(temp == ERROR)
        {
          raise_error("COMPILE failed");
          return;
        }

        reg_accumulator = car(temp);
        reg_current_value_rib = NIL;
        reg_next_expression = cons(cons(RETURN, NIL), cdr(reg_next_expression));
      }
      else
      {
	char buf[SYMBOL_STRING_SIZE];
	print_symbol(operator, buf);
	sprintf(err_buf, "Symbol not bound(4): %s", buf);
        raise_error(err_buf);
        return;
      }
    }
    else //user-defined operator (closure, macro, or continuation)
    {
      if(IS_CLOSURE_OBJECT(reg_accumulator) || IS_MACRO_OBJECT(reg_accumulator))
      {
        continuations_map = cons(cons(reg_accumulator, create_current_continuation()),
                                 continuations_map);

        OBJECT_PTR params = get_params_object(reg_accumulator);

        //no longer valid because of &rest -- parameters may
        //not match the value rib
        /* if(length(params) != length(reg_current_value_rib)) */
        /* { */
        /*   raise_error("Arguments to function not supplied\n"); */
        /*   return; */
        /* } */

        OBJECT_PTR params_env = NIL;

        OBJECT_PTR rest_params = params;
        OBJECT_PTR rest_args = reg_current_value_rib;

        while(rest_params != NIL)
        {
          int package_index = car(rest_params) >> (SYMBOL_BITS + OBJECT_SHIFT);
          int symbol_index =  (car(rest_params) >> OBJECT_SHIFT) & TWO_RAISED_TO_SYMBOL_BITS_MINUS_1;

          char *param_name = packages[package_index].symbols[symbol_index];

          if(!strcmp(param_name, "&REST"))
          {
            if(params_env == NIL)
              params_env = cons(cons(CADR(rest_params), rest_args), NIL);
            else
              set_heap((last_cell(params_env) >> OBJECT_SHIFT) + 1, 
                       cons(cons(CADR(rest_params),rest_args), NIL));            
            break;
          }
          else
          {
            if(params_env == NIL)
              params_env = cons(cons(car(rest_params), car(rest_args)), NIL);
            else
              set_heap((last_cell(params_env) >> OBJECT_SHIFT) + 1, 
                       cons(cons(car(rest_params),car(rest_args)), NIL));         
          }

          rest_params = cdr(rest_params);
          rest_args = cdr(rest_args);
        }

        reg_current_env = cons(params_env, get_env_list(reg_accumulator));

        reg_current_value_rib = NIL;
        reg_next_expression = get_body_object(reg_accumulator); 
      }
      else if(IS_CONTINUATION_OBJECT(reg_accumulator))
      {
        if(length(reg_current_value_rib) != 1)
        {
          raise_error("Continuations take exactly one argument");
          return;
        }

        reg_current_stack = ((reg_accumulator >> OBJECT_SHIFT) << OBJECT_SHIFT) + CONS_TAG;

        reg_accumulator = car(reg_current_value_rib);
        reg_current_value_rib = NIL;
        reg_next_expression = cons(cons(RETURN, NIL), cdr(reg_next_expression));
      }
      else
      {
        raise_error("Illegal operator");
        return;
      }
    }
  }
  else if(opcode == RETURN)
  {
    if(reg_current_stack == NIL)
      assert(false);

    OBJECT_PTR frame = car(reg_current_stack);
    reg_current_stack = cdr(reg_current_stack);

    RAW_PTR ptr = frame >> OBJECT_SHIFT;
    reg_next_expression   = get_heap(ptr+1);
    reg_current_env       = get_heap(ptr+2);
    reg_current_value_rib = get_heap(ptr+3);
  }
}

OBJECT_PTR create_call_frame(OBJECT_PTR next_expression,
                             OBJECT_PTR env,
                             OBJECT_PTR rib)
{
  log_function_entry("create_call_frame");

  RAW_PTR ptr = object_alloc(4);

  set_heap(ptr, convert_int_to_object(3));
  set_heap(ptr+1, next_expression);
  set_heap(ptr+2, env);
  set_heap(ptr+3, rib);

  insert_node(&white, create_node((ptr << OBJECT_SHIFT) + ARRAY_TAG));

  log_function_exit("create_call_frame");

  return (ptr << OBJECT_SHIFT) + ARRAY_TAG;
}

OBJECT_PTR create_current_continuation()
{
  return ((reg_current_stack >> OBJECT_SHIFT) << OBJECT_SHIFT) + CONTINUATION_TAG;
}

void raise_error(char *err_str)
{
  fprintf(stdout, "%s\n", err_str);

  fprintf(stdout, "Begin backtrace\n");
  print_backtrace();
  fprintf(stdout, "End backtrace\n");

  reg_accumulator = NIL;
  reg_current_value_rib = NIL;
  reg_next_expression = NIL;

  in_error = true;
}

void print_stack()
{
  OBJECT_PTR rest = reg_current_stack;

  while(rest != NIL)
  {
    OBJECT_PTR frame = car(rest);

    RAW_PTR ptr = frame >> OBJECT_SHIFT;

    printf("---- begin frame %0x----\n, frame");
    printf("Next expression: ");
    print_object(get_heap(ptr+1));
    printf("\n");

    printf("Env: ");
    print_object(get_heap(ptr+2));
    printf("\n");

    printf("Value rib: ");
    print_object(get_heap(ptr+3));
    printf("\n");
    printf("---- end frame %0x----\n", frame);

    rest = cdr(rest);
  }
}

OBJECT_PTR eval_backquote(OBJECT_PTR form)
{
  assert(is_valid_object(form));

  if(is_atom(form))
    return form;

  OBJECT_PTR car_obj = car(form);

  assert(is_valid_object(car_obj));

  if(IS_SYMBOL_OBJECT(car_obj))
  {
    char buf[SYMBOL_STRING_SIZE];
    print_symbol(car_obj, buf);

    if(car_obj == COMMA)
    {
      OBJECT_PTR temp = compile(CADR(form), NIL);

      if(temp == ERROR)
      {
        raise_error("Backquote evaluation(1): compile failed");
        return NIL;
      }

      reg_next_expression = cons(cons(FRAME, cons(cons(cons(HALT, NIL), CADR(form)),
                                                  cons(temp, CADR(form)))),
                                 CADR(form));

      reg_current_value_rib = NIL;

      while(car(reg_next_expression) != NIL)
      {
        eval();
        if(in_error)
        {
          raise_error("Evaluation of backquote failed(1)");
          return NIL;
        }
      }

      reg_next_expression = cons(cons(RETURN, NIL), cdr(reg_next_expression));
      reg_current_value_rib = NIL;

      return reg_accumulator;
    }
  }

  if(form_contains_comma_at(form))
  {
    //1. loop through elements in form
    //2. if element is not comma-at, call eval_backquote on
    //   it and append it to the result list without splicing
    //3. if it is comma-at, get its symbol value and
    //   splice the value to the result list
    //4. return the result list

    OBJECT_PTR result = NIL;

    OBJECT_PTR rest = form;

    while(rest != NIL)
    {
      OBJECT_PTR ret;
      OBJECT_PTR obj;

      if(IS_CONS_OBJECT(car(rest)) &&
	 IS_SYMBOL_OBJECT(CAAR(rest)))
      {
	char buf[SYMBOL_STRING_SIZE];
	print_symbol(CAAR(rest), buf);

	if(CAAR(rest) == COMMA_AT)
        {
          OBJECT_PTR temp = compile(CADAR(rest), NIL);
          if(temp == ERROR)
          {
            raise_error("Backquote evaluation(2): compile failed");
            return NIL;
          }

          reg_next_expression = cons(cons(FRAME, cons(cons(cons(HALT, NIL), CADAR(rest)),
                                                      cons(temp, CADAR(rest)))),
                                     CADAR(rest));

          reg_current_value_rib = NIL;

          while(car(reg_next_expression) != NIL)
          {
            eval();
            if(in_error)
            {
              raise_error("Evaluation of backquote failed(2)");
              return NIL;
            }
          }

          reg_next_expression = cons(cons(RETURN, NIL), cdr(reg_next_expression));
          reg_current_value_rib = NIL;

	  obj = reg_accumulator;

	  if(result == NIL)
	    result = obj;
	  else
	    set_heap((last_cell(result) >> OBJECT_SHIFT) + 1, obj);
	}
	else
	{
	  obj = eval_backquote(car(rest));
	  
	  if(result == NIL)
	    result = cons(obj, NIL);
	  else
	    set_heap((last_cell(result) >> OBJECT_SHIFT) + 1, cons(obj, NIL));
	}
      }
      else
      {
	obj = eval_backquote(car(rest));

	if(result == NIL)
	  result = cons(obj, NIL);
	else
	  set_heap((last_cell(result) >> OBJECT_SHIFT) + 1, cons(obj, NIL));
      }
      rest = cdr(rest);
    }

    return result;
  }

  return cons(eval_backquote(car(form)),
	      eval_backquote(cdr(form)));

}

OBJECT_PTR eval_string(OBJECT_PTR literal)
{
  assert(IS_STRING_LITERAL_OBJECT(literal));

  char *str_val = strings[literal >> OBJECT_SHIFT];

  char *ptr = NULL;

  int len = strlen(str_val);

  RAW_PTR raw_ptr = object_alloc(len + 1);

  set_heap(raw_ptr, convert_int_to_object(len));

  int i=1;

  for(ptr=str_val;*ptr;ptr++) 
  { 
    set_heap(raw_ptr + i, (*ptr << OBJECT_SHIFT) + CHAR_TAG);
    i++;
  }

  insert_node(&white, create_node((raw_ptr << OBJECT_SHIFT) + ARRAY_TAG));

  return (raw_ptr << OBJECT_SHIFT) + ARRAY_TAG;
}

OBJECT_PTR eval_make_array(OBJECT_PTR size, OBJECT_PTR default_value)
{
  assert(IS_INTEGER_OBJECT(size));
  
  int sz = get_int_value(size);

  RAW_PTR ptr = object_alloc(sz+1);

  set_heap(ptr, size);

  int i;

  for(i=0; i<sz; i++)
    set_heap(ptr + i + 1, default_value);

  insert_node(&white, create_node((ptr << OBJECT_SHIFT) + ARRAY_TAG));

  return (ptr << OBJECT_SHIFT) + ARRAY_TAG;
}

OBJECT_PTR eval_sub_array(OBJECT_PTR array, OBJECT_PTR start, OBJECT_PTR length)
{

  OBJECT_PTR ret;
  int st = get_int_value(start);
  int len = get_int_value(length);

  RAW_PTR orig_ptr = array >> OBJECT_SHIFT;

  RAW_PTR ptr = object_alloc(len + 1);

  set_heap(ptr, convert_int_to_object(len));

  int i;

  for(i=1; i<=len; i++)
    set_heap(ptr+i, get_heap(orig_ptr + st + i));

  insert_node(&white, create_node((ptr << OBJECT_SHIFT) + ARRAY_TAG));

  ret = (ptr << OBJECT_SHIFT) + ARRAY_TAG;

  log_function_exit("eval_sub_array");

  return ret;
}

BOOLEAN is_permitted_in_debug_mode(OBJECT_PTR exp)
{
  if(IS_CONS_OBJECT(exp))
  {
    OBJECT_PTR car_obj = car(exp);

    if(IS_SYMBOL_OBJECT(car_obj))
    {
      return (car_obj == RESUME)     || 
             (car_obj == ENV)        || 
             (car_obj == BACKTRACE)  ||
             (car_obj == CREATE_IMAGE);
    }

    return false;
  }
  else 
    return IS_SYMBOL_OBJECT(exp);
}

void print_backtrace()
{
  OBJECT_PTR rest = (debug_mode ? debug_execution_stack : reg_current_stack);

  int i=0;

  while(rest != NIL)
  {
    //print_object(car(rest));
    print_object(cdr(get_heap((car(rest) >> OBJECT_SHIFT) + 1)));

    printf("\n");

    i++;
    if((i % 20) == 0)
    {
      fprintf(stdout, "--- Press any key to continue (q to abort) ---");
      char c = getchar();
      if(c == 'q')
        return;
    }

    rest = cdr(rest);
  }
}

void print_state()
{
  fprintf(stdout, "Begin print state\n");

  fprintf(stdout, "Accumulator: ");
  print_object(reg_accumulator);
  fprintf(stdout, "\n");

  fprintf(stdout, "Value rib: ");
  print_object(reg_current_value_rib);
  fprintf(stdout, "\n");

  fprintf(stdout, "Next expression: ");
  print_object(car(reg_next_expression));
  fprintf(stdout, "\n");

  fprintf(stdout, "Environment: ");
  print_object(reg_current_env);
  fprintf(stdout, "\n");

  /* fprintf(stdout, "Stack: "); */
  /* print_object(reg_current_stack); */
  /* fprintf(stdout, "\n"); */

  fprintf(stdout, "End print state\n");
}

OBJECT_PTR get_continuation_for_return(OBJECT_PTR obj)
{
  OBJECT_PTR rest = continuations_map;

  while(rest != NIL)
  {
    if(CAAR(rest) == obj)
      return CDAR(rest);

    rest = cdr(rest);
  }
  return NIL;
}
