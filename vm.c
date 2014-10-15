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
#include <string.h>
#include <time.h>

#include "plisp.h"

#include "memory.h"

#include "hashtable.h"

//#include "util.h"
double get_wall_time();

extern OBJECT_PTR CAAR(OBJECT_PTR);
extern OBJECT_PTR CADR(OBJECT_PTR);
extern OBJECT_PTR CDAR(OBJECT_PTR);
extern OBJECT_PTR CADAR(OBJECT_PTR);
extern OBJECT_PTR CADDR(OBJECT_PTR);
extern OBJECT_PTR CADDDR(OBJECT_PTR);
extern OBJECT_PTR CADDDDR(OBJECT_PTR);

extern OBJECT_PTR first(OBJECT_PTR);
extern OBJECT_PTR second(OBJECT_PTR);
extern OBJECT_PTR third(OBJECT_PTR);
extern OBJECT_PTR fourth(OBJECT_PTR);
extern OBJECT_PTR fifth(OBJECT_PTR);

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
extern OBJECT_PTR SYMBL;
extern OBJECT_PTR SYMBOL_NAME;
extern OBJECT_PTR UNBIND;

extern OBJECT_PTR NEWLINE;

extern OBJECT_PTR ABORT;

extern OBJECT_PTR TIME;

extern OBJECT_PTR PROFILE;

extern OBJECT_PTR LAMBDA;

extern OBJECT_PTR NOT;
extern OBJECT_PTR LT;
extern OBJECT_PTR LEQ;
extern OBJECT_PTR GEQ;
extern OBJECT_PTR NEQ;

extern OBJECT_PTR SAVE_OBJECT;
extern OBJECT_PTR LOAD_OBJECT;

extern OBJECT_PTR COMPILEFN;

extern OBJECT_PTR top_level_env;

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

extern BOOLEAN system_changed;

extern char *foreign_library_names[];

extern unsigned int POINTER_MASK;

extern OBJECT_PTR CONS_APPLY_NIL;
extern OBJECT_PTR CONS_HALT_NIL;
extern OBJECT_PTR CONS_RETURN_NIL;

extern int refer(OBJECT_PTR);
extern void constant(OBJECT_PTR);
extern int assign(OBJECT_PTR);
extern int define(OBJECT_PTR);
extern int add();
extern int sub();
extern int eq();
extern void closure(OBJECT_PTR);
extern void macro(OBJECT_PTR);
extern void conti();
extern void frame(OBJECT_PTR);
extern void argument();
extern void return_op();
extern void bind_formal_parameters(OBJECT_PTR);

extern void break1();
extern unsigned int cons_compiled();

extern unsigned int neq();
extern unsigned int not();
extern unsigned int atom();
extern unsigned int car_compiled();
extern unsigned int cdr_compiled();
extern unsigned int mult();
extern unsigned int div_compiled();
extern unsigned int error();
extern unsigned int print();
extern unsigned int newline();

extern unsigned int lst();
extern unsigned int backquote();
extern unsigned int listp();
extern unsigned int symbol_value();
extern unsigned int gt();
extern unsigned int lt();
extern unsigned int leq();
extern unsigned int geq();
extern unsigned int gensym_compiled();
extern unsigned int setcar();

extern unsigned int setcdr();
extern unsigned int create_package_compiled();
extern unsigned int in_package();
extern unsigned int expand_macro();
extern unsigned int apply();
extern unsigned int string();
extern unsigned int make_array();
extern unsigned int array_set();
extern unsigned int array_get();
extern unsigned int sub_array();

extern cmpfn compile_function(OBJECT_PTR, char *);

extern hashtable_t *native_functions;

//variables related to profiling
double wall_time_var;
clock_t cpu_time_var;
unsigned int mem_alloc_var;
unsigned int mem_dealloc_var;
OBJECT_PTR last_operator;
OBJECT_PTR prev_operator;
hashtable_t *profiling_tab = NULL;
//end variables related to profiling

BOOLEAN in_break;

OBJECT_PTR prev_reg_accumulator;
OBJECT_PTR prev_reg_next_expression;
OBJECT_PTR prev_reg_current_value_rib;
OBJECT_PTR prev_reg_current_env;
OBJECT_PTR prev_reg_current_stack;
OBJECT_PTR prev_debug_env;
OBJECT_PTR prev_debug_continuation;
OBJECT_PTR prev_debug_execution_stack;
OBJECT_PTR prev_continuations_map;

BOOLEAN profiling_in_progress;

void eval(BOOLEAN do_gc)
{
  static unsigned int count = 0;

  OBJECT_PTR exp = car(reg_next_expression);

  OBJECT_PTR opcode = car(exp);

  pin_globals();

  if(do_gc)
  {
    count++;

    if(count == GC_FREQUENCY)
    {
      gc(false, true);
      count = 0;
    }
  }

  if(opcode == APPLY && profiling_in_progress)
  {
    last_operator = reg_accumulator;

    if(prev_operator != NIL)
    {
      OBJECT_PTR operator_to_be_used;

      hashtable_entry_t *e;

      unsigned int count;
      unsigned int mem_alloc;
      double elapsed_wall_time;
      double elapsed_cpu_time;

      double temp1 = get_wall_time();
      clock_t temp2 = clock();
      unsigned int temp3 = memory_allocated();

      profiling_datum_t *pd = (profiling_datum_t *)malloc(sizeof(profiling_datum_t));

      if(IS_SYMBOL_OBJECT(prev_operator))
         operator_to_be_used = prev_operator;
      else
      {
        OBJECT_PTR res = get_symbol_from_value(prev_operator, reg_current_env);
        if(car(res) != NIL)
          operator_to_be_used = cdr(res);
        else
          operator_to_be_used = cons(LAMBDA,
                                     cons(get_params_object(prev_operator),
                                          cons(car(get_source_object(prev_operator)), NIL)));
      }

      e = hashtable_get(profiling_tab, (void *)operator_to_be_used);

      if(e)
      {
        profiling_datum_t *pd = (profiling_datum_t *)e->value;

        count = pd->count + 1;

        elapsed_wall_time = pd->elapsed_wall_time + temp1 - wall_time_var;
        elapsed_cpu_time = pd->elapsed_cpu_time + (temp2 - cpu_time_var) * 1.0 / CLOCKS_PER_SEC;
      
        mem_alloc = pd->mem_allocated + temp3 - mem_alloc_var;

        hashtable_remove(profiling_tab, (void *)operator_to_be_used);
        free(pd);
      }
      else
      {
        count = 1;
        elapsed_wall_time = temp1 - wall_time_var;
        elapsed_cpu_time = (temp2 - cpu_time_var) * 1.0 / CLOCKS_PER_SEC;
        mem_alloc = temp3 - mem_alloc_var;
      }

      pd->count = count;
      pd->elapsed_wall_time = elapsed_wall_time;
      pd->elapsed_cpu_time = elapsed_cpu_time;
      pd->mem_allocated = mem_alloc;

      hashtable_put(profiling_tab, (void *)operator_to_be_used, (void *)pd);
    }

    wall_time_var = get_wall_time();
    cpu_time_var = clock();
    mem_alloc_var = memory_allocated();

    prev_operator = reg_accumulator;
  }

  if(opcode == HALT)
  {
    reg_next_expression = NIL;
  }
  else if(opcode == REFER)
  {
    refer(CADR(exp));
    reg_next_expression = CADDR(exp);
  }
  else if(opcode == CONSTANT)
  {
    constant(CADR(exp));
    reg_next_expression = CADDR(exp);
  }
  else if(opcode == CLOSE)
  {
    closure(exp);
    reg_next_expression = fifth(exp);
  }
  else if(opcode == MACRO)
  {
    macro(exp);
    reg_next_expression = CADDDDR(exp);
  }
  else if(opcode == TEST)
  {
    if(reg_accumulator != NIL)
      reg_next_expression = CADR(exp);
    else
      reg_next_expression = CADDR(exp);
  }
  //Not using this WHILE; reverting 
  //to macro definition, as this
  //version doesn't handle (BREAK)
  else if(opcode == WHILE)
  {
    OBJECT_PTR cond = CADR(exp);
    OBJECT_PTR body  = CADDR(exp);

    OBJECT_PTR ret = NIL;

    while(1)
    {
      OBJECT_PTR temp = reg_current_stack;

      reg_next_expression = cond;

      while(car(reg_next_expression) != NIL)
      {
        eval(false);
        if(in_error)
          return;
      }

      if(reg_accumulator == NIL)
        break;

      reg_next_expression = body;

      while(car(reg_next_expression) != NIL)
      {
        eval(false);
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
    assign(CADR(exp));
    reg_next_expression = CADDR(exp);
  }
  else if(opcode == DEFINE)
  {
    define(CADR(exp));
    reg_next_expression = CADDR(exp);
  }
  else if(opcode == CONTI)
  {
    conti();
    reg_next_expression = CADR(exp);
  }
  else if(opcode == NUATE) //this never gets called
  {
    reg_current_stack = CADR(exp);
    reg_accumulator = CADDR(exp);
    reg_current_value_rib = NIL;
    reg_next_expression =  cons(CONS_RETURN_NIL, cdr(reg_next_expression));
  }
  else if(opcode == FRAME)
  {
    frame(exp);
    reg_next_expression = CADDR(exp);
  }
  else if(opcode == ARGUMENT)
  {
    argument();
    reg_next_expression = CADR(exp);
  }
  else if(opcode == APPLY)
  {
    OBJECT_PTR operator = reg_accumulator;

    //OBJECT_PTR res = get_symbol_from_value(reg_accumulator, reg_current_env);

    /* continuations_map = cons(cons(operator, create_current_continuation()), */
    /*                          continuations_map); */

    //primitive operators
    if(IS_SYMBOL_OBJECT(operator))
    {
      char val[SYMBOL_STRING_SIZE];
      print_symbol(operator, val);
        
      if(operator == CONS)
      {
	cons_compiled();
      }
      else if(operator == EQ)
      {
	eq();
      }
      else if(operator == NEQ)
      {
	neq();
      }
      else if(operator == NOT)
      {
	not();
      }
      else if(operator == ATOM)
      {
	atom();
      }
      else if(operator == CAR)
      {
	car_compiled();
      }
      else if(operator == CDR)
      {
	cdr_compiled();
      }
      else if(operator == ADD)
      {
	add();
      }
      else if(operator == SUB)
      {
	sub();
      }
      else if(operator == MULT)
      {
	mult();
      }
      else if(operator == DIV)
      {
	div_compiled();
      }
      else if(operator == ERROR)
      {
	error();
      }
      else if(operator == PRINT)
      {
	print();
      }
      else if(operator == NEWLINE)
      {
	newline();
      }
      else if(operator == LST)
      {
	lst();
      }
      else if(operator == BACKQUOTE)
      {
	backquote();
      }
      else if(operator == LISTP)
      {
	listp();
      }
      else if(operator == SYMBOL_VALUE)
      {
	symbol_value();
      }
      else if(operator == GT)
      {
	gt();
      }
      else if(operator == LT)
      {
	lt();
      }
      else if(operator == LEQ)
      {
	leq();
      }
      else if(operator == GEQ)
      {
	geq();
      }
      else if(operator == GENSYM)
      {
	gensym_compiled();
      }
      else if(operator == SETCAR)
      {
	setcar();
      }
      else if(operator == SETCDR)
      {
	setcdr();
      }
      else if(operator == CREATE_PACKAGE)
      {
	create_package_compiled();
      }
      else if(operator == IN_PACKAGE)
      {
	in_package();
      }
      else if(operator == EXPAND_MACRO)
      {
	expand_macro();
      }
      else if(operator == APPLY)
      {
	apply();
      }
      else if(operator == STRING)
      {
	string();
      }
      else if(operator == MAKE_ARRAY)
      {
	make_array();
      }
      else if(operator == ARRAY_SET)
      {
	array_set();
      }
      else if(operator == ARRAY_GET)
      {
	array_get();
      }
      else if(operator == SUB_ARRAY)
      {
	sub_array();
      }
      else if(operator == ARRAY_LENGTH)
      {
        OBJECT_PTR array;

        if(length(reg_current_value_rib) != 1)
        {
          throw_exception("ARG-MISMATCH", "ARRAY-LENGTH requires exactly one argument, an array object");
          return;
        }        

        array = car(reg_current_value_rib);

        if(array == NIL)
        {
          reg_accumulator = convert_int_to_object(0);
          reg_current_value_rib = NIL;
          reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));
          return;
        }

        if(IS_STRING_LITERAL_OBJECT(array))
          reg_accumulator = convert_int_to_object(strlen(strings[(int)array >> OBJECT_SHIFT]));
        else
        {
          if(!(IS_ARRAY_OBJECT(array)))
          {
            throw_exception("INVALID-ARGUMENT", "Argument to ARRAY-LENGTH should be an ARRAY object");
            return;
          }

          //reg_accumulator = get_heap(array & POINTER_MASK, 0);
          reg_accumulator = convert_int_to_object(*((unsigned int *)(array & POINTER_MASK)));
        }

        reg_current_value_rib = NIL;
        reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));
      }
      else if(operator == PRINT_STRING)
      {
        OBJECT_PTR str;

        if(length(reg_current_value_rib) != 1)
        {
          throw_exception("ARG-MISMATCH", "PRINT-STRING requires exactly one argument, a string object");
          return;
        }        

        str = car(reg_current_value_rib);

        if(!(is_string_object(str)) && (!(IS_STRING_LITERAL_OBJECT(str))))
        {
          throw_exception("INVALID-ARGUMENT", "Argument to PRINT_STRING should be a string object");
          return;
        }

        print_object(str);
        fprintf(stdout, "\n");

        reg_accumulator = NIL;

        reg_current_value_rib = NIL;
        reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));
      }
      else if(operator == CREATE_IMAGE)
      {
        OBJECT_PTR file_name;

        if(length(reg_current_value_rib) != 1)
        {
          throw_exception("ARG-MISMATCH", "CREATE-IMAGE requires exactly one argument, a string object denoting the file name of the image");
          return;
        }        

        file_name = car(reg_current_value_rib);

        if(!(is_string_object(file_name)) && (!(IS_STRING_LITERAL_OBJECT(file_name))))
        {
          throw_exception("INVALID-ARGUMENT", "Argument to CREATE-IMAGE should be a string object or a string literal denoting the file name of the image");
          return;
        }

        if(is_string_object(file_name))
          create_image(get_string(file_name));
        else
          create_image(strings[(int)file_name >> OBJECT_SHIFT]);

        system_changed = false;

        reg_accumulator = NIL;

        reg_current_value_rib = NIL;
        reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));
      }
      else if(operator == LOAD_FOREIGN_LIBRARY)
      {
        OBJECT_PTR file_name;
        void **temp;
        char *fname;
        void *ret;

        if(length(reg_current_value_rib) != 1)
        {
          throw_exception("ARG-MISMATCH", "LOAD-FOREIGN-LIBRARY requires exactly one argument, a string object denoting the library name");
          return;
        }        

        file_name = car(reg_current_value_rib);

        if(!(is_string_object(file_name)) && (!(IS_STRING_LITERAL_OBJECT(file_name))))
        {
          throw_exception("INVALID-ARGUMENT", "Argument to LOAD-FOREIGN-LIBRARY should be a string object denoting the library name");
          return;
        }

        if(nof_dl_handles == MAX_FOREIGN_LIBRARY_COUNT)
        {
          throw_generic_exception("Maximum number of foreign libraries has been exceeded");
          return;
        }

        nof_dl_handles++;
  
        temp = (void **)realloc(dl_handles, nof_dl_handles * sizeof(void *));

        if(temp == NULL)
        {
          throw_exception("OUT-OF-MEMORY", "Unable to extend memory for dl_handles");
          return;
        }

        dl_handles = temp;

        fname = IS_STRING_LITERAL_OBJECT(file_name) ? strings[(int)file_name >> OBJECT_SHIFT] : get_string(file_name);

        /* if(IS_STRING_LITERAL_OBJECT(file_name)) */
        /*   ret = dlopen(strings[(int)file_name >> OBJECT_SHIFT], RTLD_LAZY); */
        /* else */
        /*   ret = dlopen(get_string(file_name), RTLD_LAZY); */
        ret = dlopen(fname, RTLD_LAZY);

        if(!ret)
        {
          throw_exception("FFI-OPEN-FAILED", "dl_open() failed");
          return;
        }

        dl_handles[nof_dl_handles - 1] = ret;

        foreign_library_names[nof_dl_handles - 1] = strdup(fname);

        reg_accumulator = NIL;

        reg_current_value_rib = NIL;
        reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));
      }
      else if(operator == CALL_FOREIGN_FUNCTION)
      {
        OBJECT_PTR fn_name, ret_type, args, rest_args;

        if(length(reg_current_value_rib) != 3)
        {
          throw_exception("ARG-MISMATCH", "CALL-FOREIGN-FUNCTION requires exactly three arguments");
          return;
        }        

        fn_name = car(reg_current_value_rib);

        if(!IS_STRING_LITERAL_OBJECT(fn_name) && !is_string_object(fn_name))
        {
          throw_exception("INVALID-ARGUMENT", "First argument to CALL-FOREIGN-FUNCTION should be the funtion name (string)");
          return;
        }

        ret_type = CADR(reg_current_value_rib);

        if(!(ret_type == INTEGR        ||
             ret_type == FLOT          ||
             ret_type == CHAR          ||
             ret_type == VOID          ||
             /* ret_type == INT_POINTER   || */ //not handling int and float pointer return values
             /* ret_type == FLOAT_POINTER || */
             ret_type == CHAR_POINTER))
        {
          throw_exception("INVALID-ARGUMENT", "Second parameter to CALL-FOREIGN-FUNCTION should be a valid return type");
          return;
        }

        args = CADDR(reg_current_value_rib);

        if(args != NIL && !IS_CONS_OBJECT(args))
        {
          throw_exception("INVALID-ARGUMENT", "Arguments should be a list of two-element lists (argument, type) -- 1");
          return;
        }

        rest_args = args;

        while(rest_args != NIL)
        {
          OBJECT_PTR car_rest_args = car(rest_args);

          OBJECT_PTR val, type;

          if(!IS_CONS_OBJECT(car_rest_args))
          {
            throw_exception("INVALID-ARGUMENT", "Arguments should be a list of two-element lists (argument, type) -- 2");
            return;
          }

          if(length(car_rest_args) != 2)
          {
            throw_exception("INVALID-ARGUMENT", "Arguments should be a list of two-element lists (argument, type) -- 3");
            return;
          }

          val = CAAR(rest_args);

          if(!(IS_INTEGER_OBJECT(val)        ||
               IS_FLOAT_OBJECT(val)          ||
               IS_CHAR_OBJECT(val)           ||
               IS_STRING_LITERAL_OBJECT(val) ||
               is_string_object(val)         ||
               IS_SYMBOL_OBJECT(val)))
          {
            throw_exception("INVALID-ARGUMENT", "Parameter should be integer-, float-, charcter-, or string constant, or a symbol");
            return;
          }

          if(IS_SYMBOL_OBJECT(val))
          {
            OBJECT_PTR res = get_symbol_value(val, reg_current_env);
            if(car(res) == NIL)
            {
              char buf[SYMBOL_STRING_SIZE];
              print_qualified_symbol(val, buf);
              sprintf(err_buf, "Symbol not bound(3): %s", buf);
              throw_exception("SYMBOL-NOT-BOUND", err_buf);
              return;
            }
            val = cdr(res);
          }

          type = CADAR(rest_args);

          if(type == INTEGR)
          {
            if(!IS_INTEGER_OBJECT(val))
            {
              throw_exception("ARG-MISMATCH", "Argument type mismatch: integer expected");
              return;
            }
          }          
          else if(type == FLOT)
          {
            if(!IS_FLOAT_OBJECT(val))
            {
              throw_exception("ARG-MISMATCH", "Argument type mismatch: float expected");
              return;
            }
          }
          else if(type == CHAR)
          {
            if(!IS_CHAR_OBJECT(val))
            {
              throw_exception("ARG-MISMATCH", "Argument type mismatch: character expected");
              return;
            }
          }
          else if(type == CHAR_POINTER)
          {
            if(!IS_STRING_LITERAL_OBJECT(val) && !is_string_object(val))
            {
              throw_exception("ARG-MISMATCH", "Argument type mismatch: string object/literal expected");
              return;
            }
          }
          else if(type == INT_POINTER)
          {
            if(!IS_SYMBOL_OBJECT(CAAR(rest_args)) || !IS_INTEGER_OBJECT(val))
            {
              throw_exception("ARG-MISMATCH", "Mapping a non-variable to INTEGER-POINTER / Argument type mismatch");
              return;
            }
          }
          else if(type == FLOAT_POINTER)
          {
            if(!IS_SYMBOL_OBJECT(CAAR(rest_args)) || !IS_FLOAT_OBJECT(val))
            {
              throw_exception("ARG-MISMATCH", "Mapping a non-variable to FLOAT-POINTER / Argument type mismatch");
              return;
            }
          }
          else
          {
            throw_exception("INVALID-ARGUMENT", "call_foreign_function(): non-primitive object type not handled");
            return;
          }

          rest_args = cdr(rest_args);
        }

        reg_accumulator = call_foreign_function(fn_name, ret_type, args);

        reg_current_value_rib = NIL;
        reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));
      }
      else if(operator == ENV)
      {
        reg_accumulator = cons(top_level_env, debug_mode ? debug_env : reg_current_env);

        reg_current_value_rib = NIL;
        reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));
      }
      else if(operator == EVAL)
      {
        OBJECT_PTR temp = compile(car(reg_current_value_rib), NIL);

        if(temp == ERROR)
        {
          throw_generic_exception("EVAL: Compilation failed");
          return;
        }

        /* reg_next_expression = cons(cons(FRAME, cons(cons(cons(HALT, NIL), car(reg_current_value_rib)), */
        /*                                             cons(cons(temp, NIL), car(reg_current_value_rib)))), */
        /*                            car(reg_current_value_rib)); */
        reg_next_expression = cons(cons(FRAME, cons(cons(CONS_HALT_NIL, car(reg_current_value_rib)),
                                                    cons(temp, car(reg_current_value_rib)))),
                                   car(reg_current_value_rib));

        while(car(reg_next_expression) != NIL)
        {
          eval(false);
          if(in_error)
          {
            throw_generic_exception("EVAL failed");
            return;
          }
        }

        reg_current_value_rib = NIL;
        reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));
      }
      else if(operator == TIME)
      {
        OBJECT_PTR temp = compile(car(reg_current_value_rib), NIL);

        char form[500];

        clock_t start, diff;
        int msec;

#ifdef GUI
        char buf[100];
#endif

        memset(form, '\0', 500);
        print_object_to_string(car(reg_current_value_rib), form, 0);

        if(temp == ERROR)
        {
          throw_generic_exception("TIME: Compilation failed");
          return;
        }

        reg_next_expression = cons(cons(FRAME, cons(cons(CONS_HALT_NIL, car(reg_current_value_rib)),
                                                    cons(temp, car(reg_current_value_rib)))),
                                   car(reg_current_value_rib));

        start = clock();

        while(car(reg_next_expression) != NIL)
        {
          eval(false);
          if(in_error)
          {
            throw_generic_exception("TIME failed");
            return;
          }
        }

        diff = clock() - start;
        msec = diff * 1000 / CLOCKS_PER_SEC;

#ifdef GUI
        memset(buf, '\0', 100);
        sprintf(buf, "%s took %d seconds %d milliseconds\n", form, msec/1000, msec%1000);
        print_to_transcript(buf);
#else
        printf("%s took %d seconds %d milliseconds\n", form, msec/1000, msec%1000);
#endif

        reg_current_value_rib = NIL;
        reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));
      }
      else if(operator == PROFILE)
      {
        OBJECT_PTR temp = compile(car(reg_current_value_rib), NIL);

        double initial_wall_time;
        clock_t initial_cpu_time;
        unsigned int initial_mem_alloc;

        OBJECT_PTR operator_to_be_used;

        unsigned int count;
        double elapsed_wall_time;
        double elapsed_cpu_time;
        unsigned int mem_alloc;

        hashtable_entry_t *e;

        profiling_datum_t *pd;

        double final_wall_time;
        clock_t final_cpu_time;
        unsigned int final_mem_alloc;

#ifdef GUI
        char buf[1000];
#endif

        hashtable_entry_t *entries;

        profiling_in_progress = true;

        if(temp == ERROR)
        {
          throw_generic_exception("PROFILE: Compilation failed");
          return;
        }

        reg_next_expression = cons(cons(FRAME, cons(cons(CONS_HALT_NIL, car(reg_current_value_rib)),
                                                    cons(temp, car(reg_current_value_rib)))),
                                   car(reg_current_value_rib));

        profiling_tab = hashtable_create(1000001);

        prev_operator = NIL;

        wall_time_var = get_wall_time();
        cpu_time_var = clock();
        mem_alloc_var = memory_allocated();

        initial_wall_time = wall_time_var;
        initial_cpu_time = cpu_time_var;
        initial_mem_alloc = mem_alloc_var;

        while(car(reg_next_expression) != NIL)
        {
          eval(false);
          if(in_error)
          {
            hashtable_delete(profiling_tab);
            profiling_tab = NULL;

            throw_generic_exception("PROFILE failed");
            return;
          }
        }

        if(IS_SYMBOL_OBJECT(last_operator))
          operator_to_be_used = last_operator;
        else
        {
          OBJECT_PTR res = get_symbol_from_value(last_operator, reg_current_env);
          if(car(res) != NIL)
            operator_to_be_used = cdr(res);
          else
            operator_to_be_used = cons(LAMBDA,
                                       cons(get_params_object(last_operator),
                                            cons(car(get_source_object(last_operator)), NIL)));
        }

        e = hashtable_get(profiling_tab, (void *)operator_to_be_used);

        if(e)
        {
          profiling_datum_t *pd = (profiling_datum_t *)e->value;

          count = pd->count + 1;
          elapsed_wall_time = pd->elapsed_wall_time + get_wall_time() - wall_time_var;
          elapsed_cpu_time = pd->elapsed_cpu_time + (clock() - cpu_time_var) * 1.0 / CLOCKS_PER_SEC;
          mem_alloc = pd->mem_allocated + memory_allocated() - mem_alloc_var;

          free(pd);
          hashtable_remove(profiling_tab, (void *)operator_to_be_used);
        }
        else
        {
          count = 1;
          elapsed_wall_time = get_wall_time() - wall_time_var;
          elapsed_cpu_time = (clock() - cpu_time_var) * 1.0 / CLOCKS_PER_SEC;
          mem_alloc = memory_allocated() - mem_alloc_var;
        }

        pd = (profiling_datum_t *)malloc(sizeof(profiling_datum_t));
        pd->count = count;
        pd->elapsed_wall_time = elapsed_wall_time;
        pd->elapsed_cpu_time = elapsed_cpu_time;
        pd->mem_allocated = mem_alloc;

        hashtable_put(profiling_tab, (void *)operator_to_be_used, (void *)pd);

        final_wall_time = get_wall_time();
        final_cpu_time = clock();
        final_mem_alloc = memory_allocated();

#ifdef GUI
        create_profiler_window(DEFAULT_PROFILER_WINDOW_POSX,
                               DEFAULT_PROFILER_WINDOW_POSY,
                               DEFAULT_PROFILER_WINDOW_WIDTH,
                               DEFAULT_PROFILER_WINDOW_HEIGHT);

        memset(buf, '\0', 1000);
        sprintf(buf,
                "Expression took %lf seconds (elapsed), %lf seconds (CPU), %d words allocated\n",
                final_wall_time - initial_wall_time,
                (final_cpu_time - initial_cpu_time) * 1.0 / CLOCKS_PER_SEC,
                final_mem_alloc - initial_mem_alloc);
        print_to_transcript(buf);

        //deleting profiling_tab will be done in the delete_event in the UI code

#else
        printf("Expression took %lf seconds (elapsed), %lf seconds (CPU), %d words allocated\n",
               final_wall_time - initial_wall_time,
               (final_cpu_time - initial_cpu_time) * 1.0 / CLOCKS_PER_SEC,
               final_mem_alloc - initial_mem_alloc);

        hashtable_delete(profiling_tab);
        profiling_tab = NULL;
#endif

        profiling_in_progress = false;

        reg_current_value_rib = NIL;
        reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));
      }
      else if(operator == BREAK)
      {
/*         in_break = true; */

/*         debug_mode = true; */
/*         debug_continuation = create_current_continuation(); */
/*         debug_env = reg_current_env; */
/*         reg_next_expression = NIL; */

/*         debug_execution_stack = reg_current_stack; */

/* #ifdef GUI */
/*         create_debug_window(DEFAULT_DEBUG_WINDOW_POSX, */
/*                             DEFAULT_DEBUG_WINDOW_POSY, */
/*                             DEFAULT_DEBUG_WINDOW_WIDTH, */
/*                             DEFAULT_DEBUG_WINDOW_HEIGHT); */
/* #endif */
	break1();
      }
      else if(operator == RESUME)
      {
        in_break = false;

        debug_mode = false;

        reg_current_stack = get_heap(debug_continuation & POINTER_MASK, 0);

        reg_current_value_rib = NIL;
        reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));

        debug_execution_stack = NIL;

        return;
      }
      else if(operator == BACKTRACE)
      {
        print_backtrace();
        reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));
        return;
      }
      else if(operator == LOAD_FILE)
      {
        OBJECT_PTR arg;
        FILE *temp;

        int ret;

        if(length(reg_current_value_rib) != 1)
        {
          throw_exception("ARG-MISMATCH", "LOAD-FILE requires exactly one argument");
          return;
        }        

        arg = car(reg_current_value_rib);

        if(!is_string_object(arg) && (!IS_STRING_LITERAL_OBJECT(arg)))
        {
          throw_exception("INVALID-ARGUMENT", "Argument to LOAD-FILE should be a string");
          return;
        }

        temp = fopen(is_string_object(arg) ?  get_string(arg) : strings[(int)arg >> OBJECT_SHIFT], "r");

        if(!temp)
        {
          throw_exception("FILE-OPEN-ERROR", "LOAD-FILE unable to open file");
          return;
        }

        if(set_up_new_yyin(temp))
        {
          printf("error\n");
          throw_exception("FILE-READ-ERROR", "Unable to read from file");
          return;
        }

        while(!yyparse())
        {
          repl(2);
        }
        pop_yyin();

        reg_accumulator = NIL;

        reg_current_value_rib = NIL;
      }
      else if(operator == CONSP)
      {
        if(length(reg_current_value_rib) != 1)
        {
          throw_exception("ARG-MISMATCH", "CONSP requires exactly one argument");
          return;
        }

        reg_accumulator = IS_CONS_OBJECT(car(reg_current_value_rib)) ? TRUE : NIL;
        reg_current_value_rib = NIL;
        reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));
      }
      else if(operator ==  INTEGERP)
      {
        if(length(reg_current_value_rib) != 1)
        {
          throw_exception("ARG-MISMATCH", "INTEGERP requires exactly one argument");
          return;
        }

        reg_accumulator = IS_INTEGER_OBJECT(car(reg_current_value_rib)) ? TRUE : NIL;
        reg_current_value_rib = NIL;
        reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));
      }
      else if(operator ==  FLOATP)
      {
        if(length(reg_current_value_rib) != 1)
        {
          throw_exception("ARG-MISMATCH", "FLOATP requires exactly one argument");
          return;
        }

        reg_accumulator = IS_FLOAT_OBJECT(car(reg_current_value_rib)) ? TRUE : NIL;
        reg_current_value_rib = NIL;
        reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));
      }
      else if(operator ==  CHARACTERP)
      {
        if(length(reg_current_value_rib) != 1)
        {
          throw_exception("ARG-MISMATCH", "CHARACTERP requires exactly one argument");
          return;
        }

        reg_accumulator = IS_CHAR_OBJECT(car(reg_current_value_rib)) ? TRUE : NIL;
        reg_current_value_rib = NIL;
        reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));
      }
      else if(operator ==  SYMBOLP)
      {
        if(length(reg_current_value_rib) != 1)
        {
          throw_exception("ARG-MISMATCH", "SYMBOLP requires exactly one argument");
          return;
        }

        reg_accumulator = IS_SYMBOL_OBJECT(car(reg_current_value_rib)) ? TRUE : NIL;
        reg_current_value_rib = NIL;
        reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));
      }
      else if(operator ==  STRINGP)
      {
        if(length(reg_current_value_rib) != 1)
        {
          throw_exception("ARG-MISMATCH", "STRINGP requires exactly one argument");
          return;
        }

        reg_accumulator = (IS_STRING_LITERAL_OBJECT(car(reg_current_value_rib)) || is_string_object(car(reg_current_value_rib))) ? TRUE : NIL;
        reg_current_value_rib = NIL;
        reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));
      }
      else if(operator ==  ARRAYP)
      {
        OBJECT_PTR obj;

        if(length(reg_current_value_rib) != 1)
        {
          throw_exception("ARG-MISMATCH", "ARRAYP requires exactly one argument");
          return;
        }

        obj = car(reg_current_value_rib);
        reg_accumulator = (IS_ARRAY_OBJECT(obj) || IS_STRING_LITERAL_OBJECT(obj)) ? TRUE : NIL;
        reg_current_value_rib = NIL;
        reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));
      }
      else if(operator ==  CLOSUREP)
      {
        if(length(reg_current_value_rib) != 1)
        {
          throw_exception("ARG-MISMATCH", "CLOSUREP requires exactly one argument");
          return;
        }

        reg_accumulator = IS_CLOSURE_OBJECT(car(reg_current_value_rib)) ? TRUE : NIL;
        reg_current_value_rib = NIL;
        reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));
      }
      else if(operator ==  MACROP)
      {
        if(length(reg_current_value_rib) != 1)
        {
          throw_exception("ARG-MISMATCH", "MACROP requires exactly one argument");
          return;
        }

        reg_accumulator = IS_MACRO_OBJECT(car(reg_current_value_rib)) ? TRUE : NIL;
        reg_current_value_rib = NIL;
        reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));
      }
      else if(operator ==  CONTINUATIONP)
      {
        if(length(reg_current_value_rib) != 1)
        {
          throw_exception("ARG-MISMATCH", "CONTINUATIONP requires exactly one argument");
          return;
        }

        reg_accumulator = IS_CONTINUATION_OBJECT(car(reg_current_value_rib)) ? TRUE : NIL;
        reg_current_value_rib = NIL;
        reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));
      }
      else if(operator == LAMBDA_EXPRESSION)
      {
        OBJECT_PTR obj;

        if(length(reg_current_value_rib) != 1)
        {
          throw_exception("ARG-MISMATCH", "LAMBDA-EXPRESSION requires exactly one argument, a closure or macro object");
          return;
        }        

        obj = car(reg_current_value_rib);        

        if(!IS_CLOSURE_OBJECT(obj) && !IS_MACRO_OBJECT(obj))
        {
          throw_exception("INVALID-ARGUMENT", "Argument to LAMBDA-EXPRESSION should be a closure or macro object");
          return;
        }

        reg_accumulator = cons(get_params_object(obj),
                               get_body_object(obj));
        reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));
      }
      else if(operator == FORMAT)
      {
        OBJECT_PTR rest;
        if(length(reg_current_value_rib) < 2)
        {
          throw_exception("ARG-MISMATCH", "FORMAT requires at least two arguments, a file descriptor and a format specification string");
          return;
        }

        if(car(reg_current_value_rib) != NIL && !IS_INTEGER_OBJECT(car(reg_current_value_rib)))
        {
          throw_exception("INVALID-ARGUMENT", "First parameter to FORMAT must be NIL or an integer denoting a file descriptor");
          return;
        }

        if(!IS_STRING_LITERAL_OBJECT(CADR(reg_current_value_rib)) && !is_string_object(CADR(reg_current_value_rib)))
        {
          throw_exception("INVALID-ARGUMENT", "Second parameter to FORMAT must be a format specification string");
          return;
        }
        
        rest = cdr(reg_current_value_rib);

        while(rest != NIL)
        {
          OBJECT_PTR val = car(rest);

          if(!(IS_INTEGER_OBJECT(val)        ||
               IS_FLOAT_OBJECT(val)          ||
               IS_CHAR_OBJECT(val)           ||
               IS_STRING_LITERAL_OBJECT(val) ||
               is_string_object(val)))
          {
            throw_exception("INVALID-ARGUMENT", "Parameters to FORMAT should be integers, floats, characters or strings");
            return;
          }

          rest = cdr(rest);
        }

#ifdef GUI
        if(format_for_gui(reg_current_value_rib) == -1)
#else
        if(format(reg_current_value_rib) == -1)
#endif
        {
          //error message would have been set in format()
          return;
        }

        reg_accumulator = NIL;
        reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));
      }
      else if(operator == CLONE)
      {
        if(length(reg_current_value_rib) != 1)
        {
          throw_exception("ARG-MISMATCH", "CLONE takes exactly one parameter, the object to be cloned");
          return;
        }

        reg_accumulator  = clone_object(car(reg_current_value_rib));
        reg_current_value_rib = NIL;
        reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));
      }
      else if(operator == RETURN_FROM)
      {
        OBJECT_PTR cont;

        if(length(reg_current_value_rib) != 2)
        {
          throw_exception("ARG-MISMATCH", "RETURN-FROM requires two parameters: the closure/macro from which to return, and the value to be returned");
          return;
        }

        cont = get_continuation_for_return(car(reg_current_value_rib));

        if(cont == NIL)
        {
          throw_exception("INVALID-ARGUMENT", "RETURN-FROM passed non-existent closure/macro object");
          return;
        }

        reg_current_stack = get_heap(cont & POINTER_MASK, 0);

        reg_accumulator = CADR(reg_current_value_rib);
        reg_current_value_rib = NIL;
        reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));
      }
      else if(operator == COMPILE)
      {
        OBJECT_PTR temp;

        if(length(reg_current_value_rib) != 2)
        {
          throw_exception("ARG-MISMATCH", "COMPILE needs two arguments, an expression to be compiled and a 'next' expression");
          return;
        }

        temp = compile(car(reg_current_value_rib), CADR(reg_current_value_rib));

        if(temp == ERROR)
        {
          throw_generic_exception("COMPILE failed");
          return;
        }

        reg_accumulator = car(temp);
        reg_current_value_rib = NIL;
        reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));
      }
      else if(operator == SYMBL)
      {
        OBJECT_PTR str;

        if(length(reg_current_value_rib) != 1)
        {
          throw_exception("ARG-MISMATCH", "SYMBOL needs one argument, a string object/literal");
          return;
        }

        str = car(reg_current_value_rib);
        if(!IS_STRING_LITERAL_OBJECT(str) && !is_string_object(str))
        {
          throw_exception("INVALID-ARGUMENT", "SYMBOL needs one argument, a string object/literal");
          return;
        }

        if(IS_STRING_LITERAL_OBJECT(str))
        {
          reg_accumulator = get_symbol_object((char *)convert_to_upper_case(strdup(strings[(int)str >> OBJECT_SHIFT])));
        }
        else if(is_string_object(str))
        {
          char msg[500];

          uintptr_t ptr = str & POINTER_MASK;

          //int len = get_int_value(get_heap(ptr, 0));
          int len = *((unsigned int *)ptr);

          int i;

          memset(msg, '\0', 500);

          for(i=1; i <= len; i++)
            msg[i-1] = (int)get_heap(ptr, i) >> OBJECT_SHIFT;

          reg_accumulator = get_symbol_object((char *)convert_to_upper_case(msg));
        }

        reg_current_value_rib = NIL;
        reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));
      }
      else if(operator == SYMBOL_NAME)
      {
        OBJECT_PTR sym;
        char buf[SYMBOL_STRING_SIZE];

        if(length(reg_current_value_rib) != 1)
        {
          throw_exception("ARG-MISMATCH", "SYMBOL-NAME requires exactly one argument, a symbol object");
          return;
        }

        sym = car(reg_current_value_rib);

        if(!IS_SYMBOL_OBJECT(sym))
        {
          throw_exception("INVALID-ARGUMENT", "Parameter to SYMBOL_NAME should be a symbol object");
          return;
        }

        memset(buf,'\0',SYMBOL_STRING_SIZE);

        print_symbol(sym, buf);

        reg_accumulator = (OBJECT_PTR)((add_string(buf) << OBJECT_SHIFT) + STRING_LITERAL_TAG);
        reg_current_value_rib = NIL;
        reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));
      }
      else if(operator == UNBIND)
      {
        OBJECT_PTR sym;

        OBJECT_PTR rest = top_level_env;
        OBJECT_PTR prev = NIL;
        BOOLEAN symbol_exists = false;

        /* if(current_package == 0) */
        /* { */
        /*   throw_exception("ACCESS-VIOLATION","Core package cannot be updated"); */
        /*   return; */
        /* } */

        if(length(reg_current_value_rib) != 1)
        {
          throw_exception("ARG-MISMATCH", "UNBIND requires exactly one argument, a symbol object");
          return;
        }

        sym = car(reg_current_value_rib);
        /* OBJECT_PTR sym = cdr(get_qualified_symbol_object(packages[current_package].name, */
        /*                                                  get_symbol_name(car(reg_current_value_rib)))); */

        if(!IS_SYMBOL_OBJECT(sym))
        {
          throw_exception("INVALID-ARGUMENT", "Parameter to UNBIND should be a symbol object");
          return;
        }

        while(rest != NIL)
        {

          if(CAAR(rest) == sym)
          {
            symbol_exists = true;
            if(prev == NIL)
              top_level_env = cdr(top_level_env);
            else
              set_heap(prev & POINTER_MASK, 1, cdr(rest));
            break;
          }

          prev = rest;
          rest = cdr(rest);
        }

        if(!symbol_exists)
        {
          char buf[SYMBOL_STRING_SIZE];
          print_qualified_symbol(sym, buf);
          sprintf(err_buf, "Symbol not bound(5): %s", buf);
          throw_exception("SYMBOL-NOT-BOUND", err_buf);
          return;
        }

        reg_accumulator = NIL;
        reg_current_value_rib = NIL;
        reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));
      }
      else if(operator == ABORT)
      {
        if(!debug_mode)
        {
          throw_exception("EXCEPTION", "ABORT must be invoked only in debug mode");
          return;
        }

        reg_current_env = NIL;

        reg_current_value_rib = NIL;
        reg_current_stack = NIL;

        continuations_map = NIL;

        reg_accumulator = NIL;
        reg_next_expression = NIL;

        in_error = false;

        debug_mode = false;

        return;
      }
      else if(operator == SAVE_OBJECT)
      {
        if(length(reg_current_value_rib) != 2)
        {
          throw_exception("ARG-MISMATCH", "SAVE-OBJECT requires exactly two arguments, an object and a file name");
          return;
        }
        
        OBJECT_PTR obj = car(reg_current_value_rib);
        
        OBJECT_PTR file_name = CADR(reg_current_value_rib);

        if(!(is_string_object(file_name)) && (!(IS_STRING_LITERAL_OBJECT(file_name))))
        {
          throw_exception("INVALID-ARGUMENT", "Second argument to SAVE-OBJECT should be a string object or a string literal denoting the file name");
          return;
        }

        if(is_string_object(file_name))
          serialize(obj, get_string(file_name));
        else
          serialize(obj, strings[(int)file_name >> OBJECT_SHIFT]);

        reg_accumulator = NIL;

        reg_current_value_rib = NIL;
        reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));
      }
      else if(operator == LOAD_OBJECT)
      {
        if(length(reg_current_value_rib) != 1)
        {
          throw_exception("ARG-MISMATCH", "LOAD-OBJECT requires exactly one argument, a file name");
          return;
        }
        
        OBJECT_PTR file_name = car(reg_current_value_rib);

        if(!(is_string_object(file_name)) && (!(IS_STRING_LITERAL_OBJECT(file_name))))
        {
          throw_exception("INVALID-ARGUMENT", "Argument to LOAD-OBJECT should be a string object or a string literal denoting the file name");
          return;
        }

        int ret;

        if(is_string_object(file_name))
          ret = deserialize(get_string(file_name));
        else
          ret = deserialize(strings[(int)file_name >> OBJECT_SHIFT]);

        if(ret == -1)
        {
          throw_exception("EXCEPTION", "Error in LOAD-OBJECT");
          return;
        }

        reg_accumulator = ret;

        reg_current_value_rib = NIL;
        reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));        
      }
      else if(operator == COMPILEFN)
      {
        if(length(reg_current_value_rib) != 1)
        {
          throw_exception("ARG-MISMATCH", "COMPILE-FN requires exactly one arguments a closure object");
          return;
        }

        OBJECT_PTR obj = car(reg_current_value_rib);

        if(!(IS_CLOSURE_OBJECT(obj)))
        {
          throw_exception("INVALID-ARGUMENT", "Argument to COMPILE-FN should be a closure object");
          return;
        }

	char err_buf1[500];
	memset(err_buf1, 500, '\0');

	cmpfn fn = compile_function(obj, err_buf1);

        if(!fn)
        {
	  memset(err_buf, 500, '\0');
	  sprintf(err_buf, "Error in COMPILE-FN: %s", err_buf1);
          throw_exception("EXCEPTION", err_buf);
          return;
        }

	reg_accumulator = NIL;
        reg_current_value_rib = NIL;
        reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));        
      }
      else
      {
	char buf[SYMBOL_STRING_SIZE];
	print_qualified_symbol(operator, buf);
	sprintf(err_buf, "Symbol not bound(4): %s", buf);
        throw_exception("SYMBOL-NOT-BOUND", err_buf);
        return;
      }
    }
    else //user-defined operator (closure, macro, or continuation)
    {
      if(IS_CLOSURE_OBJECT(reg_accumulator) || IS_MACRO_OBJECT(reg_accumulator))
      {
	bind_formal_parameters(reg_accumulator);

	if(IS_CLOSURE_OBJECT(reg_accumulator))
	{
	  hashtable_entry_t *e = hashtable_get(native_functions, (void *)reg_accumulator);
	  if(e)
	  {
	    cmpfn fn = (cmpfn)e->value;
	    fn();
	  }
	  else
	    reg_next_expression = get_body_object(reg_accumulator); 
	}
	else
	  reg_next_expression = get_body_object(reg_accumulator); 

      }
      else if(IS_CONTINUATION_OBJECT(reg_accumulator))
      {
        if(length(reg_current_value_rib) != 1)
        {
          throw_generic_exception("Continuations take exactly one argument");
          return;
        }

        reg_current_stack = get_heap(reg_accumulator & POINTER_MASK, 0);

        reg_accumulator = car(reg_current_value_rib);
        reg_current_value_rib = NIL;
        reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));
      }
      else
      {
        throw_generic_exception("Illegal operator");
        return;
      }
    }
  }
  else if(opcode == RETURN)
  {
    return_op();
  }
}

OBJECT_PTR create_call_frame(OBJECT_PTR next_expression,
                             OBJECT_PTR env,
                             OBJECT_PTR rib,
                             OBJECT_PTR source_expression)
{
  uintptr_t ptr = object_alloc(5, ARRAY_TAG);
  unsigned int *raw_ptr;

  log_function_entry("create_call_frame");

  //see comment in main.c for why we're not using object_alloc()
  //posix_memalign((void **)&raw_ptr, 16, sizeof(unsigned int *));
  //*((int *)raw_ptr) = 4;

  //set_heap(ptr, 0, convert_int_to_object(4));
  //set_heap(ptr, 0, (uintptr_t)raw_ptr + INTEGER_TAG);
  *((unsigned int *)ptr) = 4;

  set_heap(ptr, 1, next_expression);
  set_heap(ptr, 2, env);
  set_heap(ptr, 3, rib);
  set_heap(ptr, 4, source_expression);

  log_function_exit("create_call_frame");

  return ptr + ARRAY_TAG;
}

OBJECT_PTR create_current_continuation()
{
  uintptr_t ptr = object_alloc(1, CONTINUATION_TAG);
  set_heap(ptr, 0, reg_current_stack);
  return ptr + CONTINUATION_TAG;
}

void raise_error(char *err_str)
{
#ifdef GUI
  show_error_dialog(err_str);

  debug_mode = true;
  debug_continuation = create_current_continuation();
  debug_env = reg_current_env;
  reg_next_expression = NIL;

  debug_execution_stack = reg_current_stack;

  create_debug_window(DEFAULT_DEBUG_WINDOW_POSX,
                      DEFAULT_DEBUG_WINDOW_POSY,
                      DEFAULT_DEBUG_WINDOW_WIDTH,
                      DEFAULT_DEBUG_WINDOW_HEIGHT);
#else
  fprintf(stdout, "%s\n", err_str);
#endif

  //to stay commented out till we are
  //able to prpvide a meaningful backtrace
  //fprintf(stdout, "Begin backtrace\n");
  //print_backtrace();
  //fprintf(stdout, "End backtrace\n");

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

    uintptr_t ptr = frame & POINTER_MASK;

    printf("---- begin frame %0x----\n", frame);
    printf("Next expression: ");
    print_object(get_heap(ptr,1));
    printf("\n");

    printf("Env: ");
    print_object(get_heap(ptr,2));
    printf("\n");

    printf("Value rib: ");
    print_object(get_heap(ptr,3));
    printf("\n");
    printf("---- end frame %0x----\n", frame);

    rest = cdr(rest);
  }
}

OBJECT_PTR eval_backquote(OBJECT_PTR form)
{
  OBJECT_PTR car_obj;

  assert(is_valid_object(form));

  if(is_atom(form))
    return form;

  car_obj = car(form);

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
        throw_generic_exception("Backquote evaluation(1): compile failed");
        return NIL;
      }

      reg_next_expression = cons(cons(FRAME, cons(cons(CONS_HALT_NIL, CADR(form)),
                                                  cons(temp, CADR(form)))),
                                 CADR(form));

      reg_current_value_rib = NIL;

      while(car(reg_next_expression) != NIL)
      {
        eval(false);
        if(in_error)
        {
          throw_generic_exception("Evaluation of backquote failed(1)");
          return NIL;
        }
      }

      reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));
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
            throw_generic_exception("Backquote evaluation(2): compile failed");
            return NIL;
          }

          reg_next_expression = cons(cons(FRAME, cons(cons(CONS_HALT_NIL, CADAR(rest)),
                                                      cons(temp, CADAR(rest)))),
                                     CADAR(rest));

          reg_current_value_rib = NIL;

          while(car(reg_next_expression) != NIL)
          {
            eval(false);
            if(in_error)
            {
              throw_generic_exception("Evaluation of backquote failed(2)");
              return NIL;
            }
          }

          reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));
          reg_current_value_rib = NIL;

	  obj = reg_accumulator;

	  if(result == NIL)
	    result = obj;
	  else
	    set_heap(last_cell(result) & POINTER_MASK, 1, obj);
	}
	else
	{
	  obj = eval_backquote(car(rest));
	  
	  if(result == NIL)
	    result = cons(obj, NIL);
	  else
	    set_heap(last_cell(result) & POINTER_MASK, 1, cons(obj, NIL));
	}
      }
      else
      {
	obj = eval_backquote(car(rest));

	if(result == NIL)
	  result = cons(obj, NIL);
	else
	  set_heap(last_cell(result) & POINTER_MASK, 1, cons(obj, NIL));
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
  char *str_val = strings[(int)literal >> OBJECT_SHIFT];

  char *ptr = NULL;

  unsigned int len = strlen(str_val);

  unsigned int *raw_ptr1;

  uintptr_t raw_ptr;

  int i=1;

  assert(IS_STRING_LITERAL_OBJECT(literal));

  //see comment in main.c for why we're not using object_alloc()
  //posix_memalign((void **)&raw_ptr1, 16, sizeof(unsigned int *));
  //*((int *)raw_ptr1) = len;

  raw_ptr = object_alloc(len + 1, ARRAY_TAG);

  //set_heap(raw_ptr, 0, convert_int_to_object(len));
  //set_heap(raw_ptr, 0, (uintptr_t)raw_ptr1 + INTEGER_TAG);
  *((unsigned int *)raw_ptr) = len;

  for(ptr=str_val;*ptr;ptr++) 
  { 
    set_heap(raw_ptr, i, (OBJECT_PTR)((*ptr << OBJECT_SHIFT) + CHAR_TAG));
    i++;
  }

  return raw_ptr + ARRAY_TAG;
}

OBJECT_PTR eval_make_array(OBJECT_PTR size, OBJECT_PTR default_value)
{
  int sz = get_int_value(size);

  unsigned int *raw_ptr;

  uintptr_t ptr;

  int i;

  assert(IS_INTEGER_OBJECT(size));

  //see comment in main.c for why we're not using object_alloc()
  //posix_memalign((void **)&raw_ptr, 16, sizeof(unsigned int *));
  //*((int *)raw_ptr) = sz;

  ptr = object_alloc(sz+1, ARRAY_TAG);

  //set_heap(ptr, 0, size);
  //set_heap(ptr, 0, (uintptr_t)raw_ptr + INTEGER_TAG);
  *((unsigned int *)ptr) = sz;

  for(i=0; i<sz; i++)
    set_heap(ptr, i + 1, default_value);

  return ptr + ARRAY_TAG;
}

OBJECT_PTR eval_sub_array(OBJECT_PTR array, OBJECT_PTR start, OBJECT_PTR length)
{
  OBJECT_PTR ret;
  int st = get_int_value(start);
  int len = get_int_value(length);

  unsigned int *raw_ptr;

  uintptr_t orig_ptr, ptr;

  int i;

  //see comment in main.c for why we're not using object_alloc()
  //posix_memalign((void **)&raw_ptr, 16, sizeof(unsigned int *));
  //*((int *)raw_ptr) = len;

  orig_ptr = array & POINTER_MASK;

  ptr = object_alloc(len + 1, ARRAY_TAG);

  //set_heap(ptr, 0, convert_int_to_object(len));
  //set_heap(ptr, 0, (uintptr_t)raw_ptr + INTEGER_TAG);
  *((unsigned int *)ptr) = len;

  for(i=1; i<=len; i++)
    set_heap(ptr, i, get_heap(orig_ptr, st + i));

  ret = ptr;

  log_function_exit("eval_sub_array");

  return ret + ARRAY_TAG;
}

BOOLEAN is_permitted_in_debug_mode(OBJECT_PTR exp)
{
  if(IS_CONS_OBJECT(exp))
  {
    OBJECT_PTR car_obj = car(exp);

    if(IS_SYMBOL_OBJECT(car_obj))
    {
      return (car_obj == RESUME)       || 
             (car_obj == ENV)          || 
             (car_obj == BACKTRACE)    ||
             (car_obj == CREATE_IMAGE) ||
             (car_obj == ABORT);
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

  //to skip the two elements
  //corresponding to (throw ..)
  rest = cdr(cdr(rest));

  while(rest != NIL)
  {
    //print_object(car(rest));
    print_object(cdr(get_heap(car(rest) & POINTER_MASK, 1)));

    printf("\n");

    i++;
    if((i % 20) == 0)
    {
      char c;
      fprintf(stdout, "--- Press any key to continue (q to abort) ---");
      c = getchar();
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

void throw_generic_exception(char *err_str)
{
  /* reg_current_value_rib = cons(cons(get_symbol_object("EXCEPTION"), get_string_object(err_str)), NIL); */

  /* //basically (REFER THROW (APPLY)) */
  /* reg_next_expression = cons(cons(REFER, cons(get_symbol_object("THROW"),cons(cons(cons(APPLY, NIL), NIL), NIL))), NIL); */

  throw_exception("EXCEPTION", err_str);
}

void throw_exception(char *excp, char *err_str)
{
  //printf("In throw_exception() %s %s\n", excp, err_str);
  //getchar();
  reg_current_value_rib = cons(cons(get_symbol_object(excp), (OBJECT_PTR)get_string_object(err_str)), NIL);

  //basically (REFER THROW (APPLY))
  reg_next_expression = cons(cons(REFER, cons(get_symbol_object("THROW"),cons(cons(CONS_APPLY_NIL, NIL), NIL))), NIL);
}
