/**
  Copyright 2011-2016 Rajesh Jayaprakash <rajesh.jayaprakash@gmail.com>

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

extern exception_object;

extern unsigned int refer(OBJECT_PTR);
extern unsigned int constant(OBJECT_PTR);
extern unsigned int assign(OBJECT_PTR);
extern unsigned int define(OBJECT_PTR);
extern unsigned int closure(OBJECT_PTR);
extern unsigned int macro(OBJECT_PTR);
extern unsigned int conti();
extern unsigned int frame(OBJECT_PTR);
extern unsigned int argument();
extern unsigned int return_op();
extern unsigned int apply_compiled();

extern BOOLEAN core_library_loaded;

extern BOOLEAN console_mode, single_expression_mode, pipe_mode;

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
    halt_op();
  }
  else if(opcode == REFER)
  {
    if(refer(CADR(exp)))
       return;
    reg_next_expression = CADDR(exp);
  }
  else if(opcode == CONSTANT)
  {
    if(constant(CADR(exp)))
      return;
    reg_next_expression = CADDR(exp);
  }
  else if(opcode == CLOSE)
  {
    if(closure(exp))
      return;
    reg_next_expression = fifth(exp);
  }
  else if(opcode == MACRO)
  {
    if(macro(exp))
      return;
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
    if(assign(CADR(exp)))
      return;
    reg_next_expression = CADDR(exp);
  }
  else if(opcode == DEFINE)
  {
    if(define(CADR(exp)))
      return;
    reg_next_expression = CADDR(exp);
  }
  else if(opcode == CONTI)
  {
    if(conti())
      return;
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
    if(frame(exp))
      return;
    reg_next_expression = CADDR(exp);
  }
  else if(opcode == ARGUMENT)
  {
    if(argument())
      return;
    reg_next_expression = CADR(exp);
  }
  else if(opcode == APPLY)
  {
    apply_compiled();
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
  if(!console_mode && !single_expression_mode && !pipe_mode)
  {
    show_error_dialog(err_str);

#ifdef INTERPRETER_MODE
    debug_mode = true;
    debug_continuation = create_current_continuation();
    debug_env = reg_current_env;
    reg_next_expression = NIL;

    debug_execution_stack = reg_current_stack;

    create_debug_window(DEFAULT_DEBUG_WINDOW_POSX,
			DEFAULT_DEBUG_WINDOW_POSY,
			DEFAULT_DEBUG_WINDOW_WIDTH,
			DEFAULT_DEBUG_WINDOW_HEIGHT);
#endif
  }
  else
    fprintf(stdout, "%s\n", err_str);

  //to stay commented out till we are
  //able to prpvide a meaningful backtrace
  //fprintf(stdout, "Begin backtrace\n");
  //print_backtrace();
  //fprintf(stdout, "End backtrace\n");

#ifdef INTERPRETER_MODE
  reg_accumulator = NIL;
  reg_current_value_rib = NIL;
  reg_next_expression = NIL;
#endif

  in_error = true;
  exception_object = cons(get_symbol_object("EXCEPTION"), get_string_object(err_str));
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
	//print_object(car(reg_next_expression));printf("\n");getchar();
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
    set_heap(ptr, i + 1, clone_object(default_value));

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
  throw_exception("EXCEPTION", err_str);
}

void throw_exception(char *excp, char *err_str)
{
  if(!core_library_loaded)
  {
    printf("Exception %s: %s\n", excp, err_str);
    cleanup();
    exit(1);
  }

  //printf("In throw_exception() %s %s\n", excp, err_str);
  //getchar();
  reg_current_value_rib = cons(cons(get_symbol_object(excp), (OBJECT_PTR)get_string_object(err_str)), NIL);

  //basically (REFER THROW (APPLY))
  reg_next_expression = cons(cons(REFER, cons(get_symbol_object("THROW"),cons(cons(CONS_APPLY_NIL, NIL), NIL))), NIL);
}
