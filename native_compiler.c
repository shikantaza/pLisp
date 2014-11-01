#include <stdio.h>
#include <stdint.h>
#include <assert.h> 
#include <time.h>
#include <dlfcn.h>

#include "libtcc.h"

#include "plisp.h"

#include "hashtable.h"

extern void initialize();
extern OBJECT_PTR cons();
extern OBJECT_PTR CADR(OBJECT_PTR);
extern OBJECT_PTR CDDR(OBJECT_PTR);
extern OBJECT_PTR CADDR(OBJECT_PTR);

extern OBJECT_PTR list(int, ...);

extern OBJECT_PTR NIL;

extern OBJECT_PTR REFER;
extern OBJECT_PTR CONSTANT;
extern OBJECT_PTR CLOSE;
extern OBJECT_PTR MACRO;
extern OBJECT_PTR TEST;

extern OBJECT_PTR ASSIGN;
extern OBJECT_PTR CONTI;
extern OBJECT_PTR FRAME;
extern OBJECT_PTR ARGUMENT;
extern OBJECT_PTR RETURN;
extern OBJECT_PTR HALT;
extern OBJECT_PTR DEFINE;

extern OBJECT_PTR APPLY;
extern OBJECT_PTR ADD;
extern OBJECT_PTR SUB;
extern OBJECT_PTR EQ;

extern OBJECT_PTR BREAK;
extern OBJECT_PTR CONS;

extern OBJECT_PTR NEQ;
extern OBJECT_PTR NOT;
extern OBJECT_PTR ATOM;
extern OBJECT_PTR CAR;
extern OBJECT_PTR CDR;
extern OBJECT_PTR MULT;
extern OBJECT_PTR DIV;
extern OBJECT_PTR ERROR;
extern OBJECT_PTR PRINT;
extern OBJECT_PTR NEWLINE;

extern OBJECT_PTR LST;
extern OBJECT_PTR BACKQUOTE;
extern OBJECT_PTR LISTP;
extern OBJECT_PTR SYMBOL_VALUE;
extern OBJECT_PTR GT;
extern OBJECT_PTR LT;
extern OBJECT_PTR LEQ;
extern OBJECT_PTR GEQ;
extern OBJECT_PTR GENSYM;
extern OBJECT_PTR SETCAR;

extern OBJECT_PTR SETCDR;
extern OBJECT_PTR CREATE_PACKAGE;
extern OBJECT_PTR IN_PACKAGE;
extern OBJECT_PTR EXPAND_MACRO;
extern OBJECT_PTR STRING;
extern OBJECT_PTR MAKE_ARRAY;
extern OBJECT_PTR ARRAY_SET;
extern OBJECT_PTR ARRAY_GET;
extern OBJECT_PTR SUB_ARRAY;

extern OBJECT_PTR ARRAY_LENGTH;
extern OBJECT_PTR PRINT_STRING;
extern OBJECT_PTR CREATE_IMAGE;
extern OBJECT_PTR LOAD_FOREIGN_LIBRARY;
extern OBJECT_PTR CALL_FOREIGN_FUNCTION;
extern OBJECT_PTR ENV;
extern OBJECT_PTR EVAL;
extern OBJECT_PTR TIME;
extern OBJECT_PTR PROFILE;
extern OBJECT_PTR RESUME;

extern OBJECT_PTR BACKTRACE;
extern OBJECT_PTR LOAD_FILE;
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
extern OBJECT_PTR FORMAT;
extern OBJECT_PTR CLONE;
extern OBJECT_PTR RETURN_FROM;
extern OBJECT_PTR COMPILE;
extern OBJECT_PTR SYMBL;
extern OBJECT_PTR SYMBOL_NAME;
extern OBJECT_PTR UNBIND;
extern OBJECT_PTR ABORT;
extern OBJECT_PTR SAVE_OBJECT;
extern OBJECT_PTR LOAD_OBJECT;
extern OBJECT_PTR COMPILEFN;

extern OBJECT_PTR top_level_env;

extern OBJECT_PTR reg_accumulator;
extern OBJECT_PTR reg_next_expression;
extern OBJECT_PTR reg_current_env;
extern OBJECT_PTR reg_current_value_rib;
extern OBJECT_PTR reg_current_stack;

extern OBJECT_PTR continuations_map;

extern OBJECT_PTR debug_env;
extern BOOLEAN debug_mode;
extern OBJECT_PTR debug_continuation;
extern OBJECT_PTR debug_execution_stack;

extern BOOLEAN in_break;

extern unsigned int current_package;
extern package_t *packages;

extern OBJECT_PTR CONS_RETURN_NIL;
extern OBJECT_PTR CONS_APPLY_NIL;
extern OBJECT_PTR CONS_HALT_NIL;

extern OBJECT_PTR TRUE;

extern unsigned int POINTER_MASK;

extern OBJECT_PTR first(OBJECT_PTR);
extern OBJECT_PTR second(OBJECT_PTR);
extern OBJECT_PTR third(OBJECT_PTR);
extern OBJECT_PTR fourth(OBJECT_PTR);

extern char **strings;

extern BOOLEAN in_error;

//variables related to profiling
extern double wall_time_var;
extern clock_t cpu_time_var;
extern unsigned int mem_alloc_var;
extern unsigned int mem_dealloc_var;
extern OBJECT_PTR last_operator;
extern OBJECT_PTR prev_operator;
extern hashtable_t *profiling_tab;
//end variables related to profiling

extern OBJECT_PTR INTEGR;
extern OBJECT_PTR FLOT;
extern OBJECT_PTR CHAR;
extern OBJECT_PTR VOID;
extern OBJECT_PTR INT_POINTER;
extern OBJECT_PTR FLOAT_POINTER;
extern OBJECT_PTR CHAR_POINTER;

extern BOOLEAN profiling_in_progress;

extern OBJECT_PTR LAMBDA;

extern int nof_dl_handles;
extern void **dl_handles;

extern char err_buf[500];

extern BOOLEAN system_changed;

extern char *foreign_library_names[];

extern BOOLEAN expanding_macro;

TCCState *tcc_state = NULL;
/* TCCState **tcc_states = NULL; */
/* unsigned int nof_tcc_states = 0; */

hashtable_t *native_functions;

unsigned int compile_to_c(OBJECT_PTR, char *, unsigned int, char *, OBJECT_PTR **, unsigned int *);

cmpfn compile_closure(OBJECT_PTR, char *);

int dummy()
{
  return 0;
}

unsigned int refer(OBJECT_PTR sym)
{
  if(is_special_form(sym))
  {
    reg_accumulator = sym;
  }
  else
  {
    OBJECT_PTR symbol_to_be_used = sym;

    OBJECT_PTR res = get_symbol_value(symbol_to_be_used,
				      debug_mode ? debug_env : reg_current_env);

    if(car(res) != NIL)
      reg_accumulator = cdr(res);
    else
    {
      OBJECT_PTR res1;

      symbol_to_be_used = cdr(get_qualified_symbol_object(packages[current_package].name, get_symbol_name(sym)));

      res1 = get_symbol_value(symbol_to_be_used,
			      debug_mode ? debug_env : reg_current_env);

      if(car(res1) != NIL)
	reg_accumulator = cdr(res1);
      else
      {
	char buf[SYMBOL_STRING_SIZE];
	char err_buf[500];
	memset(err_buf, 500, '\0');
	print_qualified_symbol(symbol_to_be_used, buf);
	sprintf(err_buf, "Symbol not bound(1): %s", buf);
	throw_exception("SYMBOL-NOT-BOUND", err_buf);
	return 1;
      }
    }
  }
  return 0;
}

unsigned int constant(OBJECT_PTR c)
{
  reg_accumulator = c;
  return 0;
} 

unsigned int closure(OBJECT_PTR exp)
{
  OBJECT_PTR fn_obj = create_closure_object(reg_current_env, 
					    second(exp), 
					    third(exp), 
					    fourth(exp));

  char err_buf1[500];
  memset(err_buf1, 500, '\0');

  cmpfn fn = compile_closure(fn_obj, err_buf1);

  if(!fn_obj)
  {
    memset(err_buf, 500, '\0');
    sprintf(err_buf, "Error in compiling closure: %s", err_buf1);
    throw_exception("EXCEPTION", err_buf);
    return 1;
  }

  reg_accumulator = fn_obj;

  return 0;
}

OBJECT_PTR get_reg_accumulator()
{
  return reg_accumulator;
}

unsigned int macro(OBJECT_PTR exp)
{
  reg_accumulator = create_macro_object(reg_current_env, 
				        second(exp), 
					third(exp), 
					fourth(exp));
  return 0;
}

unsigned int assign(OBJECT_PTR sym)
{
  OBJECT_PTR symbol_to_be_used = sym;

  if(update_environment(reg_current_env, symbol_to_be_used, reg_accumulator) == NIL)
  {
    symbol_to_be_used = cdr(get_qualified_symbol_object(packages[current_package].name, get_symbol_name(sym)));

    if(update_environment(reg_current_env, symbol_to_be_used, reg_accumulator) == NIL)
    {
      char buf[SYMBOL_STRING_SIZE];
      char err_buf[500];
      memset(err_buf, 500, '\0');
      print_qualified_symbol(symbol_to_be_used, buf);
      sprintf(err_buf, "Symbol not bound(2): %s", buf);
      throw_exception("SYMBOL-NOT-BOUND", err_buf);
      return 1;
    }
  }
  return 0;
}

unsigned int conti()
{
  reg_accumulator = create_current_continuation();
  reg_current_value_rib = NIL;
  return 0;
}

unsigned int frame(OBJECT_PTR exp)
{
  reg_current_stack = cons(create_call_frame(CADR(exp),
					     reg_current_env,
					     reg_current_value_rib,
					     cdr(reg_next_expression)),
			   reg_current_stack);

  reg_current_value_rib = NIL;
  return 0;
}

unsigned int argument()
{
  reg_current_value_rib = cons(reg_accumulator, reg_current_value_rib);
  return 0;
}

unsigned int return_op()
{
  OBJECT_PTR frame;
  uintptr_t ptr;

  if(reg_current_stack == NIL)
    assert(false);

  frame = car(reg_current_stack);
  reg_current_stack = cdr(reg_current_stack);

  ptr = frame & POINTER_MASK;
  reg_next_expression   = get_heap(ptr,1);
  reg_current_env       = get_heap(ptr,2);
  reg_current_value_rib = get_heap(ptr,3);

  return 0;
}

unsigned int halt_op()
{
  reg_next_expression = NIL;
  return 0;
}

unsigned int define(OBJECT_PTR sym)
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

    add_to_top_level_environment(symbol_to_be_used, reg_accumulator);

    reg_accumulator = symbol_to_be_used;

    return 0;
}

unsigned int add()
{
  float sum = 0;
  OBJECT_PTR rest = reg_current_value_rib;
  BOOLEAN is_float = false;

  if(length(reg_current_value_rib) < 2)
  {
    throw_exception("ARG-MISMATCH", "Operator '+' requires at least two arguments");
    return 1;
  }

  while(rest != NIL)
  {
    OBJECT_PTR val = car(rest);
    if(IS_FLOAT_OBJECT(val))
    {
      is_float = true;
      sum += get_float_value(val);
    }
    else if(IS_INTEGER_OBJECT(val))
      sum += get_int_value(val);
    else
    {
      throw_exception("INVALID-ARGUMENT", "Argument to operator '+' should be a number");
      return 1;            
    }

    rest = cdr(rest);
  }

  if(is_float)
    reg_accumulator = convert_float_to_object(sum);
  else
    reg_accumulator = convert_int_to_object((int)sum);

  reg_current_value_rib = NIL;
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));

  return 0;
}

unsigned int sub()
{
  float val;
  BOOLEAN is_float = false;

  OBJECT_PTR first;
  OBJECT_PTR rest;

  float sum = 0;

  if(length(reg_current_value_rib) < 2)
  {
    throw_exception("ARG-MISMATCH", "Operator '-' requires at least two arguments");
    return 1;
  }
        
  first = car(reg_current_value_rib);

  if(IS_FLOAT_OBJECT(first))
  {
    is_float = true;
    val = get_float_value(first);
  }
  else if(IS_INTEGER_OBJECT(first))
    val = get_int_value(first);
  else
  {
    throw_exception("INVALID-ARGUMENT", "Argument to operator '-' should be a number");
    return 1;
  }

  rest = cdr(reg_current_value_rib);

  while(rest != NIL)
  {
    OBJECT_PTR val = car(rest);

    if(IS_FLOAT_OBJECT(val))
    {
      is_float = true;
      sum += get_float_value(val);
    }
    else if(IS_INTEGER_OBJECT(val))
      sum += get_int_value(val);
    else
    {
      throw_exception("INVALID-ARGUMENT", "Argument to operator '-' should be a number");
      return 1;
    }

    rest = cdr(rest);
  }

  if(is_float)
    reg_accumulator = convert_float_to_object(val - sum);
  else
    reg_accumulator = convert_int_to_object((int)(val - sum));

  reg_current_value_rib = NIL;
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));

  return 0;
}

unsigned int eq()
{
  OBJECT_PTR v1, v2;

  if(length(reg_current_value_rib) != 2)
  {
    throw_exception("ARG-MISMATCH", "EQ expects two arguments");
    return 1;
  }

  v1 = car(reg_current_value_rib);
  v2 = CADR(reg_current_value_rib);

  reg_accumulator = equal(v1, v2) ? TRUE : NIL;
  reg_current_value_rib = NIL;

  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));

  return 0;
}

void bind_formal_parameters(fn_macro_obj)
{
  OBJECT_PTR params, params_env, rest_params, rest_args;

  continuations_map = cons(cons(fn_macro_obj, create_current_continuation()),
			   continuations_map);

  params = get_params_object(fn_macro_obj);

  params_env = NIL;

  rest_params = params;
  rest_args = reg_current_value_rib;

  while(rest_params != NIL)
  {
    int package_index = (int)car(rest_params) >> (SYMBOL_BITS + OBJECT_SHIFT);
    int symbol_index =  ((int)car(rest_params) >> OBJECT_SHIFT) & TWO_RAISED_TO_SYMBOL_BITS_MINUS_1;

    char *param_name = packages[package_index].symbols[symbol_index];

    if(!strcmp(param_name, "&REST"))
    {
      if(params_env == NIL)
	params_env = cons(cons(CADR(rest_params), rest_args), NIL);
      else
      {
	uintptr_t ptr = last_cell(params_env) & POINTER_MASK;
	set_heap(ptr, 1, 
		 cons(cons(CADR(rest_params),rest_args), NIL));            
      }
      break;
    }
    else
    {
      if(params_env == NIL)
	params_env = cons(cons(car(rest_params), car(rest_args)), NIL);
      else
      {
	uintptr_t ptr = last_cell(params_env) & POINTER_MASK;
	set_heap(ptr, 1, 
		 cons(cons(car(rest_params),car(rest_args)), NIL));
      }
    }

    rest_params = cdr(rest_params);
    rest_args = cdr(rest_args);
  }

  reg_current_env = cons(params_env, get_env_list(fn_macro_obj));
  reg_current_value_rib = NIL;
}

unsigned int break1()
{
  in_break = true;

  debug_mode = true;
  debug_continuation = create_current_continuation();
  debug_env = reg_current_env;
  reg_next_expression = NIL;

  debug_execution_stack = reg_current_stack;

#ifdef GUI
  create_debug_window(DEFAULT_DEBUG_WINDOW_POSX,
		      DEFAULT_DEBUG_WINDOW_POSY,
		      DEFAULT_DEBUG_WINDOW_WIDTH,
		      DEFAULT_DEBUG_WINDOW_HEIGHT);
#endif

  return 0;
}

unsigned int cons_compiled()
{
  if(length(reg_current_value_rib) != 2)
  {
    throw_exception("ARG-MISMATCH", "CONS expects two arguments");
    return 1;
  }

  reg_accumulator = cons(car(reg_current_value_rib), CADR(reg_current_value_rib));
  reg_current_value_rib = NIL;

  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));

  return 0;
}

unsigned int neq()
{
  OBJECT_PTR v1, v2;

  if(length(reg_current_value_rib) != 2)
    {
      throw_exception("ARG-MISMATCH", "NEQ expects two arguments");
      return 1;
    }

  v1 = car(reg_current_value_rib);
  v2 = CADR(reg_current_value_rib);

  reg_accumulator = equal(v1, v2) ? NIL : TRUE;
  reg_current_value_rib = NIL;
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));

  return 0;
}

unsigned int not()
{
  OBJECT_PTR v;

  if(length(reg_current_value_rib) != 1)
  {
    throw_exception("ARG-MISMATCH", "NOT expects one argument");
    return 1;
  }

  v = car(reg_current_value_rib);

  reg_accumulator = (v == NIL) ? TRUE : NIL;
  reg_current_value_rib = NIL;
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));

  return 0;
}

unsigned int atom()
{
  OBJECT_PTR v;

  if(length(reg_current_value_rib) != 1)
  {
    throw_generic_exception("ATOM expects one argument");
    return 1;
  }

  v = car(reg_current_value_rib);

  reg_accumulator = is_atom(v) ? TRUE : NIL;
  reg_current_value_rib = NIL;
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));

  return 0;
}

unsigned int car_compiled()
{
  OBJECT_PTR car_obj;

  if(length(reg_current_value_rib) != 1)
  {
    throw_exception("NOT-A-CONS", "CAR expects one argument, a CONS object");
    return 1;
  }

  car_obj = car(reg_current_value_rib);
        
  if(car_obj == NIL)
    reg_accumulator = NIL;
  else
  {
    if(!IS_CONS_OBJECT(car(reg_current_value_rib)))
    {
      throw_exception("NOT-A-CONS", "Argument to CAR should be a CONS object");
      return 1;
    }
    reg_accumulator = CAAR(reg_current_value_rib);
  }

  reg_current_value_rib = NIL;
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));

  return 0;
}

unsigned int cdr_compiled()
{
  OBJECT_PTR car_obj;

  if(length(reg_current_value_rib) != 1)
  {
    throw_exception("NOT-A-CONS", "CDR expects one argument, a CONS object");
    return 1;
  }

  car_obj = car(reg_current_value_rib);
        
  if(car_obj == NIL)
    reg_accumulator = NIL;
  else
  {
    if(!IS_CONS_OBJECT(car(reg_current_value_rib)))
    {
      throw_exception("NOT-A-CONS", "Argument to CDR should be a CONS object");
      return 1;
    }

    reg_accumulator = CDAR(reg_current_value_rib);
  }

  reg_current_value_rib = NIL;
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));   

  return 0;
}

unsigned int mult()
{
  float sum = 1;
  OBJECT_PTR rest = reg_current_value_rib;
  BOOLEAN is_float = false;

  if(length(reg_current_value_rib) < 2)
  {
    throw_exception("ARG-MISMATCH", "Operator '*' requires at least two arguments");
    return 1;
  }

  while(rest != NIL)
  {
    OBJECT_PTR val = car(rest);

    if(IS_FLOAT_OBJECT(val))
    {
      is_float = true;
      sum *= get_float_value(val);
    }
    else if(IS_INTEGER_OBJECT(val))
      sum *= get_int_value(val);
    else
    {
      throw_exception("INVALID-ARGUMENT", "Argument to operator '*' should be a number");
      return 1;
    }

    rest = cdr(rest);
  }

  if(is_float)
    reg_accumulator = convert_float_to_object(sum);
  else
    reg_accumulator = convert_int_to_object((int)sum);

  reg_current_value_rib = NIL;
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));        

  return 0;
}

unsigned int div_compiled()
{
  float val;
  BOOLEAN is_float = false;

  OBJECT_PTR first;        
  OBJECT_PTR rest;

  float sum = 1;

  if(length(reg_current_value_rib) < 2)
  {
    throw_exception("ARG-MISMATCH", "Operator '/' requires at least two arguments");
    return 1;
  }

  first = car(reg_current_value_rib);

  if(IS_FLOAT_OBJECT(first))
  {
    is_float = true;
    val = get_float_value(first);
  }
  else if(IS_INTEGER_OBJECT(first))
    val = get_int_value(first);
  else
  {
    throw_exception("INVALID-ARGUMENT", "Argument to operator '/' should be a number");
    return 1;
  }

  rest = cdr(reg_current_value_rib);

  while(rest != NIL)
  {
    OBJECT_PTR val = car(rest);

    if(IS_FLOAT_OBJECT(val))
    {
      is_float = true;
      sum *= get_float_value(val);
    }
    else if(IS_INTEGER_OBJECT(val))
      sum *= get_int_value(val);
    else
    {
      throw_exception("INVALID-ARGUMENT", "Argument to operator '/' should be a number");
      return 1;
    }

      rest = cdr(rest);
  }

  if(sum == 0)
  {
    throw_exception("DIV-BY-ZERO-EXCEPTION", "Division by zero");
    return 1;
  }

  if(is_float)
    reg_accumulator = convert_float_to_object(val / sum);
  else
    reg_accumulator = convert_int_to_object((int)(val / sum));

  reg_current_value_rib = NIL;
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));   

  return 0;
}

unsigned int error()
{
  OBJECT_PTR error_string_obj = car(reg_current_value_rib);

  if(IS_STRING_LITERAL_OBJECT(error_string_obj))
    raise_error(strdup(strings[(int)error_string_obj >> OBJECT_SHIFT]));
  else if(is_string_object(error_string_obj))
  {
    char msg[500];

    uintptr_t ptr = error_string_obj & POINTER_MASK;

    //int len = get_int_value(get_heap(ptr, 0));
    int len = *((unsigned int *)ptr);

    int i;

    memset(msg, '\0', 500);

    for(i=1; i <= len; i++)
      msg[i-1] = (int)get_heap(ptr, i) >> OBJECT_SHIFT;

    raise_error(msg);
  }

  return 0;
}

unsigned int print()
{
  print_object(car(reg_current_value_rib));
#ifdef GUI
  print_to_transcript("\n");
#else
  fprintf(stdout, "\n");
#endif
  reg_accumulator = car(reg_current_value_rib);
  reg_current_value_rib = NIL;
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));

  return 0;
}

unsigned int newline()
{
  if(length(reg_current_value_rib) != 1)
  {
    throw_exception("ARG-MISMATCH", "NEWLINE should be passed exactly one argument, NIL or an integer denoting a file descriptor");
    return;
  }

  if(car(reg_current_value_rib) != NIL && !IS_INTEGER_OBJECT(car(reg_current_value_rib)))
  {
    throw_exception("INVALID-ARGUMENT", "NEWLINE should be passed exactly one argument, NIL or an integer denoting a file descriptor");
    return;
  }

  if(car(reg_current_value_rib) != NIL)
  {
    fprintf((FILE *)get_int_value(car(reg_current_value_rib)), "\n");
  }
  else
  {
#ifdef GUI
    print_to_transcript("\n");
#else
    fprintf(stdout, "\n");
#endif
  }

  reg_accumulator = NIL;
  reg_current_value_rib = NIL;
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));

  return 0;
}

unsigned int lst()
{
  OBJECT_PTR rest = reg_current_value_rib;

  reg_accumulator = NIL;

  while(rest != NIL)
  {
    if(reg_accumulator == NIL)
      reg_accumulator = cons(car(rest), NIL);
    else
    {
      uintptr_t ptr = last_cell(reg_accumulator) & POINTER_MASK;
      set_heap(ptr, 1, 
	       cons(car(rest), NIL));         
    }

    rest = cdr(rest);
  }

  reg_current_value_rib = NIL;
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));

  return 0;
}

unsigned int backquote()
{
  reg_accumulator = eval_backquote(car(reg_current_value_rib));

  reg_current_value_rib = NIL;
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));

  return 0;
}

unsigned int listp()
{
  OBJECT_PTR arg;

  if(length(reg_current_value_rib) != 1)
  {
    throw_exception("ARG-MISMATCH", "LISTP requires exactly one argument");
    return 1;
  }

  arg = car(reg_current_value_rib);

  if(arg == NIL)
    reg_accumulator = TRUE;
  else
    reg_accumulator = IS_CONS_OBJECT(arg) ? TRUE : NIL;

  reg_current_value_rib = NIL;
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));

  return 0;
}

unsigned int symbol_value()
{
  OBJECT_PTR sym, res;

  if(length(reg_current_value_rib) != 1)
  {
    throw_exception("ARG-MISMATCH", "SYMBOL-VALUE requires exactly one argument");
    return 1;
  }

  sym = car(reg_current_value_rib);

  if(!IS_SYMBOL_OBJECT(sym))
  {
    throw_exception("INVALID-ARGUMENT", "Argument to SYMBOL-VALUE should be a symbol object");
    return 1;
  }

  res = get_symbol_value(sym, reg_current_env);

  if(car(res) == NIL)
  {
    throw_exception("SYMBOL-NOT-BOUND", "Symbol not bound");
    return 1;
  }

  reg_accumulator = cdr(res);

  reg_current_value_rib = NIL;
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));

  return 0;
}

unsigned int gt()
{
  OBJECT_PTR v1, v2;
  float val1, val2;

  if(length(reg_current_value_rib) != 2)
  {
    throw_exception("ARG-MISMATCH", "> requires exactly two arguments");
    return 1;
  }

  v1 = car(reg_current_value_rib);
  v2 = CADR(reg_current_value_rib);

  if((!(IS_INTEGER_OBJECT(v1)) && !(IS_FLOAT_OBJECT(v1))) ||
     (!(IS_INTEGER_OBJECT(v2)) && !(IS_FLOAT_OBJECT(v2))))
  {
    throw_exception("INVALID-ARGUMENT", "Arguments to > should be numbers (integer or float)");
    return 1;
  }

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
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));

  return 0;
}

unsigned int lt()
{
  OBJECT_PTR v1, v2;
  float val1, val2;

  if(length(reg_current_value_rib) != 2)
  {
    throw_exception("ARG-MISMATCH", "< requires exactly two arguments");
    return 1;
  }

  v1 = car(reg_current_value_rib);
  v2 = CADR(reg_current_value_rib);

  if((!(IS_INTEGER_OBJECT(v1)) && !(IS_FLOAT_OBJECT(v1))) ||
     (!(IS_INTEGER_OBJECT(v2)) && !(IS_FLOAT_OBJECT(v2))))
  {
    throw_exception("INVALID-ARGUMENT", "Arguments to < should be numbers (integer or float)");
    return 1;
  }
 
  if(IS_FLOAT_OBJECT(v1))
    val1 = get_float_value(v1);
  else
    val1 = get_int_value(v1);

  if(IS_FLOAT_OBJECT(v2))
    val2 = get_float_value(v2);
  else
    val2 = get_int_value(v2);
	    
  reg_accumulator = (val1 < val2) ? TRUE : NIL;        

  reg_current_value_rib = NIL;
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));

  return 0;
}

unsigned int leq()
{
  OBJECT_PTR v1, v2;
  float val1, val2;

  if(length(reg_current_value_rib) != 2)
  {
    throw_exception("ARG-MISMATCH", "<= requires exactly two arguments");
    return 1;
  }

  v1 = car(reg_current_value_rib);
  v2 = CADR(reg_current_value_rib);

  if((!(IS_INTEGER_OBJECT(v1)) && !(IS_FLOAT_OBJECT(v1))) ||
     (!(IS_INTEGER_OBJECT(v2)) && !(IS_FLOAT_OBJECT(v2))))
  {
    throw_exception("INVALID-ARGUMENT", "Arguments to <= should be numbers (integer or float)");
    return 1;
  }
	  
  if(IS_FLOAT_OBJECT(v1))
    val1 = get_float_value(v1);
  else
    val1 = get_int_value(v1);

  if(IS_FLOAT_OBJECT(v2))
    val2 = get_float_value(v2);
  else
    val2 = get_int_value(v2);
	    
  reg_accumulator = (val1 <= val2) ? TRUE : NIL;        

  reg_current_value_rib = NIL;
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));

  return 0;
}

unsigned int geq()
{
  OBJECT_PTR v1, v2;
  float val1, val2;

  if(length(reg_current_value_rib) != 2)
  {
    throw_exception("ARG-MISMATCH", ">= requires exactly two arguments");
    return 1;
  }

  v1 = car(reg_current_value_rib);
  v2 = CADR(reg_current_value_rib);

  if((!(IS_INTEGER_OBJECT(v1)) && !(IS_FLOAT_OBJECT(v1))) ||
     (!(IS_INTEGER_OBJECT(v2)) && !(IS_FLOAT_OBJECT(v2))))
  {
    throw_exception("INVALID-ARGUMENT", "Arguments to >= should be numbers (integer or float)");
    return 1;
  }
 
  if(IS_FLOAT_OBJECT(v1))
    val1 = get_float_value(v1);
  else
    val1 = get_int_value(v1);

  if(IS_FLOAT_OBJECT(v2))
    val2 = get_float_value(v2);
  else
    val2 = get_int_value(v2);
	    
  reg_accumulator = (val1 >= val2) ? TRUE : NIL;        

  reg_current_value_rib = NIL;
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));

  return 0;
}

unsigned int gensym_compiled()
{
  if(reg_current_value_rib != NIL)
  {
    throw_exception("ARG-MISMATCH", "GEMSYM requires no argument");
    return 1;
  }

  reg_accumulator = gensym();
  reg_current_value_rib = NIL;
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));

  return 0;
}

unsigned int setcar()
{
  OBJECT_PTR car_obj;

  if(length(reg_current_value_rib) != 2)
  {
    throw_exception("ARG-MISMATCH", "SETCAR requires two arguments");
    return 1;
  }

  car_obj = car(reg_current_value_rib);

  if((!(IS_CONS_OBJECT(car_obj))))
  {
    throw_exception("ARG-MISMATCH", "First argument to SETCAR should be a CONS object");
    return 1;
  }

  set_heap(car_obj & POINTER_MASK, 0, CADR(reg_current_value_rib));

  reg_accumulator = CADR(reg_current_value_rib);

  reg_current_value_rib = NIL;
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));

  return 0;
}

unsigned int setcdr()
{
  OBJECT_PTR car_obj;

  if(length(reg_current_value_rib) != 2)
  {
    throw_exception("ARG-MISMATCH", "SETCDR requires two arguments");
    return 1;
  }

  car_obj = car(reg_current_value_rib);

  if((!(IS_CONS_OBJECT(car_obj))))
  {
    throw_exception("ARG-MISMATCH", "First argument to SETCDR should be a CONS object");
    return 1;
  }

  set_heap(car_obj & POINTER_MASK, 1, CADR(reg_current_value_rib));

  reg_accumulator = CADR(reg_current_value_rib);

  reg_current_value_rib = NIL;
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));

  return 0;
}

unsigned int create_package_compiled()
{
  OBJECT_PTR package;
  char *package_name;

  if(length(reg_current_value_rib) != 1)
  {
    throw_exception("ARG-MISMATCH", "CREATE-PACKAGE requires exactly one argument");
    return 1;
  }

  package = car(reg_current_value_rib);

  if(!IS_STRING_LITERAL_OBJECT(package) && !is_string_object(package))
  {
    throw_exception("INVALID-ARGUMENT", "CREATE-PACKAGE requires a string object or string literal as its argument");
    return 1;
  }

  package_name = (char *)convert_to_upper_case(strings[(int)package >> OBJECT_SHIFT]);

  /* if(!strcmp(package_name,"CORE")) */
  /* { */
  /*   throw_exception("CORE-PACKAGE-EXISTS", "Core package already exists"); */
  /*   return 1; */
  /* } */

  if(find_package(package_name) != NOT_FOUND)
  {
    throw_exception("PACKAGE-ALREADY-EXISTS", "Package already exists");
    return 1;
  }

  create_package(package_name);

  reg_accumulator = package;

  reg_current_value_rib = NIL;
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));

  return 0;
}

unsigned int in_package()
{
  OBJECT_PTR package;
  char *package_name;

  if(length(reg_current_value_rib) != 1)
  {
    throw_exception("ARG-MISMATCH", "IN-PACKAGE requires exactly one argument");
    return 1;
  }

  package = car(reg_current_value_rib);

  if(!IS_STRING_LITERAL_OBJECT(package) && !is_string_object(package))
  {
    throw_exception("INVALID-ARGUMENT", "IN-PACKAGE requires a string object or string literal as its argument");
    return 1;
  }

  package_name = (char *)convert_to_upper_case(strings[(int)package >> OBJECT_SHIFT]);

  if(!strcmp(package_name,"CORE"))
  {
    throw_exception("ACCESS-VIOLATION","Core package cannot be updated");
    return 1;
  }
  else
  {
    int index = find_package(package_name);
    if(index == NOT_FOUND)
    {
      throw_exception("PACKAGE-NOT-FOUND", "Package does not exist");
      return 1;
    }
    else
    {
      current_package = index;
      reg_accumulator = NIL;
    }
  }

  reg_current_value_rib = NIL;
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));

  return 0;
}

unsigned int expand_macro()
{
  OBJECT_PTR macro_body, res;

  if(length(reg_current_value_rib) != 1)
  {
    throw_exception("ARG-MISMATCH", "EXPAND-MACRO requires exactly one argument");
    return 1;
  }

  macro_body = car(reg_current_value_rib);

  if(!IS_CONS_OBJECT(macro_body))
  {
    throw_exception("INVALID-ARGUMENT", "EXPAND-MACRO requires a CONS form as argument");
    return 1;
  }

  res = get_symbol_value(car(macro_body), reg_current_env);

  if(car(res) == NIL)
  {
    throw_exception("MACRO-UNDEFINED", "Macro undefined");
    return 1;
  }
  else
  {
    OBJECT_PTR obj = cdr(res);
    OBJECT_PTR args = cdr(macro_body);

    /* reg_next_expression = cons(cons(FRAME, */
    /*                                 (cons(cons(cons(HALT, NIL), car(macro_body)), */
    /*                                       cons(cons(cons(APPLY, NIL), car(macro_body)), */
    /*                                            NIL)))), */
    /*                            car(macro_body)); */

    /* eval(false); */
    /* if(in_error) */
    /*   return 1; */

    reg_current_value_rib = NIL;

    //build the value rib
    while(args != NIL)
    {
      if(reg_current_value_rib == NIL)
	reg_current_value_rib = cons(car(args), NIL);
      else
      {
	uintptr_t ptr = last_cell(reg_current_value_rib) & POINTER_MASK;
	set_heap(ptr, 1, 
		 cons(car(args), NIL));         
      }
      args = cdr(args);
    }

    //place the macro object in the accumulator (to invoke APPLY)
    reg_accumulator = obj;
    reg_next_expression = cons(CONS_APPLY_NIL, car(macro_body));
          
    //evaluate the macro invocation
    while(car(reg_next_expression) != NIL)
    {
      eval(false);
      if(in_error)
	return;
    }
  }        

  reg_current_value_rib = NIL;
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));

  return 0;
}

unsigned int apply()
{
  OBJECT_PTR obj, args;

  if(length(reg_current_value_rib) != 2)
  {
    throw_exception("ARG-MISMATCH", "APPLY requires exactly two arguments");
    return 1;
  }        

  obj = car(reg_current_value_rib);

  if((!(IS_SYMBOL_OBJECT(obj))) &&
     (!(IS_CLOSURE_OBJECT(obj)))     &&
     (!(IS_CONTINUATION_OBJECT(obj))))
  {
    throw_exception("INVALID-ARGUMENT", "First argument to APPLY should be a special form, a closure or a continuation");
    return 1;
  }

  args = CADR(reg_current_value_rib);

  if(args != NIL && (!(IS_CONS_OBJECT(args))))
  {
    throw_exception("INVALID-ARGUMENT", "Second argument to APPLY should be a list of arguments");
    return 1;
  }

  reg_accumulator = obj;
  reg_current_value_rib = args;

  reg_next_expression = cons(CONS_APPLY_NIL, cdr(reg_next_expression));

  return 0;
}

unsigned int string()
{
  OBJECT_PTR string_literal;

  if(length(reg_current_value_rib) != 1)
  {
    throw_exception("ARG-MISMATCH", "STRING requires exactly one argument, a literal string");
    return 1;
  }        

  string_literal = car(reg_current_value_rib);

  if((!(IS_STRING_LITERAL_OBJECT(string_literal))))
  {
    throw_exception("INVALID-ARGUMENT", "Argument to STRING should be a literal string");
    return 1;
  }        
 
  reg_accumulator = eval_string(string_literal);

  reg_current_value_rib = NIL;
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));

  return 0;
}

unsigned int make_array()
{
  int len = length(reg_current_value_rib);
  OBJECT_PTR size;

  if((len != 1) && (len != 2))
  {
    throw_exception("ARG-MISMATCH", "MAKE-ARRAY requires the size as the first parameter, and optionally, the default value as the second");
    return 1;
  }        
        
  size = car(reg_current_value_rib);

  if((!(IS_INTEGER_OBJECT(size))))
  {
    throw_exception("INVALID-ARGUMENT", "First argument to MAKE-ARRAY should be the size of the array (integer)");
    return 1;
  }        

  reg_accumulator = eval_make_array(size, CADR(reg_current_value_rib));

  reg_current_value_rib = NIL;
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));

  return 0;
}

unsigned int array_set()
{
  OBJECT_PTR array_obj, idx;
  uintptr_t ptr;
  int array_len, index;

  if(length(reg_current_value_rib) != 3)
  {
    throw_exception("ARG-MISMATCH", "ARRAY-SET requires exactly three arguments");
    return 1;
  }        

  array_obj = car(reg_current_value_rib);

  ptr = array_obj & POINTER_MASK;

  if((!(IS_ARRAY_OBJECT(array_obj))))
  {
    throw_exception("INVALID-ARGUMENT", "First argument to ARRAY-SET should be an array");
    return 1;
  }        

  idx = CADR(reg_current_value_rib);

  if((!(IS_INTEGER_OBJECT(idx))))
  {
    throw_exception("INVALID-ARGUMENT", "Second argument to ARRAY-SET should be an integer (index into the array)");
    return 1;
  }        

  //array_len = get_int_value(get_heap(ptr, 0));
  array_len = *((unsigned int *)ptr);

  index = get_int_value(idx);

  if(index < 0 || (index >= array_len))
  {
    throw_exception("INDEX-OUT-OF-BOUNDS", "Array index out of bounds");
    return 1;
  }        
 
  set_heap(ptr, index + 1, CADDR(reg_current_value_rib));

  reg_accumulator = CADDR(reg_current_value_rib);

  reg_current_value_rib = NIL;
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));

  return 0;
}

unsigned int array_get()
{
  OBJECT_PTR array_obj, idx;
  uintptr_t ptr;
  int index;

  if(length(reg_current_value_rib) != 2)
  {
    throw_exception("ARG-MISMATCH", "ARRAY-GET requires exactly two arguments");
    return 1;
  }        

  array_obj = car(reg_current_value_rib);
  ptr = array_obj & POINTER_MASK;

  idx = CADR(reg_current_value_rib);

  if(!IS_INTEGER_OBJECT(idx))
  {
    throw_exception("INVALID-ARGUMENT", "Second argument to ARRAY-GET should be an integer (index into the array)");
    return 1;
  }        

  index = get_int_value(idx);

  if(IS_STRING_LITERAL_OBJECT(array_obj))
  {
    char *str = strings[(int)array_obj >> OBJECT_SHIFT];

    if(index < 0 || index >= strlen(str))
    {
      throw_exception("INDEX-OUT-OF-BOUNDS", "Array index out of bounds");
      return 1;
    }        

    reg_accumulator = (OBJECT_PTR)((str[index] << OBJECT_SHIFT) + CHAR_TAG);
  }
  else
  {
    int array_len;

    if(!IS_ARRAY_OBJECT(array_obj))
    {
      throw_exception("INVALID-ARGUMENT", "First argument to ARRAY-GET should be an array");
      return 1;
    }        

    //array_len = get_int_value(get_heap(ptr, 0));
    array_len = *((unsigned int *)ptr);

    if(index < 0 || (index >= array_len))
    {
      throw_exception("INDEX-OUT-OF-BOUNDS", "Array index out of bounds");
      return 1;
    }        

    reg_accumulator = get_heap(ptr, index + 1);
  }

  reg_current_value_rib = NIL;
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));

  return 0;
}

unsigned int sub_array()
{
  OBJECT_PTR array, start, array_length;

  int len;

  if(length(reg_current_value_rib) != 3)
  {
    throw_exception("ARG-MISMATCH", "SUB-ARRAY requires exactly three arguments");
    return 1;
  }        

  array = car(reg_current_value_rib);

  if(!(IS_ARRAY_OBJECT(array)))
  {
    throw_exception("INVALID-ARGUMENT", "First argument to SUB-ARRAY should be an ARRAY object");
    return 1;
  }

  start = CADR(reg_current_value_rib);

  if(!(IS_INTEGER_OBJECT(start)))
  {
    throw_exception("INVALID-ARGUMENT", "Second argument to SUB-ARRAY should be an integer (start index)");
    return 1;
  }

  if(!(get_int_value(start) >= 0))
  {
    throw_exception("INVALID-ARGUMENT", "Second argument to SUB-ARRAY should be a non-negative integer");
    return 1;
  }

  array_length = CADDR(reg_current_value_rib);

  if(!(IS_INTEGER_OBJECT(array_length)))
  {
    throw_exception("INVALID-ARGUMENT", "Third argument to SUB-ARRAY should be an integer (length of the sub-array)");
    return 1;
  }

  if(!(get_int_value(array_length) >= 0))
  {
    throw_exception("INVALID-ARGUMENT", "Third argument to SUB-ARRAY should be a non-negative integer");
    return 1;
  }

  len = *((unsigned int *)(array & POINTER_MASK));

  if((get_int_value(start) + get_int_value(array_length)) > len)
  {
    throw_exception("INDEX-OUT-OF-BOUNDS", "Range (start, length) for SUB-ARRAY out of bounds of the array");
    return 1;
  }

  reg_accumulator = eval_sub_array(array, start, array_length);

  reg_current_value_rib = NIL;
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));

  return 0;
}

unsigned int array_length()
{
  OBJECT_PTR array;

  if(length(reg_current_value_rib) != 1)
  {
    throw_exception("ARG-MISMATCH", "ARRAY-LENGTH requires exactly one argument, an array object");
    return 1;
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
      return 1;
    }

      //reg_accumulator = get_heap(array & POINTER_MASK, 0);
    reg_accumulator = convert_int_to_object(*((unsigned int *)(array & POINTER_MASK)));
  }

  reg_current_value_rib = NIL;
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));

  return 0;
}

unsigned int print_string_compiled()
{
  OBJECT_PTR str;

  if(length(reg_current_value_rib) != 1)
  {
    throw_exception("ARG-MISMATCH", "PRINT-STRING requires exactly one argument, a string object");
    return 1;
  }        

  str = car(reg_current_value_rib);

  if(!(is_string_object(str)) && (!(IS_STRING_LITERAL_OBJECT(str))))
  {
    throw_exception("INVALID-ARGUMENT", "Argument to PRINT_STRING should be a string object");
    return 1;
  }

  print_object(str);
  fprintf(stdout, "\n");

  reg_accumulator = NIL;

  reg_current_value_rib = NIL;
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));

  return 0;
}

unsigned int create_image_compiled()
{
  OBJECT_PTR file_name;

  if(length(reg_current_value_rib) != 1)
  {
    throw_exception("ARG-MISMATCH", "CREATE-IMAGE requires exactly one argument, a string object denoting the file name of the image");
    return 1;
  }        

  file_name = car(reg_current_value_rib);

  if(!(is_string_object(file_name)) && (!(IS_STRING_LITERAL_OBJECT(file_name))))
  {
    throw_exception("INVALID-ARGUMENT", "Argument to CREATE-IMAGE should be a string object or a string literal denoting the file name of the image");
    return 1;
  }

  if(is_string_object(file_name))
    create_image(get_string(file_name));
  else
    create_image(strings[(int)file_name >> OBJECT_SHIFT]);

  system_changed = false;

  reg_accumulator = NIL;

  reg_current_value_rib = NIL;
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));

  return 0;
}

unsigned int load_foreign_library_compiled()
{
  OBJECT_PTR file_name;
  void **temp;
  char *fname;
  void *ret;

  if(length(reg_current_value_rib) != 1)
  {
    throw_exception("ARG-MISMATCH", "LOAD-FOREIGN-LIBRARY requires exactly one argument, a string object denoting the library name");
    return 1;
  }        

  file_name = car(reg_current_value_rib);

  if(!(is_string_object(file_name)) && (!(IS_STRING_LITERAL_OBJECT(file_name))))
  {
    throw_exception("INVALID-ARGUMENT", "Argument to LOAD-FOREIGN-LIBRARY should be a string object denoting the library name");
      return 1;
  }

  if(nof_dl_handles == MAX_FOREIGN_LIBRARY_COUNT)
  {
    throw_generic_exception("Maximum number of foreign libraries has been exceeded");
    return 1;
  }

  nof_dl_handles++;
  
  temp = (void **)realloc(dl_handles, nof_dl_handles * sizeof(void *));

  if(temp == NULL)
  {
    throw_exception("OUT-OF-MEMORY", "Unable to extend memory for dl_handles");
    return 1;
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

  return 0;
}

unsigned int call_foreign_function_compiled()
{
  OBJECT_PTR fn_name, ret_type, args, rest_args;

  if(length(reg_current_value_rib) != 3)
  {
    throw_exception("ARG-MISMATCH", "CALL-FOREIGN-FUNCTION requires exactly three arguments");
    return 1;
  }        

  fn_name = car(reg_current_value_rib);

  if(!IS_STRING_LITERAL_OBJECT(fn_name) && !is_string_object(fn_name))
  {
    throw_exception("INVALID-ARGUMENT", "First argument to CALL-FOREIGN-FUNCTION should be the funtion name (string)");
    return 1;
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
    return 1;
  }

  args = CADDR(reg_current_value_rib);

  if(args != NIL && !IS_CONS_OBJECT(args))
  {
    throw_exception("INVALID-ARGUMENT", "Arguments should be a list of two-element lists (argument, type) -- 1");
    return 1;
  }

  rest_args = args;

  while(rest_args != NIL)
  {
    OBJECT_PTR car_rest_args = car(rest_args);

    OBJECT_PTR val, type;

    if(!IS_CONS_OBJECT(car_rest_args))
    {
      throw_exception("INVALID-ARGUMENT", "Arguments should be a list of two-element lists (argument, type) -- 2");
      return 1;
    }

    if(length(car_rest_args) != 2)
    {
      throw_exception("INVALID-ARGUMENT", "Arguments should be a list of two-element lists (argument, type) -- 3");
      return 1;
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
	return 1;
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
	return 1;
      }
      val = cdr(res);
    }

    type = CADAR(rest_args);

    if(type == INTEGR)
    {
      if(!IS_INTEGER_OBJECT(val))
      {
	throw_exception("ARG-MISMATCH", "Argument type mismatch: integer expected");
	return 1;
      }
    }          
    else if(type == FLOT)
    {
      if(!IS_FLOAT_OBJECT(val))
      {
	throw_exception("ARG-MISMATCH", "Argument type mismatch: float expected");
	return 1;
      }
    }
    else if(type == CHAR)
    {
      if(!IS_CHAR_OBJECT(val))
      {
	throw_exception("ARG-MISMATCH", "Argument type mismatch: character expected");
	return 1;
      }
    }
    else if(type == CHAR_POINTER)
    {
      if(!IS_STRING_LITERAL_OBJECT(val) && !is_string_object(val))
      {
	throw_exception("ARG-MISMATCH", "Argument type mismatch: string object/literal expected");
	return 1;
      }
    }
    else if(type == INT_POINTER)
    {
      if(!IS_SYMBOL_OBJECT(CAAR(rest_args)) || !IS_INTEGER_OBJECT(val))
      {
	throw_exception("ARG-MISMATCH", "Mapping a non-variable to INTEGER-POINTER / Argument type mismatch");
	return 1;
      }
    }
    else if(type == FLOAT_POINTER)
    {
      if(!IS_SYMBOL_OBJECT(CAAR(rest_args)) || !IS_FLOAT_OBJECT(val))
      {
	throw_exception("ARG-MISMATCH", "Mapping a non-variable to FLOAT-POINTER / Argument type mismatch");
	return 1;
      }
    }
    else
    {
      throw_exception("INVALID-ARGUMENT", "call_foreign_function(): non-primitive object type not handled");
      return 1;
    }

    rest_args = cdr(rest_args);
  }

  reg_accumulator = call_foreign_function(fn_name, ret_type, args);

  reg_current_value_rib = NIL;
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));

  return 0;
}

unsigned int env()
{
  reg_accumulator = cons(top_level_env, debug_mode ? debug_env : reg_current_env);

  reg_current_value_rib = NIL;
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));

  return 0;
}

unsigned int eval_compiled()
{
  OBJECT_PTR temp = compile(car(reg_current_value_rib), NIL);

  if(temp == ERROR)
  {
    throw_generic_exception("EVAL: Compilation failed");
    return 1;
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
       return 1;
     }
  }

  reg_current_value_rib = NIL;
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));

  return 0;
}

unsigned int time_compiled()
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
    return 1;
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
      return 1;
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

  return 0;
}

unsigned int profile()
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
    return 1;
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
      return 1;
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

  return 0;
}

unsigned int resume_compiled()
{
  in_break = false;

  debug_mode = false;

  reg_current_stack = get_heap(debug_continuation & POINTER_MASK, 0);

  reg_current_value_rib = NIL;
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));

  debug_execution_stack = NIL;

  return 0;
}

unsigned int backtrace()
{
  print_backtrace();
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));
  return 0;
}

unsigned int load_file()
{
  OBJECT_PTR arg;
  FILE *temp;

  int ret;

  if(length(reg_current_value_rib) != 1)
  {
    throw_exception("ARG-MISMATCH", "LOAD-FILE requires exactly one argument");
    return 1;
  }        

  arg = car(reg_current_value_rib);

  if(!is_string_object(arg) && (!IS_STRING_LITERAL_OBJECT(arg)))
  {
    throw_exception("INVALID-ARGUMENT", "Argument to LOAD-FILE should be a string");
    return 1;
  }

  temp = fopen(is_string_object(arg) ?  get_string(arg) : strings[(int)arg >> OBJECT_SHIFT], "r");

  if(!temp)
  {
    throw_exception("FILE-OPEN-ERROR", "LOAD-FILE unable to open file");
    return 1;
  }

  if(set_up_new_yyin(temp))
  {
    printf("error\n");
    throw_exception("FILE-READ-ERROR", "Unable to read from file");
    return 1;
  }

  while(!yyparse())
  {
    repl(2);
  }
  pop_yyin();

  reg_accumulator = NIL;

  reg_current_value_rib = NIL;

  return 0;
}

unsigned int consp()
{
  if(length(reg_current_value_rib) != 1)
  {
    throw_exception("ARG-MISMATCH", "CONSP requires exactly one argument");
    return 1;
  }

  reg_accumulator = IS_CONS_OBJECT(car(reg_current_value_rib)) ? TRUE : NIL;
  reg_current_value_rib = NIL;
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));

  return 0;
}

unsigned int integerp()
{
  if(length(reg_current_value_rib) != 1)
  {
    throw_exception("ARG-MISMATCH", "INTEGERP requires exactly one argument");
    return 1;
  }

  reg_accumulator = IS_INTEGER_OBJECT(car(reg_current_value_rib)) ? TRUE : NIL;
  reg_current_value_rib = NIL;
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));

  return 0;
}

unsigned int floatp()
{
  if(length(reg_current_value_rib) != 1)
  {
    throw_exception("ARG-MISMATCH", "FLOATP requires exactly one argument");
    return 1;
  }

  reg_accumulator = IS_FLOAT_OBJECT(car(reg_current_value_rib)) ? TRUE : NIL;
  reg_current_value_rib = NIL;
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));

  return 0;
}

unsigned int characterp()
{
  if(length(reg_current_value_rib) != 1)
  {
    throw_exception("ARG-MISMATCH", "CHARACTERP requires exactly one argument");
    return 1;
  }

  reg_accumulator = IS_CHAR_OBJECT(car(reg_current_value_rib)) ? TRUE : NIL;
  reg_current_value_rib = NIL;
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));

  return 0;
}

unsigned int symbolp()
{
  if(length(reg_current_value_rib) != 1)
   {
     throw_exception("ARG-MISMATCH", "SYMBOLP requires exactly one argument");
     return 1;
   }

  reg_accumulator = IS_SYMBOL_OBJECT(car(reg_current_value_rib)) ? TRUE : NIL;
  reg_current_value_rib = NIL;
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));

  return 0;
}

unsigned int stringp()
{
  if(length(reg_current_value_rib) != 1)
  {
    throw_exception("ARG-MISMATCH", "STRINGP requires exactly one argument");
    return 1;
  }

  reg_accumulator = (IS_STRING_LITERAL_OBJECT(car(reg_current_value_rib)) || is_string_object(car(reg_current_value_rib))) ? TRUE : NIL;
  reg_current_value_rib = NIL;
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));

  return 0;
}

unsigned int arrayp()
{
  OBJECT_PTR obj;

  if(length(reg_current_value_rib) != 1)
  {
    throw_exception("ARG-MISMATCH", "ARRAYP requires exactly one argument");
    return 1;
  }

  obj = car(reg_current_value_rib);
  reg_accumulator = (IS_ARRAY_OBJECT(obj) || IS_STRING_LITERAL_OBJECT(obj)) ? TRUE : NIL;
  reg_current_value_rib = NIL;
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));

  return 0;
}

unsigned int closurep()
{
  if(length(reg_current_value_rib) != 1)
  {
    throw_exception("ARG-MISMATCH", "CLOSUREP requires exactly one argument");
    return 1;
  }

  reg_accumulator = IS_CLOSURE_OBJECT(car(reg_current_value_rib)) ? TRUE : NIL;
  reg_current_value_rib = NIL;
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));

  return 0;
}

unsigned int macrop()
{
  if(length(reg_current_value_rib) != 1)
  {
    throw_exception("ARG-MISMATCH", "MACROP requires exactly one argument");
    return 1;
  }

  reg_accumulator = IS_MACRO_OBJECT(car(reg_current_value_rib)) ? TRUE : NIL;
  reg_current_value_rib = NIL;
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));

  return 0;
}

unsigned int continuationp()
{
  if(length(reg_current_value_rib) != 1)
  {
    throw_exception("ARG-MISMATCH", "CONTINUATIONP requires exactly one argument");
    return 1;
  }

  reg_accumulator = IS_CONTINUATION_OBJECT(car(reg_current_value_rib)) ? TRUE : NIL;
  reg_current_value_rib = NIL;
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));

  return 0;
}

unsigned int lambda_expression()
{
  OBJECT_PTR obj;

  if(length(reg_current_value_rib) != 1)
  {
    throw_exception("ARG-MISMATCH", "LAMBDA-EXPRESSION requires exactly one argument, a closure or macro object");
    return 1;
  }        

  obj = car(reg_current_value_rib);        

  if(!IS_CLOSURE_OBJECT(obj) && !IS_MACRO_OBJECT(obj))
  {
    throw_exception("INVALID-ARGUMENT", "Argument to LAMBDA-EXPRESSION should be a closure or macro object");
    return 1;
  }

  reg_accumulator = cons(get_params_object(obj),
			 get_body_object(obj));
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));

  return 0;
}

unsigned int format_compiled()
{
  OBJECT_PTR rest;
  if(length(reg_current_value_rib) < 2)
  {
    throw_exception("ARG-MISMATCH", "FORMAT requires at least two arguments, a file descriptor and a format specification string");
    return 1;
  }

  if(car(reg_current_value_rib) != NIL && !IS_INTEGER_OBJECT(car(reg_current_value_rib)))
  {
    throw_exception("INVALID-ARGUMENT", "First parameter to FORMAT must be NIL or an integer denoting a file descriptor");
    return 1;
  }

  if(!IS_STRING_LITERAL_OBJECT(CADR(reg_current_value_rib)) && !is_string_object(CADR(reg_current_value_rib)))
  {
    throw_exception("INVALID-ARGUMENT", "Second parameter to FORMAT must be a format specification string");
    return 1;
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
	return 1;
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
    return 1;
  }

  reg_accumulator = NIL;
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));

  return 0;
}

unsigned int clone()
{
  if(length(reg_current_value_rib) != 1)
  {
    throw_exception("ARG-MISMATCH", "CLONE takes exactly one parameter, the object to be cloned");
    return 1;
  }

  reg_accumulator  = clone_object(car(reg_current_value_rib));
  reg_current_value_rib = NIL;
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));

  return 0;
}

unsigned int return_from()
{
  OBJECT_PTR cont;

  if(length(reg_current_value_rib) != 2)
  {
    throw_exception("ARG-MISMATCH", "RETURN-FROM requires two parameters: the closure/macro from which to return, and the value to be returned");
    return 1;
  }

  cont = get_continuation_for_return(car(reg_current_value_rib));

  if(cont == NIL)
  {
    throw_exception("INVALID-ARGUMENT", "RETURN-FROM passed non-existent closure/macro object");
    return 1;
  }

  reg_current_stack = get_heap(cont & POINTER_MASK, 0);

  reg_accumulator = CADR(reg_current_value_rib);
  reg_current_value_rib = NIL;
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));

  return 0;
}

unsigned int compile_compiled()
{
  OBJECT_PTR temp;

  if(length(reg_current_value_rib) != 2)
  {
    throw_exception("ARG-MISMATCH", "COMPILE needs two arguments, an expression to be compiled and a 'next' expression");
    return 1;
  }

  temp = compile(car(reg_current_value_rib), CADR(reg_current_value_rib));

  if(temp == ERROR)
  {
    throw_generic_exception("COMPILE failed");
    return 1;
  }

  reg_accumulator = car(temp);
  reg_current_value_rib = NIL;
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));

  return 0;
}

unsigned int symbl()
{
  OBJECT_PTR str;

  if(length(reg_current_value_rib) != 1)
  {
    throw_exception("ARG-MISMATCH", "SYMBOL needs one argument, a string object/literal");
    return 1;
  }

  str = car(reg_current_value_rib);
  if(!IS_STRING_LITERAL_OBJECT(str) && !is_string_object(str))
  {
    throw_exception("INVALID-ARGUMENT", "SYMBOL needs one argument, a string object/literal");
    return 1;
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

  return 0;
}

unsigned int symbol_name()
{
  OBJECT_PTR sym;
  char buf[SYMBOL_STRING_SIZE];

  if(length(reg_current_value_rib) != 1)
  {
    throw_exception("ARG-MISMATCH", "SYMBOL-NAME requires exactly one argument, a symbol object");
    return 1;
  }

  sym = car(reg_current_value_rib);

  if(!IS_SYMBOL_OBJECT(sym))
  {
    throw_exception("INVALID-ARGUMENT", "Parameter to SYMBOL_NAME should be a symbol object");
    return 1;
  }

  memset(buf,'\0',SYMBOL_STRING_SIZE);

  print_symbol(sym, buf);

  reg_accumulator = (OBJECT_PTR)((add_string(buf) << OBJECT_SHIFT) + STRING_LITERAL_TAG);
  reg_current_value_rib = NIL;
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));

  return 0;
}

unsigned int unbind()
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
    return 1;
  }

  sym = car(reg_current_value_rib);
  /* OBJECT_PTR sym = cdr(get_qualified_symbol_object(packages[current_package].name, */
  /*                                                  get_symbol_name(car(reg_current_value_rib)))); */

  if(!IS_SYMBOL_OBJECT(sym))
  {
    throw_exception("INVALID-ARGUMENT", "Parameter to UNBIND should be a symbol object");
    return 1;
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
    return 1;
  }

  reg_accumulator = NIL;
  reg_current_value_rib = NIL;
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));

  return 0;
}

unsigned int abort_compiled()
{
  if(!debug_mode)
  {
    throw_exception("EXCEPTION", "ABORT must be invoked only in debug mode");
    return 1;
  }

  reg_current_env = NIL;

  reg_current_value_rib = NIL;
  reg_current_stack = NIL;

  continuations_map = NIL;

  reg_accumulator = NIL;
  reg_next_expression = NIL;

  in_error = false;

  debug_mode = false;

  return 0;
}

unsigned int save_object()
{
  if(length(reg_current_value_rib) != 2)
  {
    throw_exception("ARG-MISMATCH", "SAVE-OBJECT requires exactly two arguments, an object and a file name");
    return 1;
  }
        
  OBJECT_PTR obj = car(reg_current_value_rib);
        
  OBJECT_PTR file_name = CADR(reg_current_value_rib);

  if(!(is_string_object(file_name)) && (!(IS_STRING_LITERAL_OBJECT(file_name))))
  {
    throw_exception("INVALID-ARGUMENT", "Second argument to SAVE-OBJECT should be a string object or a string literal denoting the file name");
    return 1;
  }

  if(is_string_object(file_name))
    serialize(obj, get_string(file_name));
  else
    serialize(obj, strings[(int)file_name >> OBJECT_SHIFT]);

  reg_accumulator = NIL;

  reg_current_value_rib = NIL;
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));

  return 0;
}

unsigned int load_object()
{
  if(length(reg_current_value_rib) != 1)
  {
    throw_exception("ARG-MISMATCH", "LOAD-OBJECT requires exactly one argument, a file name");
    return 1;
  }
        
  OBJECT_PTR file_name = car(reg_current_value_rib);

  if(!(is_string_object(file_name)) && (!(IS_STRING_LITERAL_OBJECT(file_name))))
  {
    throw_exception("INVALID-ARGUMENT", "Argument to LOAD-OBJECT should be a string object or a string literal denoting the file name");
    return 1;
  }

  int ret;

  if(is_string_object(file_name))
    ret = deserialize(get_string(file_name));
  else
    ret = deserialize(strings[(int)file_name >> OBJECT_SHIFT]);

  if(ret == -1)
  {
    throw_exception("EXCEPTION", "Error in LOAD-OBJECT");
    return 1;
  }

  reg_accumulator = ret;

  reg_current_value_rib = NIL;
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));   
  return 0;
}

unsigned int compilefn()
{
  if(length(reg_current_value_rib) != 1)
  {
    throw_exception("ARG-MISMATCH", "COMPILE-FN requires exactly one arguments a closure object");
    return 1;
  }

  OBJECT_PTR obj = car(reg_current_value_rib);

  if(!(IS_CLOSURE_OBJECT(obj)))
  {
    throw_exception("INVALID-ARGUMENT", "Argument to COMPILE-FN should be a closure object");
    return 1;
  }

  char err_buf1[500];
  memset(err_buf1, 500, '\0');

  cmpfn fn = compile_closure(obj, err_buf1);

  if(!fn)
  {
    memset(err_buf, 500, '\0');
    sprintf(err_buf, "Error in COMPILE-FN: %s", err_buf1);
    throw_exception("EXCEPTION", err_buf);
    return 1;
  }

  reg_accumulator = NIL;
  reg_current_value_rib = NIL;
  reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));   

  return 0;
}

unsigned int apply_compiled()
{
  OBJECT_PTR operator = reg_accumulator;

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
      array_length();
    }
    else if(operator == PRINT_STRING)
    {
      print_string_compiled();
    }
    else if(operator == CREATE_IMAGE)
    {
      create_image_compiled();
    }
    else if(operator == LOAD_FOREIGN_LIBRARY)
    {
      load_foreign_library_compiled();
    }
    else if(operator == CALL_FOREIGN_FUNCTION)
    {
      call_foreign_function_compiled();
    }
    else if(operator == ENV)
    {
      env();
    }
    else if(operator == EVAL)
    {
      eval_compiled();
    }
    else if(operator == TIME)
    {
      time_compiled();
    }
    else if(operator == PROFILE)
    {
      profile();
    }
    else if(operator == BREAK)
    {
      break1();
    }
    else if(operator == RESUME)
    {
      resume_compiled();
    }
    else if(operator == BACKTRACE)
    {
      backtrace();
    }
    else if(operator == LOAD_FILE)
    {
      load_file();
    }
    else if(operator == CONSP)
    {
      consp();
    }
    else if(operator ==  INTEGERP)
    {
      integerp();
    }
    else if(operator ==  FLOATP)
    {
      floatp();
    }
    else if(operator ==  CHARACTERP)
    {
      characterp();
    }
    else if(operator ==  SYMBOLP)
    {
      symbolp();
    }
    else if(operator ==  STRINGP)
    {
      stringp();
    }
    else if(operator ==  ARRAYP)
    {
      arrayp();
    }
    else if(operator ==  CLOSUREP)
    {
      closurep();
    }
    else if(operator ==  MACROP)
    {
      macrop();
    }
    else if(operator ==  CONTINUATIONP)
    {
      continuationp();
    }
    else if(operator == LAMBDA_EXPRESSION)
    {
      lambda_expression();
    }
    else if(operator == FORMAT)
    {
      format_compiled();
    }
    else if(operator == CLONE)
    {
      clone();
    }
    else if(operator == RETURN_FROM)
    {
      return_from();
    }
    else if(operator == COMPILE)
    {
      compile_compiled();
    }
    else if(operator == SYMBL)
    {
      symbl();
    }
    else if(operator == SYMBOL_NAME)
    {
      symbol_name();
    }
    else if(operator == UNBIND)
    {
      unbind();
    }
    else if(operator == ABORT)
    {
      abort_compiled();
    }
    else if(operator == SAVE_OBJECT)
    {
      save_object();
    }
    else if(operator == LOAD_OBJECT)
    {
      load_object();
    }
    else if(operator == COMPILEFN)
    {
      compilefn();
    }
    else
    {
      char buf[SYMBOL_STRING_SIZE];
      print_qualified_symbol(operator, buf);
      sprintf(err_buf, "Symbol not bound(4): %s", buf);
      throw_exception("SYMBOL-NOT-BOUND", err_buf);
      return 1;
    }
  } //end of if(IS_SYMBOL_OBJECT(operator))
  else //user-defined operator (closure, macro, or continuation)
  {
    if(IS_CLOSURE_OBJECT(reg_accumulator) || IS_MACRO_OBJECT(reg_accumulator))
    {
      bind_formal_parameters(reg_accumulator);

      if(IS_CLOSURE_OBJECT(reg_accumulator))
      {
	hashtable_entry_t *e = hashtable_get(native_functions, (void *)reg_accumulator);
	cmpfn fn;

	if(e)
	{
	  fn = (cmpfn)e->value;
	  if(!fn)
	  {
	    throw_generic_exception("Unable to fetch compiled closure");
	    return 1;
	  }
	}
	else
	{
	  char err_buf1[500];
	  memset(err_buf1, '\0', 500);

	  fn = compile_closure(reg_accumulator, err_buf1);

	  if(!fn)
	  {
	    throw_generic_exception(err_buf1);
	    return 1;
	  }
	}
	//reg_next_expression = get_body_object(reg_accumulator); 
	/* if(expanding_macro) */
	/*   reg_next_expression = get_body_object(reg_accumulator); */
	/* else */
	  fn();
      }
      else
	reg_next_expression = get_body_object(reg_accumulator); 
    }
    else if(IS_CONTINUATION_OBJECT(reg_accumulator))
    {
      if(length(reg_current_value_rib) != 1)
      {
	throw_generic_exception("Continuations take exactly one argument");
	return 1;
      }

      reg_current_stack = get_heap(reg_accumulator & POINTER_MASK, 0);

      reg_accumulator = car(reg_current_value_rib);
      reg_current_value_rib = NIL;
      reg_next_expression = cons(CONS_RETURN_NIL, cdr(reg_next_expression));
    }
    else
    {
      throw_generic_exception("Illegal operator");
      return 1;
    }
  }

  return 0;
}

unsigned int compile_to_c(OBJECT_PTR exp, 
			  char *buf, 
			  unsigned int filled_len, 
			  char *err_buf, 
			  //TCCState *s, 
			  OBJECT_PTR **called_closures,
			  unsigned int *nof_called_closures)
{
  unsigned int len = 0;

  unsigned int temp;

  if(exp == NIL)
    return len;

  OBJECT_PTR car_exp = car(exp);

  if(car_exp == REFER)
  {
    if(car(car(CADDR(exp))) == APPLY)
    {
      if(IS_SYMBOL_OBJECT(CADR(exp)))
      {
        if(CADR(exp) == ADD)
        {
	  len += sprintf(buf+filled_len+len, "if(add())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == SUB)
        {
	  len += sprintf(buf+filled_len+len, "if(sub())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == EQ)
        {
	  len += sprintf(buf+filled_len+len, "if(eq())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == BREAK)
        {
	  //not handling BREAK statements in compiled code for now
	  /* len += sprintf(buf+filled_len+len, "if(break1())\n  return 1;\n"); */
	  /* temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures); */
	  /* if(temp == -1) */
	  /*   return -1; */
	  /* len += temp; */
	  sprintf(err_buf, "Function contains BREAK expressions. Please remove them");
	  return -1;
	}
	else if(CADR(exp) == CONS)
        {
	  len += sprintf(buf+filled_len+len, "if(cons_compiled())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}

	else if(CADR(exp) == NEQ)
        {
	  len += sprintf(buf+filled_len+len, "if(neq())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == NOT)
        {
	  len += sprintf(buf+filled_len+len, "if(not())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == ATOM)
        {
	  len += sprintf(buf+filled_len+len, "if(atom())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == CAR)
        {
	  len += sprintf(buf+filled_len+len, "if(car_compiled())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == CDR)
        {
	  len += sprintf(buf+filled_len+len, "if(cdr_compiled())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == MULT)
        {
	  len += sprintf(buf+filled_len+len, "if(mult())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == DIV)
        {
	  len += sprintf(buf+filled_len+len, "if(div_compiled())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == ERROR)
        {
	  len += sprintf(buf+filled_len+len, "if(error())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == PRINT)
        {
	  len += sprintf(buf+filled_len+len, "if(print())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == NEWLINE)
        {
	  len += sprintf(buf+filled_len+len, "if(newline())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == LST)
        {
	  len += sprintf(buf+filled_len+len, "if(lst())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == BACKQUOTE)
        {
	  len += sprintf(buf+filled_len+len, "if(backquote())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == LISTP)
        {
	  len += sprintf(buf+filled_len+len, "if(listp())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == SYMBOL_VALUE)
        {
	  len += sprintf(buf+filled_len+len, "if(symbol_value())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == GT)
        {
	  len += sprintf(buf+filled_len+len, "if(gt())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == LT)
        {
	  len += sprintf(buf+filled_len+len, "if(lt())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == LEQ)
        {
	  len += sprintf(buf+filled_len+len, "if(leq())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == GEQ)
        {
	  len += sprintf(buf+filled_len+len, "if(geq())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == GENSYM)
        {
	  len += sprintf(buf+filled_len+len, "if(gensym_compiled())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == SETCAR)
        {
	  len += sprintf(buf+filled_len+len, "if(setcar())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == SETCDR)
        {
	  len += sprintf(buf+filled_len+len, "if(setcdr())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == CREATE_PACKAGE)
        {
	  len += sprintf(buf+filled_len+len, "if(create_package_compiled())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == IN_PACKAGE)
        {
	  len += sprintf(buf+filled_len+len, "if(in_package())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == EXPAND_MACRO)
        {
	  len += sprintf(buf+filled_len+len, "if(expand_macro())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == APPLY)
        {
	  len += sprintf(buf+filled_len+len, "if(apply())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == STRING)
        {
	  len += sprintf(buf+filled_len+len, "if(string())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == MAKE_ARRAY)
        {
	  len += sprintf(buf+filled_len+len, "if(make_array())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == ARRAY_SET)
        {
	  len += sprintf(buf+filled_len+len, "if(array_set())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == ARRAY_GET)
        {
	  len += sprintf(buf+filled_len+len, "if(array_get())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == SUB_ARRAY)
        {
	  len += sprintf(buf+filled_len+len, "if(sub_array())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == ARRAY_LENGTH)
        {
	  len += sprintf(buf+filled_len+len, "if(array_length())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == PRINT_STRING)
        {
	  len += sprintf(buf+filled_len+len, "if(print_string_compiled())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == CREATE_IMAGE)
        {
	  len += sprintf(buf+filled_len+len, "if(create_image_compiled())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == LOAD_FOREIGN_LIBRARY)
        {
	  len += sprintf(buf+filled_len+len, "if(load_foreign_library_compiled())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == CALL_FOREIGN_FUNCTION)
        {
	  len += sprintf(buf+filled_len+len, "if(call_foreign_function_compiled())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == ENV)
        {
	  len += sprintf(buf+filled_len+len, "if(env())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == EVAL)
        {
	  len += sprintf(buf+filled_len+len, "if(eval_compiled())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == TIME)
        {
	  len += sprintf(buf+filled_len+len, "if(time_compiled())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == PROFILE)
        {
	  len += sprintf(buf+filled_len+len, "if(profile())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == RESUME)
        {
	  len += sprintf(buf+filled_len+len, "if(resume_compiled())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == BACKTRACE)
        {
	  len += sprintf(buf+filled_len+len, "if(backtrace())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == LOAD_FILE)
        {
	  len += sprintf(buf+filled_len+len, "if(load_file())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == CONSP)
        {
	  len += sprintf(buf+filled_len+len, "if(consp())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == INTEGERP)
        {
	  len += sprintf(buf+filled_len+len, "if(integerp())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == FLOATP)
        {
	  len += sprintf(buf+filled_len+len, "if(floatp())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == CHARACTERP)
        {
	  len += sprintf(buf+filled_len+len, "if(characterp())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == SYMBOLP)
        {
	  len += sprintf(buf+filled_len+len, "if(symbolp())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == STRINGP)
        {
	  len += sprintf(buf+filled_len+len, "if(stringp())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == ARRAYP)
        {
	  len += sprintf(buf+filled_len+len, "if(arrayp())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == CLOSUREP)
        {
	  len += sprintf(buf+filled_len+len, "if(closurep())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == MACROP)
        {
	  len += sprintf(buf+filled_len+len, "if(macrop())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == CONTINUATIONP)
        {
	  len += sprintf(buf+filled_len+len, "if(continuationp())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == LAMBDA_EXPRESSION)
        {
	  len += sprintf(buf+filled_len+len, "if(lambda_expression())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == FORMAT)
        {
	  len += sprintf(buf+filled_len+len, "if(format_compiled())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == CLONE)
        {
	  len += sprintf(buf+filled_len+len, "if(clone())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == RETURN_FROM)
        {
	  len += sprintf(buf+filled_len+len, "if(return_from())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == COMPILE)
        {
	  len += sprintf(buf+filled_len+len, "if(compile_compiled())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == SYMBL)
        {
	  len += sprintf(buf+filled_len+len, "if(symbl())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == SYMBOL_NAME)
        {
	  len += sprintf(buf+filled_len+len, "if(symbol_name())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == UNBIND)
        {
	  len += sprintf(buf+filled_len+len, "if(unbind())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == ABORT)
        {
	  len += sprintf(buf+filled_len+len, "if(abort_compiled())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == SAVE_OBJECT)
        {
	  len += sprintf(buf+filled_len+len, "if(save_object())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == LOAD_OBJECT)
        {
	  len += sprintf(buf+filled_len+len, "if(load_object())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == COMPILEFN)
        {
	  len += sprintf(buf+filled_len+len, "if(compilefn())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else //it's a user-defined closure object
        {
	  //TODO: does calling refer() to get at the closure object
	  //work for all cases?

	  //refer() places the closure object in reg_accumulator, this may
	  //be a problem if the contents of reg_accumulator is needed elsewhere,
	  //so we preserve its contents

	  OBJECT_PTR prev_reg_accumulator = reg_accumulator;
	  OBJECT_PTR fn_object;

	  if(refer(CADR(exp)))
	  {
	    char buf[SYMBOL_STRING_SIZE];
	    memset(buf, SYMBOL_STRING_SIZE, '\0');
	    print_qualified_symbol(CADR(exp), buf);
	    sprintf(err_buf, "Call to undefined closure (%s)", buf);
	    return -1;
	  }

	  assert(IS_CLOSURE_OBJECT(reg_accumulator));

	  fn_object = reg_accumulator;
	  reg_accumulator = prev_reg_accumulator;

	  BOOLEAN closure_already_exists = false;
	  int i;
	  for(i=0; i< *nof_called_closures; i++)
	  {
	    if((*called_closures)[i] == fn_object)
	    {
	      closure_already_exists = true;
	      break;
	    }
	  }

	  if(!closure_already_exists && IS_CLOSURE_OBJECT(fn_object))
	  {
	    if(*called_closures == NULL)
	    {
	      *called_closures = (OBJECT_PTR *)malloc(sizeof(OBJECT_PTR));
	      assert(*called_closures != NULL);
	      *nof_called_closures = 1;
	      (*called_closures)[0] = fn_object;
	    }
	    else
	    {
	      (*nof_called_closures)++;
	      *called_closures = (OBJECT_PTR *)realloc(*called_closures, *nof_called_closures * sizeof(OBJECT_PTR));
	      assert(*called_closures != NULL);
	      (*called_closures)[*nof_called_closures-1] = fn_object;	      
	    }
	  }

	  if(IS_CLOSURE_OBJECT(fn_object))
	  {
	    len += sprintf(buf+filled_len+len, "bind_formal_parameters(%d);\n", fn_object);

	    len += sprintf(buf+filled_len+len, "if(f_%d())\n  return 1;\n", fn_object);
	    temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
	    if(temp == -1)
	      return -1;
	    len += temp;
	  }
	}
      }
    } //end of if(car(car(CADDR(exp))) == APPLY)
    else
    {
      len += sprintf(buf+filled_len+len, "if(refer(%d))\n  return 1;\n", CADR(exp));
      temp = compile_to_c(car(CADDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
      if(temp == -1)
	return -1;
      len += temp;
    }
  }
  else if(car_exp == APPLY)
  {
    len += sprintf(buf+filled_len+len, "if(apply_compiled())\n return 1;\n");
    temp = compile_to_c(car(CADR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
    if(temp == -1)
      return -1;
    len += temp;    
  }
  else if(car_exp == CONSTANT)
  {
    len += sprintf(buf+filled_len+len, "if(constant(%d))\n  return 1;\n", CADR(exp));
    temp = compile_to_c(car(car(CDDR(exp))), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
    if(temp == -1)
      return -1;
    len += temp;
  }
  else if(car_exp == CLOSE)
  {
    len += sprintf(buf+filled_len+len, "if(closure(%d))\n  return 1;\n", exp);
    temp = compile_to_c(car(fifth(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
    if(temp == -1)
      return -1;
    len += temp;
  }
  else if(car_exp == MACRO)
  {
    len += sprintf(buf+filled_len+len, "if(macro(%d))\n  return 1;\n", exp);
    temp = compile_to_c(car(fifth(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
    if(temp == -1)
      return -1;
    len += temp;
  }
  else if(car_exp == TEST)
  {
    len += sprintf(buf+filled_len+len, "if(get_reg_accumulator() != %d) {\n", NIL);
    temp = compile_to_c(car(CADR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
    if(temp == -1)
      return -1;
    len += temp;
    len += sprintf(buf+filled_len+len, "}\n");
    len += sprintf(buf+filled_len+len, "else {\n");
    temp = compile_to_c(car(CADDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
    if(temp == -1)
      return -1;
    len += temp;
    len += sprintf(buf+filled_len+len, "}\n");
  }
  else if(car_exp == ASSIGN)
  {
    len += sprintf(buf+filled_len+len, "if(assign(%d))\n  return 1;\n",CADR(exp));
    temp = compile_to_c(car(CADDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
    if(temp == -1)
      return -1;
    len += temp;
  }
  else if(car_exp == CONTI)
  {
    len += sprintf(buf+filled_len+len, "if(conti())\n  return 1;\n");
    temp = compile_to_c(car(CADR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
    if(temp == -1)
      return -1;
    len += temp;
  }
  else if (car_exp == FRAME)
  {
    //TODO: need to replace this with calls to compile_to_c() for car(CADR(exp)) and car(CADDR(exp))
    len += sprintf(buf+filled_len+len, "if(frame(%d))\n  return 1;\n", exp);
    temp = compile_to_c(car(CADDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
    if(temp == -1)
      return -1;
    len += temp;
  }
  else if(car_exp == ARGUMENT)
  {
    len += sprintf(buf+filled_len+len, "if(argument())\n  return 1;\n");
    temp = compile_to_c(car(CADR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
    if(temp == -1)
      return -1;
    len += temp;
  }
  else if(car_exp == RETURN)
  {
    len += sprintf(buf+filled_len+len, "if(return_op())\n return 1;\n");
  }
  else if(car_exp == HALT)
  {
    len += sprintf(buf+filled_len+len, "if(halt_op())\n  return 1;\n");
  }
  else if(car_exp == DEFINE)
  {
    len += sprintf(buf+filled_len+len, "if(define(%d))\n return 1;\n", CADR(exp));
    temp = compile_to_c(car(CADDR(exp)), buf, filled_len+len, err_buf, called_closures, nof_called_closures);
    if(temp == -1)
      return -1;
    len += temp;
  }
  else
  {
    assert(false);
  }

  return len;
}

TCCState *create_tcc_state()
{
  TCCState *tcc_state = tcc_new();

  if (!tcc_state)
  {
    fprintf(stderr, "Could not create tcc state\n");
    return NULL;
  }

  tcc_set_output_type(tcc_state, TCC_OUTPUT_MEMORY);

  tcc_add_symbol(tcc_state, "refer",                         refer);
  tcc_add_symbol(tcc_state, "assign",                        assign);
  tcc_add_symbol(tcc_state, "define",                        define);
  tcc_add_symbol(tcc_state, "add",                           add);
  tcc_add_symbol(tcc_state, "sub",                           sub);
  tcc_add_symbol(tcc_state, "eq",                            eq);
  tcc_add_symbol(tcc_state, "constant",                      constant);
  tcc_add_symbol(tcc_state, "closure",                       closure);
  tcc_add_symbol(tcc_state, "macro",                         macro);
  tcc_add_symbol(tcc_state, "get_reg_accumulator",           get_reg_accumulator);
  tcc_add_symbol(tcc_state, "conti",                         conti);
  tcc_add_symbol(tcc_state, "frame",                         frame);
  tcc_add_symbol(tcc_state, "argument",                      argument);
  tcc_add_symbol(tcc_state, "return_op",                     return_op);
  tcc_add_symbol(tcc_state, "halt_op",                       halt_op);
  tcc_add_symbol(tcc_state, "bind_formal_parameters",        bind_formal_parameters);

  tcc_add_symbol(tcc_state, "break1",                        break1);
  tcc_add_symbol(tcc_state, "cons_compiled",                 cons_compiled);

  tcc_add_symbol(tcc_state, "neq",                            neq);
  tcc_add_symbol(tcc_state, "not",                            not);
  tcc_add_symbol(tcc_state, "atom",                           atom);
  tcc_add_symbol(tcc_state, "car_compiled",                   car_compiled);
  tcc_add_symbol(tcc_state, "cdr_compiled",                   cdr_compiled);
  tcc_add_symbol(tcc_state, "mult",                           mult);
  tcc_add_symbol(tcc_state, "div_compiled",                   div_compiled);
  tcc_add_symbol(tcc_state, "error",                          error);
  tcc_add_symbol(tcc_state, "print",                          print);
  tcc_add_symbol(tcc_state, "newline",                        newline);

  tcc_add_symbol(tcc_state, "lst",                            lst);
  tcc_add_symbol(tcc_state, "backquote",                      backquote);
  tcc_add_symbol(tcc_state, "listp",                          listp);
  tcc_add_symbol(tcc_state, "symbol_value",                   symbol_value);
  tcc_add_symbol(tcc_state, "gt",                             gt);
  tcc_add_symbol(tcc_state, "lt",                             lt);
  tcc_add_symbol(tcc_state, "leq",                            leq);
  tcc_add_symbol(tcc_state, "geq",                            geq);
  tcc_add_symbol(tcc_state, "gensym_compiled",                gensym_compiled);
  tcc_add_symbol(tcc_state, "setcar",                         setcar);

  tcc_add_symbol(tcc_state, "setcdr",                         setcdr);
  tcc_add_symbol(tcc_state, "create_package_compiled",        create_package_compiled);
  tcc_add_symbol(tcc_state, "in_package",                     in_package);
  tcc_add_symbol(tcc_state, "expand_macro",                   expand_macro);
  tcc_add_symbol(tcc_state, "apply",                          apply);
  tcc_add_symbol(tcc_state, "string",                         string);
  tcc_add_symbol(tcc_state, "make_array",                     make_array);
  tcc_add_symbol(tcc_state, "array_set",                      array_set);
  tcc_add_symbol(tcc_state, "array_get",                      array_get);
  tcc_add_symbol(tcc_state, "sub_array",                      sub_array);

  tcc_add_symbol(tcc_state, "array_length",                   array_length);
  tcc_add_symbol(tcc_state, "print_string_compiled",          print_string_compiled);
  tcc_add_symbol(tcc_state, "create_image_compiled",          create_image_compiled);
  tcc_add_symbol(tcc_state, "load_foreign_library_compiled",  load_foreign_library_compiled);
  tcc_add_symbol(tcc_state, "call_foreign_function_compiled", call_foreign_function_compiled);
  tcc_add_symbol(tcc_state, "env",                            env);
  tcc_add_symbol(tcc_state, "eval_compiled",                  eval_compiled);
  tcc_add_symbol(tcc_state, "time_compiled",                  time_compiled);
  tcc_add_symbol(tcc_state, "profile",                        profile);
  tcc_add_symbol(tcc_state, "resume_compiled",                resume_compiled);

  tcc_add_symbol(tcc_state, "backtrace",                      backtrace);
  tcc_add_symbol(tcc_state, "load_file",                      load_file);
  tcc_add_symbol(tcc_state, "consp",                          consp);
  tcc_add_symbol(tcc_state, "integerp",                       integerp);
  tcc_add_symbol(tcc_state, "floatp",                         floatp);
  tcc_add_symbol(tcc_state, "characterp",                     characterp);
  tcc_add_symbol(tcc_state, "symbolp",                        symbolp);
  tcc_add_symbol(tcc_state, "stringp",                        stringp);
  tcc_add_symbol(tcc_state, "arrayp",                         arrayp);
  tcc_add_symbol(tcc_state, "closurep",                       closurep);

  tcc_add_symbol(tcc_state, "macrop",                         macrop);
  tcc_add_symbol(tcc_state, "continuationp",                  continuationp);
  tcc_add_symbol(tcc_state, "lambda_expression",              lambda_expression);
  tcc_add_symbol(tcc_state, "format_compiled",                format_compiled);
  tcc_add_symbol(tcc_state, "clone",                          clone);
  tcc_add_symbol(tcc_state, "return_from",                    return_from);
  tcc_add_symbol(tcc_state, "compile_compiled",               compile_compiled);
  tcc_add_symbol(tcc_state, "symbl",                          symbl);
  tcc_add_symbol(tcc_state, "symbol_name",                    symbol_name);
  tcc_add_symbol(tcc_state, "unbind",                         unbind);
  tcc_add_symbol(tcc_state, "abort_compiled",                 abort_compiled);
  tcc_add_symbol(tcc_state, "save_object",                    save_object);
  tcc_add_symbol(tcc_state, "load_object",                    load_object);
  tcc_add_symbol(tcc_state, "compilefn",                      compilefn);

  tcc_add_symbol(tcc_state, "apply_compiled",                 apply_compiled);

  /* if(!tcc_states) */
  /* { */
  /*   tcc_states = (TCCState **)malloc(sizeof(TCCState *)); */
  /*   nof_tcc_states = 1; */
  /*   tcc_states[0] = tcc_state; */
  /* } */
  /* else */
  /* { */
  /*   nof_tcc_states++; */
  /*   tcc_states = (TCCState **)realloc(tcc_states, nof_tcc_states * sizeof(TCCState *)); */
  /*   tcc_states[nof_tcc_states-1] = tcc_state; */
  /* } */

  return tcc_state;
}

cmpfn compile_closure(OBJECT_PTR fn, char *err_buf)
{
  //if the closure has already been compiled,
  //our work is done
  hashtable_entry_t *e = hashtable_get(native_functions, (void *)fn);
  if(e)
    return (cmpfn)e->value;

  char buf[MAX_STRING_LENGTH];
  memset(buf, MAX_STRING_LENGTH, '\0');

  char fname[20];
  memset(fname, 20, '\0');

  sprintf(fname, "f_%d", fn);

  unsigned int len = 0;

  unsigned int temp;

  OBJECT_PTR *called_closures = NULL;
  unsigned int nof_called_closures = 0;

  int i;

  TCCState *tcc_state = create_tcc_state();

  len += sprintf(buf, "unsigned int %s() {\n", fname);

  temp = compile_to_c(car(get_body_object(fn)), buf, len, err_buf, &called_closures, &nof_called_closures);
  if(temp == -1)
  {
    if(called_closures)
      free(called_closures);
    return NULL;
  }
  len += temp;

  len += sprintf(buf+len, "return 0;\n");
  sprintf(buf+len, "}");

  char cname[20];

  for(i=0; i<nof_called_closures; i++)
  {
    memset(cname, 20, '\0');
    sprintf(cname, "f_%d", called_closures[i]);

    if(called_closures[i] != fn) //i.e. it's not a self-recursive call
    {
      if(!tcc_get_symbol(tcc_state, cname)) //symbol for the closure not already defined
      {
	hashtable_entry_t *e = hashtable_get(native_functions, (void *)called_closures[i]);

	if(e) //closure already compiled
	  tcc_add_symbol(tcc_state, cname, (cmpfn)(e->value));
	else
	  tcc_add_symbol(tcc_state, cname, dummy);
      }
    }
  }

  if(tcc_compile_string(tcc_state, buf) == -1)
  {
    sprintf(err_buf, "tcc_compile_string() failed");
    if(called_closures)
      free(called_closures);
    return NULL;
  }

  if(tcc_relocate(tcc_state, TCC_RELOCATE_AUTO) < 0)
  {
    sprintf(err_buf, "tcc_relocate() failed");
    if(called_closures)
      free(called_closures);
    return NULL;
  }

  for(i=0; i<nof_called_closures; i++)
  {
    //ignore self-recursive calls
    if(called_closures[i] == fn)
      continue;

    memset(cname, 20, '\0');
    sprintf(cname, "f_%d", called_closures[i]);    

    hashtable_entry_t *e = hashtable_get(native_functions, (void *)called_closures[i]);

    if(!e) //if closure not already compiled
    {
      cmpfn fn1 = compile_closure(called_closures[i], err_buf);

      if(!fn1)
      {
	if(called_closures)
	  free(called_closures);
	return NULL;      
      }

      //add the freshly produced function pointer
      if(!tcc_get_symbol(tcc_state, cname))
	tcc_add_symbol(tcc_state, cname, fn1);
    }
    else
    {
      //add the function pointer corresponding to the compiled closure
      //(if it's not been added already)
      if(!tcc_get_symbol(tcc_state, cname))
	tcc_add_symbol(tcc_state, cname, (cmpfn)(e->value));
    }
  }

  if(called_closures)
    free(called_closures);

  cmpfn ret = tcc_get_symbol(tcc_state, fname);

  assert(ret != NULL);

  hashtable_put(native_functions, (void *)fn, (void *)ret);

  return ret;
}

void initialize_tcc()
{
  /* tcc_states = NULL; */
  /* nof_tcc_states = 0; */
  tcc_state = create_tcc_state();

  native_functions = hashtable_create(1001);
}

void cleanup_tcc()
{
  /* int i; */
  /* for(i=0; i<nof_tcc_states; i++) */
  /*   tcc_delete(tcc_states[i]); */
  tcc_delete(tcc_state);

  hashtable_delete(native_functions);
}
