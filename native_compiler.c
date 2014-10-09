#include <stdio.h>
#include <stdint.h>
#include <assert.h> 

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

extern OBJECT_PTR top_level_env;

extern OBJECT_PTR reg_accumulator;
extern OBJECT_PTR reg_next_expression;
extern OBJECT_PTR reg_current_env;
extern OBJECT_PTR reg_current_value_rib;
extern OBJECT_PTR reg_current_stack;

extern OBJECT_PTR continuations_map;

extern OBJECT_PTR debug_env;
extern BOOLEAN debug_mode;

extern unsigned int current_package;
extern package_t *packages;

extern OBJECT_PTR CONS_RETURN_NIL;

extern OBJECT_PTR TRUE;

extern unsigned int POINTER_MASK;

extern OBJECT_PTR first(OBJECT_PTR);
extern OBJECT_PTR second(OBJECT_PTR);
extern OBJECT_PTR third(OBJECT_PTR);
extern OBJECT_PTR fourth(OBJECT_PTR);

TCCState **tcc_states = NULL;
unsigned int nof_tcc_states = 0;

hashtable_t *native_functions;

unsigned int compile_to_c(OBJECT_PTR, char *, unsigned int, char *, TCCState *, OBJECT_PTR **, unsigned int *);

cmpfn compile_function(OBJECT_PTR, char *);

int dummy()
{
  return 0;
}

int refer(OBJECT_PTR sym)
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

void constant(OBJECT_PTR c)
{
  reg_accumulator = c;
}

void closure(OBJECT_PTR exp)
{
  reg_accumulator = create_closure_object(reg_current_env, 
					  second(exp), 
					  third(exp), 
					  fourth(exp));
}

OBJECT_PTR get_reg_accumulator()
{
  return reg_accumulator;
}

void macro(OBJECT_PTR exp)
{
  reg_accumulator = create_macro_object(reg_current_env, 
				        second(exp), 
					third(exp), 
					fourth(exp));
}

int assign(OBJECT_PTR sym)
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

void conti()
{
  reg_accumulator = create_current_continuation();
  reg_current_value_rib = NIL;
}

void frame(OBJECT_PTR exp)
{
  reg_current_stack = cons(create_call_frame(CADR(exp),
					     reg_current_env,
					     reg_current_value_rib,
					     cdr(reg_next_expression)),
			   reg_current_stack);

  reg_current_value_rib = NIL;
}

void argument()
{
  reg_current_value_rib = cons(reg_accumulator, reg_current_value_rib);
}

void return_op()
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
}

void halt_op()
{
  reg_next_expression = NIL;
}

void define(OBJECT_PTR sym)
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
}

int add()
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

int sub()
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

int eq()
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

unsigned int compile_to_c(OBJECT_PTR exp, 
			  char *buf, 
			  unsigned int filled_len, 
			  char *err_buf, 
			  TCCState *s, 
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
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, s, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == SUB)
        {
	  len += sprintf(buf+filled_len+len, "if(sub())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, s, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	else if(CADR(exp) == EQ)
        {
	  len += sprintf(buf+filled_len+len, "if(eq())\n  return 1;\n");
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, s, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
	//TODO: other special operators
	else //it's a user-defined closure object
        {
	  //TODO: does calling refer() to get at the closure object
	  //work for all cases?

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

	  //refer() places the closure object in reg_accumulator, this may
	  //be a problem if the contents of reg_accumulator is needed elsewhere,
	  //so we preserve its contents

	  fn_object = reg_accumulator;
	  reg_accumulator = prev_reg_accumulator;

	  hashtable_entry_t *e = hashtable_get(native_functions, (void *)fn_object);

	  if(e)
	  {
	    //add the symbol corresponding to this closure object
	    //to this function's TCCState
	    char fname[20];
	    memset(fname, 20, '\0');
	    sprintf(fname, "f_%d", fn_object);
	    tcc_add_symbol(s, fname, e->value);
	  }
	  else
	  {
	    if(*called_closures == NULL)
	    {
	      *called_closures = (OBJECT_PTR *)malloc(sizeof(OBJECT_PTR));
	      *nof_called_closures = 1;
	      *called_closures[0] = fn_object;
	    }
	    else
	    {
	      *nof_called_closures++;
	      *called_closures = (OBJECT_PTR *)realloc(*called_closures, *nof_called_closures * sizeof(OBJECT_PTR));
	      *called_closures[*nof_called_closures-1] = fn_object;	      
	    }
	  }

	  len += sprintf(buf+filled_len+len, "bind_formal_parameters(%d);\n", fn_object);

	  len += sprintf(buf+filled_len+len, "if(f_%d())\n  return;\n", fn_object);
	  temp = compile_to_c(car(CADDDR(exp)), buf, filled_len+len, err_buf, s, called_closures, nof_called_closures);
	  if(temp == -1)
	    return -1;
	  len += temp;
	}
      }
    }
    else
    {
      len += sprintf(buf+filled_len+len, "if(refer(%d))\n  return 1;\n", CADR(exp));
      temp = compile_to_c(car(CADDR(exp)), buf, filled_len+len, err_buf, s, called_closures, nof_called_closures);
      if(temp == -1)
	return -1;
      len += temp;
    }
  }
  else if(car_exp == CONSTANT)
  {
    len += sprintf(buf+filled_len+len, "constant(%d);\n", CADR(exp));
    temp = compile_to_c(car(car(CDDR(exp))), buf, filled_len+len, err_buf, s, called_closures, nof_called_closures);
    if(temp == -1)
      return -1;
    len += temp;
  }
  else if(car_exp == CLOSE)
  {
    len += sprintf(buf+filled_len+len, "closure(%d);\n", exp);
    temp = compile_to_c(fifth(exp), buf, filled_len+len, err_buf, s, called_closures, nof_called_closures);
    if(temp == -1)
      return -1;
    len += temp;
  }
  else if(car_exp == MACRO)
  {
    len += sprintf(buf+filled_len+len, "macro(%d);\n", exp);
    temp = compile_to_c(fifth(exp), buf, filled_len+len, err_buf, s, called_closures, nof_called_closures);
    if(temp == -1)
      return -1;
    len += temp;
  }
  else if(car_exp == TEST)
  {
    len += sprintf(buf+filled_len+len, "if(get_reg_accumulator() != %d) {\n", NIL);
    temp = compile_to_c(car(CADR(exp)), buf, filled_len+len, err_buf, s, called_closures, nof_called_closures);
    if(temp == -1)
      return -1;
    len += temp;
    len += sprintf(buf+filled_len+len, "}\n");
    len += sprintf(buf+filled_len+len, "else {\n");
    temp = compile_to_c(car(CADDR(exp)), buf, filled_len+len, err_buf, s, called_closures, nof_called_closures);
    if(temp == -1)
      return -1;
    len += temp;
    len += sprintf(buf+filled_len+len, "}\n");
  }
  else if(car_exp == ASSIGN)
  {
    len += sprintf(buf+filled_len+len, "if(assign(%d))\n  return 1;\n",CADR(exp));
    temp = compile_to_c(CADDR(exp), buf, filled_len+len, err_buf, s, called_closures, nof_called_closures);
    if(temp == -1)
      return -1;
    len += temp;
  }
  else if(car_exp == CONTI)
  {
    len += sprintf(buf+filled_len+len, "conti();\n");
    temp = compile_to_c(CADR(exp), buf, filled_len+len, err_buf, s, called_closures, nof_called_closures);
    if(temp == -1)
      return -1;
    len += temp;
  }
  else if (car_exp == FRAME)
  {
    len += sprintf(buf+filled_len+len, "frame(%d);\n", exp);
    temp = compile_to_c(car(CADDR(exp)), buf, filled_len+len, err_buf, s, called_closures, nof_called_closures);
    if(temp == -1)
      return -1;
    len += temp;
  }
  else if(car_exp == ARGUMENT)
  {
    len += sprintf(buf+filled_len+len, "argument();\n");
    temp = compile_to_c(car(CADR(exp)), buf, filled_len+len, err_buf, s, called_closures, nof_called_closures);
    if(temp == -1)
      return -1;
    len += temp;
  }
  else if(car_exp == RETURN)
  {
    len += sprintf(buf+filled_len+len, "return_op();\n");
  }
  else if(car_exp == HALT)
  {
    len += sprintf(buf+filled_len+len, "halt_op();\n");
  }
  else if(car_exp == DEFINE)
  {
    len += sprintf(buf+filled_len+len, "define(%d);\n", CADR(exp));
    temp = compile_to_c(CADDR(exp), buf, filled_len+len, err_buf, s, called_closures, nof_called_closures);
    if(temp == -1)
      return -1;
    len += temp;
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

  tcc_add_symbol(tcc_state, "refer",                  refer);
  tcc_add_symbol(tcc_state, "assign",                 assign);
  tcc_add_symbol(tcc_state, "define",                 define);
  tcc_add_symbol(tcc_state, "add",                    add);
  tcc_add_symbol(tcc_state, "sub",                    sub);
  tcc_add_symbol(tcc_state, "eq",                     eq);
  tcc_add_symbol(tcc_state, "constant",               constant);
  tcc_add_symbol(tcc_state, "closure",                closure);
  tcc_add_symbol(tcc_state, "macro",                  macro);
  tcc_add_symbol(tcc_state, "get_reg_accumulator",    get_reg_accumulator);
  tcc_add_symbol(tcc_state, "conti",                  conti);
  tcc_add_symbol(tcc_state, "frame",                  frame);
  tcc_add_symbol(tcc_state, "argument",               argument);
  tcc_add_symbol(tcc_state, "return_op",              return_op);
  tcc_add_symbol(tcc_state, "halt_op",                halt_op);
  tcc_add_symbol(tcc_state, "bind_formal_parameters", bind_formal_parameters);

  if(!tcc_states)
  {
    tcc_states = (TCCState **)malloc(sizeof(TCCState *));
    nof_tcc_states = 1;
    tcc_states[0] = tcc_state;
  }
  else
  {
    nof_tcc_states++;
    tcc_states = (TCCState **)realloc(tcc_states, nof_tcc_states * sizeof(TCCState *));
    tcc_states[nof_tcc_states-1] = tcc_state;
  }

  return tcc_state;
}

cmpfn compile_function(OBJECT_PTR fn, char *err_buf)
{
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

  temp = compile_to_c(car(get_body_object(fn)), buf, len, err_buf, tcc_state, &called_closures, &nof_called_closures);
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

    tcc_add_symbol(tcc_state, cname, dummy);
  }

  if (tcc_compile_string(tcc_state, buf) == -1)
  {
    sprintf(err_buf, "tcc_compile_string() failed");
    if(called_closures)
      free(called_closures);
    return NULL;
  }

  for(i=0; i<nof_called_closures; i++)
  {
    memset(cname, 20, '\0');
    sprintf(cname, "f_%d", called_closures[i]);    

    cmpfn fn1 = compile_function(called_closures[i], err_buf);

    if(!fn1)
    {
      if(called_closures)
	free(called_closures);
      return NULL;      
    }

    tcc_add_symbol(tcc_state, cname, fn1);
  }

  if (tcc_relocate(tcc_state, TCC_RELOCATE_AUTO) < 0)
  {
    sprintf(err_buf, "tcc_relocate() failed");
    if(called_closures)
      free(called_closures);
    return NULL;
  }

  if(called_closures)
    free(called_closures);

  cmpfn ret = tcc_get_symbol(tcc_state, fname);

  hashtable_put(native_functions, (void *)fn, (void *)ret);

  return ret;
}

void initialize_tcc()
{
  tcc_states = NULL;
  nof_tcc_states = 0;

  native_functions = hashtable_create(1001);
}

void cleanup_tcc()
{
  int i;
  for(i=0; i<nof_tcc_states; i++)
    tcc_delete(tcc_states[i]);

  hashtable_delete(native_functions);
}
