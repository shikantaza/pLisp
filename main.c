#include <stdio.h>
#include <assert.h>
#include <string.h>

#include "plisp.h"
#include "util.h"

expression_t *g_expr = NULL;

extern void yyparse();

//these are the variables that
//should be serialized to
//implement images

int nof_strings = 0;
char **strings = NULL;

OBJECT_PTR init_env_list;

//RAW_PTR heap_ptr = 0;
//RAW_PTR *heap = NULL;
extern RAW_PTR *heap;

unsigned int current_package;
unsigned int nof_packages;
package_t *packages;

int gen_sym_count = 0;

//end of variables that should
//be serialized to implement images

//standard objects
OBJECT_PTR TRUE  =                       SYMBOL_TAG;
OBJECT_PTR NIL   = (1 << SYMBOL_SHIFT) + SYMBOL_TAG;
OBJECT_PTR QUOTE = (2 << SYMBOL_SHIFT) + SYMBOL_TAG;

extern FILE *yyin;

#define NOF_SPECIAL_SYMBOLS 52

BOOLEAN in_exception = false;
OBJECT_PTR execution_stack;
char err_buf[500];

#define SYMBOL_STRING_SIZE 100

#define CORE_PACKAGE_INDEX 0

//constants defined for speed and clarity
#define TWO_RAISED_TO_27 134217728
#define TWO_RAISED_TO_28 268435456
#define TWO_RAISED_TO_SYMBOL_BITS_MINUS_1 4194303

BOOLEAN debug_mode = false;
OBJECT_PTR debug_env;
BOOLEAN abort_debug = false;

//used to keep track of when
//to exit debugging mode, also
//maybe useful when implementing continuations
OBJECT_PTR root_form;

int nof_dl_handles = 0;
void **dl_handles = NULL;

extern struct node *white;

void initialize()
{

  initialize_heap();

  initialize_free_list();

  nof_packages = 0;

  create_package("CORE");

  initialize_core_package();

  //dummy object to circumvent the issue with add_to_environment
  //choking on a NIL environment)
  init_env_list = cons(cons(cons(get_symbol_object("DUMMY"),
				 NIL), 
			    NIL),
		       NIL);

  reset_exception_mechanism();
}

int add_string(char *str)
{
  log_function_entry("add_string");

  nof_strings++;

  char **temp = (char **)realloc(strings, nof_strings * sizeof(char *));

  if(temp != NULL)
    strings = temp;
  else
  {
    fprintf(stderr, "Out of memory extending strings space\n");
    cleanup();
    exit(1);
  }

  strings[nof_strings - 1] = strdup(str);

  log_function_exit("add_string");

  return nof_strings - 1;
}


int add_symbol(char *sym)
{
  log_function_entry("add_symbol");

  packages[current_package].nof_symbols++;
  
  char **temp = (char **)realloc(packages[current_package].symbols, packages[current_package].nof_symbols * sizeof(char *));

  if(temp != NULL)
      packages[current_package].symbols = temp;
  else
    {
      fprintf(stderr, "Out of memory extending symbol space\n");
      cleanup();
      exit(1);
    }

  packages[current_package].symbols[packages[current_package].nof_symbols - 1] = strdup(sym);

  log_function_exit("add_symbol");

  return packages[current_package].nof_symbols - 1;
}

int find_string(char *str)
{
  int i;

  for(i=0; i<nof_strings; i++)
  {
    if(!strcmp(strings[i],str))
      return i;
  }

  return NOT_FOUND;
}

int find_symbol(char *sym, int package_index)
{
  log_function_entry("find_symbol");
  OBJECT_PTR ret;
  BOOLEAN found = false;

  int i;

  for(i=0; i<packages[package_index].nof_symbols; i++)
  {
    if(!strcmp(packages[package_index].symbols[i],sym))
    {
      ret = i;
      found = true;
      break;
    };
  }

  if(!found)
    ret = NOT_FOUND;

  log_function_exit("find_symbol");

  return ret;
}

expression_t *create_expression(int type, char *char_val, int int_val, float float_val, int nof_elements)
{
  expression_t *e = (expression_t *)malloc(sizeof(expression_t));

  e->type = type;

  if(type == SYMBOL)
  {
    if(char_val[0] == ':')
    {
      e->package_name = NULL;
      e->atom_value = strdup(char_val);
    }
    else
    {
      char *temp = strdup(char_val);
      char *res = strtok(temp, ":");

      if(strlen(res) == strlen(char_val)) //no colon
      {
	e->package_name = NULL;
	e->atom_value = strdup(char_val);
      }
      else //colon present
      {
	e->package_name = strdup(res);
	e->atom_value = strdup(strtok(NULL, ":"));
      }
      free(temp);
    }
  }
  else if(type == INTEGER)
    e->integer_value = int_val;
  else if(type == FLOAT)
    e->float_value = float_val;
  else if(type == STRING_LITERAL)
    e->atom_value = strdup(char_val);
  else if(type == CHARACTER)
    e->char_value = char_val[2]; //the first two characters are '#' and '\'
  else if(type == LIST)
  {
    e->nof_elements = nof_elements;
    if(nof_elements > 0)
      e->elements = (expression_t **)malloc(nof_elements * sizeof(expression_t *));
    else
      e->elements = NULL;
  }
  else
  {
    fprintf(stderr, "create_expression(): unknown type!\n");
    cleanup();
    exit(1);
  }
  
  return e;
}

void delete_expression(expression_t *e)
{

  log_function_entry("delete_expression");

#ifdef DEBUG
  print_expression(e);
  fprintf(stdout, "\n");
#endif

  int i;

  if(e->type == SYMBOL)
  {
    free(e->atom_value);
    if(e->package_name != NULL)
      free(e->package_name);
  }
  else if(e->type == STRING_LITERAL)
    free(e->atom_value);
  else if(e->type == LIST)
  {
    for(i=0; i<e->nof_elements; i++)
      delete_expression(e->elements[i]);
  }

  free(e);

  log_function_exit("delete_expression");

}

void print_expression(expression_t *e)
{
  int i;

  if(e->type == SYMBOL)
  {
    if(e->package_name == NULL)
      fprintf(stdout, "%s ", e->atom_value);
    else 
    fprintf(stdout, "%s:%s ", e->package_name, e->atom_value);
  }
  else if(e->type == INTEGER)
    fprintf(stdout, "%d ", e->integer_value);
  else if(e->type == FLOAT)
    fprintf(stdout, "%f ", e->float_value);
  else if(e->type == STRING_LITERAL)
    fprintf(stdout, "%s ", e->atom_value);
  else if(e->type == CHARACTER)
    fprintf(stdout, "%c ", e->char_value);
  else if(e->type == LIST)
  {
    fprintf(stdout, "(");
    
    for(i=0; i<e->nof_elements; i++)
      print_expression(e->elements[i]);

    if(e->nof_elements > 0)
      fprintf(stdout, "\b");

    fprintf(stdout, ") ");
  }
  else
  {
    fprintf(stderr, "print_expression(): unknown type!\n");
    cleanup();
    exit(1);
  }
}

void repl()
{
  yyparse();

  if(debug_mode)
  {
    if(g_expr->type == SYMBOL)
    {
      if(!strcmp(g_expr->atom_value, "RESUME"))
      {
	fprintf(stdout, "Exiting debug mode.\n");
	debug_mode = false;
	return;
      }
      else if(!strcmp(g_expr->atom_value, "STEP"))
      {
	//do nothing; debug_mode will take care of the stepping
	return;
      }
      else if(!strcmp(g_expr->atom_value, "ABORT"))
      {
	abort_debug = true;
	return;
      }
      else if(!strcmp(g_expr->atom_value,"QUIT") ||
	      !strcmp(g_expr->atom_value,"EXIT") ||
	      !strcmp(g_expr->atom_value,"Q"))
      {
	fprintf(stdout, "Bye.\n");
	cleanup();
	exit(0);
      }
      else
      {
	OBJECT_PTR sym = convert_expression_to_object(g_expr);
	OBJECT_PTR res = get_symbol_value(sym, debug_env);

	if(car(res) == NIL)
	{
	  fprintf(stdout, "Symbol not bound: ");
	  print_object(sym);
	}
	else
	  print_object(cdr(res));

	prompt();
	//yyparse();
	repl();
	return;
      }
    }
    else if(g_expr->type == LIST && 
	  g_expr->elements[0]->type == SYMBOL &&
	  !strcmp(g_expr->elements[0]->atom_value, "SET"))
    {
      print_object(evaluate_expression(g_expr, debug_env));
      prompt();
      //yyparse();      
      return;
    }
    else
    {
      fprintf(stdout, "Only forms allowed in debug mode are SET forms and atoms\n");
      prompt();
      //yyparse();
      return;
    }
    return;
  } //end of if(debug_mode)

  if(g_expr->type == SYMBOL && ( !strcmp(g_expr->atom_value,"QUIT") ||
				 !strcmp(g_expr->atom_value,"EXIT") ||
				 !strcmp(g_expr->atom_value,"Q") ))
  {
    fprintf(stdout, "Bye.\n");
    cleanup();
    exit(0);
  }
  else if(g_expr->type == LIST && 
	  g_expr->elements[0]->type == SYMBOL &&
	  !strcmp(g_expr->elements[0]->atom_value, "LOAD-FILE"))
  {
    FILE *next_yyin = yyin;

    yyin = fopen(g_expr->elements[1]->atom_value,"r");

    if(yyin == NULL)
    {
      fprintf(stdout, "Unable to open file \"%s\"\n", g_expr->elements[1]->atom_value);
      yyin = next_yyin;
      prompt();
      //yyparse();
      return;
    }
    else
      //yyparse();
      return;
  }
  else
  {

#ifdef DEEP_DEBUG
    print_expression(g_expr);
#else

    OBJECT_PTR value = evaluate_expression(g_expr, init_env_list);

    if(in_exception)
    {
      fprintf(stdout, "Error: %s", err_buf);

      print_stack_trace();
    }
    else if(abort_debug)
    {
      debug_mode = false;
      abort_debug = false;
    }
    else
    {
      if(yyin == stdin)
	print_object(value);
    }

#endif

    delete_expression(g_expr);

    reset_exception_mechanism();

    gc();

    if(yyin == stdin)
      prompt();

    //yyparse();
    return;
  }
}

void prompt()
{
  if(!debug_mode)
    fprintf(stdout, "\n%s> ", packages[current_package].name);
  else
    fprintf(stdout, "\nDEBUG> ");
}

void cleanup()
{

  int i,j;

  log_function_entry("cleanup");

  fclose(yyin);

  delete_expression(g_expr);

  for(i=0; i<nof_packages; i++)
  {
    free(packages[i].name);

    for(j=0; j<packages[i].nof_symbols; j++)
      free(packages[i].symbols[j]);

    free(packages[i].symbols);
  }

  if(heap)
    free(heap);

  free(dl_handles);

  log_function_exit("cleanup");
}

void welcome()
{
  fprintf(stdout, "Welcome to PLISP. Type 'quit' to exit.");
  //prompt();
}

int main(int argc, char **argv)
{

  if(argc == 1)
    initialize();
  else
    load_from_image(argv[1]);

  welcome();
  
  yyin = fopen("plisp.lisp", "r");

  if(!yyin)
  {
    fprintf(stderr, "Unable to open file plisp.lisp; exiting\n");
    cleanup();
    exit(1);
  }
  //yyin = stdin;

  //yyparse();
  while(1)
  {
    repl();
  }

  cleanup(); //don't think this ever gets called

}

void print_object(OBJECT_PTR obj_ptr)
{
  log_function_entry("print_object");

  if(IS_SYMBOL_OBJECT(obj_ptr))
  {
    int package_index = obj_ptr >> (SYMBOL_BITS + SYMBOL_SHIFT);

    if(package_index == current_package)
      fprintf(stdout, "%s", get_symbol_name(obj_ptr));
    else
    {
      char buf[SYMBOL_STRING_SIZE];
      print_symbol(obj_ptr, buf);
      fprintf(stdout, "%s", buf);
    }
  }
  else if(IS_CONS_OBJECT(obj_ptr))
    print_cons_object(obj_ptr);
  else if(IS_FN_OBJECT(obj_ptr))
    print_function_object(obj_ptr);
  else if(IS_MACRO_OBJECT(obj_ptr))
    print_macro_object(obj_ptr);
  else if(IS_INTEGER_OBJECT(obj_ptr))
    fprintf(stdout, "%d", get_int_value(obj_ptr));
  else if(IS_FLOAT_OBJECT(obj_ptr))
    fprintf(stdout, "%f", get_float_value(obj_ptr));
  else if(IS_STRING_LITERAL_OBJECT(obj_ptr))
    fprintf(stdout, "\"%s\"", strings[obj_ptr >> STRING_LITERAL_SHIFT]);
  else if(IS_CHAR_OBJECT(obj_ptr))
    fprintf(stdout, "#\\%c", obj_ptr >> CHAR_SHIFT);
  else if(IS_ARRAY_OBJECT(obj_ptr))
  {
    if(is_string_object(obj_ptr))
       print_string(obj_ptr);
    else
      print_array_object(obj_ptr);
  }

  log_function_exit("print_object");
}

OBJECT_PTR evaluate_expression(expression_t *e, OBJECT_PTR env_list)
{
  log_function_entry("evaluate_expression");

  OBJECT_PTR obj = convert_expression_to_object(e);

  root_form = obj;

  OBJECT_PTR ret = eval(obj, env_list);

  log_function_exit("evaluate_expression");

  return ret;
}

/*
RAW_PTR object_alloc(int size)
{
  log_function_entry("object_alloc");

  void *temp = realloc(heap, (heap_ptr + size) * sizeof(RAW_PTR));

  if(temp != NULL)
    heap = temp;
  else
  {
    fprintf(stderr, "object_alloc(): out of memory");
    cleanup();
    exit(1);
  }

  RAW_PTR old_heap_ptr = heap_ptr;

  heap_ptr += size;

  log_function_exit("object_alloc");

  return old_heap_ptr;
}
*/

OBJECT_PTR cons(OBJECT_PTR car, OBJECT_PTR cdr)
{
  log_function_entry("cons");

  RAW_PTR ptr = object_alloc(2);

  //heap[ptr] = car;
  //heap[ptr+1] = cdr;  
  set_heap(ptr, car);
  set_heap(ptr+1, cdr);

  insert_node(&white, create_node((ptr << CONS_SHIFT) + CONS_TAG));

  log_function_exit("cons");

  return (ptr << CONS_SHIFT) + CONS_TAG;
}

OBJECT_PTR get_string_object(char *str)
{

  log_function_entry("get_string_object");

  int index = find_string(str);

  OBJECT_PTR retval;

  if(index != NOT_FOUND) //string exists in string table
    retval = (index << STRING_LITERAL_SHIFT) + STRING_LITERAL_TAG;
  else
    retval = (add_string(str) << STRING_LITERAL_SHIFT) + STRING_LITERAL_TAG;

  log_function_exit("get_string_object");

  return retval;
}

OBJECT_PTR get_symbol_object(char *symbol_name)
{
  log_function_entry("get_symbol_object");

  int i;
  int package_index = current_package;

  for(i=0; i<packages[CORE_PACKAGE_INDEX].nof_symbols; i++)
  {
    if(!strcmp(symbol_name,packages[CORE_PACKAGE_INDEX].symbols[i]))
    {
      package_index = CORE_PACKAGE_INDEX;
      break;
    }
  }

  int index = find_symbol(symbol_name, package_index);

  OBJECT_PTR retval;

  if(index != NOT_FOUND) //symbol exists in symbol table
    retval = (package_index << (SYMBOL_BITS + SYMBOL_SHIFT)) + (index << SYMBOL_SHIFT) + SYMBOL_TAG;
  else
    retval = (package_index << (SYMBOL_BITS + SYMBOL_SHIFT)) + (add_symbol(symbol_name) << SYMBOL_SHIFT) + SYMBOL_TAG;

  log_function_exit("get_symbol_object");

  return retval;
}

OBJECT_PTR build_list_object(expression_t **e, int size, int n)
{
  log_function_entry("build_list_object");

  OBJECT_PTR ret;

  if(n == (size - 1))
    ret = cons(convert_expression_to_object(e[n]), NIL);
  else
    ret = cons(convert_expression_to_object(e[n]), build_list_object(e, size, n+1));

#ifdef DEBUG
  print_object(ret);
  fprintf(stdout, "\n");
#endif

  log_function_exit("build_list_object");

  return ret;
}

OBJECT_PTR car(OBJECT_PTR cons_obj)
{
  log_function_entry("car");

  //assert(IS_CONS_OBJECT(cons_obj));

  OBJECT_PTR ret = heap[cons_obj >> CONS_SHIFT];

  log_function_exit("car");

  return ret;

}

OBJECT_PTR cdr(OBJECT_PTR cons_obj)
{
  log_function_entry("cdr");

  assert(IS_CONS_OBJECT(cons_obj));

  OBJECT_PTR ret = heap[(cons_obj >> CONS_SHIFT) + 1];

  log_function_exit("cdr");

  return ret;
}

void print_cons_object(OBJECT_PTR obj)
{
  log_function_entry("print_cons_object");

  fflush(stdout);

  assert(IS_CONS_OBJECT(obj));

  OBJECT_PTR car_obj = car(obj);
  OBJECT_PTR cdr_obj = cdr(obj);

  if((is_atom(cdr_obj) || IS_FN_OBJECT(cdr_obj) || IS_MACRO_OBJECT(cdr_obj))  && cdr_obj != NIL)
  {
    fprintf(stdout, "(");
    print_object(car_obj);
    fprintf(stdout, " . ");
    print_object(cdr_obj);
    fprintf(stdout, ")");
  }
  else
  {
    fprintf(stdout, "(");

    OBJECT_PTR rest = obj;

    while(rest != NIL && !(is_atom(rest) || IS_FN_OBJECT(rest) || IS_MACRO_OBJECT(rest)))
    {
      print_object(car(rest));
      fprintf(stdout, " ");
      rest = cdr(rest);
    }

    if((is_atom(rest) || IS_FN_OBJECT(rest) || IS_MACRO_OBJECT(rest)) && rest != NIL)
    {
      fprintf(stdout, " . ");
      print_object(rest);
      fprintf(stdout, ")");
    }
    else
      fprintf(stdout, "\b)");
  }

  log_function_exit("print_cons_object");
}

BOOLEAN is_atom(OBJECT_PTR ptr)
{
  log_function_entry("is_atom");

  BOOLEAN ret = IS_SYMBOL_OBJECT(ptr) || 
                IS_INTEGER_OBJECT(ptr) || 
                IS_FLOAT_OBJECT(ptr) ||
                IS_STRING_LITERAL_OBJECT(ptr) ||
                IS_CHAR_OBJECT(ptr);

  log_function_exit("is_atom");

  return ret;
}

OBJECT_PTR convert_expression_to_object(expression_t *e)
{
  OBJECT_PTR ret;

  log_function_entry("convert_expression_to_object");

  if(e->type == SYMBOL)
  {
    if(e->package_name == NULL)
      ret = get_symbol_object(e->atom_value);
    else
      ret = get_qualified_symbol_object(e->package_name, e->atom_value);
  }
  else if(e->type == INTEGER)
    ret = convert_int_to_object(e->integer_value);
  else if(e->type == FLOAT)
    ret = convert_float_to_object(e->float_value);
  else if(e->type == STRING_LITERAL)
    ret = get_string_object(e->atom_value);
  else if(e->type == CHARACTER)
    ret = (e->char_value << CHAR_SHIFT) + CHAR_TAG;
  else if(e->type == LIST)
  {
    if(e->nof_elements == 0)
      ret = NIL;
    else
    {
      int i;

      OBJECT_PTR cons_obj = cons(convert_expression_to_object(e->elements[e->nof_elements-1]), NIL);

      for(i=e->nof_elements - 2; i>=0; i--)
	cons_obj = cons(convert_expression_to_object(e->elements[i]), cons_obj);

      ret = cons_obj;
    }
  }

#ifdef DEBUG
  print_object(ret);
  fprintf(stdout, "\n");
#endif

  log_function_exit("convert_expression_to_object");

  return ret;
}

//content-based equality check (i.e., EQUAL as oppposed to EQL)
BOOLEAN equal(OBJECT_PTR obj1, OBJECT_PTR obj2)
{
  BOOLEAN ret = false;

  if(obj1 == obj2)
    ret = true;
  else
  {
    if(IS_CONS_OBJECT(obj1) && IS_CONS_OBJECT(obj2))
      ret = (equal(car(obj1), car(obj2)) && 
	     equal(cdr(obj1), cdr(obj2)));
  }
  
  return ret;
}

OBJECT_PTR eval(OBJECT_PTR form, OBJECT_PTR env_list)
{

  log_function_entry("eval");

  if(abort_debug)
  {
    log_function_exit("eval");
    return NIL;
  }

  if(debug_mode)
    debug_env = env_list;

  OBJECT_PTR ret;

  if(in_exception)
  {
    log_function_exit("eval");
    return NIL;
  }

  if(IS_SYMBOL_OBJECT(form))
  {
    if(form == NIL || form == TRUE || is_special_form(form))
      ret = form;
    else
    {
      OBJECT_PTR res = get_symbol_value(form, env_list);
      if(car(res) == NIL)
      {
	char buf[SYMBOL_STRING_SIZE];
	print_symbol(form, buf);
	sprintf(err_buf, "Symbol not bound: %s", buf);
	raise_error();
	ret = NIL;
      }
      else
	ret = cdr(res);
    }
  }
  else if(IS_INTEGER_OBJECT(form) || IS_STRING_LITERAL_OBJECT(form) || IS_CHAR_OBJECT(form) || IS_FLOAT_OBJECT(form))
      ret = form;
  else if(IS_CONS_OBJECT(form))
  {
    if(form == NIL) //check for NIL incorrect here, actually; NIL is a symbol object
      ret = NIL;
    else
    {
      OBJECT_PTR car_obj = car(form);
      if(IS_CONS_OBJECT(car_obj))
      {

	if(execution_stack == NIL)
	  execution_stack = cons(form, NIL);
	else
	  execution_stack = cons(form, execution_stack);

	/*

	OBJECT_PTR fn_obj = eval(car_obj, env_list);

	if(!IS_FN_OBJECT(fn_obj))
	{
	  sprintf(err_buf, "Form does not begin with an operator");
	  raise_error();
	  ret = NIL;
	}
	else
	{
	  OBJECT_PTR args = cdr(form);
	  ret = invoke_function(fn_obj, args, env_list);
	}
	*/

	OBJECT_PTR f = cons(eval(car_obj, env_list), cdr(form));

	ret = eval(f, env_list);
      }
      else if(IS_FN_OBJECT(car_obj))
      {
	OBJECT_PTR args = cdr(form);
	ret = invoke_function(car_obj, args, env_list);
      }
      else if(IS_SYMBOL_OBJECT(car_obj))
      {
	char val[SYMBOL_STRING_SIZE];
	print_symbol(car_obj, val);
	if(!strcmp(val,QUOT))
	  ret = CADR(form);
	else if(!strcmp(val,CONS))
	{
	  OBJECT_PTR v1 = eval(CADR(form), env_list);
	  OBJECT_PTR v2 = eval(CADDR(form), env_list);
	  ret = cons(v1, v2);
	}
	else if(!strcmp(val,ATOM))
	  ret = is_atom(eval(CADR(form), env_list)) ? TRUE : NIL;
	else if(!strcmp(val,EQ))
	  ret = equal(eval(CADR(form), env_list), eval(CADDR(form), env_list)) ? TRUE : NIL;
	else if(!strcmp(val,CAR))
	{
	  OBJECT_PTR obj = eval(CADR(form), env_list);
	  if(!(IS_CONS_OBJECT(obj)))
	  {
	    sprintf(err_buf, "Argument to CAR not a list");
	    raise_error();
	    ret = NIL;
	  }
	  ret = car(obj);
	}
	else if(!strcmp(val,CDR))
	{
	  OBJECT_PTR obj = eval(CADR(form), env_list);
	  if(!(IS_CONS_OBJECT(obj)))
	  {
	    sprintf(err_buf, "Argument to CDR not a list");
	    raise_error();
	    ret = NIL;
	  }
	  return cdr(obj);
	}
	else if(!strcmp(val,COND))
	{
	  OBJECT_PTR cond_clauses = cdr(form);
	  BOOLEAN found = false;
	  while(cond_clauses != NIL)
	  {
	    if(eval(car(car(cond_clauses)), env_list) == TRUE)
	    {
	      ret = eval(CADR(car(cond_clauses)), env_list);
	      found = true;
	      break;
	    }
	    cond_clauses = cdr(cond_clauses);
	  }
	  if(!found)
	    ret = NIL;
	}
	else if(!strcmp(val,LAMBDA))
	  ret = create_function_object(env_list, CADR(form), CDDR(form));
	else if(!strcmp(val,DEFUN))
	{
	  OBJECT_PTR fn_name = CADR(form);
	  add_to_environment(env_list, fn_name, create_function_object(env_list,CADDR(form),CDDDR(form)));
	  ret = fn_name;
	}
	else if(!strcmp(val,DEFMACRO))
	{
	  OBJECT_PTR macro_name = CADR(form);
	  add_to_environment(env_list, macro_name, create_macro_object(env_list,CADDR(form),CADDDR(form)));
	  ret = macro_name;
	}
	else if(!strcmp(val, ADD))
	{
	  float sum = 0;
	  OBJECT_PTR rest = cdr(form);
	  BOOLEAN is_float = false;

	  while(rest != NIL)
	  {
	    OBJECT_PTR v = eval(car(rest), env_list);

	    if(IS_FLOAT_OBJECT(v))
	    {
	      is_float = true;
	      sum += get_float_value(v);
	    }
	    else
	      sum += get_int_value(v);

	    rest = cdr(rest);
	  }

	  if(is_float)
	    ret = convert_float_to_object(sum);
	  else
	    ret = convert_int_to_object((int)sum);
	}
	else if(!strcmp(val, MULT))
	{
	  float sum = 1;
	  OBJECT_PTR rest = cdr(form);
	  BOOLEAN is_float = false;

	  while(rest != NIL)
	  {
	    OBJECT_PTR v = eval(car(rest), env_list);

	    if(IS_FLOAT_OBJECT(v))
	    {
	      is_float = true;
	      sum *= get_float_value(v);
	    }
	    else
	      sum *= get_int_value(v);

	    rest = cdr(rest);
	  }
	  if(is_float)
	    ret = convert_float_to_object(sum);
	  else
	    ret = convert_int_to_object((int)sum);
	}
	else if(!strcmp(val, SUB))
	{
	  OBJECT_PTR v = eval(CADR(form), env_list);
	  float val;
	  BOOLEAN is_float = false;

	  if(IS_FLOAT_OBJECT(v))
	  {
	    is_float = true;
	    val = get_float_value(v);
	  }
	  else
	    val = get_int_value(v);

	  OBJECT_PTR rest = CDDR(form);

	  float sum = 0;

	  while(rest != NIL)
	  {
	    OBJECT_PTR term = eval(car(rest), env_list);

	    if(IS_FLOAT_OBJECT(term))
	    {
	      is_float = true;
	      sum += get_float_value(term);
	    }
	    else
	      sum += get_int_value(term);

	    rest = cdr(rest);
	  }

	  if(is_float)
	    ret = convert_float_to_object(val - sum);
	  else
	    ret = convert_int_to_object((int)(val - sum));
	}
	else if(!strcmp(val, DIV))
	{
	  OBJECT_PTR v = eval(CADR(form), env_list);
	  float val;
	  BOOLEAN is_float = false;

	  if(IS_FLOAT_OBJECT(v))
	  {
	    is_float = true;
	    val = get_float_value(v);
	  }
	  else
	    val = get_int_value(v);

	  OBJECT_PTR rest = CDDR(form);

	  float sum = 1;

	  while(rest != NIL)
	  {
	    OBJECT_PTR term = eval(car(rest), env_list);

	    if(IS_FLOAT_OBJECT(term))
	    {
	      is_float = true;
	      sum *= get_float_value(term);
	    }
	    else
	      sum *= get_int_value(term);

	    rest = cdr(rest);
	  }

	  if(is_float)
	    ret = convert_float_to_object(val / sum);
	  else
	    ret = convert_int_to_object((int)(val / sum));
	}
	else if(!strcmp(val, GT))
	{
	  OBJECT_PTR v1 = eval(CADR(form), env_list);
	  OBJECT_PTR v2 = eval(CADDR(form), env_list);

	  float val1, val2;
	  
	  if(IS_FLOAT_OBJECT(v1))
	    val1 = get_float_value(v1);
	  else
	    val1 = get_int_value(v1);

	  if(IS_FLOAT_OBJECT(v2))
	    val2 = get_float_value(v2);
	  else
	    val2 = get_int_value(v2);
	    
	  ret = val1 > val2 ? TRUE : NIL;
	}
	else if(!strcmp(val, SET))
	{
	  OBJECT_PTR obj = CADR(form);
	  assert(IS_SYMBOL_OBJECT(obj));
	  OBJECT_PTR value = eval(CADDR(form), env_list);
	  if(update_environment(env_list, obj, value) == NIL)
	  {
	    char buf[SYMBOL_STRING_SIZE];
	    print_symbol(obj, buf);
	    sprintf(err_buf, "Symbol not bound: %s", buf);
	    raise_error();
	    ret = NIL;
	  }
	  else
	    ret = value;
	}
	else if(!strcmp(val, PROGN))
	{
	  OBJECT_PTR rest = cdr(form);
	  OBJECT_PTR val;
	  while(rest != NIL)
	  {
	    val = eval(car(rest), env_list);
	    rest = cdr(rest);
	  }
	  ret = val;
	}
	else if(!strcmp(val, PRINT))
	{
	  OBJECT_PTR obj = eval(CADR(form), env_list);
	  print_object(obj);
	  fprintf(stdout, "\n");
	  ret = obj;
	}
	else if(!strcmp(val, DEFVAR))
	{
	  OBJECT_PTR obj = CADR(form);
	  assert(IS_SYMBOL_OBJECT(obj));
	  OBJECT_PTR value = eval(CADDR(form), env_list);
	  add_to_environment(env_list, obj, value);
	  ret = value;	  
	}
	else if(!strcmp(val, "PRINTENV"))
	{
	  print_object(env_list);
	  printf("\n");
	  ret = NIL;
	}
	else if(!strcmp(val, "CURRENTENV"))
	{
	  ret = env_list;
	}
	else if(!strcmp(val, LET))
	{
	  OBJECT_PTR decl = CADR(form);
	  OBJECT_PTR body = CDDR(form);

	  OBJECT_PTR rest_decl = decl;

	  OBJECT_PTR new_env = cons(cons(get_symbol_object("DUMMY"),
					 NIL), 
				    NIL);

	  OBJECT_PTR extended_env_list = cons(new_env, env_list);

	  while(rest_decl != NIL)
	  {
	    add_to_environment(extended_env_list, CAAR(rest_decl), eval(CADAR(rest_decl), env_list));
	    rest_decl = cdr(rest_decl);
	  }

	  OBJECT_PTR val;
	  while(body != NIL)
	  {
	    val = eval(car(body), extended_env_list);
	    body = cdr(body);
	  }

	  ret = val;
	}
	else if(!strcmp(val, LST)) //using LST because LIST has already been taken
	  ret = eval_and_build_list(cdr(form), env_list);
	else if(!strcmp(val, LISTP))
	  ret = IS_CONS_OBJECT(eval(CADR(form), env_list)) ? TRUE : NIL;
	else if(!strcmp(val, SYMBOL_VALUE))
	{
	  OBJECT_PTR symbol_obj = eval(CADR(form),env_list);
	  OBJECT_PTR res = get_symbol_value(symbol_obj, env_list);
	  if(car(res) == NIL)
	  {
	    char buf[SYMBOL_STRING_SIZE];
	    print_symbol(symbol_obj, buf);
	    sprintf(err_buf, "Symbol not bound: %s", buf);
	    raise_error();
	    ret = NIL;
	  }
	  else
	    ret = cdr(res);
	}
	else if(!strcmp(val, BACKQUOTE))
	  ret = eval_backquote(CADR(form), env_list);
	else if(!strcmp(val, GENSYM))
	  ret = gensym();
	else if(!strcmp(val, SETCAR))
	{
	  OBJECT_PTR obj = eval(CADDR(form), env_list);
	  //heap[eval(CADR(form), env_list) >> CONS_SHIFT] = obj;
	  set_heap(eval(CADR(form), env_list) >> CONS_SHIFT, obj);
	  ret = obj;
	}
	else if(!strcmp(val, SETCDR))
	{
	  OBJECT_PTR obj = eval(CADDR(form), env_list);
	  //heap[(eval(CADR(form), env_list) >> CONS_SHIFT) + 1] = obj;
	  set_heap((eval(CADR(form), env_list) >> CONS_SHIFT) + 1, obj);
	  ret = obj;
	}
	else if(!strcmp(val, ERROR))
	{
	  OBJECT_PTR msg = eval(CADR(form), env_list);
	  sprintf(err_buf, strings[msg >> STRING_LITERAL_SHIFT]);
	  raise_error();
	  ret = NIL;
	}
	else if(!strcmp(val, CREATE_PACKAGE))
	{
	  OBJECT_PTR package = eval(CADR(form), env_list);
	  create_package(convert_to_upper_case(strings[package >> STRING_LITERAL_SHIFT]));
	  ret = package;
	}
	else if(!strcmp(val, IN_PACKAGE))
	{
	  OBJECT_PTR package = eval(CADR(form), env_list);
	  char *package_name = convert_to_upper_case(strings[package >> STRING_LITERAL_SHIFT]);

	  if(!strcmp(package_name,"CORE"))
	  {
	    sprintf(err_buf, "Core package cannot be updated");
            raise_error();
            ret = NIL;
	  }
          else
	  {
	    int index = find_package(package_name);
	    if(index == NOT_FOUND)
	    {
	      sprintf(err_buf, "Package %s does not exist", package_name);
	      raise_error();
	      ret = NIL;
	    }
	    else
	    {
	      current_package = index;
	      ret = NIL;
	    }
          }
	}
	else if(!strcmp(val, EXPAND_MACRO))
	{
	  OBJECT_PTR macro_body = eval(CADR(form), env_list);
	  OBJECT_PTR res = get_symbol_value(car(macro_body), env_list);
	  if(car(res) == NIL)
	  {
	    sprintf(err_buf, "Macro undefined: %s", get_symbol_name(car(macro_body)));
	    raise_error();
	    ret = NIL;
	  }
	  else
	  {
	    OBJECT_PTR obj = cdr(res);
	    OBJECT_PTR args = cdr(macro_body);
	    ret = invoke_macro(obj, args, env_list, false);
	  }
	}
	else if(!strcmp(val, APPLY))
	{
	  OBJECT_PTR f = cons(eval(CADR(form), env_list), NIL);

	  OBJECT_PTR param_list = eval(CADDR(form), env_list);

	  if(!(IS_CONS_OBJECT(param_list)))
	  {
	    sprintf(err_buf, "Argument to APPLY should evaluate to a list");
	    raise_error();
	    ret = NIL;
	  }
	  else
	  {
	    OBJECT_PTR rest = param_list;
	    OBJECT_PTR new_list = NIL;

	    while(rest != NIL)
	    {
	      OBJECT_PTR obj = cons(QUOTE, cons(car(rest), NIL));

	      if(new_list == NIL)
		new_list = cons(obj, NIL);
	      else
		//heap[(last_cell(new_list) >> CONS_SHIFT) + 1] = cons(obj, NIL);
		set_heap((last_cell(new_list) >> CONS_SHIFT) + 1, cons(obj, NIL));
		//new_list = cons(new_list, cons(cons(QUOTE, car(rest)),NIL));

	      rest = cdr(rest);
	    }

	    //heap[(f >> CONS_SHIFT) + 1] = new_list;
	    set_heap((f >> CONS_SHIFT) + 1, new_list);

	    ret = eval(f, env_list);
	  }

	}
	else if(!strcmp(val, STRING))
	  ret = eval_string(CADR(form), env_list);
	else if(!strcmp(val, MAKE_ARRAY))
	  ret = eval_make_array(CADR(form), CDDR(form) == NIL ? NIL : CADDR(form), env_list);
	else if(!strcmp(val, ARRAY_GET))
	  ret = eval_array_get(cdr(form), env_list);
	else if(!strcmp(val, ARRAY_SET))
	  ret = eval_array_set(cdr(form), env_list);
	else if(!strcmp(val, SUB_ARRAY))
	  ret = eval_sub_array(CADR(form), CADDR(form), CADDDR(form), env_list);
	else if(!strcmp(val, ARRAY_LENGTH))
	  ret = heap[eval(CADR(form), env_list) >> ARRAY_SHIFT];
	else if(!strcmp(val, PRINT_STRING))
	  ret = eval_print_string(CADR(form), env_list);
	else if(!strcmp(val, LABELS))
	{
	  OBJECT_PTR decl = CADR(form);
	  OBJECT_PTR body = CDDR(form);

	  OBJECT_PTR rest_decl = decl;

	  OBJECT_PTR new_env = cons(cons(get_symbol_object("DUMMY"),
					 NIL), 
				    NIL);

	  OBJECT_PTR extended_env_list = cons(new_env, env_list);

	  while(rest_decl != NIL)
	  {
	    OBJECT_PTR fn_form = car(rest_decl);
	    OBJECT_PTR fn_name = car(fn_form);
	    OBJECT_PTR fn_params = CADR(fn_form);
	    OBJECT_PTR fn_body = CDDR(fn_form);

	    add_to_environment(extended_env_list, 
			       fn_name, 
			       create_function_object(env_list, fn_params, fn_body));

	    rest_decl = cdr(rest_decl);
	  }

	  OBJECT_PTR val;
	  while(body != NIL)
	  {
	    val = eval(car(body), extended_env_list);
	    body = cdr(body);
	  }

	  ret = val;
	}
	else if(!strcmp(val, CREATE_IMAGE))
	{
	  create_image(strings[eval(CADR(form),env_list) >> STRING_LITERAL_SHIFT]);
	  ret = NIL;
	}
	else if(!strcmp(val, BREAK))
	{
	  if(!debug_mode)
	    enter_debug_mode(env_list);
	  ret = NIL;
	}
	else if(!strcmp(val, LOAD_FOREIGN_LIBRARY))
	  ret = load_foreign_library(CADR(form), env_list);
	else if(!strcmp(val, CALL_FOREIGN_FUNCTION))
	  ret = call_foreign_function(CADR(form), CADDR(form), CADDDR(form), env_list);
	else if(!strcmp(val, IF))
	  ret = eval_if(CADR(form), CADDR(form), CADDDR(form), env_list);
	else if(!strcmp(val, WHILE))
	  ret = eval_while(CADR(form), CDDR(form), env_list);
	else //it's a named function application
	{
	  OBJECT_PTR res = get_symbol_value(car(form), env_list);
	  if(car(res) == NIL)
	  {
	    sprintf(err_buf, "Undefined function: %s", val);
	    raise_error();
	    ret = NIL;
	  }
	  else
	  {
	    OBJECT_PTR obj = cdr(res);
	    OBJECT_PTR args = cdr(form);

	    if(IS_FN_OBJECT(obj))
	    {
	      if(execution_stack == NIL)
		execution_stack = cons(form, NIL);
	      else
		execution_stack = cons(form, execution_stack);

	      ret = invoke_function(obj, args, env_list);
	    }
	    else if(IS_MACRO_OBJECT(obj))
	      ret = invoke_macro(obj, args, env_list,true);
	    else
	      ret = eval(cons(obj,args), env_list);
	  }
	}
      }
    }
  }

  if(debug_mode)
  {
    if(abort_debug)
    {
      fprintf(stdout, "\nAborting debug mode.");
      debug_mode = false;
      return NIL;
    }

    fprintf(stdout, "Evaluating form ");
    print_object(form);
    fprintf(stdout, " => ");
    print_object(ret);

    if(equal(root_form, form))
    {
      fprintf(stdout, "\nExiting debug mode.");
      debug_mode = false;
    }

    prompt();
    //yyparse();
    repl();

  }

  log_function_exit("eval");

  return ret;
}

OBJECT_PTR create_function_object(OBJECT_PTR env_list, OBJECT_PTR params, OBJECT_PTR body)
{
  RAW_PTR ptr = object_alloc(3);

  /*
  heap[ptr] = equal(env_list, init_env_list) ? NIL : env_list;
  heap[ptr + 1] = params;
  heap[ptr + 2] = body;
  */
  set_heap(ptr, equal(env_list, init_env_list) ? NIL : env_list);
  set_heap(ptr + 1, params);
  set_heap(ptr + 2, body);

  insert_node(&white, create_node((ptr << FN_SHIFT) + FN_TAG));
  
  return (ptr << FN_SHIFT) + FN_TAG;

  /*
  OBJECT_PTR ret = cons(equal(env_list, init_env_list) ? NIL : env_list, cons(params, body));

  return ((ret >> CONS_SHIFT) << FN_SHIFT) + FN_TAG;
  */
}

OBJECT_PTR clone_object(OBJECT_PTR obj)
{
  log_function_entry("clone_object");

#ifdef DEBUG
  print_object(obj);
  fprintf(stdout, "\n");
#endif
  
  OBJECT_PTR ret;

  if(is_atom(obj))
    ret = obj; //atoms are immutable and are reused
  else
  {
    if(IS_CONS_OBJECT(obj))
      ret = cons(clone_object(car(obj)), clone_object(cdr(obj)));
    else if(IS_FN_OBJECT(obj))
      ret = create_function_object(clone_object(get_env_list(obj)),
				   clone_object(get_params_object(obj)), 
				   clone_object(get_body_object(obj)));
    else if(IS_MACRO_OBJECT(obj))
      ret = create_macro_object(clone_object(get_env_list(obj)),
				clone_object(get_params_object(obj)), 
				clone_object(get_body_object(obj)));
    //TODO: handle array objects

  }

  log_function_exit("clone_object");

#ifdef DEBUG
  print_object(ret);
  fprintf(stdout, "\n");
#endif

  return ret;
}

OBJECT_PTR get_env_list(OBJECT_PTR fn_obj)
{
  //return car(fn_obj);
  return heap[fn_obj >> FN_SHIFT];
}

OBJECT_PTR get_params_object(OBJECT_PTR fn_obj)
{
  //return CADR(fn_obj);
  return heap[(fn_obj >> FN_SHIFT) + 1];
}

OBJECT_PTR get_body_object(OBJECT_PTR fn_obj)
{
  //return CDDR(fn_obj);
  return heap[(fn_obj >> FN_SHIFT) + 2];
}

void print_function_object(OBJECT_PTR fn_obj)
{
  fprintf(stdout, "#<FUNCTION #x%08x> ", fn_obj);
#ifdef DEBUG
  fprintf(stdout, "\nPARAMETERS: ");
  print_object(get_params_object(fn_obj));
  fprintf(stdout,"\nBODY: ");
  print_object(get_body_object(fn_obj));
#endif
}

int length(OBJECT_PTR cons_obj)
{
  assert(IS_CONS_OBJECT(cons_obj));

  OBJECT_PTR rest = cons_obj;

  int l = 0;

  while(rest != NIL)
  {
    l++;
    rest = cdr(rest);
  }

  return l;
}

OBJECT_PTR get_symbol_value(OBJECT_PTR symbol_obj, OBJECT_PTR env_list)
{
  log_function_entry("get_symbol_value");

  OBJECT_PTR rest = env_list;
  OBJECT_PTR ret;
  BOOLEAN found = false;

  while(rest != NIL)
  {
    OBJECT_PTR result = get_symbol_value_from_env(symbol_obj, car(rest));

    if(car(result) == TRUE)
    {
       ret = cons(TRUE, cdr(result));
       found = true;
       break;
    }
    rest =  cdr(rest);
  }

  if(!found)
    ret = cons(NIL, NIL);

  log_function_exit("get_symbol_value");

  return ret;
}

OBJECT_PTR get_symbol_value_from_env(OBJECT_PTR symbol_obj, OBJECT_PTR env_obj)
{
  log_function_entry("get_symbol_value_from_env");

  OBJECT_PTR rest = env_obj;
  OBJECT_PTR ret;
  BOOLEAN found = false;

  while(rest != NIL)
  {
    if(equal(CAAR(rest), symbol_obj))
    {
      ret = cons(TRUE, CDAR(rest));
      found = true;
      break;
    }
    
    rest = cdr(rest);
  }

  if(!found)
    ret = cons(NIL, NIL);

  log_function_exit("get_symbol_value_from_env");

  return ret;
}

OBJECT_PTR update_environment(OBJECT_PTR env_list, OBJECT_PTR symbol_obj, OBJECT_PTR val)
{
  OBJECT_PTR rest1 = env_list;

  while(rest1 != NIL)
  {
    OBJECT_PTR rest2 = car(rest1);

    while(rest2 != NIL)
    {
      if(equal(CAAR(rest2),symbol_obj))
      {
	//heap[(car(rest2) >> CONS_SHIFT) + 1] = val;
	set_heap((car(rest2) >> CONS_SHIFT) + 1, val);
	return symbol_obj;
      }

      rest2 = cdr(rest2);
    }

    rest1 = cdr(rest1);
  }    

  return NIL;
}

void add_to_environment(OBJECT_PTR env_list, OBJECT_PTR symbol_obj, OBJECT_PTR val)
{
  //this will never actually be true, 
  //since we populate all environments with
  //a dummy variable  when they are created
  if(car(env_list) == NIL) 
  {
    //heap[env_list >> CONS_SHIFT] = cons(cons(symbol_obj, val), NIL);
    set_heap(env_list >> CONS_SHIFT, cons(cons(symbol_obj, val), NIL));
    return;
  }
  else 
  {
    OBJECT_PTR rest = car(env_list);
    //OBJECT_PTR prev = rest;

    while(rest != NIL)
    {
      //symbol already exists in the environment,
      //so replace its existing binding with
      //the new value
      if(equal(CAAR(rest),symbol_obj))
      {
	//heap[(car(rest) >> CONS_SHIFT) + 1] = val;
	set_heap((car(rest) >> CONS_SHIFT) + 1, val);
	return;
      }

      //prev = rest;

      rest = cdr(rest);
    }

    //symbol does not exist in the environment
    //heap[(prev >> CONS_SHIFT) +1] = cons(cons(symbol_obj, val), NIL);
    //heap[(last_cell(car(env_list)) >> CONS_SHIFT) + 1] = cons(cons(symbol_obj, val), NIL);
    set_heap((last_cell(car(env_list)) >> CONS_SHIFT) + 1, cons(cons(symbol_obj, val), NIL));
  }
}

OBJECT_PTR invoke_function(OBJECT_PTR fn_obj, OBJECT_PTR args, OBJECT_PTR exec_env)
{
  log_function_entry("invoke_function");

  OBJECT_PTR env_list = get_env_list(fn_obj);
  OBJECT_PTR params = get_params_object(fn_obj);
  OBJECT_PTR body = get_body_object(fn_obj);

  OBJECT_PTR rest_params = params;
  OBJECT_PTR rest_args = args;

  OBJECT_PTR new_env = cons(cons(get_symbol_object("DUMMY"),
				 NIL), 
			    NIL);

  OBJECT_PTR extended_env_list = cons(new_env, (env_list == NIL) ? init_env_list : env_list);

  while(rest_params != NIL)
  {
    int package_index = car(rest_params) >> (SYMBOL_BITS + SYMBOL_SHIFT);
    int symbol_index =  (car(rest_params) >> SYMBOL_SHIFT) & TWO_RAISED_TO_SYMBOL_BITS_MINUS_1;

    char *param_name = packages[package_index].symbols[symbol_index];

    if(!strcmp(param_name, "&REST"))
    {
      add_to_environment(extended_env_list, CADR(rest_params), eval_and_build_list(rest_args, exec_env));
      break;
    }
    else if(!strcmp(param_name, "&OPTIONAL"))
    {
      OBJECT_PTR p = cdr(rest_params);
      OBJECT_PTR a = rest_args;

      while(p != NIL)
      {
	if(is_atom(car(p))) //optional parameter does not have init form and supplied-p parameter
	   add_to_environment(extended_env_list, car(p), (a == NIL) ? NIL : eval(car(a), exec_env));
	else
	{
	  if(a != NIL) //optional parameter available
	  {
	    add_to_environment(extended_env_list, CAAR(p), eval(car(a), exec_env));
	    if(CDDAR(p) != NIL)
	      add_to_environment(extended_env_list, CADDAR(p), TRUE);
	  }
	  else
	  {
	    
	    if(CDAR(p) != NIL) //an init form is available
	    {
	      OBJECT_PTR init_form = eval(CADAR(p), exec_env);
	      add_to_environment(extended_env_list, CAAR(p), init_form);
	      if(CDDAR(p) != NIL)
		add_to_environment(extended_env_list, CADDAR(p), (init_form == NIL) ? NIL :TRUE);
	    }
	    else //init form not available
	    {
	      add_to_environment(extended_env_list, CAAR(p), NIL);
	    }
	  }
	}

	p = cdr(p);
	if(a != NIL)
	  a = cdr(a);
      }
      break;
    }
    else if(!strcmp(param_name, "&KEY"))
    {
      OBJECT_PTR p = cdr(rest_params);
      OBJECT_PTR a = rest_args;

      if(a == NIL || !contains_keyword_parameter(a))
      {
	sprintf(err_buf,"Invalid keyword parameter");
	raise_error();
	log_function_exit("invoke_function");
	return NIL;
      }

      while(p != NIL)
      {
	OBJECT_PTR temp = get_keyword_arg(car(p), rest_args);

	add_to_environment(extended_env_list, car(p), (temp == NIL) ? NIL: eval(temp, exec_env));

	p = cdr(p);
	if(a != NIL)
	  a = CDDR(a);
      }
      break;
    }
    else
    {
      add_to_environment(extended_env_list, car(rest_params), eval(car(rest_args), exec_env));
    }

    rest_params = cdr(rest_params);
    rest_args = cdr(rest_args);
  }
  
  OBJECT_PTR rest = body;
  OBJECT_PTR val;

  while(rest != NIL)
  {
    val = eval(car(rest), extended_env_list);
    rest = cdr(rest);
  }

  log_function_exit("invoke_function");

  return val;
}

OBJECT_PTR eval_and_build_list(OBJECT_PTR lst, OBJECT_PTR env_list)
{
  if(lst == NIL)
    return NIL;
  else
    return cons(eval(car(lst), env_list), eval_and_build_list(cdr(lst), env_list));
}

BOOLEAN is_special_form(OBJECT_PTR form)
{
  if(IS_SYMBOL_OBJECT(form))
  {
    int index = form >> SYMBOL_SHIFT;

    if(index >= 0 && index <= NOF_SPECIAL_SYMBOLS)
      return true;
    else
      return false;
  }

  return false;
     
}

OBJECT_PTR invoke_macro(OBJECT_PTR macro_obj, OBJECT_PTR args, OBJECT_PTR exec_env, BOOLEAN evaluate)
{
  OBJECT_PTR env_list = get_env_list(macro_obj);
  OBJECT_PTR params = get_params_object(macro_obj);
  OBJECT_PTR body = get_body_object(macro_obj);

  OBJECT_PTR rest_params = params;
  OBJECT_PTR rest_args = args;

  OBJECT_PTR new_env = cons(cons(get_symbol_object("DUMMY"),
				 NIL), 
			    NIL);

  OBJECT_PTR extended_env_list = cons(new_env, (env_list == NIL) ? init_env_list : env_list);

  while(rest_params != NIL)
  {

    int package_index = car(rest_params) >> (SYMBOL_BITS + SYMBOL_SHIFT);
    int symbol_index =  (car(rest_params) >> SYMBOL_SHIFT) & TWO_RAISED_TO_SYMBOL_BITS_MINUS_1;

    char *param_name = packages[package_index].symbols[symbol_index];

    if(!strcmp(param_name, "&REST"))
    {
      //add_to_environment(extended_env_list, CADR(rest_params), eval_and_build_list(rest_args, exec_env));
      add_to_environment(extended_env_list, CADR(rest_params), rest_args);
      break;
    }
    else
    {
      add_to_environment(extended_env_list, car(rest_params), car(rest_args));
    }

    rest_params = cdr(rest_params);
    rest_args = cdr(rest_args);
  }
  
  OBJECT_PTR expanded_form = eval(body, extended_env_list);

  if(evaluate)
    return eval(expanded_form, exec_env);
  else
    return expanded_form;
}

OBJECT_PTR create_macro_object(OBJECT_PTR env_list, OBJECT_PTR params, OBJECT_PTR body)
{
  RAW_PTR ptr = object_alloc(3);

  /*
  heap[ptr] = equal(env_list, init_env_list) ? NIL : env_list;
  heap[ptr + 1] = params;
  heap[ptr + 2] = body;
  */
  set_heap(ptr, equal(env_list, init_env_list) ? NIL : env_list);
  set_heap(ptr + 1, params);
  set_heap(ptr + 2, body);

  insert_node(&white, create_node((ptr << MACRO_SHIFT) + MACRO_TAG));
  
  return (ptr << MACRO_SHIFT) + MACRO_TAG;

  /*
  OBJECT_PTR ret = cons(equal(env_list, init_env_list) ? NIL : env_list, cons(params, body));

  return ((ret >> MACRO_SHIFT) << MACRO_SHIFT) + MACRO_TAG;
  */
}

void print_macro_object(OBJECT_PTR macro_obj)
{
  fprintf(stdout, "#<MACRO #x%08x> ", macro_obj);
#ifdef DEBUG
  fprintf(stdout, "\nPARAMETERS: ");
  print_object(get_params_object(macro_obj));
  fprintf(stdout,"\nBODY: ");
  print_object(get_body_object(macro_obj));
#endif
}

OBJECT_PTR eval_backquote(OBJECT_PTR form, OBJECT_PTR env_list)
{
  if(is_atom(form))
    return form;

  OBJECT_PTR car_obj = car(form);

  if(IS_SYMBOL_OBJECT(car_obj))
  {
    char buf[SYMBOL_STRING_SIZE];
    print_symbol(car_obj, buf);

    if(!strcmp(buf, COMMA))
    {
      OBJECT_PTR sym = CADR(form);

      /*
      OBJECT_PTR res = get_symbol_value(sym, env_list);
      

      if(car(res) == NIL)
      {
	char buf[SYMBOL_STRING_SIZE];
	print_symbol(sym, buf);
	fprintf(stdout, "Symbol not bound: %s\n", buf);
	return NIL;
      }
      else
	return cdr(res);
      */

      return eval(sym, env_list);
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

	if(!strcmp(buf, COMMA_AT))
        {

	  /*
	  ret = get_symbol_value(CADAR(rest), env_list);

	  if(car(ret) == NIL)
	  {
	    char buf[SYMBOL_STRING_SIZE];
	    print_symbol(CADAR(rest), buf);
	    fprintf(stdout, "Symbol not bound: %s\n", buf);
	    obj = NIL;
	  }
	  else
	    obj = cdr(ret);
	  */

	  obj = eval(CADAR(rest), env_list);

	  if(result == NIL)
	    result = obj;
	  else
	    //heap[(last_cell(result) >> CONS_SHIFT) + 1] = obj;
	    set_heap((last_cell(result) >> CONS_SHIFT) + 1, obj);
	}
	else
	{
	  obj = eval_backquote(car(rest), env_list);
	  
	  if(result == NIL)
	    result = cons(obj, NIL);
	  else
	    //heap[(last_cell(result) >> CONS_SHIFT) + 1] = cons(obj, NIL);
	    set_heap((last_cell(result) >> CONS_SHIFT) + 1, cons(obj, NIL));
	}
      }
      else
      {
	obj = eval_backquote(car(rest), env_list);

	if(result == NIL)
	  result = cons(obj, NIL);
	else
	  //heap[(last_cell(result) >> CONS_SHIFT) + 1] = cons(obj, NIL);
	  set_heap((last_cell(result) >> CONS_SHIFT) + 1, cons(obj, NIL));
      }
      rest = cdr(rest);
    }

    return result;
  }

  return cons(eval_backquote(car(form), env_list),
	      eval_backquote(cdr(form), env_list));

}

BOOLEAN form_contains_comma_at(OBJECT_PTR form)
{
  OBJECT_PTR rest = form;

  while(rest != NIL)
  {
    if(IS_CONS_OBJECT(car(rest)) &&
       IS_SYMBOL_OBJECT(CAAR(rest)))
    {
      char buf[SYMBOL_STRING_SIZE];
      print_symbol(CAAR(rest), buf);

      if(!strcmp(buf, COMMA_AT))
	return true;
    }

    rest = cdr(rest);
  }

  return false;
}

OBJECT_PTR last_cell(OBJECT_PTR list)
{
  if(cdr(list) == NIL)
    return list;
  else
    return last_cell(cdr(list));
}


//TODO: package-awareness (is this required?)
OBJECT_PTR gensym()
{
  gen_sym_count++;

  char sym[7];

  sprintf(sym, "#:G%04d", gen_sym_count);

  return (current_package << (SYMBOL_BITS + SYMBOL_SHIFT)) + (add_symbol(sym) << SYMBOL_SHIFT) + SYMBOL_TAG;
  
}

void reset_exception_mechanism()
{
  in_exception = false;
  memset(err_buf,'\0',500);
  execution_stack = NIL;
}

void print_stack_trace()
{
  if(execution_stack == NIL)
    return;

  int i = length(execution_stack);
  OBJECT_PTR rest = execution_stack;

  fprintf(stdout, "\nBacktrace:\n");

  while(rest != NIL)
  {
    fprintf(stdout, "%02d: ", i);
    print_object(car(rest));
    fprintf(stdout, "\n");

    i--;
    rest = cdr(rest);
  }
}

void raise_error()
{
  in_exception = true;
}

void create_package(char *name)
{
  nof_packages++;

  package_t *temp = (package_t *)realloc(packages, nof_packages * sizeof(package_t));

  if(temp != NULL)
    packages = temp;
  else
    {
      fprintf(stderr, "Out of memory extending package space\n");
      cleanup();
      exit(1);
    }

  packages[nof_packages - 1].name = strdup(name);
  packages[nof_packages - 1].symbols = NULL;
  packages[nof_packages - 1].nof_symbols = 0;
}

void initialize_core_package()
{
  packages[CORE_PACKAGE_INDEX].nof_symbols = NOF_SPECIAL_SYMBOLS;
  packages[CORE_PACKAGE_INDEX].symbols = (char **)malloc(packages[CORE_PACKAGE_INDEX].nof_symbols * sizeof(char *));

  packages[CORE_PACKAGE_INDEX].symbols[0] = strdup("T");
  TRUE = SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[1] = strdup("NIL");
  NIL = (1 << SYMBOL_SHIFT) + SYMBOL_TAG;  

  packages[CORE_PACKAGE_INDEX].symbols[2] = strdup(QUOT);
  QUOTE = (2 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[3] = strdup(ATOM);
  packages[CORE_PACKAGE_INDEX].symbols[4] = strdup(EQ);
  packages[CORE_PACKAGE_INDEX].symbols[5] = strdup(CAR);
  packages[CORE_PACKAGE_INDEX].symbols[6] = strdup(CDR);
  packages[CORE_PACKAGE_INDEX].symbols[7] = strdup(CONS);
  packages[CORE_PACKAGE_INDEX].symbols[8] = strdup(COND);

  packages[CORE_PACKAGE_INDEX].symbols[9] = strdup(LAMBDA);
  packages[CORE_PACKAGE_INDEX].symbols[10] = strdup(DEFUN);
  packages[CORE_PACKAGE_INDEX].symbols[11] = strdup(SET);
  packages[CORE_PACKAGE_INDEX].symbols[12] = strdup(ADD);
  packages[CORE_PACKAGE_INDEX].symbols[13] = strdup(SUB);
  packages[CORE_PACKAGE_INDEX].symbols[14] = strdup(MULT);
  packages[CORE_PACKAGE_INDEX].symbols[15] = strdup(DIV);
  packages[CORE_PACKAGE_INDEX].symbols[16] = strdup(PROGN);
  packages[CORE_PACKAGE_INDEX].symbols[17] = strdup(PRINT);
  packages[CORE_PACKAGE_INDEX].symbols[18] = strdup(DEFVAR);
  packages[CORE_PACKAGE_INDEX].symbols[19] = strdup(LET);
  packages[CORE_PACKAGE_INDEX].symbols[20] = strdup(LST);
  packages[CORE_PACKAGE_INDEX].symbols[21] = strdup(LISTP);
  packages[CORE_PACKAGE_INDEX].symbols[22] = strdup(SYMBOL_VALUE);
  packages[CORE_PACKAGE_INDEX].symbols[23] = strdup(DEFMACRO);
  packages[CORE_PACKAGE_INDEX].symbols[24] = strdup(BACKQUOTE);
  packages[CORE_PACKAGE_INDEX].symbols[25] = strdup(GT);
  packages[CORE_PACKAGE_INDEX].symbols[26] = strdup(GENSYM);
  packages[CORE_PACKAGE_INDEX].symbols[27] = strdup(SETCAR);
  packages[CORE_PACKAGE_INDEX].symbols[28] = strdup(SETCDR);
  packages[CORE_PACKAGE_INDEX].symbols[29] = strdup(ERROR);
  packages[CORE_PACKAGE_INDEX].symbols[30] = strdup(CREATE_PACKAGE);
  packages[CORE_PACKAGE_INDEX].symbols[31] = strdup(IN_PACKAGE);

  packages[CORE_PACKAGE_INDEX].symbols[32] = strdup(COMMA);
  packages[CORE_PACKAGE_INDEX].symbols[33] = strdup(COMMA_AT);

  packages[CORE_PACKAGE_INDEX].symbols[34] = strdup(EXPAND_MACRO);
  packages[CORE_PACKAGE_INDEX].symbols[35] = strdup(APPLY);

  packages[CORE_PACKAGE_INDEX].symbols[36] = strdup(STRING);

  packages[CORE_PACKAGE_INDEX].symbols[37] = strdup(MAKE_ARRAY);
  packages[CORE_PACKAGE_INDEX].symbols[38] = strdup(ARRAY_GET);
  packages[CORE_PACKAGE_INDEX].symbols[39] = strdup(ARRAY_SET);
  packages[CORE_PACKAGE_INDEX].symbols[40] = strdup(SUB_ARRAY);
  packages[CORE_PACKAGE_INDEX].symbols[41] = strdup(ARRAY_LENGTH);

  packages[CORE_PACKAGE_INDEX].symbols[42] = strdup(PRINT_STRING);

  packages[CORE_PACKAGE_INDEX].symbols[43] = strdup(LABELS);

  packages[CORE_PACKAGE_INDEX].symbols[44] = strdup(CREATE_IMAGE);

  packages[CORE_PACKAGE_INDEX].symbols[45] = strdup(BREAK);

  packages[CORE_PACKAGE_INDEX].symbols[46] = strdup(LOAD_FOREIGN_LIBRARY);
  packages[CORE_PACKAGE_INDEX].symbols[47] = strdup(CALL_FOREIGN_FUNCTION);

  packages[CORE_PACKAGE_INDEX].symbols[48] = strdup(PRINTENV);
  packages[CORE_PACKAGE_INDEX].symbols[49] = strdup(CURRENTENV); 

  packages[CORE_PACKAGE_INDEX].symbols[50] = strdup(IF);
  packages[CORE_PACKAGE_INDEX].symbols[51] = strdup(WHILE); 
}

int find_package(char* package_name)
{
  int i;

  for(i=0; i < nof_packages; i++)
  {
    if(!strcmp(packages[i].name, package_name))
       return i;
  }

  return NOT_FOUND;
}

int find_qualified_symbol(int package_index, char *sym)
{
  int i;

  for(i=0; i<packages[package_index].nof_symbols; i++)
  {
    if(!strcmp(packages[package_index].symbols[i],sym))
      return i;
  }

  return NOT_FOUND;
}

OBJECT_PTR get_qualified_symbol_object(char *package_name, char *symbol_name)
{
  int package_index = find_package(package_name);

  if(package_index == NOT_FOUND)
  {
    sprintf(err_buf, "Package %s does not exist", package_name);
    raise_error();
    return NIL;
  }

  int symbol_index = find_qualified_symbol(package_index, symbol_name);

  OBJECT_PTR retval;

  if(symbol_index != NOT_FOUND) //symbol exists in symbol table
    retval = (package_index << (SYMBOL_BITS + SYMBOL_SHIFT)) + (symbol_index << SYMBOL_SHIFT) + SYMBOL_TAG;
  else
    retval = (package_index << (SYMBOL_BITS + SYMBOL_SHIFT)) + (add_qualified_symbol(package_name, symbol_name) << SYMBOL_SHIFT) + SYMBOL_TAG;

  return retval;
}

void print_symbol(OBJECT_PTR ptr, char *buf)
{

  log_function_entry("print_symbol");

  assert(IS_SYMBOL_OBJECT(ptr));

  memset(buf,'\0',SYMBOL_STRING_SIZE);

  int package_index = ptr >> (SYMBOL_BITS + SYMBOL_SHIFT);
  int symbol_index =  (ptr >> SYMBOL_SHIFT) & TWO_RAISED_TO_SYMBOL_BITS_MINUS_1;

  assert(package_index >= 0 && package_index < nof_packages);
  assert(symbol_index >= 0 && symbol_index < packages[package_index].nof_symbols);

  if(package_index != 0)
    sprintf(buf, "%s:%s", packages[package_index].name, packages[package_index].symbols[symbol_index]);
  else
    sprintf(buf, "%s", packages[package_index].symbols[symbol_index]);

  log_function_exit("print_symbol");
}

int add_qualified_symbol(char *package_name, char *sym)
{
  log_function_entry("add_symbol");

  int package_index = find_package(package_name);

  if(package_index == NOT_FOUND)
  {
    sprintf(err_buf, "Package %s does not exist", package_name);
    raise_error();
    return NIL;
  }

  packages[package_index].nof_symbols++;

  char **temp = (char **)realloc(packages[package_index].symbols, packages[package_index].nof_symbols * sizeof(char *));

  if(temp != NULL)
    packages[package_index].symbols = temp;
  else
    {
      fprintf(stderr, "Out of memory extending symbol space\n");
      cleanup();
      exit(1);
    }

  packages[package_index].symbols[packages[package_index].nof_symbols - 1] = strdup(sym);

  log_function_exit("add_symbol");

  return packages[package_index].nof_symbols - 1;
}

//NOTE: this function returns the unqualified symbol name
char *get_symbol_name(OBJECT_PTR symbol_object)
{

  assert(IS_SYMBOL_OBJECT(symbol_object));

  int package_index = symbol_object >> (SYMBOL_BITS + SYMBOL_SHIFT);
  int symbol_index =  (symbol_object >> SYMBOL_SHIFT) & TWO_RAISED_TO_SYMBOL_BITS_MINUS_1;

  return packages[package_index].symbols[symbol_index];
}

//given a key (e.g. 'a') and an arg list (e.g. (:a 1 :b 2 :c 3)
//this function returns the corresponding value (1 in this case)
OBJECT_PTR get_keyword_arg(OBJECT_PTR key, OBJECT_PTR arg_list)
{

  log_function_entry("get_keyword_arg");

  assert(IS_SYMBOL_OBJECT(key));

  OBJECT_PTR ret;
  BOOLEAN found = false;

  OBJECT_PTR rest = arg_list;

  while(rest != NIL)
  {

    if(!(IS_SYMBOL_OBJECT(car(rest))))
    {
      rest = cdr(rest);
      continue;
    }

    char *temp1 = get_symbol_name(car(rest));

    assert(temp1[0] == ':');

    char *temp2 = substring(temp1, 1, strlen(temp1) - 1);

    if(!strcmp(get_symbol_name(key), temp2))
    {
      ret = CADR(rest);
      free(temp2);
      found = true;
      break;
    }

    free(temp2); //temp1 need not be freed
    rest = CDDR(rest); //need to skip the values and only consider :a, :b, ...
  }

  if(!found)
    ret = NIL;

  log_function_exit("get_keyword_arg");

  return ret;
}

BOOLEAN is_keyword_symbol(OBJECT_PTR symbol_object)
{

  log_function_entry("is_keyword_symbol");
  OBJECT_PTR ret;

  if(!(IS_SYMBOL_OBJECT(symbol_object)))
    ret = false;
  else
  {
    char *temp1 = get_symbol_name(symbol_object);
    ret = (temp1[0] == ':');
  }


  log_function_exit("is_keyword_symbol");

  return ret;
}

BOOLEAN contains_keyword_parameter(OBJECT_PTR list)
{
  log_function_entry("contains_keyword_parameter");

  BOOLEAN ret = false;

  OBJECT_PTR rest = list;

  while(rest != NIL)
  {
    if(is_keyword_symbol(car(rest)))
    {
      ret = true;
      break;
    }

    rest = cdr(rest);
  }

  log_function_exit("contains_keyword_parameter");

  return ret;
}

int get_int_value(OBJECT_PTR obj)
{
  log_function_entry("get_int_value");

  assert(IS_INTEGER_OBJECT(obj));

  int ret;

  int v = obj >> INTEGER_SHIFT;

  if(v > TWO_RAISED_TO_27) // most significant bit is 1, and hence number is negative)
    ret = v - TWO_RAISED_TO_28;
  else
    ret = v;

  log_function_exit("get_int_value");

  return ret;
}

OBJECT_PTR convert_int_to_object(int v)
{
  log_function_entry("convert_int_to_object");

  OBJECT_PTR ret;

  if(v > 0)
    ret = (v << INTEGER_SHIFT) + INTEGER_TAG;
  else
    ret = ((TWO_RAISED_TO_28 + v) << INTEGER_SHIFT) + INTEGER_TAG;

  log_function_exit("convert_int_to_object");

  return ret;
}

float get_float_value(OBJECT_PTR obj)
{
  log_function_entry("get_float_value");

  assert(IS_FLOAT_OBJECT(obj));

  union float_and_uint fi;
  fi.i = heap[obj >> FLOAT_SHIFT]; 

  float ret;

  ret = fi.f; 

  log_function_exit("get_float_value");

  return ret;
}

OBJECT_PTR convert_float_to_object(float v)
{
  log_function_entry("convert_float_to_object");

  union float_and_uint fi;
  fi.f = v;
  
  RAW_PTR ptr = object_alloc(1);

  //heap[ptr] = fi.i;
  set_heap(ptr, fi.i);

  insert_node(&white, create_node((ptr << FLOAT_SHIFT) + FLOAT_TAG));

  log_function_exit("convert_float_to_object");

  return (ptr << FLOAT_SHIFT) + FLOAT_TAG;
  
}

OBJECT_PTR eval_string(OBJECT_PTR literal, OBJECT_PTR env_list)
{
  log_function_entry("eval_string");

  assert(IS_STRING_LITERAL_OBJECT(literal));

  //eval in not actually required here
  char *str_val = strings[eval(literal, env_list)  >> STRING_LITERAL_SHIFT];

  char *ptr = NULL;

  int len = strlen(str_val);

  RAW_PTR raw_ptr = object_alloc(len + 1);

  //heap[raw_ptr] = convert_int_to_object(len);
  set_heap(raw_ptr, convert_int_to_object(len));

  insert_node(&white, create_node((raw_ptr << ARRAY_SHIFT) + ARRAY_TAG));

  int i=1;

  for(ptr=str_val;*ptr;ptr++) 
  { 
    //heap[raw_ptr + i] = (*ptr << CHAR_SHIFT) + CHAR_TAG;
    set_heap(raw_ptr + i, (*ptr << CHAR_SHIFT) + CHAR_TAG);
    i++;
  }

  log_function_exit("eval_string");

  return (raw_ptr << ARRAY_SHIFT) + ARRAY_TAG;
}

OBJECT_PTR eval_make_array(OBJECT_PTR size_form, OBJECT_PTR default_form, OBJECT_PTR env_list)
{
  log_function_entry("eval_make_array");

  OBJECT_PTR size = eval(size_form, env_list);
  assert(IS_INTEGER_OBJECT(size));
  
  int sz = get_int_value(size);

  RAW_PTR ptr = object_alloc(sz+1);

  //heap[ptr] = size;
  set_heap(ptr, size);

  insert_node(&white, create_node((ptr << ARRAY_SHIFT) + ARRAY_TAG));

  OBJECT_PTR default_value = default_form == NIL ? NIL : eval(default_form, env_list);

  int i;

  for(i=0; i< sz; i++)
    //heap[ptr + i + 1] = default_value;
    set_heap(ptr + i + 1, default_value);

  log_function_exit("eval_make_array");

  return (ptr << ARRAY_SHIFT) + ARRAY_TAG;
}

OBJECT_PTR eval_array_get(OBJECT_PTR form, OBJECT_PTR env_list)
{
  log_function_entry("eval_array_get");

  OBJECT_PTR array_obj = eval(car(form), env_list);
  assert(IS_ARRAY_OBJECT(array_obj));

  OBJECT_PTR idx = eval(CADR(form), env_list);
  assert(IS_INTEGER_OBJECT(idx));

  log_function_exit("eval_array_get");

  return heap[(array_obj >> ARRAY_SHIFT) + get_int_value(idx) + 1];
}

OBJECT_PTR eval_array_set(OBJECT_PTR form, OBJECT_PTR env_list)
{
  log_function_entry("eval_array_set");

  OBJECT_PTR array_obj = eval(car(form), env_list);
  assert(IS_ARRAY_OBJECT(array_obj));

  OBJECT_PTR idx = eval(CADR(form), env_list);
  assert(IS_INTEGER_OBJECT(idx));

  OBJECT_PTR val = eval(CADDR(form), env_list);

  //heap[(array_obj >> ARRAY_SHIFT) + get_int_value(idx) + 1] = val;
  set_heap((array_obj >> ARRAY_SHIFT) + get_int_value(idx) + 1, val);

  log_function_exit("eval_array_set");

  return val;
}

void print_array_object(OBJECT_PTR array)
{

  log_function_entry("print_array_object");

  fprintf(stdout, "[");

  int length = get_int_value(heap[array >> ARRAY_SHIFT]);

  int i;

  for(i=0; i< length; i++)
  {
    print_object(heap[(array >> ARRAY_SHIFT) + i + 1]);
    fprintf(stdout, " ");
  }

  if(length > 0)
    fprintf(stdout, "\b");

  fprintf(stdout, "]");

  log_function_exit("print_array_object");
}

OBJECT_PTR eval_sub_array(OBJECT_PTR array_form, OBJECT_PTR start_form, OBJECT_PTR length_form, OBJECT_PTR env_list)
{
  log_function_entry("eval_sub_array");

  OBJECT_PTR array = eval(array_form, env_list);
  assert(IS_ARRAY_OBJECT(array));

  OBJECT_PTR start = eval(start_form, env_list);
  assert(IS_INTEGER_OBJECT(start));

  OBJECT_PTR length = eval(length_form, env_list);
  assert(IS_INTEGER_OBJECT(length));

  assert(get_int_value(start) >= 0);

  //TODO: check that start + length is within the array bounds

  OBJECT_PTR ret;
  int st = get_int_value(start);
  int len = get_int_value(length);

  RAW_PTR orig_ptr = array >> ARRAY_SHIFT;

  RAW_PTR ptr = object_alloc(len + 1);

  //heap[ptr] = convert_int_to_object(len);
  set_heap(ptr, convert_int_to_object(len));

  insert_node(&white, create_node((ptr << ARRAY_SHIFT) + ARRAY_TAG));

  int i;

  for(i=1; i<=len; i++)
    //heap[ptr+i] = heap[orig_ptr + st + i];
    set_heap(ptr+i, heap[orig_ptr + st + i]);

  ret = (ptr << ARRAY_SHIFT) + ARRAY_TAG;

  log_function_exit("eval_sub_array");

  return ret;
}

OBJECT_PTR eval_print_string(OBJECT_PTR string_form, OBJECT_PTR env_list)
{
  log_function_entry("eval_print_string");

  if(IS_STRING_LITERAL_OBJECT(string_form))
    print_object(string_form);
  else
    print_object(eval(string_form, env_list));

  fprintf(stdout, "\n");

  log_function_exit("eval_print_string");

  return NIL;
}

void print_string(OBJECT_PTR string_object)
{
  assert(is_string_object(string_object));

  RAW_PTR ptr = string_object >> ARRAY_SHIFT;

  int len = get_int_value(heap[ptr]);

  int i;

  fprintf(stdout, "\"");

  for(i=1; i<=len; i++)
    fprintf(stdout, "%c", heap[ptr + i] >> CHAR_SHIFT);

  fprintf(stdout, "\"");
}

void enter_debug_mode(OBJECT_PTR env)
{
  fprintf(stdout, "Entering debug mode.");

  debug_mode = true;
  debug_env = env;
  prompt();
  //yyparse();
  repl();
}

BOOLEAN is_string_object(OBJECT_PTR obj)
{
  if(!(IS_ARRAY_OBJECT(obj)))
    return false;

  RAW_PTR ptr = obj >> ARRAY_SHIFT;

  int len = get_int_value(heap[ptr]);

  int i;

  for(i=1; i<=len; i++)
  {
    if(!(IS_CHAR_OBJECT(heap[ptr+i])))
      return false;
  }

  return true;
}

char *get_string(OBJECT_PTR string_object)
{
  assert(is_string_object(string_object));

  RAW_PTR ptr = string_object >> ARRAY_SHIFT;

  int len = get_int_value(heap[ptr]);

  char *ret = (char *)malloc(len * sizeof(char));

  int i;

  for(i=1; i<=len; i++)
    ret[i-1] = heap[ptr + i] >> CHAR_SHIFT;

  ret[len] = '\0';

  return ret;
}

void initialize_heap()
{
  heap = (RAW_PTR *)malloc(HEAP_SIZE * sizeof(RAW_PTR));

  if(!heap)
  {
    fprintf(stderr, "Unable to create heap of size %d\n", HEAP_SIZE);
    cleanup();
    exit(1);
  }
}


//the assumption is that all object types
//have the same shift (CONS_SHIFT)
OBJECT_PTR shallow_copy(OBJECT_PTR obj)
{
  RAW_PTR ptr = object_alloc(1);

  int tag = obj & BIT_MASK ;

  //heap[ptr] = heap[obj >> CONS_SHIFT];
  set_heap(ptr, heap[obj >> CONS_SHIFT]);

  insert_node(&white, create_node((ptr << CONS_SHIFT) + tag));

  return (ptr << CONS_SHIFT) + tag;
}

OBJECT_PTR eval_if(OBJECT_PTR cond_form, OBJECT_PTR then_form, OBJECT_PTR else_form, OBJECT_PTR env_list)
{
  if(eval(cond_form, env_list) == TRUE)
    return eval(then_form, env_list);
  else
    return eval(else_form, env_list);
}

OBJECT_PTR eval_while(OBJECT_PTR cond_form, OBJECT_PTR body_form, OBJECT_PTR env_list)
{
  OBJECT_PTR val;

  while(eval(cond_form, env_list) == TRUE)
  {
    OBJECT_PTR rest = body_form;

    while(rest != NIL)
    {
      val = eval(car(rest), env_list);
      rest = cdr(rest);
    }
  }

  return val;
}

#ifndef DEBUG_MEMORY
inline
#endif
void set_heap(RAW_PTR index, OBJECT_PTR val)
{
  assert(IS_CONS_OBJECT(val)           ||
	 IS_FN_OBJECT(val)             ||
         IS_MACRO_OBJECT(val)          ||
	 IS_ARRAY_OBJECT(val)          ||
	 IS_SYMBOL_OBJECT(val)         ||
	 IS_INTEGER_OBJECT(val)        ||
	 IS_STRING_LITERAL_OBJECT(val) ||
	 IS_CHAR_OBJECT(val)           ||
	 IS_FLOAT_OBJECT(val));

  heap[index] = val;
}
