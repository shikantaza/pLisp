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

OBJECT_PTR top_level_env;

extern RAW_PTR *heap;

unsigned int current_package;
unsigned int nof_packages;
package_t *packages;

int gen_sym_count = 0;

//end of variables that should
//be serialized to implement images

//standard objects defined
//to avoid creating them
//each time they're needed
OBJECT_PTR TRUE;
OBJECT_PTR NIL;
OBJECT_PTR QUOTE;
OBJECT_PTR LAMBDA;
OBJECT_PTR IF;
OBJECT_PTR SET;
OBJECT_PTR CALL_CC;
OBJECT_PTR DEFINE;
OBJECT_PTR PROGN;
OBJECT_PTR BACKQUOTE;

OBJECT_PTR CONS;
OBJECT_PTR EQ;
OBJECT_PTR ATOM;
OBJECT_PTR CAR;
OBJECT_PTR CDR;

OBJECT_PTR DEFUN;

OBJECT_PTR ADD;
OBJECT_PTR SUB;
OBJECT_PTR MULT;
OBJECT_PTR DIV;

OBJECT_PTR PRINT;
OBJECT_PTR DEFVAR;
OBJECT_PTR LST;
OBJECT_PTR LISTP;
OBJECT_PTR SYMBOL_VALUE;
OBJECT_PTR DEFMACRO;

OBJECT_PTR GT;
OBJECT_PTR GENSYM;
OBJECT_PTR SETCAR;
OBJECT_PTR SETCDR;
OBJECT_PTR ERROR;
OBJECT_PTR CREATE_PACKAGE;
OBJECT_PTR IN_PACKAGE;
OBJECT_PTR COMMA;
OBJECT_PTR COMMA_AT;
OBJECT_PTR EXPAND_MACRO;

OBJECT_PTR STRING;
OBJECT_PTR MAKE_ARRAY;
OBJECT_PTR ARRAY_GET;
OBJECT_PTR ARRAY_SET;
OBJECT_PTR SUB_ARRAY;
OBJECT_PTR ARRAY_LENGTH;
OBJECT_PTR PRINT_STRING;
OBJECT_PTR LABELS;
OBJECT_PTR CREATE_IMAGE;
OBJECT_PTR BREAK;
OBJECT_PTR LOAD_FOREIGN_LIBRARY;
OBJECT_PTR CALL_FOREIGN_FUNCTION;
OBJECT_PTR PRINTENV;
OBJECT_PTR CURRENTENV;
OBJECT_PTR EVAL;

OBJECT_PTR RESUME;

OBJECT_PTR BACKTRACE;

//end of standard object definition

extern FILE *yyin;

#define NOF_SPECIAL_SYMBOLS 54

BOOLEAN in_exception = false;
OBJECT_PTR execution_stack;
char err_buf[500];

#define SYMBOL_STRING_SIZE 100

#define CORE_PACKAGE_INDEX 0

BOOLEAN debug_mode = false;

//used to keep track of when
//to exit debugging mode, also
//maybe useful when implementing continuations
OBJECT_PTR root_form;

int nof_dl_handles = 0;
void **dl_handles = NULL;

extern struct node *white;

inline OBJECT_PTR CAAR(x)   { return car(car(x)); }
inline OBJECT_PTR CDAR(x)   { return cdr(car(x)); }
inline OBJECT_PTR CADR(x)   { return car(cdr(x)); }
inline OBJECT_PTR CDDR(x)   { return cdr(cdr(x)); }
inline OBJECT_PTR CDDAR(x)  { return cdr(cdr(car(x))); }
inline OBJECT_PTR CAADR(x)  { return car(car(cdr(x))); }
inline OBJECT_PTR CADAR(x)  { return car(cdr(car(x))); }
inline OBJECT_PTR CADDR(x)  { return car(cdr(cdr(x))); }
inline OBJECT_PTR CDDDR(x)  { return cdr(cdr(cdr(x))); }
inline OBJECT_PTR CADDDR(x) { return car(cdr(cdr(cdr(x)))); }
inline OBJECT_PTR CADDAR(x) { return car(cdr(cdr(car(x)))); }
inline OBJECT_PTR CADADR(x) { return car(cdr(car(cdr(x)))); }

inline BOOLEAN IS_SYMBOL_OBJECT(OBJECT_PTR x)         { return (x & BIT_MASK) == SYMBOL_TAG; }
inline BOOLEAN IS_CONS_OBJECT(OBJECT_PTR x)           { return (x & BIT_MASK) == CONS_TAG; }
inline BOOLEAN IS_CLOSURE_OBJECT(OBJECT_PTR x)        { return (x & BIT_MASK) == CLOSURE_TAG; }
inline BOOLEAN IS_INTEGER_OBJECT(OBJECT_PTR x)        { return (x & BIT_MASK) == INTEGER_TAG; }
inline BOOLEAN IS_FLOAT_OBJECT(OBJECT_PTR x)          { return (x & BIT_MASK) == FLOAT_TAG; }
inline BOOLEAN IS_STRING_LITERAL_OBJECT(OBJECT_PTR x) { return (x & BIT_MASK) == STRING_LITERAL_TAG; }
inline BOOLEAN IS_CHAR_OBJECT(OBJECT_PTR x)           { return (x & BIT_MASK) == CHAR_TAG; }
inline BOOLEAN IS_MACRO_OBJECT(OBJECT_PTR x)          { return (x & BIT_MASK) == MACRO_TAG; }
inline BOOLEAN IS_ARRAY_OBJECT(OBJECT_PTR x)          { return (x & BIT_MASK) == ARRAY_TAG; }
inline BOOLEAN IS_CONTINUATION_OBJECT(OBJECT_PTR x)   { return (x & BIT_MASK) == CONTINUATION_TAG; }

//registers
OBJECT_PTR reg_accumulator;
OBJECT_PTR reg_next_expression;
OBJECT_PTR reg_current_env;
OBJECT_PTR reg_current_value_rib;
OBJECT_PTR reg_current_stack;

/*symbols corresponding to assembler mnemonics */
OBJECT_PTR HALT;                  
OBJECT_PTR REFER;
OBJECT_PTR CONSTANT;
OBJECT_PTR CLOSE;
OBJECT_PTR MACRO;
OBJECT_PTR TEST;
OBJECT_PTR ASSIGN;         
OBJECT_PTR CONTI;
OBJECT_PTR NUATE;
OBJECT_PTR FRAME;
OBJECT_PTR ARGUMENT;
OBJECT_PTR APPLY;
OBJECT_PTR RETURN;
/* end symbols corresponding to assembler mnemonics */

extern void print_stack();

void initialize()
{
  initialize_heap();

  initialize_free_list();

  nof_packages = 0;

  create_package("CORE");

  initialize_core_package();

  top_level_env = NIL;
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
  fprintf(stdout, "Welcome to pLISP. Type 'quit' to exit.");
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
  else if(IS_CLOSURE_OBJECT(obj_ptr))
    print_closure_object(obj_ptr);
  else if(IS_CONTINUATION_OBJECT(obj_ptr))
    fprintf(stdout, "#<CONTINUATION #x%08x> ", obj_ptr);
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
  else
    assert(false);

  fflush(stdout);

  log_function_exit("print_object");
}

OBJECT_PTR cons(OBJECT_PTR car, OBJECT_PTR cdr)
{
  log_function_entry("cons");

  RAW_PTR ptr = object_alloc(2);

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

OBJECT_PTR car(OBJECT_PTR cons_obj)
{
  log_function_entry("car");

  OBJECT_PTR ret;

  if(cons_obj == NIL)
    ret = NIL;
  else
  {
    assert(IS_CONS_OBJECT(cons_obj));

    ret = get_heap(cons_obj >> CONS_SHIFT);
  }

  log_function_exit("car");

  return ret;

}

OBJECT_PTR cdr(OBJECT_PTR cons_obj)
{
  log_function_entry("cdr");

  OBJECT_PTR ret;

  if(cons_obj == NIL)
    ret = NIL;
  else
  {
    assert(IS_CONS_OBJECT(cons_obj));

    ret = get_heap((cons_obj >> CONS_SHIFT) + 1);
  }

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

  if((is_atom(cdr_obj) || IS_CLOSURE_OBJECT(cdr_obj) || IS_MACRO_OBJECT(cdr_obj) || IS_CONTINUATION_OBJECT(cdr_obj))  && cdr_obj != NIL)
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

    while(rest != NIL && !(is_atom(rest) || IS_CLOSURE_OBJECT(rest) || IS_MACRO_OBJECT(rest) || IS_CONTINUATION_OBJECT(rest)))
    {
      print_object(car(rest));
      fprintf(stdout, " ");
      rest = cdr(rest);
    }

    if((is_atom(rest) || IS_CLOSURE_OBJECT(rest) || IS_MACRO_OBJECT(rest) || IS_CONTINUATION_OBJECT(rest)) && rest != NIL)
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

  //TODO: extend this for array objects (any others?)

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

OBJECT_PTR create_closure_object(OBJECT_PTR env_list, OBJECT_PTR params, OBJECT_PTR body)
{
  RAW_PTR ptr = object_alloc(3);

  set_heap(ptr, env_list);
  set_heap(ptr + 1, params);
  set_heap(ptr + 2, body);

  insert_node(&white, create_node((ptr << CLOSURE_SHIFT) + CLOSURE_TAG));
  
  return (ptr << CLOSURE_SHIFT) + CLOSURE_TAG;
}

OBJECT_PTR clone_object(OBJECT_PTR obj)
{
  log_function_entry("clone_object");

#ifdef DEBUG
  print_object(obj);
  fprintf(stdout, "\n");
#endif
  
  OBJECT_PTR ret;

  if(is_atom(obj) || IS_CONTINUATION_OBJECT(obj))
    ret = obj; //atoms are immutable and are reused; continuation objects are also not cloned
  else
  {
    if(IS_CONS_OBJECT(obj))
      ret = cons(clone_object(car(obj)), clone_object(cdr(obj)));
    else if(IS_CLOSURE_OBJECT(obj))
      ret = create_closure_object(clone_object(get_env_list(obj)),
                                  clone_object(get_params_object(obj)), 
                                  clone_object(get_body_object(obj)));
    else if(IS_MACRO_OBJECT(obj))
      ret = create_macro_object(clone_object(get_env_list(obj)),
				clone_object(get_params_object(obj)), 
				clone_object(get_body_object(obj)));
    else if(IS_ARRAY_OBJECT(obj))
    {
      RAW_PTR ptr = obj >> ARRAY_SHIFT;
      int len = get_int_value(get_heap(ptr));

      RAW_PTR new_ptr = object_alloc(len+1);
      
      set_heap(new_ptr, get_heap(ptr));

      int i;

      for(i=1; i<=len; i++)
	set_heap(new_ptr + i, clone_object(get_heap(ptr + i)));

      insert_node(&white, create_node((new_ptr << ARRAY_SHIFT) + ARRAY_TAG));

      ret = (new_ptr << ARRAY_SHIFT) + ARRAY_TAG;
    }
  }

  log_function_exit("clone_object");

#ifdef DEBUG
  print_object(ret);
  fprintf(stdout, "\n");
#endif

  return ret;
}

OBJECT_PTR get_env_list(OBJECT_PTR obj)
{
  assert(IS_CLOSURE_OBJECT(obj) || IS_MACRO_OBJECT(obj));
  return get_heap(obj >> (IS_CLOSURE_OBJECT(obj) ? CLOSURE_SHIFT : MACRO_SHIFT));
}

OBJECT_PTR get_params_object(OBJECT_PTR obj)
{
  assert(IS_CLOSURE_OBJECT(obj) || IS_MACRO_OBJECT(obj));
  return get_heap((obj >> (IS_CLOSURE_OBJECT(obj) ? CLOSURE_SHIFT : MACRO_SHIFT)) + 1);
}

OBJECT_PTR get_body_object(OBJECT_PTR obj)
{
  assert(IS_CLOSURE_OBJECT(obj) || IS_MACRO_OBJECT(obj));
  return get_heap((obj >> (IS_CLOSURE_OBJECT(obj) ? CLOSURE_SHIFT : MACRO_SHIFT)) + 2);
}

void print_closure_object(OBJECT_PTR obj)
{
  fprintf(stdout, "#<FUNCTION #x%08x> ", obj);

#ifdef DEBUG
  fprintf(stdout, "\nPARAMETERS: ");
  print_object(get_params_object(obj));
  fprintf(stdout,"\nBODY: ");
  print_object(get_body_object(obj));
  fprintf(stdout,"\nENV: ");
  print_object(get_env_list(obj));
#endif
}

int length(OBJECT_PTR cons_obj)
{
  if(cons_obj == NIL)
    return 0;

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

  //if symbol is not found, check
  //in the top level environment
  if(!found)
  {
    OBJECT_PTR result = get_symbol_value_from_env(symbol_obj, top_level_env);

    if(car(result) == TRUE)
    {
       ret = cons(TRUE, cdr(result));
       found = true;
    }
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
	set_heap((car(rest2) >> CONS_SHIFT) + 1, val);
	return symbol_obj;
      }

      rest2 = cdr(rest2);
    }

    rest1 = cdr(rest1);
  }    

  //check the top level environment 
  OBJECT_PTR rest2 = top_level_env;

  while(rest2 != NIL)
  {
    if(equal(CAAR(rest2),symbol_obj))
    {
      set_heap((car(rest2) >> CONS_SHIFT) + 1, val);
      return symbol_obj;
    }

    rest2 = cdr(rest2);
  }

  return NIL;
}

void add_to_top_level_environment(OBJECT_PTR symbol_obj, OBJECT_PTR val)
{
  OBJECT_PTR rest;

  if(top_level_env == NIL)
  {
    top_level_env = cons(cons(symbol_obj,val), 
                         NIL);
    return;
  }
  else
  { 
    rest = top_level_env;

    while(rest != NIL)
    {
      //symbol already exists in the environment,
      //so replace its existing binding with
      //the new value
      if(equal(CAAR(rest),symbol_obj))
      {
        set_heap((car(rest) >> CONS_SHIFT) + 1, val);
        return;
      }
      rest = cdr(rest);
    }

    //symbol does not exist in the environment
    set_heap((last_cell(top_level_env) >> CONS_SHIFT) + 1, cons(cons(symbol_obj, val), NIL));
  }
}

BOOLEAN is_special_form(OBJECT_PTR form)
{
  if(IS_SYMBOL_OBJECT(form))
  {
    int index = form >> SYMBOL_SHIFT;

    return (index >= 0 && index <= NOF_SPECIAL_SYMBOLS);
  }

  return false;
}

OBJECT_PTR create_macro_object(OBJECT_PTR env_list, OBJECT_PTR params, OBJECT_PTR body)
{
  RAW_PTR ptr = object_alloc(3);

  set_heap(ptr, env_list);
  set_heap(ptr + 1, params);
  set_heap(ptr + 2, body);

  insert_node(&white, create_node((ptr << MACRO_SHIFT) + MACRO_TAG));
  
  return (ptr << MACRO_SHIFT) + MACRO_TAG;
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

      if(CAAR(rest) == COMMA_AT)
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
  /* There are 11 assembler mnemonics that are defined for convenience
     in addtion to the special symbols . These mnemonics are not special,
     i.e., it doesn't matter if the user source code uses them */
  packages[CORE_PACKAGE_INDEX].nof_symbols = NOF_SPECIAL_SYMBOLS + 12; 
  packages[CORE_PACKAGE_INDEX].symbols = (char **)malloc(packages[CORE_PACKAGE_INDEX].nof_symbols * sizeof(char *));

  packages[CORE_PACKAGE_INDEX].symbols[0] = strdup("T");
  TRUE = SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[1] = strdup("NIL");
  NIL = (1 << SYMBOL_SHIFT) + SYMBOL_TAG;  

  packages[CORE_PACKAGE_INDEX].symbols[2] = strdup("QUOTE");
  QUOTE = (2 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[3] = strdup("ATOM");
  ATOM = (3 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[4] = strdup("EQ");
  EQ = (4 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[5] = strdup("CAR");
  CAR = (5 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[6] = strdup("CDR");
  CDR = (6 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[7] = strdup("CONS");
  CONS = (7 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[8] = strdup("LAMBDA");
  LAMBDA = (8 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[9] = strdup("DEFUN");
  DEFUN = (9 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[10] = strdup("SET");
  SET = (10 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[11] = strdup("+");
  ADD = (11 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[12] = strdup("-");
  SUB = (12 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[13] = strdup("*");
  MULT = (13 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[14] = strdup("/");
  DIV = (14 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[15] = strdup("PROGN");
  PROGN = (15 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[16] = strdup("PRINT");
  PRINT = (16 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[17] = strdup("DEFVAR");
  DEFVAR = (17 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[18] = strdup("LIST");
  LST = (18 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[19] = strdup("LISTP");
  LISTP = (19 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[20] = strdup("SYMBOL-VALUE");
  SYMBOL_VALUE = (20 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[21] = strdup("DEFMACRO");
  DEFMACRO = (21 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[22] = strdup("BACKQUOTE");
  BACKQUOTE = (22 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[23] = strdup(">");
  GT = (23 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[24] = strdup("GENSYM");
  GENSYM = (24 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[25] = strdup("SETCAR");
  SETCAR = (25 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[26] = strdup("SETCDR");
  SETCDR = (26 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[27] = strdup("ERROR");
  ERROR = (27 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[28] = strdup("CREATE-PACKAGE");
  CREATE_PACKAGE = (28 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[29] = strdup("IN-PACKAGE");
  IN_PACKAGE = (29 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[30] = strdup("COMMA");
  COMMA = (30 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[31] = strdup("COMMA-AT");
  COMMA_AT = (31 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[32] = strdup("EXPAND-MACRO");
  EXPAND_MACRO = (32 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[33] = strdup("APPLY");
  APPLY = (33 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[34] = strdup("STRING");
  STRING = (34 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[35] = strdup("MAKE-ARRAY");
  MAKE_ARRAY = (35 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[36] = strdup("ARRAY-GET");
  ARRAY_GET = (36 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[37] = strdup("ARRAY-SET");
  ARRAY_SET = (37 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[38] = strdup("SUB-ARRAY");
  SUB_ARRAY = (38 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[39] = strdup("ARRAY-LENGTH");
  ARRAY_LENGTH = (39 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[40] = strdup("PRINT-STRING");
  PRINT_STRING = (40 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[41] = strdup("LABELS");
  LABELS = (41 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[42] = strdup("CREATE-IMAGE");
  CREATE_IMAGE = (42 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[43] = strdup("BREAK");
  BREAK = (43 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[44] = strdup("LOAD-FOREIGN-LIBRARY");
  LOAD_FOREIGN_LIBRARY = (44 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[45] = strdup("CALL-FOREIGN-FUNCTION");
  CALL_FOREIGN_FUNCTION = (45 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[46] = strdup("PRINTENV");
  PRINTENV = (46 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[47] = strdup("CURRENTENV"); 
  CURRENTENV = (47 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[48] = strdup("IF");
  IF = (48 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[49] = strdup("EVAL"); 
  EVAL = (49 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[50] = strdup("CALL-CC"); 
  CALL_CC = (50 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[51] = strdup("DEFINE"); 
  DEFINE = (51 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[52] = strdup("RESUME"); 
  RESUME = (52 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[53] = strdup("BACKTRACE"); 
  BACKTRACE = (53 << SYMBOL_SHIFT) + SYMBOL_TAG;

  /* symbols corresponding to assembler mnemonics */

  packages[CORE_PACKAGE_INDEX].symbols[54] =  strdup("HALT");
  HALT = (54 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[55] =  strdup("REFER");
  REFER = (55 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[56] =  strdup("CONSTANT");
  CONSTANT = (56 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[57] =  strdup("CLOSE");
  CLOSE = (57 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[58] =  strdup("TEST");
  TEST = (58 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[59] =  strdup("ASSIGN");         
  ASSIGN = (59 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[60] =  strdup("CONTI");
  CONTI = (60 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[61] =  strdup("NUATE");
  NUATE = (61 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[62] =  strdup("FRAME");
  FRAME = (62 << SYMBOL_SHIFT) + SYMBOL_TAG;

  packages[CORE_PACKAGE_INDEX].symbols[63] =  strdup("ARGUMENT");
  ARGUMENT = (63 << SYMBOL_SHIFT) + SYMBOL_TAG;

  /* APPLY already defined as a special symbol */

  packages[CORE_PACKAGE_INDEX].symbols[64] = strdup("RETURN");
  RETURN = (64 << SYMBOL_SHIFT) + SYMBOL_TAG;

  /* DEFINE already defined as a special symbol */

  packages[CORE_PACKAGE_INDEX].symbols[65] = strdup("MACRO");
  MACRO = (65 << SYMBOL_SHIFT) + SYMBOL_TAG;

  /* end symbols corresponding to assembler mnemonics */

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

  assert(package_index != NOT_FOUND);

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

  if(package_index < 0 || package_index >= nof_packages)
    assert(false);

  if(symbol_index < 0 || symbol_index >= packages[package_index].nof_symbols)
    assert(false);

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

  assert(package_index != NOT_FOUND);

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

  //get_heap() will fail (is_valid_object(), actually)
  //fi.i = get_heap(obj >> FLOAT_SHIFT); 
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

  //set_heap() will fail if the last
  //four bits of the float object are not 0111 (i.e. FLOAT_TAG)
  //set_heap(ptr, fi.i);
  heap[ptr] = fi.i;

  insert_node(&white, create_node((ptr << FLOAT_SHIFT) + FLOAT_TAG));

  log_function_exit("convert_float_to_object");

  return (ptr << FLOAT_SHIFT) + FLOAT_TAG;
  
}

void print_array_object(OBJECT_PTR array)
{

  log_function_entry("print_array_object");

  fprintf(stdout, "[");

  int length = get_int_value(get_heap(array >> ARRAY_SHIFT));

  int i;

  for(i=0; i< length; i++)
  {
    print_object(get_heap((array >> ARRAY_SHIFT) + i + 1));
    fprintf(stdout, " ");
  }

  if(length > 0)
    fprintf(stdout, "\b");

  fprintf(stdout, "]");

  log_function_exit("print_array_object");
}

void print_string(OBJECT_PTR string_object)
{
  assert(is_string_object(string_object));

  RAW_PTR ptr = string_object >> ARRAY_SHIFT;

  int len = get_int_value(get_heap(ptr));

  int i;

  fprintf(stdout, "\"");

  for(i=1; i<=len; i++)
    fprintf(stdout, "%c", get_heap(ptr + i) >> CHAR_SHIFT);

  fprintf(stdout, "\"");
}

BOOLEAN is_string_object(OBJECT_PTR obj)
{
  if(!(IS_ARRAY_OBJECT(obj)))
    return false;

  RAW_PTR ptr = obj >> ARRAY_SHIFT;

  int len = get_int_value(get_heap(ptr));

  int i;

  for(i=1; i<=len; i++)
  {
    if(!(IS_CHAR_OBJECT(get_heap(ptr+i))))
      return false;
  }

  return true;
}

char *get_string(OBJECT_PTR string_object)
{
  assert(is_string_object(string_object));

  RAW_PTR ptr = string_object >> ARRAY_SHIFT;

  int len = get_int_value(get_heap(ptr));

  char *ret = (char *)malloc(len * sizeof(char));

  int i;

  for(i=1; i<=len; i++)
    ret[i-1] = get_heap(ptr + i) >> CHAR_SHIFT;

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

#ifndef DEBUG_MEMORY
inline
#endif
void set_heap(RAW_PTR index, OBJECT_PTR val)
{
  if(!is_valid_object(val))
    assert(false);

  heap[index] = val;
}

BOOLEAN is_valid_object(OBJECT_PTR obj)
{

  if(IS_SYMBOL_OBJECT(obj))
  {
    int package_index = obj >> (SYMBOL_BITS + SYMBOL_SHIFT);
    int symbol_index =  (obj >> SYMBOL_SHIFT) & TWO_RAISED_TO_SYMBOL_BITS_MINUS_1;

    return package_index >= 0 && 
           package_index < nof_packages &&
           symbol_index >= 0 &&
           symbol_index < packages[package_index].nof_symbols;
  }

  return IS_CONS_OBJECT(obj)           ||
         IS_CLOSURE_OBJECT(obj)        ||
         IS_MACRO_OBJECT(obj)          ||
         IS_ARRAY_OBJECT(obj)          ||
         IS_INTEGER_OBJECT(obj)        ||
         IS_STRING_LITERAL_OBJECT(obj) ||
         IS_CHAR_OBJECT(obj)           ||
         IS_FLOAT_OBJECT(obj)          ||
         IS_CONTINUATION_OBJECT(obj);
}

#ifndef DEBUG_MEMORY
inline
#endif
OBJECT_PTR get_heap(RAW_PTR ptr)
{
  if(ptr == null)
    assert(false);

  OBJECT_PTR ret = heap[ptr];

  if(!is_valid_object(ret))
  {
    printf("%d %d\n", ptr, ret);
    assert(false);
  }

  return ret;
}
