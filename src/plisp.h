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

#include <stdlib.h>
#include <string.h>

#define SYMBOL_STRING_SIZE 100

#define MAX_STRING_LENGTH 16192

#define MAX_STACK_DEPTH 10000

#define YY_NO_INPUT

#define SYMBOL 1
#define LIST 2
#define INTEGER 3
#define STRING_LITERAL 4
#define CHARACTER 5
#define FLOAT 6

#define null -1 //not using NULL because 0 is a valid entry in our heap

#define NOT_FOUND -1

#define BIT_MASK 15

//the 28 bits are split into six bits
//for indexing into the package table,
//and 22 bits for indexing into the
//symbols table of the chosen package table entry
#define PACKAGE_BITS         6
#define SYMBOL_BITS         22

#define OBJECT_SHIFT         4

#define SYMBOL_TAG           1
#define STRING_LITERAL_TAG   2
#define CHAR_TAG             3
#define INTEGER_TAG          4
#define FLOAT_TAG            5
#define CONS_TAG             6
#define CLOSURE_TAG          7
#define MACRO_TAG            8
#define ARRAY_TAG            9
#define CONTINUATION_TAG    10

#define SYMBOL_STRING_SIZE 100

#define CORE_PACKAGE_INDEX 0

#define MAX_FOREIGN_LIBRARY_COUNT 100

#define true 1
#define false 0

#define TWO_RAISED_TO_SYMBOL_BITS_MINUS_1 4194303

#define DEFAULT_DEBUG_WINDOW_POSX 650
#define DEFAULT_DEBUG_WINDOW_POSY 200
#define DEFAULT_DEBUG_WINDOW_WIDTH 600
#define DEFAULT_DEBUG_WINDOW_HEIGHT 400

#define DEFAULT_PROFILER_WINDOW_POSX 650
#define DEFAULT_PROFILER_WINDOW_POSY 200
#define DEFAULT_PROFILER_WINDOW_WIDTH 600
#define DEFAULT_PROFILER_WINDOW_HEIGHT 400

typedef unsigned int OBJECT_PTR;

typedef int BOOLEAN;

typedef struct package
{
  char *name;
  int nof_symbols;
  char ** symbols;
} package_t;

typedef struct expression
{
  int type;
  char *package_name;
  char *atom_value;
  int integer_value;
  float float_value;
  char char_value;
  int nof_elements;
  struct expression **elements;
} expression_t;

//for implementing garbage collection (tri-colour marking)
struct node
{
  struct node *left;
  struct node *right;
  OBJECT_PTR key;
} ;

struct nlist
{
  struct nlist *next;
  OBJECT_PTR ptr;
  int value;
};

union float_and_uint
{
  unsigned int i;
  float f;
};

typedef struct profiling_datum
{
  unsigned int count;
  unsigned int mem_allocated;
  unsigned int mem_deallocated;
  double elapsed_wall_time;
  double elapsed_cpu_time;
} profiling_datum_t;

typedef unsigned int (*cmpfn)();

typedef enum {IN_CODE, IN_STRING_LITERAL, IN_SINGLE_LINE_COMMENT, IN_MULTI_LINE_COMMENT} cursor_pos_t;

expression_t *create_expression(int, char *, int, float, int);
void delete_expression(expression_t *);
void print_expression(expression_t *);

int repl();
void prompt();
void cleanup();

void welcome();

void print_object(OBJECT_PTR);

OBJECT_PTR evaluate_expression(expression_t *, OBJECT_PTR);

OBJECT_PTR get_symbol_object(char *);
OBJECT_PTR cons(OBJECT_PTR, OBJECT_PTR);
OBJECT_PTR build_list_object(expression_t **, int, int);

OBJECT_PTR car(OBJECT_PTR);
OBJECT_PTR cdr(OBJECT_PTR);

void print_cons_object_orig(OBJECT_PTR);
void print_cons_object(OBJECT_PTR);

int is_atom(OBJECT_PTR);

int convert_expression_to_object(expression_t *, OBJECT_PTR *);

BOOLEAN equal(OBJECT_PTR, OBJECT_PTR);

OBJECT_PTR create_closure_object(OBJECT_PTR, OBJECT_PTR, OBJECT_PTR, OBJECT_PTR);

OBJECT_PTR invoke_function(OBJECT_PTR, OBJECT_PTR, OBJECT_PTR);

OBJECT_PTR clone_object(OBJECT_PTR);

OBJECT_PTR get_env_list(OBJECT_PTR);
OBJECT_PTR get_params_object(OBJECT_PTR);
OBJECT_PTR get_body_object(OBJECT_PTR);
OBJECT_PTR get_source_object(OBJECT_PTR);

void print_closure_object(OBJECT_PTR);

OBJECT_PTR get_symbol_value_from_env(OBJECT_PTR, OBJECT_PTR);
OBJECT_PTR get_symbol_value(OBJECT_PTR, OBJECT_PTR);

OBJECT_PTR update_environment(OBJECT_PTR, OBJECT_PTR, OBJECT_PTR);
void add_to_top_level_environment(OBJECT_PTR, OBJECT_PTR);

BOOLEAN is_special_form(OBJECT_PTR);

OBJECT_PTR invoke_macro(OBJECT_PTR, OBJECT_PTR, OBJECT_PTR, BOOLEAN);
OBJECT_PTR create_macro_object(OBJECT_PTR, OBJECT_PTR, OBJECT_PTR, OBJECT_PTR);
void print_macro_object(OBJECT_PTR);

BOOLEAN form_contains_comma_at(OBJECT_PTR);
OBJECT_PTR last_cell(OBJECT_PTR);

OBJECT_PTR gensym();

//void raise_error();

void create_package(char *);
void initialize_core_package();

int find_package(char *);
int find_qualified_symbol(int, char *);
OBJECT_PTR get_qualified_symbol_object(char *, char *);

void print_symbol(OBJECT_PTR, char *);
void print_qualified_symbol(OBJECT_PTR, char *);
int add_qualified_symbol(char *, char *);
int find_symbol(char *, int);

char *get_symbol_name(OBJECT_PTR);

OBJECT_PTR get_keyword_arg(OBJECT_PTR, OBJECT_PTR);

BOOLEAN is_keyword_symbol(OBJECT_PTR symbol_object);
BOOLEAN contains_keyword_parameter(OBJECT_PTR list);

int get_int_value(OBJECT_PTR);
OBJECT_PTR convert_int_to_object(int);

float get_float_value(OBJECT_PTR);
OBJECT_PTR convert_float_to_object(float);

void print_array_object(OBJECT_PTR);

void create_image(char *);
int load_from_image(char *);

OBJECT_PTR load_foreign_library(OBJECT_PTR, OBJECT_PTR);

BOOLEAN is_string_object(OBJECT_PTR);

char *get_string(OBJECT_PTR);
void print_string(OBJECT_PTR);

OBJECT_PTR eval_if(OBJECT_PTR, OBJECT_PTR, OBJECT_PTR, OBJECT_PTR);
OBJECT_PTR eval_while(OBJECT_PTR, OBJECT_PTR, OBJECT_PTR);

BOOLEAN is_valid_object(OBJECT_PTR);

void print_continuation_object(OBJECT_PTR);

OBJECT_PTR create_call_frame(OBJECT_PTR,
                             OBJECT_PTR,
                             OBJECT_PTR,
                             OBJECT_PTR);
OBJECT_PTR add_call_frame_to_stack(OBJECT_PTR, OBJECT_PTR);

OBJECT_PTR create_current_continuation();

OBJECT_PTR compile_loop(OBJECT_PTR, OBJECT_PTR, OBJECT_PTR);
OBJECT_PTR compile(OBJECT_PTR, OBJECT_PTR);
OBJECT_PTR compile_progn(OBJECT_PTR, OBJECT_PTR);
void eval();
void raise_error(char *);

void print_stack();

OBJECT_PTR eval_backquote(OBJECT_PTR);
OBJECT_PTR eval_string(OBJECT_PTR);
OBJECT_PTR eval_make_array(OBJECT_PTR, OBJECT_PTR);
OBJECT_PTR eval_sub_array(OBJECT_PTR, OBJECT_PTR, OBJECT_PTR);

OBJECT_PTR call_foreign_function(OBJECT_PTR, OBJECT_PTR, OBJECT_PTR);

BOOLEAN is_permitted_in_debug_mode(OBJECT_PTR);

void print_backtrace();

int load_core_library();

void print_state();

int format(OBJECT_PTR);

#ifdef GUI
int format_for_gui(OBJECT_PTR);
#endif

OBJECT_PTR get_symbol_from_value_from_env(OBJECT_PTR, OBJECT_PTR);
OBJECT_PTR get_symbol_from_value(OBJECT_PTR, OBJECT_PTR);

OBJECT_PTR get_continuation_for_return(OBJECT_PTR);

void print_copyright_notice();

void throw_generic_exception(char *);
void throw_exception(char *, char *);

int print_object_to_string(OBJECT_PTR, char *, int);
int print_cons_object_to_string(OBJECT_PTR, char *, int);
int print_closure_object_to_string(OBJECT_PTR, char *, int);
int print_macro_object_to_string(OBJECT_PTR, char *, int);
int print_array_object_to_string(OBJECT_PTR, char *, int);
int print_string_object_to_string(OBJECT_PTR, char *, int);

OBJECT_PTR list(int, ...);

OBJECT_PTR convert_symbol_to_core_package_symbol(OBJECT_PTR);
