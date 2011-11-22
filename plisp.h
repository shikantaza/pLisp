#include <stdlib.h>

#define YY_NO_INPUT

#define SYMBOL 1
#define LIST 2
#define INTEGER 3
#define STRING_LITERAL 4
#define CHARACTER 5
#define FLOAT 6

#define HEAP_SIZE 1048576
#define null -1  //not using NULL because 0 is a valid entry in our heap

#define NOT_FOUND -1

//all in-built types are
//identified by a four-bit tag
#define SYMBOL_SHIFT         4
#define CONS_SHIFT           4
#define FN_SHIFT             4
#define INTEGER_SHIFT        4
#define STRING_LITERAL_SHIFT 4
#define CHAR_SHIFT           4
#define FLOAT_SHIFT          4
#define MACRO_SHIFT          4
#define ARRAY_SHIFT          4

#define BIT_MASK 15

//the 28 bits reserved for
//symbols is split into six bits
//for indexing into the package table,
//and 22 bits for indexing into the
//symbols table of the chosen package table entry
#define PACKAGE_BITS         6
#define SYMBOL_BITS         22

#define SYMBOL_TAG          1
#define CONS_TAG            2
#define FN_TAG              3
#define INTEGER_TAG         4
#define STRING_LITERAL_TAG  5
#define CHAR_TAG            6
#define FLOAT_TAG           7
#define MACRO_TAG           8
#define ARRAY_TAG           9

#define IS_SYMBOL_OBJECT(x)         (x & BIT_MASK) == SYMBOL_TAG
#define IS_CONS_OBJECT(x)           (x & BIT_MASK) == CONS_TAG
#define IS_FN_OBJECT(x)             (x & BIT_MASK) == FN_TAG
#define IS_INTEGER_OBJECT(x)        (x & BIT_MASK) == INTEGER_TAG
#define IS_FLOAT_OBJECT(x)          (x & BIT_MASK) == FLOAT_TAG
#define IS_STRING_LITERAL_OBJECT(x) (x & BIT_MASK) == STRING_LITERAL_TAG
#define IS_CHAR_OBJECT(x)           (x & BIT_MASK) == CHAR_TAG
#define IS_MACRO_OBJECT(x)          (x & BIT_MASK) == MACRO_TAG
#define IS_ARRAY_OBJECT(x)          (x & BIT_MASK) == ARRAY_TAG

#define CAAR(x) car(car(x))
#define CDAR(x) cdr(car(x))
#define CADR(x) car(cdr(x))
#define CDDR(x) cdr(cdr(x))
#define CDDAR(x) cdr(cdr(car(x)))
#define CAADR(x) car(car(cdr(x)))
#define CADAR(x) car(cdr(car(x)))
#define CADDR(x) car(cdr(cdr(x)))
#define CDDDR(x) cdr(cdr(cdr(x)))
#define CADDDR(x) car(cdr(cdr(cdr(x))))
#define CADDAR(x) car(cdr(cdr(car(x))))
#define CADADR(x) car(cdr(car(cdr(x))))

#define QUOT                 "QUOTE"
#define CONS                 "CONS"
#define EQ                   "EQ"
#define ATOM                 "ATOM"
#define CAR                  "CAR"
#define CDR                  "CDR"
#define COND                 "COND"
#define LAMBDA               "LAMBDA"
#define DEFUN                "DEFUN"
#define SET                  "SET"

#define ADD                  "+"
#define SUB                  "-"
#define MULT                 "*"
#define DIV                  "/"

#define PROGN                "PROGN"
#define PRINT                "PRINT"
#define DEFVAR               "DEFVAR"
#define LET                  "LET"
#define LST                  "LIST"
#define LISTP                "LISTP"
#define SYMBOL_VALUE         "SYMBOL-VALUE"
#define DEFMACRO             "DEFMACRO"
#define BACKQUOTE            "BACKQUOTE"

#define GT                    ">"
#define GENSYM                "GENSYM"

#define SETCAR                "SETCAR"
#define SETCDR                "SETCDR"

#define ERROR                 "ERROR"

#define CREATE_PACKAGE        "CREATE-PACKAGE"
#define IN_PACKAGE            "IN-PACKAGE"

#define COMMA                 "COMMA"
#define COMMA_AT              "COMMA-AT"

#define EXPAND_MACRO          "EXPAND-MACRO"

#define APPLY                 "APPLY"

#define STRING                "STRING"

#define MAKE_ARRAY            "MAKE-ARRAY"
#define ARRAY_GET             "ARRAY-GET"
#define ARRAY_SET             "ARRAY-SET"
#define SUB_ARRAY             "SUB-ARRAY"
#define ARRAY_LENGTH          "ARRAY-LENGTH"

#define PRINT_STRING          "PRINT-STRING"

#define LABELS                "LABELS"

#define CREATE_IMAGE          "CREATE-IMAGE"

#define BREAK                 "BREAK"

#define LOAD_FOREIGN_LIBRARY  "LOAD-FOREIGN-LIBRARY"
#define CALL_FOREIGN_FUNCTION "CALL-FOREIGN-FUNCTION"

#define PRINTENV              "PRINTENV"
#define CURRENTENV            "CURRENTENV"

typedef unsigned int RAW_PTR;
typedef unsigned int OBJECT_PTR;

typedef int BOOLEAN;

#define true 1
#define false 0

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

union float_and_uint
{
  unsigned int i;
  float f;
};

//for implementing garbage collection (tri-colour marking)
struct node
{
  struct node *left;
  struct node *right;
  OBJECT_PTR key;
} ;

expression_t *create_expression(int, char *, int, float, int);
void delete_expression(expression_t *);
void print_expression(expression_t *);

void repl();
void prompt();
void cleanup();

void welcome();

void print_object(OBJECT_PTR);

OBJECT_PTR evaluate_expression(expression_t *, OBJECT_PTR);

RAW_PTR object_alloc(int);
OBJECT_PTR get_symbol_object(char *);
OBJECT_PTR cons(OBJECT_PTR, OBJECT_PTR);
OBJECT_PTR build_list_object(expression_t **, int, int);

OBJECT_PTR car(OBJECT_PTR);
OBJECT_PTR cdr(OBJECT_PTR);

void print_cons_object_orig(OBJECT_PTR);
void print_cons_object(OBJECT_PTR);

int is_atom(OBJECT_PTR);

OBJECT_PTR convert_expression_to_object(expression_t *);

BOOLEAN equal(OBJECT_PTR, OBJECT_PTR);

OBJECT_PTR eval(OBJECT_PTR, OBJECT_PTR);

OBJECT_PTR create_function_object(OBJECT_PTR, OBJECT_PTR, OBJECT_PTR);

OBJECT_PTR invoke_function(OBJECT_PTR, OBJECT_PTR, OBJECT_PTR);

OBJECT_PTR clone_object(OBJECT_PTR);

OBJECT_PTR get_env_list(OBJECT_PTR);
OBJECT_PTR get_params_object(OBJECT_PTR);
OBJECT_PTR get_body_object(OBJECT_PTR);

void print_function_object(OBJECT_PTR);

OBJECT_PTR get_symbol_value_from_env(OBJECT_PTR, OBJECT_PTR);
OBJECT_PTR get_symbol_value(OBJECT_PTR, OBJECT_PTR);

OBJECT_PTR update_environment(OBJECT_PTR, OBJECT_PTR, OBJECT_PTR);
void add_to_environment(OBJECT_PTR, OBJECT_PTR, OBJECT_PTR);

OBJECT_PTR eval_and_build_list(OBJECT_PTR, OBJECT_PTR);

BOOLEAN is_special_form(OBJECT_PTR);

OBJECT_PTR invoke_macro(OBJECT_PTR, OBJECT_PTR, OBJECT_PTR, BOOLEAN);
OBJECT_PTR create_macro_object(OBJECT_PTR, OBJECT_PTR, OBJECT_PTR);
void print_macro_object(OBJECT_PTR);

OBJECT_PTR eval_backquote(OBJECT_PTR, OBJECT_PTR);
BOOLEAN form_contains_comma_at(OBJECT_PTR);
OBJECT_PTR last_cell(OBJECT_PTR);

OBJECT_PTR gensym();

void reset_exception_mechanism();
void print_stack_trace();
void raise_error();

void create_package(char *);
void initialize_core_package();

int find_package(char *);
int find_qualified_symbol(int, char *);
OBJECT_PTR get_qualified_symbol_object(char *, char *);

void print_symbol(OBJECT_PTR, char *);
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

OBJECT_PTR eval_string(OBJECT_PTR, OBJECT_PTR);

OBJECT_PTR eval_make_array(OBJECT_PTR, OBJECT_PTR, OBJECT_PTR);
OBJECT_PTR eval_array_get(OBJECT_PTR, OBJECT_PTR);
OBJECT_PTR eval_array_set(OBJECT_PTR, OBJECT_PTR);
void print_array_object(OBJECT_PTR);
OBJECT_PTR eval_sub_array(OBJECT_PTR, OBJECT_PTR, OBJECT_PTR, OBJECT_PTR);

OBJECT_PTR eval_print_string(OBJECT_PTR, OBJECT_PTR);

void create_image(char *);
void load_from_image(char *);

void enter_debug_mode(OBJECT_PTR);

OBJECT_PTR load_foreign_library(OBJECT_PTR, OBJECT_PTR);
OBJECT_PTR call_foreign_function(OBJECT_PTR, OBJECT_PTR, OBJECT_PTR, OBJECT_PTR);

BOOLEAN is_string_object(OBJECT_PTR);

char *get_string(OBJECT_PTR);
void print_string(OBJECT_PTR);

void initialize_free_list();
void initialize_heap();

RAW_PTR get_last_segment();
void dealloc(RAW_PTR);

struct node *create_node(OBJECT_PTR);
void insert_node(struct node **, struct node *);
void remove_node(struct node **, OBJECT_PTR);
void destroy(struct node *);
void print_tree(struct node *);
BOOLEAN is_set_empty(struct node *);

void gc();
BOOLEAN is_dynamic_memory_object(OBJECT_PTR);
void build_grey_set();

int get_free_memory();
int get_size_of_tree(struct node *);

OBJECT_PTR shallow_copy(OBJECT_PTR);
