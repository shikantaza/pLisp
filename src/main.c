/**
  Copyright 2011-2019 Rajesh Jayaprakash <rajesh.jayaprakash@gmail.com>

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
#include <string.h>
#include <stdarg.h>

#include <unistd.h>

#include "plisp.h"

#include "util.h"

#include "memory.h"

expression_t *g_expr = NULL;

extern void yyparse();

//these are the variables that
//should be serialized to
//implement images

int nof_strings = 0;
char **strings = NULL;

OBJECT_PTR top_level_env;

unsigned int current_package;
unsigned int nof_packages;
package_t *packages;

int gen_sym_count = 0;

//end of variables that should
//be serialized to implement images

//standard objects defined
//to avoid creating them
//each time they're needed
OBJECT_PTR TRUE                  =  (OBJECT_PTR)(                      SYMBOL_TAG);
OBJECT_PTR NIL                   =  (OBJECT_PTR)((1 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR QUOTE                 =  (OBJECT_PTR)((2 << OBJECT_SHIFT) + SYMBOL_TAG);

#ifdef WIN32
OBJECT_PTR ATOM1                 =  (OBJECT_PTR)((3 << OBJECT_SHIFT) + SYMBOL_TAG);
#else
OBJECT_PTR ATOM                  =  (OBJECT_PTR)((3 << OBJECT_SHIFT) + SYMBOL_TAG);
#endif

OBJECT_PTR EQ                    =  (OBJECT_PTR)((4 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR CAR                   =  (OBJECT_PTR)((5 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR CDR                   =  (OBJECT_PTR)((6 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR CONS                  =  (OBJECT_PTR)((7 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR LAMBDA                =  (OBJECT_PTR)((8 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR SET                   =  (OBJECT_PTR)((9 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR ADD                   = (OBJECT_PTR)((10 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR SUB                   = (OBJECT_PTR)((11 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR MULT                  = (OBJECT_PTR)((12 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR DIV                   = (OBJECT_PTR)((13 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR PROGN                 = (OBJECT_PTR)((14 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR PRINT                 = (OBJECT_PTR)((15 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR LST                   = (OBJECT_PTR)((16 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR LISTP                 = (OBJECT_PTR)((17 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR SYMBOL_VALUE          = (OBJECT_PTR)((18 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR BACKQUOTE             = (OBJECT_PTR)((19 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR GT                    = (OBJECT_PTR)((20 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR GENSYM                = (OBJECT_PTR)((21 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR SETCAR                = (OBJECT_PTR)((22 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR SETCDR                = (OBJECT_PTR)((23 << OBJECT_SHIFT) + SYMBOL_TAG);

#ifdef WIN32
OBJECT_PTR ERROR1                = (OBJECT_PTR)((24 << OBJECT_SHIFT) + SYMBOL_TAG);
#else
OBJECT_PTR ERROR                 = (OBJECT_PTR)((24 << OBJECT_SHIFT) + SYMBOL_TAG);
#endif

OBJECT_PTR CREATE_PACKAGE        = (OBJECT_PTR)((25 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR IN_PACKAGE            = (OBJECT_PTR)((26 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR COMMA                 = (OBJECT_PTR)((27 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR COMMA_AT              = (OBJECT_PTR)((28 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR EXPAND_MACRO          = (OBJECT_PTR)((29 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR APPLY                 = (OBJECT_PTR)((30 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR STRING                = (OBJECT_PTR)((31 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR MAKE_ARRAY            = (OBJECT_PTR)((32 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR ARRAY_GET             = (OBJECT_PTR)((33 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR ARRAY_SET             = (OBJECT_PTR)((34 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR SUB_ARRAY             = (OBJECT_PTR)((35 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR ARRAY_LENGTH          = (OBJECT_PTR)((36 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR PRINT_STRING          = (OBJECT_PTR)((37 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR LABELS                = (OBJECT_PTR)((38 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR CREATE_IMAGE          = (OBJECT_PTR)((39 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR BREAK                 = (OBJECT_PTR)((40 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR LOAD_FOREIGN_LIBRARY  = (OBJECT_PTR)((41 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR CALL_FOREIGN_FUNCTION = (OBJECT_PTR)((42 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR ENV                   = (OBJECT_PTR)((43 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR IF                    = (OBJECT_PTR)((44 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR EVAL                  = (OBJECT_PTR)((45 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR CALL_CC               = (OBJECT_PTR)((46 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR DEFINE                = (OBJECT_PTR)((47 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR RESUME                = (OBJECT_PTR)((48 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR BACKTRACE             = (OBJECT_PTR)((49 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR LOAD_FILE             = (OBJECT_PTR)((50 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR CONSP                 = (OBJECT_PTR)((51 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR INTEGERP              = (OBJECT_PTR)((52 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR FLOATP                = (OBJECT_PTR)((53 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR CHARACTERP            = (OBJECT_PTR)((54 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR SYMBOLP               = (OBJECT_PTR)((55 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR STRINGP               = (OBJECT_PTR)((56 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR ARRAYP                = (OBJECT_PTR)((57 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR CLOSUREP              = (OBJECT_PTR)((58 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR MACROP                = (OBJECT_PTR)((59 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR CONTINUATIONP         = (OBJECT_PTR)((60 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR LAMBDA_EXPRESSION     = (OBJECT_PTR)((61 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR WHILE                 = (OBJECT_PTR)((62 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR FORMAT                = (OBJECT_PTR)((63 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR CLONE                 = (OBJECT_PTR)((64 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR RETURN                = (OBJECT_PTR)((65 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR COMPILE               = (OBJECT_PTR)((66 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR RETURN_FROM           = (OBJECT_PTR)((67 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR SYMBL                 = (OBJECT_PTR)((68 << OBJECT_SHIFT) + SYMBOL_TAG); //SYMBOL already taken
OBJECT_PTR SYMBOL_NAME           = (OBJECT_PTR)((69 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR UNBIND                = (OBJECT_PTR)((70 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR NEWLINE               = (OBJECT_PTR)((71 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR ABORT                 = (OBJECT_PTR)((72 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR TIME                  = (OBJECT_PTR)((73 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR PROFILE               = (OBJECT_PTR)((74 << OBJECT_SHIFT) + SYMBOL_TAG);

OBJECT_PTR NOT                   = (OBJECT_PTR)((75 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR LT                    = (OBJECT_PTR)((76 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR LEQ                   = (OBJECT_PTR)((77 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR GEQ                   = (OBJECT_PTR)((78 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR NEQ                   = (OBJECT_PTR)((79 << OBJECT_SHIFT) + SYMBOL_TAG);

OBJECT_PTR SAVE_OBJECT           = (OBJECT_PTR)((80 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR LOAD_OBJECT           = (OBJECT_PTR)((81 << OBJECT_SHIFT) + SYMBOL_TAG);

OBJECT_PTR COMPILEFN             = (OBJECT_PTR)((82 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR EXPORT_PACKAGE        = (OBJECT_PTR)((83 << OBJECT_SHIFT) + SYMBOL_TAG);

OBJECT_PTR COMPILE_EXP           = (OBJECT_PTR)((84 << OBJECT_SHIFT) + SYMBOL_TAG);

OBJECT_PTR IMPORT_PACKAGE        = (OBJECT_PTR)((85 << OBJECT_SHIFT) + SYMBOL_TAG);
//end of standard object definition

/* symbols corresponding to assembler mnemonics */
OBJECT_PTR HALT     = (OBJECT_PTR)((86 << OBJECT_SHIFT) + SYMBOL_TAG);                  
OBJECT_PTR REFER    = (OBJECT_PTR)((87 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR CONSTANT = (OBJECT_PTR)((88 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR CLOSE    = (OBJECT_PTR)((89 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR TEST     = (OBJECT_PTR)((90 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR ASSIGN   = (OBJECT_PTR)((91 << OBJECT_SHIFT) + SYMBOL_TAG);         
OBJECT_PTR CONTI    = (OBJECT_PTR)((92 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR NUATE    = (OBJECT_PTR)((93 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR FRAME    = (OBJECT_PTR)((94 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR ARGUMENT = (OBJECT_PTR)((95 << OBJECT_SHIFT) + SYMBOL_TAG);
/* APPLY already defined as a special symbol */
/* RETURN already defined as a special symbol */
/* DEFINE already defind as a special symbol */
OBJECT_PTR MACRO    = (OBJECT_PTR)((96 << OBJECT_SHIFT) + SYMBOL_TAG);
/* end symbols corresponding to assembler mnemonics */

/* symbols useful in FFI */
OBJECT_PTR INTEGR        = (OBJECT_PTR)((97 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR FLOT          = (OBJECT_PTR)((98 << OBJECT_SHIFT) + SYMBOL_TAG);

#ifdef WIN32
OBJECT_PTR CHAR1         = (OBJECT_PTR)((99 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR VOID1         = (OBJECT_PTR)((100 << OBJECT_SHIFT) + SYMBOL_TAG);
#else
OBJECT_PTR CHAR          = (OBJECT_PTR)((99 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR VOID          = (OBJECT_PTR)((100 << OBJECT_SHIFT) + SYMBOL_TAG);
#endif

OBJECT_PTR INT_POINTER   = (OBJECT_PTR)((101 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR FLOAT_POINTER = (OBJECT_PTR)((102 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR CHAR_POINTER  = (OBJECT_PTR)((103 << OBJECT_SHIFT) + SYMBOL_TAG);
/* end symbols useful in FFI */

OBJECT_PTR LET           = (OBJECT_PTR)((104 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR COND          = (OBJECT_PTR)((105 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR DOTIMES       = (OBJECT_PTR)((106 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR DOLIST        = (OBJECT_PTR)((107 << OBJECT_SHIFT) + SYMBOL_TAG);

OBJECT_PTR LET1          = (OBJECT_PTR)((108 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR DEFUN         = (OBJECT_PTR)((109 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR DEFMACRO      = (OBJECT_PTR)((110 << OBJECT_SHIFT) + SYMBOL_TAG);

//symbols needed for compile-exp
OBJECT_PTR NTH               = (OBJECT_PTR)((111 << OBJECT_SHIFT) + SYMBOL_TAG);
//OBJECT_PTR CALL_CC1          = (OBJECT_PTR)((111 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR MY_CONT_VAR       = (OBJECT_PTR)((112 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR SAVE_CONTINUATION = (OBJECT_PTR)((113 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR LETREC            = (OBJECT_PTR)((114 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR EXTRACT_NATIVE_FN = (OBJECT_PTR)((115 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR CREATE_FN_CLOSURE = (OBJECT_PTR)((116 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR CONCAT            = (OBJECT_PTR)((117 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR GET_CONTINUATION  = (OBJECT_PTR)((118 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR THROW             = (OBJECT_PTR)((119 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR GET_EXCEPTION_HANDLER  = (OBJECT_PTR)((120 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR ADD_EXCEPTION_HANDLER  = (OBJECT_PTR)((121 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR CALL_FF_INTERNAL  = (OBJECT_PTR)((122 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR REPL_FUNCTION     = (OBJECT_PTR)((123 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR SAVE_CONTINUATION_TO_RESUME     = (OBJECT_PTR)((124 << OBJECT_SHIFT) + SYMBOL_TAG);

OBJECT_PTR DISABLE_EXCEPTION_HANDLERS = (OBJECT_PTR)((125 << OBJECT_SHIFT) + SYMBOL_TAG);
OBJECT_PTR ENABLE_EXCEPTION_HANDLERS  = (OBJECT_PTR)((126 << OBJECT_SHIFT) + SYMBOL_TAG);
//end symbols needed for compile-exp

OBJECT_PTR GET_SOURCE  = (OBJECT_PTR)((127 << OBJECT_SHIFT) + SYMBOL_TAG);

OBJECT_PTR INSPECT_OBJECT  = (OBJECT_PTR)((128 << OBJECT_SHIFT) + SYMBOL_TAG);

//for performance
OBJECT_PTR CONS_NIL_NIL;
OBJECT_PTR CONS_APPLY_NIL;
OBJECT_PTR CONS_HALT_NIL;
OBJECT_PTR CONS_RETURN_NIL;

extern FILE *yyin;

extern BOOLEAN console_mode, single_expression_mode, pipe_mode;

#define NOF_SPECIAL_SYMBOLS     86
#define NOF_NON_SPECIAL_SYMBOLS 43

char err_buf[500];

BOOLEAN debug_mode = false;

int nof_dl_handles = 0;
void **dl_handles = NULL;

char *foreign_library_names[MAX_FOREIGN_LIBRARY_COUNT];

OBJECT_PTR CAAR(OBJECT_PTR x)    { return car(car(x)); }
OBJECT_PTR CDAR(OBJECT_PTR x)    { return cdr(car(x)); }
OBJECT_PTR CADR(OBJECT_PTR x)    { return car(cdr(x)); }
OBJECT_PTR CDDR(OBJECT_PTR x)    { return cdr(cdr(x)); }
OBJECT_PTR CDDAR(OBJECT_PTR x)   { return cdr(cdr(car(x))); }
OBJECT_PTR CAADR(OBJECT_PTR x)   { return car(car(cdr(x))); }
OBJECT_PTR CADAR(OBJECT_PTR x)   { return car(cdr(car(x))); }
OBJECT_PTR CADDR(OBJECT_PTR x)   { return car(cdr(cdr(x))); }
OBJECT_PTR CDDDR(OBJECT_PTR x)   { return cdr(cdr(cdr(x))); }
OBJECT_PTR CADDDR(OBJECT_PTR x)  { return car(cdr(cdr(cdr(x)))); }
OBJECT_PTR CADDAR(OBJECT_PTR x)  { return car(cdr(cdr(car(x)))); }
OBJECT_PTR CADADR(OBJECT_PTR x)  { return car(cdr(car(cdr(x)))); }
OBJECT_PTR CADDDDR(OBJECT_PTR x) { return car(cdr(cdr(cdr(cdr(x))))); }

OBJECT_PTR first(OBJECT_PTR x)  { return car(x); }
OBJECT_PTR second(OBJECT_PTR x) { return car(cdr(x)); }
OBJECT_PTR third(OBJECT_PTR x)  { return car(cdr(cdr(x))); } 
OBJECT_PTR fourth(OBJECT_PTR x) { return car(cdr(cdr(cdr(x)))); } 
OBJECT_PTR fifth(OBJECT_PTR x)  { return car(cdr(cdr(cdr(cdr(x))))); } 

BOOLEAN IS_SYMBOL_OBJECT(OBJECT_PTR x)         { return (x & BIT_MASK) == SYMBOL_TAG;         }
BOOLEAN IS_STRING_LITERAL_OBJECT(OBJECT_PTR x) { return (x & BIT_MASK) == STRING_LITERAL_TAG; }
BOOLEAN IS_CHAR_OBJECT(OBJECT_PTR x)           { return (x & BIT_MASK) == CHAR_TAG;           }
BOOLEAN IS_INTEGER_OBJECT(OBJECT_PTR x)        { return (x & BIT_MASK) == INTEGER_TAG;        }
BOOLEAN IS_FLOAT_OBJECT(OBJECT_PTR x)          { return (x & BIT_MASK) == FLOAT_TAG;          }
BOOLEAN IS_CONS_OBJECT(OBJECT_PTR x)           { return (x & BIT_MASK) == CONS_TAG;           }
BOOLEAN IS_CLOSURE_OBJECT(OBJECT_PTR x)        { return (x & BIT_MASK) == CLOSURE_TAG;        }
BOOLEAN IS_MACRO_OBJECT(OBJECT_PTR x)          { return (x & BIT_MASK) == MACRO_TAG;          }
BOOLEAN IS_ARRAY_OBJECT(OBJECT_PTR x)          { return (x & BIT_MASK) == ARRAY_TAG;          }
BOOLEAN IS_CONTINUATION_OBJECT(OBJECT_PTR x)   { return (x & BIT_MASK) == CONTINUATION_TAG;   }

BOOLEAN IS_NATIVE_FN_OBJECT(OBJECT_PTR x)      { return (x & BIT_MASK) == NATIVE_FN_TAG;      }
BOOLEAN IS_FUNCTION2_OBJECT(OBJECT_PTR x)      { return (x & BIT_MASK) == FUNCTION2_TAG;      }
BOOLEAN IS_MACRO2_OBJECT(OBJECT_PTR x)         { return (x & BIT_MASK) == MACRO2_TAG;         }

OBJECT_PTR rewrite_symbols(OBJECT_PTR);

extern void print_stack();

BOOLEAN system_changed;

extern uintptr_t POINTER_MASK;

extern OBJECT_PTR idclo;
extern OBJECT_PTR identity_function(OBJECT_PTR, OBJECT_PTR);

extern char **autocomplete_words;
extern unsigned int nof_autocomplete_words;

extern int build_help_entries(char *);
extern void cleanup_help_entries();

extern OBJECT_PTR exception_object;

extern OBJECT_PTR continuation_to_resume;

extern unsigned int nof_global_vars;
extern global_var_mapping_t *top_level_symbols;

extern OBJECT_PTR create_closure(unsigned int, BOOLEAN, OBJECT_PTR, ...);

extern OBJECT_PTR convert_native_fn_to_object(nativefn);

extern BOOLEAN is_valid_let_exp(OBJECT_PTR, BOOLEAN);
extern BOOLEAN is_valid_let1_exp(OBJECT_PTR, BOOLEAN);
extern BOOLEAN is_valid_letrec_exp(OBJECT_PTR, BOOLEAN);
extern BOOLEAN is_continuation_object(OBJECT_PTR);
extern OBJECT_PTR cons_equivalent(OBJECT_PTR);
extern OBJECT_PTR map(OBJECT_PTR (*f)(OBJECT_PTR), OBJECT_PTR lst);
extern OBJECT_PTR concat(unsigned int, ...);
extern OBJECT_PTR reverse(OBJECT_PTR);
extern BOOLEAN primop(OBJECT_PTR);

extern unsigned int nof_pkg_import_entries;
extern pkg_import_t *pkg_import_entries;

extern unsigned int print_context_pkg_index;

//variables moved from original compiler.c
char *default_transcript_text =  "Copyright 2011-2019 Rajesh Jayaprakash <rajesh.jayaprakash@gmail.com>\n\n"
                                 "pLisp is free software: you can redistribute it and/or modify\n"
                                 "it under the terms of the GNU General Public License as published by\n"
                                 "the Free Software Foundation, either version 3 of the License, or\n"
                                 "(at your option) any later version.\n\n"
                                 "pLisp is distributed in the hope that it will be useful,\n"
                                 "but WITHOUT ANY WARRANTY; without even the implied warranty of\n"
                                 "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n"
                                 "GNU General Public License for more details.\n\n"
                                 "You should have received a copy of the GNU General Public License\n"
                                 "along with pLisp.  If not, see <http://www.gnu.org/licenses/>.\n\n"
                                 "This is the transcript window. Results of evaluating expressions\n"
                                 "(entered in the workspace window) will be displayed here.\n\n";

BOOLEAN in_error;

BOOLEAN core_library_loaded = false;

char *loaded_image_file_name = NULL;

BOOLEAN console_mode = false;
BOOLEAN image_mode = false;
BOOLEAN single_expression_mode = false;
BOOLEAN interpreter_mode = false;
BOOLEAN pipe_mode = false;

BOOLEAN raw_mode = false;

char *core_library_file_name = NULL;

extern OBJECT_PTR saved_continuations;
extern OBJECT_PTR continuations_for_return;
extern OBJECT_PTR most_recent_closure;
extern OBJECT_PTR exception_handlers;

extern OBJECT_PTR debug_stack;
extern OBJECT_PTR debug_window_dbg_stack;

//end of variables moved from original compiler.c

extern void show_warning_dialog(char *);

void initialize()
{
  if(initialize_memory())
  {
    fprintf(stderr, "Initialization of memory failed\n");
    cleanup();
    exit(1);
  }

  //initialize_tcc();

  nof_packages = 0;

  create_package("CORE");

  initialize_core_package();

  top_level_env = NIL;

  idclo = create_closure(0, true, convert_native_fn_to_object((nativefn)identity_function));

  continuation_to_resume = NIL;

  nof_pkg_import_entries = 0;
  pkg_import_entries = NULL;
}

int add_string(char *str)
{
  char **temp;

  log_function_entry("add_string");

  nof_strings++;

  temp = (char **)GC_REALLOC(strings, nof_strings * sizeof(char *));

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

int add_symbol_to_package(char *sym, int package_index)
{
  char **temp;

  packages[package_index].nof_symbols++;
  
  temp = (char **)GC_REALLOC(packages[package_index].symbols, packages[package_index].nof_symbols * sizeof(char *));

  if(temp != NULL)
    packages[package_index].symbols = temp;
  else
  {
    fprintf(stderr, "Out of memory extending symbol space\n");
    cleanup();
    exit(1);
  }

  packages[package_index].symbols[packages[package_index].nof_symbols - 1] = strdup(sym);

  return packages[package_index].nof_symbols - 1;
}

int add_symbol(char *sym)
{
  char **temp;

  log_function_entry("add_symbol");

  packages[current_package].nof_symbols++;
  
  temp = (char **)GC_REALLOC(packages[current_package].symbols, packages[current_package].nof_symbols * sizeof(char *));

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
  int ret;
  BOOLEAN found = false;
  int i;

  log_function_entry("find_symbol");

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
  expression_t *e = (expression_t *)GC_MALLOC(sizeof(expression_t));

  e->type = type;

  if(type == SYMBOL)
  {
    if(char_val[0] == ':') //keyword symbols; not used currently
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
      //free(temp);
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
      e->elements = (expression_t **)GC_MALLOC(nof_elements * sizeof(expression_t *));
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
/*   int i; */

/*   log_function_entry("delete_expression"); */

/*   if(!e) */
/*     return; */

/* #ifdef DEBUG */
/*   print_expression(e); */
/*   fprintf(stdout, "\n"); */
/* #endif */

/*   if(e->type == SYMBOL) */
/*   { */
/*     free(e->atom_value); */
/*     if(e->package_name != NULL) */
/*       free(e->package_name); */
/*   } */
/*   else if(e->type == STRING_LITERAL) */
/*     free(e->atom_value); */
/*   else if(e->type == LIST) */
/*   { */
/*     for(i=0; i<e->nof_elements; i++) */
/*       delete_expression(e->elements[i]); */

/*     free(e->elements); */
/*   } */

/*   free(e); */

/*   log_function_exit("delete_expression"); */

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
  {
    if(!console_mode && !single_expression_mode && !pipe_mode)
    {
      char buf[500];
      memset(buf, '\0',500);
      sprintf(buf,"pLisp Workspace [Package: %s]", packages[current_package].name);
      set_workspace_window_title(buf);
    }
    else if(console_mode)
    {
      fprintf(stdout, "\n%s> ", packages[current_package].name);
      fflush(stdout);
    }
  }
  else
  {
    if(console_mode)
    {
      fprintf(stdout, "\nDEBUG> ");
      fflush(stdout);
    }
  }
}

void cleanup()
{

  int i,j;

  log_function_entry("cleanup");

  if(yyin != stdin && yyin)
    fclose(yyin);

  delete_expression(g_expr);

  /* for(i=0; i<nof_packages; i++) */
  /* { */
  /*   free(packages[i].name); */

  /*   for(j=0; j<packages[i].nof_symbols; j++) */
  /*     free(packages[i].symbols[j]); */

  /*   free(packages[i].symbols); */
  /* } */

  //free(dl_handles);

  cleanup_memory();

  //cleanup_tcc();

  cleanup_full_monty_global_vars();

  /* for(i=0; i<nof_autocomplete_words; i++) */
  /*   free(autocomplete_words[i]); */
  /* free(autocomplete_words);  */

  cleanup_help_entries();

  /* for(i=0;i<nof_strings; i++) */
  /*   free(strings[i]); */

  /* free(strings); */

  log_function_exit("cleanup");
}

void print_copyright_notice()
{
  fprintf(stdout, "pLisp is an interpreter for a Lisp-1 dialect.\n\n");
  fprintf(stdout, "Copyright 2011-2019 Rajesh Jayaprakash <rajesh.jayaprakash@gmail.com>\n\n");

  fprintf(stdout, "pLisp is free software: you can redistribute it and/or modify\n");
  fprintf(stdout, "it under the terms of the GNU General Public License as published by\n");
  fprintf(stdout, "the Free Software Foundation, either version 3 of the License, or\n");
  fprintf(stdout, "(at your option) any later version.\n\n");

  fprintf(stdout, "pLisp is distributed in the hope that it will be useful,\n");
  fprintf(stdout, "but WITHOUT ANY WARRANTY; without even the implied warranty of\n");
  fprintf(stdout, "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n");
  fprintf(stdout, "GNU General Public License for more details.\n\n");

  fprintf(stdout, "You should have received a copy of the GNU General Public License\n");
  fprintf(stdout, "along with pLisp.  If not, see <http://www.gnu.org/licenses/>.\n\n");

  fflush(stdout);
}

void welcome()
{
  fprintf(stdout, "Welcome to pLisp's top level. Type '(quit)' to exit.");
  fflush(stdout);
}

int print_object_to_string(OBJECT_PTR obj_ptr, char *buf, int filled_buf_len)
{
  int length = 0;

  if(IS_SYMBOL_OBJECT(obj_ptr))
  {
    //int package_index = (int)obj_ptr >> (SYMBOL_BITS + OBJECT_SHIFT);
    int package_index = extract_package_index(obj_ptr);
    
    if(package_index == current_package)
      length += sprintf(buf+filled_buf_len+length, "%s", get_symbol_name(obj_ptr));
    else
    {
      char buf1[SYMBOL_STRING_SIZE];
      print_symbol(obj_ptr, buf1);
      length += sprintf(buf+filled_buf_len+length, "%s", buf1);
    }
  }
  else if(IS_CONS_OBJECT(obj_ptr))
    length += print_cons_object_to_string(obj_ptr, buf, filled_buf_len + length);
  else if(IS_CLOSURE_OBJECT(obj_ptr))
    length += print_closure_object_to_string(obj_ptr, buf, filled_buf_len + length);
  else if(IS_CONTINUATION_OBJECT(obj_ptr))
    length += sprintf(buf+filled_buf_len+length, "#<CONTINUATION #x%08x> ", obj_ptr);
  else if(IS_MACRO_OBJECT(obj_ptr))
    length += print_macro_object_to_string(obj_ptr, buf, filled_buf_len+length);
  else if(IS_INTEGER_OBJECT(obj_ptr))
    length += sprintf(buf+filled_buf_len+length, "%d", get_int_value(obj_ptr));
  else if(IS_FLOAT_OBJECT(obj_ptr))
    length += sprintf(buf+filled_buf_len+length, "%f", get_float_value(obj_ptr));
  else if(IS_STRING_LITERAL_OBJECT(obj_ptr))
    length += sprintf(buf+filled_buf_len+length, "\"%s\"", strings[(int)obj_ptr >> OBJECT_SHIFT]);
  else if(IS_CHAR_OBJECT(obj_ptr))
    length += sprintf(buf+filled_buf_len+length, "#\\%c", (int)obj_ptr >> OBJECT_SHIFT);
  else if(IS_ARRAY_OBJECT(obj_ptr))
  {
    if(is_string_object(obj_ptr))
      length += print_string_to_string(obj_ptr, buf, filled_buf_len+length);
    else
      length += print_array_object_to_string(obj_ptr, buf, filled_buf_len+length);
  }
  else if(IS_FUNCTION2_OBJECT(obj_ptr))
    length += sprintf(buf+filled_buf_len+length, "#<FUNCTION2 #x%08x> ", obj_ptr);
  else if(IS_MACRO2_OBJECT(obj_ptr))
    length += sprintf(buf+filled_buf_len+length, "#<MACRO2 #x%08x> ", obj_ptr);
  else if(IS_NATIVE_FN_OBJECT(obj_ptr))
    length += sprintf(buf+filled_buf_len+length, "#<NATIVEFN #x%08x> ", obj_ptr);
  else
    assert(false);

  return length;
}

void print_object(OBJECT_PTR obj_ptr)
{
  log_function_entry("print_object");

  if(!console_mode && !single_expression_mode && !pipe_mode)
  {
    char buf[500];
    memset(buf, '\0', 500);

    int length = 0;

    if(IS_SYMBOL_OBJECT(obj_ptr))
    {
      //int package_index = (int)obj_ptr >> (SYMBOL_BITS + OBJECT_SHIFT);
      int package_index = extract_package_index(obj_ptr);
      
      if(package_index == current_package)
	length = sprintf(buf+length, "%s", get_symbol_name(obj_ptr));
      else
      {
	char buf1[SYMBOL_STRING_SIZE];
	print_symbol(obj_ptr, buf1);
	length = sprintf(buf+length, "%s", buf1);
      }
    }
    else if(IS_CONS_OBJECT(obj_ptr))
      print_cons_object(obj_ptr);
    else if(IS_CLOSURE_OBJECT(obj_ptr))
      print_closure_object(obj_ptr);
    else if(IS_CONTINUATION_OBJECT(obj_ptr))
      length = sprintf(buf+length, "#<CONTINUATION #x%08x> ", obj_ptr);
    else if(IS_MACRO_OBJECT(obj_ptr))
      print_macro_object(obj_ptr);
    else if(IS_INTEGER_OBJECT(obj_ptr))
      length = sprintf(buf+length, "%d", get_int_value(obj_ptr));
    else if(IS_FLOAT_OBJECT(obj_ptr))
      length = sprintf(buf+length, "%f", get_float_value(obj_ptr));
    else if(IS_STRING_LITERAL_OBJECT(obj_ptr))
      length = sprintf(buf+length, "\"%s\"", strings[(int)obj_ptr >> OBJECT_SHIFT]);
    else if(IS_CHAR_OBJECT(obj_ptr))
      length = sprintf(buf+length, "#\\%c", (int)obj_ptr >> OBJECT_SHIFT);
    else if(IS_ARRAY_OBJECT(obj_ptr))
    {
      if(is_string_object(obj_ptr))
	print_string(obj_ptr);
      else
	print_array_object(obj_ptr);
    }
    else if(IS_FUNCTION2_OBJECT(obj_ptr))
      length += sprintf(buf+length, "#<FUNCTION2 #x%08x> ", obj_ptr);
    else if(IS_MACRO2_OBJECT(obj_ptr))
      length += sprintf(buf+length, "#<MACRO2 #x%08x> ", obj_ptr);
    else if(IS_NATIVE_FN_OBJECT(obj_ptr))
      length += sprintf(buf+length, "#<NATIVEFN #x%08x> ", obj_ptr);
    else
      assert(false);

    print_to_transcript(buf);
  }
  else
  {
    if(IS_SYMBOL_OBJECT(obj_ptr))
    {
      //int package_index = (int)obj_ptr >> (SYMBOL_BITS + OBJECT_SHIFT);
      int package_index = extract_package_index(obj_ptr);
      
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
      fprintf(stdout, "\"%s\"", strings[(int)obj_ptr >> OBJECT_SHIFT]);
    else if(IS_CHAR_OBJECT(obj_ptr))
      fprintf(stdout, "#\\%c", (int)obj_ptr >> OBJECT_SHIFT);
    else if(IS_ARRAY_OBJECT(obj_ptr))
    {
      if(is_string_object(obj_ptr))
	print_string(obj_ptr);
      else
	print_array_object(obj_ptr);
    }
    else if(IS_FUNCTION2_OBJECT(obj_ptr))
      fprintf(stdout, "#<FUNCTION2 #x%08x> ", obj_ptr);
    else if(IS_MACRO2_OBJECT(obj_ptr))
      fprintf(stdout, "#<MACRO2 #x%08x> ", obj_ptr);
    else if(IS_NATIVE_FN_OBJECT(obj_ptr))
      fprintf(stdout, "#<NATIVEFN #x%08x> ", obj_ptr);
    else
      assert(false);

    fflush(stdout);
  }

  log_function_exit("print_object");
}

OBJECT_PTR cons(OBJECT_PTR car, OBJECT_PTR cdr)
{
  uintptr_t ptr = object_alloc(2, CONS_TAG);

  log_function_entry("cons");

  if(!is_valid_object(car))
    //assert(false);
  {
    throw_exception1("EXCEPTION", "Invalid first argument to CONS operator");
    return NIL;
  }
  if(!is_valid_object(cdr))
    //assert(false);
  {
    throw_exception1("EXCEPTION", "Invalid second argument to CONS operator");
    return NIL;
  }

  set_heap(ptr, 0, car);
  set_heap(ptr, 1, cdr);

  log_function_exit("cons");

  return ptr + CONS_TAG;
}

OBJECT_PTR get_string_object(char *str)
{
  int index = find_string(str);

  OBJECT_PTR retval;

  log_function_entry("get_string_object");

  if(index != NOT_FOUND) //string exists in string table
    retval = (OBJECT_PTR)((index << OBJECT_SHIFT) + STRING_LITERAL_TAG);
  else
    retval = (OBJECT_PTR)((add_string(str) << OBJECT_SHIFT) + STRING_LITERAL_TAG);

  log_function_exit("get_string_object");

  return retval;
}

OBJECT_PTR get_symbol_object(char *symbol_name)
{
  OBJECT_PTR retval;

  int i;
  int package_index = current_package;

  int index;

  log_function_entry("get_symbol_object");

  for(i=0; i<packages[CORE_PACKAGE_INDEX].nof_symbols; i++)
  {
    if(!strcmp(symbol_name,packages[CORE_PACKAGE_INDEX].symbols[i]))
    {
      package_index = CORE_PACKAGE_INDEX;
      break;
    }
  }

  index = find_symbol(symbol_name, package_index);
    
  if(index != NOT_FOUND) //symbol exists in symbol table
    //retval = (OBJECT_PTR) ((package_index << (SYMBOL_BITS + OBJECT_SHIFT)) + (index << OBJECT_SHIFT) + SYMBOL_TAG);
    retval = build_symbol_object(package_index, index);
  else
    //retval = (OBJECT_PTR) ((package_index << (SYMBOL_BITS + OBJECT_SHIFT)) + (add_symbol(symbol_name) << OBJECT_SHIFT) + SYMBOL_TAG);
    retval = build_symbol_object(package_index, add_symbol(symbol_name));
    
  log_function_exit("get_symbol_object");

  return retval;
}

OBJECT_PTR car(OBJECT_PTR cons_obj)
{
  if(cons_obj == NIL)
    return NIL;
  else
  {
     if(!IS_CONS_OBJECT(cons_obj))
     {
       print_object(cons_obj);printf("\n");
       //char buf[20];memset(buf, 20, '\0');print_qualified_symbol(cons_obj,buf);printf("%s\n",buf);
       assert(false);
     }
     //return get_heap(extract_ptr(cons_obj), 0);
     return (OBJECT_PTR)*((OBJECT_PTR *)(extract_ptr(cons_obj)));
     //return (OBJECT_PTR)*((OBJECT_PTR *)((cons_obj >> 4) << 4));
  }
}

OBJECT_PTR cdr(OBJECT_PTR cons_obj)
{
  if(cons_obj == NIL)
    return NIL;
  else
  {
    /* if(!IS_CONS_OBJECT(cons_obj)) */
    /*    assert(false); */
    //return get_heap(extract_ptr(cons_obj), 1);
    return (OBJECT_PTR)*((OBJECT_PTR *)(extract_ptr(cons_obj))+1);
  }
}

int print_cons_obj_in_single_line(OBJECT_PTR obj, char *buf, int filled_buf_len)
{
  int length = 0;

  length += sprintf(buf+filled_buf_len+length, "(");

  if(car(obj) == COMMA)
    length += sprintf(buf+filled_buf_len+length, ",");
  else if(car(obj) == COMMA_AT)
    length += sprintf(buf+filled_buf_len+length, ",@");
  else
  {
     length += print_object_to_string(car(obj), buf, filled_buf_len+length);
     if(cdr(obj) != NIL)
       length += sprintf(buf+filled_buf_len+length, " ");
  }
  
  OBJECT_PTR rest = cdr(obj);

  while(rest != NIL)
  {
    length += print_object_to_string(car(rest), buf, filled_buf_len+length);

    if(cdr(rest) != NIL)
      length += sprintf(buf+filled_buf_len+length, " ");

    rest = cdr(rest);
  }

  length += sprintf(buf+filled_buf_len+length, ")");

  return length;
}

BOOLEAN is_proper_list(OBJECT_PTR obj)
{
  if(obj == NIL)
    return true;
  
  if(!IS_CONS_OBJECT(obj))
    return false;

  OBJECT_PTR rest = obj;

  while(rest != NIL)
  {
    if(is_atom(rest))
       return false;
    rest = cdr(rest);
  }

  return true;
}

BOOLEAN is_valid_defun_defmacro_exp(OBJECT_PTR exp)
{
  if(!IS_CONS_OBJECT(exp))
    return false;

  if(car(exp) != DEFUN && car(exp) != DEFMACRO)
    return false;

  if(!IS_SYMBOL_OBJECT(second(exp)))
    return false;

  if(!IS_CONS_OBJECT(third(exp)) && third(exp) != NIL)
    return false;

  OBJECT_PTR rest = third(exp);

  while(rest != NIL)
  {
    if(!IS_SYMBOL_OBJECT(car(rest)))
      return false;

    rest = cdr(rest);
  }

  return true;
}

int print_improper_list_to_string(OBJECT_PTR obj, char *buf, int filled_buf_len)
{
  OBJECT_PTR car_obj = car(obj);
  OBJECT_PTR cdr_obj = cdr(obj);

  int length = 0;
  
  assert(IS_CONS_OBJECT(obj));

  if((is_atom(cdr_obj)                || 
      IS_CLOSURE_OBJECT(cdr_obj)      || 
      IS_MACRO_OBJECT(cdr_obj)        || 
      IS_CONTINUATION_OBJECT(cdr_obj) ||
      IS_FUNCTION2_OBJECT(cdr_obj)    ||
      IS_MACRO2_OBJECT(cdr_obj))  
     && cdr_obj != NIL)
  {
    length += sprintf(buf+filled_buf_len+length, "(");
    length += print_object_to_string(car_obj, buf, filled_buf_len+length);
    length += sprintf(buf+filled_buf_len+length, " . ");
    length += print_object_to_string(cdr_obj, buf, filled_buf_len+length);
    length += sprintf(buf+filled_buf_len+length, ")");
  }
  else
  {
    OBJECT_PTR rest = obj;

    length += sprintf(buf+filled_buf_len+length, "(");

    while(rest != NIL && 
          !(IS_ARRAY_OBJECT(rest)        || 
            is_atom(rest)                || 
            IS_CLOSURE_OBJECT(rest)      || 
            IS_MACRO_OBJECT(rest)        || 
            IS_CONTINUATION_OBJECT(rest) ||
            IS_FUNCTION2_OBJECT(rest)    ||
            IS_MACRO2_OBJECT(rest)))
    {
      length += print_object_to_string(car(rest), buf, filled_buf_len+length);
      length += sprintf(buf+filled_buf_len+length, " ");
      rest = cdr(rest);
    }

    if((IS_ARRAY_OBJECT(rest)        || 
        is_atom(rest)                || 
        IS_CLOSURE_OBJECT(rest)      || 
        IS_MACRO_OBJECT(rest)        || 
        IS_CONTINUATION_OBJECT(rest) ||
        IS_FUNCTION2_OBJECT(rest)    ||
        IS_MACRO2_OBJECT(rest)) 
       && rest != NIL)
    {
      length += sprintf(buf+filled_buf_len+length, " . ");
      length += print_object_to_string(rest, buf, filled_buf_len+length);
      length += sprintf(buf+filled_buf_len+length, ")");
    }
    else
    {
      length += sprintf(buf+filled_buf_len+length - 1, ")");
    }
  }

  return length;
}

//temporary fix to take care of flakiness
//in printing CONS objects to debug window
OBJECT_PTR car_for_print(OBJECT_PTR cons_obj)
{
  if(cons_obj == NIL)
    return NIL;
  else
  {
     if(!IS_CONS_OBJECT(cons_obj))
     {
       printf("Error obtaining CAR of object %p\n", cons_obj);
#ifdef WIN32       
       return ERROR1;
#else
       return ERROR;
#endif       
     }
     else
       return (OBJECT_PTR)*((OBJECT_PTR *)(extract_ptr(cons_obj)));
  }
}

int print_cons_object_to_string(OBJECT_PTR obj, char *buf, int filled_buf_len)
{
  assert(IS_CONS_OBJECT(obj));

  if(!is_proper_list(obj))
    return print_improper_list_to_string(obj, buf, filled_buf_len);
  
  OBJECT_PTR car_obj = car_for_print(obj);
  OBJECT_PTR cdr_obj = cdr(obj);

  int length = 0;

  //to determine the location of the
  //last newline and calculate
  //the number of indents
  char *ptr;
  int indents = 0;

  BOOLEAN macro_form;

  BOOLEAN let_star;

  if(car_obj == get_symbol_object("LET*"))
    let_star = true;
  else
    let_star = false;

  for(ptr=buf+filled_buf_len; ptr>=buf; ptr--)
  {
    if(*ptr == '\n')
      break;
    indents++;
  }

  if(((car_obj == DEFUN || car_obj == DEFMACRO) && !is_valid_defun_defmacro_exp(obj)) ||
     //(car_obj == LET && !is_valid_let_exp(obj, false)) ||
     (car_obj == LET1 && !is_valid_let1_exp(obj,false)) ||
     //(car_obj == LETREC && !is_valid_letrec_exp(obj, false)) ||
     //(let_star && !is_valid_let_exp(obj, false)) ||
     //(car_obj == LAMBDA && !is_valid_lambda_exp(obj)) ||
     (car_obj == MACRO && !is_valid_macro_exp(obj)))
  {
    length += print_cons_obj_in_single_line(obj, buf, filled_buf_len+length);
    return length;
  }

  if(car_obj == get_symbol_object("WHILE"))
  {
    int i;
    OBJECT_PTR rest;

    length += sprintf(buf+filled_buf_len+length, "(while ");

    if(IS_CONS_OBJECT(CADR(obj)))
      length += print_cons_obj_in_single_line(CADR(obj), buf, filled_buf_len+length);
    else
      length += print_object_to_string(CADR(obj), buf, filled_buf_len+length);

    rest = CDDR(obj);

    while(rest != NIL)
    {
      length += sprintf(buf+filled_buf_len+length, "\n");

      for(i=1; i<indents; i++)
        length += sprintf(buf+filled_buf_len+length, " ");

      length += sprintf(buf+filled_buf_len+length, "  ");

      length += print_object_to_string(car_for_print(rest), buf, filled_buf_len+length);
      rest = cdr(rest);
      if(rest != NIL)
        length += sprintf(buf+filled_buf_len+length, " ");
    }
  
    length += sprintf(buf+filled_buf_len+length, ")");

    return length;
  }

  /* if(car_obj == LET && !is_valid_let_exp(obj, false)) */
  /* { */
  /*   OBJECT_PTR rest = cdr(obj); */

  /*   length += sprintf(buf+filled_buf_len+length, "(let "); */

  /*   while(rest != NIL) */
  /*   { */
  /*     length +=  print_object_to_string(car(rest), buf, filled_buf_len+length); */
  /*     rest = cdr(rest); */
  /*     if(rest != NIL) */
  /*       length += sprintf(buf+filled_buf_len+length, " "); */
  /*   } */
  /*   length += sprintf(buf+filled_buf_len+length, ")"); */

  /*   return length; */
  /* } */

  /* if(let_star && !is_valid_let_exp(obj, false)) */
  /* { */
  /*   OBJECT_PTR rest = cdr(obj); */

  /*   length += sprintf(buf+filled_buf_len+length, "(let* "); */

  /*   while(rest != NIL) */
  /*   { */
  /*     length +=  print_object_to_string(car(rest), buf, filled_buf_len+length); */
  /*     rest = cdr(rest); */
  /*     if(rest != NIL) */
  /*       length += sprintf(buf+filled_buf_len+length, " "); */
  /*   } */
  /*   length += sprintf(buf+filled_buf_len+length, ")"); */

  /*   return length; */
  /* } */

  if(car_obj == LET1 && !is_valid_let1_exp(obj, false))
  {
    OBJECT_PTR rest = cdr(obj);

    length += sprintf(buf+filled_buf_len+length, "(let1 ");

    while(rest != NIL)
    {
      length +=  print_object_to_string(car_for_print(rest), buf, filled_buf_len+length);
      rest = cdr(rest);
      if(rest != NIL)
        length += sprintf(buf+filled_buf_len+length, " ");
    }
    length += sprintf(buf+filled_buf_len+length, ")");

    return length;
  }

  /* if(car_obj == LETREC && !is_valid_letrec_exp(obj, false)) */
  /* { */
  /*   OBJECT_PTR rest = cdr(obj); */

  /*   length += sprintf(buf+filled_buf_len+length, "(letrec "); */

  /*   while(rest != NIL) */
  /*   { */
  /*     length +=  print_object_to_string(car(rest), buf, filled_buf_len+length); */
  /*     rest = cdr(rest); */
  /*     if(rest != NIL) */
  /*       length += sprintf(buf+filled_buf_len+length, " "); */
  /*   } */
  /*   length += sprintf(buf+filled_buf_len+length, ")"); */

  /*   return length; */
  /* } */

  if(car_obj == LAMBDA  || 
     car_obj == MACRO   || 
     car_obj == LET     ||
     car_obj == LET1    ||
     car_obj == LETREC  ||
     let_star           ||
     //car_obj == WHILE   ||
     car_obj == DOTIMES ||
     car_obj == DOLIST)
  {
    OBJECT_PTR rest;

    if(car_obj == LAMBDA)
      length += sprintf(buf+filled_buf_len+length, "(lambda ");
    else if(car_obj == MACRO)
      length += sprintf(buf+filled_buf_len+length, "(macro ");
    else if(car_obj == LET)
      length += sprintf(buf+filled_buf_len+length, "(let ");
    else if(car_obj == LET1)
      length += sprintf(buf+filled_buf_len+length, "(let1 ");
    else if(car_obj == LETREC)
      length += sprintf(buf+filled_buf_len+length, "(letrec ");
    else if(let_star)
      length += sprintf(buf+filled_buf_len+length, "(let* ");
    /* else if(car_obj == WHILE) */
    /*   length += sprintf(buf+filled_buf_len+length, "(while "); */
    else if(car_obj == DOTIMES)
      length += sprintf(buf+filled_buf_len+length, "(dotimes ");
    else if(car_obj == DOLIST)
      length += sprintf(buf+filled_buf_len+length, "(dolist ");

    if(CADR(obj) == NIL)
      length += sprintf(buf+filled_buf_len+length, "()");
    else
    {
      if(car_obj != LET && car_obj != LET1 && car_obj != LETREC && !let_star)
        length += print_object_to_string(CADR(obj), buf, filled_buf_len+length);
      else
      {
        //LET specs
        OBJECT_PTR specs = CADR(obj);

        OBJECT_PTR rest = cdr(specs);

        length += sprintf(buf+filled_buf_len+length, "(");

        length += print_object_to_string(car_for_print(specs), buf, filled_buf_len+length);

        while(rest != NIL)
        {
          int i;

          length += sprintf(buf+filled_buf_len+length, "\n");

          for(i=1; i<indents; i++)
            length += sprintf(buf+filled_buf_len+length, " ");

          if(car_obj == LET)
            length += sprintf(buf+filled_buf_len+length, "      ");
          else if(let_star)
            length += sprintf(buf+filled_buf_len+length, "       ");
          else if(car_obj == LETREC)
            length += sprintf(buf+filled_buf_len+length, "         ");        
          else
            length += sprintf(buf+filled_buf_len+length, "       ");        

          length += print_object_to_string(car_for_print(rest), buf, filled_buf_len+length);

          rest = cdr(rest);
        }

        length += sprintf(buf+filled_buf_len+length, ")");
      }
    }

    rest = CDDR(obj);

    while(rest != NIL)
    {
      int i;

      length += sprintf(buf+filled_buf_len+length, "\n");

      for(i=1; i<indents; i++)
        length += sprintf(buf+filled_buf_len+length, " ");

      length += sprintf(buf+filled_buf_len+length, "  ");

      length += print_object_to_string(car_for_print(rest), buf, filled_buf_len+length);

      rest = cdr(rest);
    }

    length += sprintf(buf+filled_buf_len+length, ")");

    return length;
  }

  if(car_obj == DEFUN || car_obj == DEFMACRO)
  {
    int i;
    OBJECT_PTR rest;

    if(car_obj == DEFUN)
      length += sprintf(buf+filled_buf_len+length, "(defun ");
    else if(car_obj == DEFMACRO)
      length += sprintf(buf+filled_buf_len+length, "(defmacro ");

    length += print_object_to_string(CADR(obj), buf, filled_buf_len+length);

    length += sprintf(buf+filled_buf_len+length, " ");

    //length += print_object_to_string(CADDR(obj), buf, filled_buf_len+length);

    if(CADDR(obj) == NIL)
      length += sprintf(buf+filled_buf_len+length, "()");
    else
      length += print_cons_obj_in_single_line(CADDR(obj), buf, filled_buf_len+length);

    /* length += sprintf(buf+filled_buf_len+length, "\n"); */

    /* for(i=1; i<indents; i++) */
    /*   length += sprintf(buf+filled_buf_len+length, " "); */

    /* length += sprintf(buf+filled_buf_len+length, "  "); */

    rest = CDDDR(obj);

    while(rest != NIL)
    {
      length += sprintf(buf+filled_buf_len+length, "\n");

      for(i=1; i<indents; i++)
        length += sprintf(buf+filled_buf_len+length, " ");

      length += sprintf(buf+filled_buf_len+length, "  ");

      length += print_object_to_string(car_for_print(rest), buf, filled_buf_len+length);
      rest = cdr(rest);
      if(rest != NIL)
        length += sprintf(buf+filled_buf_len+length, " ");
    }

    length += sprintf(buf+filled_buf_len+length, ")");

    return length;
  }

  if(car_obj == PROGN)
  {
    OBJECT_PTR rest;

    length += sprintf(buf+filled_buf_len+length, "(progn ");

    length += print_object_to_string(CADR(obj), buf, filled_buf_len+length);

    rest = cdr(cdr_obj);

    while(rest != NIL)
    {
      int i;

      length += sprintf(buf+filled_buf_len+length, "\n");

      for(i=1; i<indents; i++)
        length += sprintf(buf+filled_buf_len+length, " ");

      length += sprintf(buf+filled_buf_len+length, "       ");
      
      length += print_object_to_string(car_for_print(rest), buf, filled_buf_len+length);

      rest = cdr(rest);
    }

    length += sprintf(buf+filled_buf_len+length, ")");

    return length;
  }

  if(car_obj == COND)
  {
    int i;
    OBJECT_PTR rest;

    //quick and dirty check
    //needed to print signature
    if(!IS_CONS_OBJECT(second(obj)))
    {
      length += print_cons_obj_in_single_line(obj, buf, filled_buf_len+length);
      return length;
    }
    
    length += sprintf(buf+filled_buf_len+length, "(cond (");

    length += print_object_to_string(first(second(obj)), buf, filled_buf_len+length);
    length += sprintf(buf+filled_buf_len+length, "\n");

    for(i=1; i<indents; i++)
        length += sprintf(buf+filled_buf_len+length, " ");

    length += sprintf(buf+filled_buf_len+length, "       ");
    
    length += print_object_to_string(second(second(obj)), buf, filled_buf_len+length);
    length += sprintf(buf+filled_buf_len+length, ")");
    
    rest = cdr(cdr_obj);

    while(rest != NIL)
    {
      length += sprintf(buf+filled_buf_len+length, "\n");

      for(i=1; i<indents; i++)
        length += sprintf(buf+filled_buf_len+length, " ");

      length += sprintf(buf+filled_buf_len+length, "      (");

      length += print_object_to_string(first(car_for_print(rest)), buf, filled_buf_len+length);
      length += sprintf(buf+filled_buf_len+length, "\n");
      
      for(i=1; i<indents; i++)
        length += sprintf(buf+filled_buf_len+length, " ");

      length += sprintf(buf+filled_buf_len+length, "       ");      
      
      length += print_object_to_string(second(car_for_print(rest)), buf, filled_buf_len+length);
      length += sprintf(buf+filled_buf_len+length, ")");      

      rest = cdr(rest);
    }

    length += sprintf(buf+filled_buf_len+length, ")");
    
    return length;
  }
  
  if(car_obj == get_symbol_object("FOR"))
  {
    int i;
    OBJECT_PTR rest;

    //quick and dirty check
    //needed to print signature
    if(!IS_CONS_OBJECT(second(obj)))
    {
      length += print_cons_obj_in_single_line(obj, buf, filled_buf_len+length);
      return length;
    }
    
    length += sprintf(buf+filled_buf_len+length, "(for ");

    length += print_cons_obj_in_single_line(second(obj), buf, filled_buf_len+length);


    rest = cdr(cdr_obj);

    while(rest != NIL)
    {
      length += sprintf(buf+filled_buf_len+length, "\n");
      
      for(i=1; i<indents; i++)
        length += sprintf(buf+filled_buf_len+length, " ");

      length += sprintf(buf+filled_buf_len+length, "  ");

      length += print_object_to_string(car_for_print(rest), buf, filled_buf_len+length);
      
      rest = cdr(rest);
    }

    length += sprintf(buf+filled_buf_len+length, ")");
    
    return length;
  }  
  
  if(car_obj == IF)
  {
    int i;

    length += sprintf(buf+filled_buf_len+length, "(if ");
    
    if(IS_CONS_OBJECT(CADR(obj)))
      length += print_cons_obj_in_single_line(CADR(obj), buf, filled_buf_len+length);
    else
      length += print_object_to_string(CADR(obj), buf, filled_buf_len+length);

    length += sprintf(buf+filled_buf_len+length, "\n");

    for(i=1; i<indents; i++)
      length += sprintf(buf+filled_buf_len+length, " ");

    length += sprintf(buf+filled_buf_len+length, "    ");

    length += print_object_to_string(CADDR(obj), buf, filled_buf_len+length);

    if(fourth(obj) != NIL)
    {
      length += sprintf(buf+filled_buf_len+length, "\n");

      for(i=1; i<indents; i++)
        length += sprintf(buf+filled_buf_len+length, " ");

      length += sprintf(buf+filled_buf_len+length, "  ");
      length += print_object_to_string(CADDDR(obj), buf, filled_buf_len+length);
    }

    length += sprintf(buf+filled_buf_len+length, ")");

    return length;
  }

  macro_form = false;

  if(car_obj == BACKQUOTE || car_obj == COMMA || car_obj == COMMA_AT || car_obj == QUOTE)
    macro_form = true;

  //to handle applications (a b c)
  if(IS_SYMBOL_OBJECT(car_obj) && !macro_form)
  {
    //
    if(cons_length(obj) <= 3)
    {
      length += sprintf(buf+filled_buf_len+length, "(");

      OBJECT_PTR rest = obj;

      while(rest != NIL)
      {
        length += print_object_to_string(car_for_print(rest), buf, filled_buf_len+length);
        if(cdr(rest) != NIL)
          length += sprintf(buf+filled_buf_len+length, " ");
        rest = cdr(rest);
      }

      length += sprintf(buf+filled_buf_len+length, ")");
    }
    else
    {
    //
      int i;

      char *s = get_symbol_name(car_obj);

      length += sprintf(buf+filled_buf_len+length, "(%s", s);

      if(cdr(obj) != NIL)
      {
        length += sprintf(buf+filled_buf_len+length, " ");

        //length += print_object_to_string(CADR(obj), buf, filled_buf_len+length);

        if(IS_CONS_OBJECT(cdr(obj)))
        {
          length += print_object_to_string(CADR(obj), buf, filled_buf_len+length);

          OBJECT_PTR rest = CDDR(obj);

          while(rest != NIL)
          {
            length += sprintf(buf+filled_buf_len+length, "\n");

            for(i=0; i<indents+1; i++)
              length += sprintf(buf+filled_buf_len+length, " ");

            for(i=0; i<strlen(s); i++)
              length += sprintf(buf+filled_buf_len+length, " ");

            length += print_object_to_string(car_for_print(rest), buf, filled_buf_len+length);

            rest = cdr(rest);
          }
        }
        else
        {
          length += sprintf(buf+filled_buf_len+length, ". ");
          length += print_object_to_string(cdr(obj), buf, filled_buf_len+length);
        }
      }

      length += sprintf(buf+filled_buf_len+length, ")");
    } //

    return length;
  }

  if((is_atom(cdr_obj)                || 
      IS_CLOSURE_OBJECT(cdr_obj)      || 
      IS_MACRO_OBJECT(cdr_obj)        || 
      IS_CONTINUATION_OBJECT(cdr_obj) || 
      IS_FUNCTION2_OBJECT(cdr_obj)    ||
      IS_MACRO2_OBJECT(cdr_obj))  
     && cdr_obj != NIL)
  {
    if(macro_form)
      length += sprintf(buf+filled_buf_len+length, "%s", (car_obj == BACKQUOTE) ? "`" : ((car_obj == COMMA) ? "," : ((car_obj == QUOTE) ? "'" : ",@")));
    else
    {
      length += sprintf(buf+filled_buf_len+length,"(");
      length += print_object_to_string(car_obj, buf, filled_buf_len+length);
    }

    length += sprintf(buf+filled_buf_len+length, " . ");
    length += print_object_to_string(cdr_obj, buf, filled_buf_len+length);

    if(!macro_form)
      length += sprintf(buf+filled_buf_len+length, ")");
  }
  else
  {
    OBJECT_PTR rest;

    if(macro_form)
      length += sprintf(buf+filled_buf_len+length, "%s", (car_obj == BACKQUOTE) ? "`" : ((car_obj == COMMA) ? "," : ((car_obj == QUOTE) ? "'" : ",@")));
    else
      length += sprintf(buf+filled_buf_len+length, "(");

    rest = macro_form ? cdr(obj) : obj;

    while(rest != NIL && 
          !(IS_ARRAY_OBJECT(rest)        || 
            is_atom(rest)                || 
            IS_CLOSURE_OBJECT(rest)      || 
            IS_MACRO_OBJECT(rest)        || 
            IS_CONTINUATION_OBJECT(rest) ||
            IS_FUNCTION2_OBJECT(rest)    ||
            IS_MACRO2_OBJECT(rest)))
    {
      length += print_object_to_string(car_for_print(rest), buf, filled_buf_len+length);
      length += sprintf(buf+filled_buf_len+length, " ");
      rest = cdr(rest);
    }

    if((IS_ARRAY_OBJECT(rest)        || 
        is_atom(rest)                || 
        IS_CLOSURE_OBJECT(rest)      || 
        IS_MACRO_OBJECT(rest)        || 
        IS_CONTINUATION_OBJECT(rest) ||
        IS_FUNCTION2_OBJECT(rest)    ||
        IS_MACRO2_OBJECT(rest)) 
       && rest != NIL)
    {
      length += sprintf(buf+filled_buf_len+length, " . ");
      length += print_object_to_string(rest, buf, filled_buf_len+length);

      if(!macro_form)
        length += sprintf(buf+filled_buf_len+length, ")");
    }
    else
    {
      if(!macro_form)
        length += sprintf(buf+filled_buf_len+length-1, ")") - 1;
      else
        length += sprintf(buf+filled_buf_len+length-1, "") - 1;
    }
  }

  return length;
}

void print_cons_object(OBJECT_PTR obj)
{
  OBJECT_PTR car_obj = car(obj);
  OBJECT_PTR cdr_obj = cdr(obj);

  log_function_entry("print_cons_object");

  assert(IS_CONS_OBJECT(obj));

  if(!console_mode && !single_expression_mode && !pipe_mode)
  {
    if((is_atom(cdr_obj)                || 
        IS_CLOSURE_OBJECT(cdr_obj)      || 
        IS_MACRO_OBJECT(cdr_obj)        || 
        IS_CONTINUATION_OBJECT(cdr_obj) ||
        IS_FUNCTION2_OBJECT(cdr_obj)    ||
        IS_MACRO2_OBJECT(cdr_obj))  
       && cdr_obj != NIL)
    {
      print_to_transcript("(");
      print_object(car_obj);
      print_to_transcript(" . ");
      print_object(cdr_obj);
      print_to_transcript( ")");
    }
    else
    {
      OBJECT_PTR rest = obj;

      print_to_transcript("(");

      while(rest != NIL && 
            !(IS_ARRAY_OBJECT(rest)        || 
              is_atom(rest)                || 
              IS_CLOSURE_OBJECT(rest)      || 
              IS_MACRO_OBJECT(rest)        || 
              IS_CONTINUATION_OBJECT(rest) ||
              IS_FUNCTION2_OBJECT(rest)    ||
              IS_MACRO2_OBJECT(rest)))
      {
	print_object(car(rest));
	print_to_transcript(" ");
	rest = cdr(rest);
      }

      if((IS_ARRAY_OBJECT(rest)        || 
          is_atom(rest)                || 
          IS_CLOSURE_OBJECT(rest)      || 
          IS_MACRO_OBJECT(rest)        || 
          IS_CONTINUATION_OBJECT(rest) ||
          IS_FUNCTION2_OBJECT(rest)    ||
          IS_MACRO2_OBJECT(rest)) 
         && rest != NIL)
      {
	print_to_transcript(". ");
	print_object(rest);
	print_to_transcript(")");
      }
      else
      {
	transcript_backspace();
	print_to_transcript(")");
      }
    }
  }
  else
  {
    fflush(stdout);

    if((is_atom(cdr_obj)                || 
        IS_CLOSURE_OBJECT(cdr_obj)      || 
        IS_MACRO_OBJECT(cdr_obj)        || 
        IS_CONTINUATION_OBJECT(cdr_obj) ||
        IS_FUNCTION2_OBJECT(cdr_obj)    ||
        IS_MACRO2_OBJECT(cdr_obj))
       && cdr_obj != NIL)
    {
      fprintf(stdout, "(");
      print_object(car_obj);
      fprintf(stdout, " . ");
      print_object(cdr_obj);
      fprintf(stdout, ")");
    }
    else
    {
      OBJECT_PTR rest = obj;

      fprintf(stdout, "(");

      while(rest != NIL && 
            !(IS_ARRAY_OBJECT(rest)        || 
              is_atom(rest)                || 
              IS_CLOSURE_OBJECT(rest)      || 
              IS_MACRO_OBJECT(rest)        || 
              IS_CONTINUATION_OBJECT(rest) ||
              IS_FUNCTION2_OBJECT(rest)    ||
              IS_MACRO2_OBJECT(rest)))
      {
	print_object(car(rest));
	fprintf(stdout, " ");
	rest = cdr(rest);
      }

      if((IS_ARRAY_OBJECT(rest)        || 
          is_atom(rest)                || 
          IS_CLOSURE_OBJECT(rest)      || 
          IS_MACRO_OBJECT(rest)        || 
          IS_CONTINUATION_OBJECT(rest) ||
          IS_FUNCTION2_OBJECT(rest)    ||
          IS_MACRO2_OBJECT(rest)) 
         && rest != NIL)
      {
	fprintf(stdout, ". ");
	print_object(rest);
	fprintf(stdout, ")");
      }
      else
	fprintf(stdout, "\b)");
    }
  }

  log_function_exit("print_cons_object");
}

BOOLEAN is_atom(OBJECT_PTR ptr)
{
  return IS_SYMBOL_OBJECT(ptr) || 
    IS_INTEGER_OBJECT(ptr) || 
    IS_FLOAT_OBJECT(ptr) ||
    IS_STRING_LITERAL_OBJECT(ptr) ||
    IS_CHAR_OBJECT(ptr);
}

int convert_expression_to_object(expression_t *e, OBJECT_PTR *out_val)
{
  int ret = 0;

  log_function_entry("convert_expression_to_object");

  if(e->type == SYMBOL)
  {
    /* if(e->package_name == NULL) */
    /*   *out_val = get_symbol_object(e->atom_value); */
    /* else */
    /* { */
    /*   OBJECT_PTR val = get_qualified_symbol_object(e->package_name, e->atom_value); */
    /*   if(car(val) == NIL) */
    /*   { */
    /*     fprintf(stdout, "Packgage %s does not exist\n", e->package_name); */
    /*     *out_val = NIL; */
    /*     ret = -1; */
    /*   } */
    /*   else */
    /*     *out_val = cdr(val); */
    /* } */

    if(e->package_name != NULL)
    {
      if(find_package(e->package_name) == NOT_FOUND)
      {
        char buf[100];
        memset(buf, '\0', 100);
        sprintf(buf, "Package %s does not exist\n", e->package_name);
        throw_exception1("EXCEPTION", buf);
        //fprintf(stdout, "Package %s does not exist\n", e->package_name);
        *out_val = NIL;
        return -1;
      }

      *out_val = cdr(get_qualified_symbol_object(e->package_name, e->atom_value));
    }
    else
    {
      if(find_symbol(e->atom_value, CORE_PACKAGE_INDEX) != NOT_FOUND)
        *out_val = cdr(get_qualified_symbol_object("CORE", e->atom_value));
      else
        *out_val = cdr(get_qualified_symbol_object(packages[current_package].name, e->atom_value));
    }
  }
  else if(e->type == INTEGER)
    *out_val = convert_int_to_object(e->integer_value);
  else if(e->type == FLOAT)
    *out_val = convert_float_to_object(e->float_value);
  else if(e->type == STRING_LITERAL)
    *out_val = get_string_object(e->atom_value);
  else if(e->type == CHARACTER)
    *out_val = (OBJECT_PTR) ((e->char_value << OBJECT_SHIFT) + CHAR_TAG);
  else if(e->type == LIST)
  {
    if(e->nof_elements == 0)
      *out_val = NIL;
    else
    {
      int i;

      OBJECT_PTR out;
      int val = convert_expression_to_object(e->elements[e->nof_elements-1], &out);

      OBJECT_PTR cons_obj;
      
      if(val != 0)
        return -1;

      cons_obj = cons(out, NIL);

      for(i=e->nof_elements - 2; i>=0; i--)
      {
        OBJECT_PTR out1;
        int val1 = convert_expression_to_object(e->elements[i], &out1);

        if(val != 0)
          return -1;

	cons_obj = cons(out1, cons_obj);
      }
      *out_val = cons_obj;
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
    else if(IS_INTEGER_OBJECT(obj1))
    {
      int val = get_int_value(obj1);
      if(IS_INTEGER_OBJECT(obj2))
        ret = (val  == get_int_value(obj2));
      else if(IS_FLOAT_OBJECT(obj2))
        ret = (val == get_float_value(obj2));
    }
    else if(IS_FLOAT_OBJECT(obj1))
    {
      float val = get_float_value(obj1);
      if(IS_INTEGER_OBJECT(obj2))
        ret = (val  == get_int_value(obj2));
      else if(IS_FLOAT_OBJECT(obj2))
        ret = (val == get_float_value(obj2));
    }
    else if(is_string_object(obj1))
    {
      char *str1 = get_string(obj1);
      if(is_string_object(obj2))
        ret = !strcmp(str1, get_string(obj2));
      else if(IS_STRING_LITERAL_OBJECT(obj2))
        ret = !strcmp(str1, strings[(int)obj2 >> OBJECT_SHIFT]);
    }
    else if(IS_STRING_LITERAL_OBJECT(obj1))
    {
      char *str1 = strings[(int)obj1 >> OBJECT_SHIFT];
      if(is_string_object(obj2))
        ret = !strcmp(str1, get_string(obj2));
      else if(IS_STRING_LITERAL_OBJECT(obj2))
        ret = !strcmp(str1, strings[(int)obj2 >> OBJECT_SHIFT]);
    }    
  }
  
  return ret;
}

OBJECT_PTR create_closure_object(OBJECT_PTR env_list, OBJECT_PTR params, OBJECT_PTR body, OBJECT_PTR source)
{
  uintptr_t ptr = object_alloc(4, CLOSURE_TAG);

  set_heap(ptr, 0, env_list);
  set_heap(ptr, 1, params);
  set_heap(ptr, 2, body);
  set_heap(ptr, 3, source);
  
  return ptr + CLOSURE_TAG;
}

OBJECT_PTR clone_object(OBJECT_PTR obj)
{
  OBJECT_PTR ret;

  log_function_entry("clone_object");

#ifdef DEBUG
  print_object(obj);
  fprintf(stdout, "\n");
#endif
  
  if(is_atom(obj) || IS_CONTINUATION_OBJECT(obj) || is_continuation_object(obj))
    ret = obj; //atoms are immutable and are reused; continuation objects are also not cloned
  else
  {
    if(IS_CONS_OBJECT(obj))
      ret = cons(clone_object(car(obj)), clone_object(cdr(obj)));
    else if(IS_CLOSURE_OBJECT(obj))
      ret = create_closure_object(clone_object(get_env_list(obj)),
                                  clone_object(get_params_object(obj)), 
                                  clone_object(get_body_object(obj)),
                                  clone_object(get_source_object(obj)));
    else if(IS_MACRO_OBJECT(obj))
      ret = create_macro_object(clone_object(get_env_list(obj)),
				clone_object(get_params_object(obj)), 
				clone_object(get_body_object(obj)),
                                clone_object(get_source_object(obj)));
    else if(IS_ARRAY_OBJECT(obj))
    {
      uintptr_t ptr = extract_ptr(obj);

      //int len = get_int_value(get_heap(ptr, 0));
      unsigned int len = *((OBJECT_PTR *)ptr);

      uintptr_t *raw_ptr;

      uintptr_t new_obj;

      int i;

      //not using object_alloc() because that would
      //result in the array size object being independently
      //added to the white set and garbage collected
      //independently of the parent array object
      //(this poses a problem only while deallocation
      //while reporting the deallocation statistics,
      //and not otherwise; the size object and the array object
      //get moved to grey/black sets in unison).
      //posix_memalign((void **)&raw_ptr, 16, sizeof(unsigned int *));
      //*((int *)raw_ptr) = len;

      new_obj = object_alloc(len+1, ARRAY_TAG);
      
      //set_heap(new_obj, 0, get_heap(ptr, 0));
      //set_heap(new_obj, 0, (uintptr_t)raw_ptr + INTEGER_TAG);
      *((OBJECT_PTR *)new_obj) = len;

      for(i=1; i<=len; i++)
	set_heap(new_obj, i, clone_object(get_heap(ptr, i)));

      ret = new_obj + ARRAY_TAG;
    }
    else if(IS_FUNCTION2_OBJECT(obj))
    {
      OBJECT_PTR cons_equiv = cons_equivalent(obj);
      OBJECT_PTR cloned_cons = clone_object(cons_equiv);
      ret = (extract_ptr(cloned_cons)) + FUNCTION2_TAG;
    }
    else if(IS_MACRO2_OBJECT(obj))
    {
      OBJECT_PTR cons_equiv = cons_equivalent(obj);
      OBJECT_PTR cloned_cons = clone_object(cons_equiv);
      ret = (cloned_cons) + MACRO2_TAG;
    }
    else if(IS_NATIVE_FN_OBJECT(obj))
    {
      uintptr_t ptr = obj - NATIVE_FN_TAG;
      ret = convert_native_fn_to_object((nativefn)(*(nativefn *)ptr));
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
  return get_heap(extract_ptr(obj), 0);
}

OBJECT_PTR get_params_object(OBJECT_PTR obj)
{
  assert(IS_CLOSURE_OBJECT(obj) || IS_MACRO_OBJECT(obj) || IS_FUNCTION2_OBJECT(obj) || IS_MACRO2_OBJECT(obj));

  if(IS_CLOSURE_OBJECT(obj) || IS_MACRO_OBJECT(obj))
    return get_heap(extract_ptr(obj), 1);
  else
  {
    OBJECT_PTR cons_equiv = cons_equivalent(obj);
    OBJECT_PTR source = car(last_cell(cons_equiv));
    return second(source);
  }
}

OBJECT_PTR get_body_object(OBJECT_PTR obj)
{
  assert(IS_CLOSURE_OBJECT(obj) || IS_MACRO_OBJECT(obj));
  return get_heap(extract_ptr(obj), 2);
}

OBJECT_PTR get_source_object(OBJECT_PTR obj)
{
  assert(IS_CLOSURE_OBJECT(obj) || IS_MACRO_OBJECT(obj) || IS_FUNCTION2_OBJECT(obj) || IS_MACRO2_OBJECT(obj));

  if(IS_CLOSURE_OBJECT(obj) || IS_MACRO_OBJECT(obj))
    return get_heap(extract_ptr(obj), 3);
  else
  {
    OBJECT_PTR cons_equiv = cons_equivalent(obj);
    OBJECT_PTR source = car(last_cell(cons_equiv));
    return CDDR(source);
  }
}

int print_closure_object_to_string(OBJECT_PTR obj, char *buf, int filled_buf_len)
{
  return sprintf(buf+filled_buf_len, "#<CLOSURE #x%08x> ", obj);
}

void print_closure_object(OBJECT_PTR obj)
{
  if(!console_mode && !single_expression_mode && !pipe_mode)
  {
    char buf[500];
    memset(buf, '\0', 500);
    sprintf(buf, "#<CLOSURE #x%08x> ", obj);
    print_to_transcript(buf);
  }
  else
    fprintf(stdout, "#<CLOSURE #x%08x> ", obj);

#ifdef DEBUG
  fprintf(stdout, "\nPARAMETERS: ");
  print_object(get_params_object(obj));
  fprintf(stdout,"\nBODY: ");
  print_object(get_body_object(obj));
  fprintf(stdout,"\nENV: ");
  print_object(get_env_list(obj));
#endif
}

int cons_length(OBJECT_PTR cons_obj)
{
  OBJECT_PTR rest;
  int l = 0;

  if(cons_obj == NIL)
    return 0;

  if(!IS_CONS_OBJECT(cons_obj))
    assert(false);

  rest = cons_obj;

  while(rest != NIL)
  {
    l++;
    rest = cdr(rest);
  }

  return l;
}

OBJECT_PTR get_symbol_value(OBJECT_PTR symbol_obj, OBJECT_PTR env_list)
{
  OBJECT_PTR rest = env_list;
  OBJECT_PTR ret;
  BOOLEAN found = false;

  log_function_entry("get_symbol_value");

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
    ret = CONS_NIL_NIL;

  log_function_exit("get_symbol_value");

  return ret;
}

OBJECT_PTR get_symbol_value_from_env(OBJECT_PTR symbol_obj, OBJECT_PTR env_obj)
{
  OBJECT_PTR rest = env_obj;
  OBJECT_PTR ret;
  BOOLEAN found = false;

  log_function_entry("get_symbol_value_from_env");

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
    ret = CONS_NIL_NIL;

  log_function_exit("get_symbol_value_from_env");

  return ret;
}

OBJECT_PTR update_environment(OBJECT_PTR env_list, OBJECT_PTR symbol_obj, OBJECT_PTR val)
{
  OBJECT_PTR rest1 = env_list;

  OBJECT_PTR rest2;

  while(rest1 != NIL)
  {
    OBJECT_PTR rest2 = car(rest1);

    while(rest2 != NIL)
    {
      if(equal(CAAR(rest2),symbol_obj))
      {
        uintptr_t ptr = extract_ptr(car(rest2));
	set_heap(ptr, 1, val);
	return symbol_obj;
      }

      rest2 = cdr(rest2);
    }

    rest1 = cdr(rest1);
  }    

  //check the top level environment 
  rest2 = top_level_env;

  while(rest2 != NIL)
  {
    if(equal(CAAR(rest2),symbol_obj))
    {
      uintptr_t ptr = extract_ptr(car(rest2));
      set_heap(ptr, 1, val);
      system_changed = true;
      return symbol_obj;
    }

    rest2 = cdr(rest2);
  }

  return NIL;
}

void add_to_top_level_environment(OBJECT_PTR symbol_obj, OBJECT_PTR val)
{
  OBJECT_PTR rest;

  uintptr_t ptr;

  if(top_level_env == NIL)
  {
    top_level_env = cons(cons(symbol_obj,val), 
                         NIL);
    system_changed = true;
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
        uintptr_t ptr = extract_ptr(car(rest));
        set_heap(ptr, 1, val);
        system_changed = true;
        return;
      }
      rest = cdr(rest);
    }

    //symbol does not exist in the environment
    ptr = extract_ptr(last_cell(top_level_env));
    set_heap(ptr, 1, cons(cons(symbol_obj, val), NIL));
    system_changed = true;
  }
}

BOOLEAN is_special_form(OBJECT_PTR form)
{
  if(IS_SYMBOL_OBJECT(form))
  {
    int index = (int)form >> OBJECT_SHIFT;

    return (index >= 0 && index <= NOF_SPECIAL_SYMBOLS);
  }

  return false;
}

OBJECT_PTR create_macro_object(OBJECT_PTR env_list, OBJECT_PTR params, OBJECT_PTR body, OBJECT_PTR source)
{
  uintptr_t ptr = object_alloc(4, MACRO_TAG);

  set_heap(ptr, 0, env_list);
  set_heap(ptr, 1, params);
  set_heap(ptr, 2, body);
  set_heap(ptr, 3, source);

  return ptr + MACRO_TAG;
}

int print_macro_object_to_string(OBJECT_PTR macro_obj, char *buf, int filled_buf_len)
{
  return sprintf(buf+filled_buf_len, "#<MACRO #x%08x> ", macro_obj);
}

void print_macro_object(OBJECT_PTR macro_obj)
{
  if(!console_mode && !single_expression_mode && !pipe_mode)
  {
    char buf[500];
    memset(buf, '\0', 500);
    sprintf(buf, "#<MACRO #x%08x> ", macro_obj);
    print_to_transcript(buf);
  }
  else
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

OBJECT_PTR last_cell(OBJECT_PTR lst)
{
  OBJECT_PTR rest = lst;

  while(cdr(rest) != NIL)
  {
    //rest = cdr(rest);
    //rest = (OBJECT_PTR)*((unsigned int *)extract_ptr(rest)+1);
    rest = (OBJECT_PTR)*((OBJECT_PTR *)(  extract_ptr(rest) )+1);
  }
  
  return rest;
}

OBJECT_PTR last_cell_orig(OBJECT_PTR list)
{
  if(cdr(list) == NIL)
    return list;
  else
    return last_cell(cdr(list));
}


//TODO: package-awareness (is this required?)
OBJECT_PTR gensym()
{
  char sym[20];

  gen_sym_count++;

  sprintf(sym, "#:G%08d", gen_sym_count);

  //return (OBJECT_PTR) ((current_package << (SYMBOL_BITS + OBJECT_SHIFT)) + (add_symbol(sym) << OBJECT_SHIFT) + SYMBOL_TAG);
  return build_symbol_object(current_package, add_symbol(sym));
}

void create_package(char *name)
{
  package_t *temp;

  nof_packages++;

  temp = (package_t *)GC_REALLOC(packages, nof_packages * sizeof(package_t));

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

  system_changed = true;
}

void initialize_core_package()
{
  /* There are 23 symbols (12 assembler mnemonics, seven FFI types, and LET,COND,DOTIMES, DOLIST)
     that are defined for convenience in addtion to the special symbols. These 
     mnemonics are not special, i.e., it doesn't matter if the user source code uses them */
  packages[CORE_PACKAGE_INDEX].nof_symbols = NOF_SPECIAL_SYMBOLS + NOF_NON_SPECIAL_SYMBOLS; 
  packages[CORE_PACKAGE_INDEX].symbols = (char **)GC_MALLOC(packages[CORE_PACKAGE_INDEX].nof_symbols * sizeof(char *));

  packages[CORE_PACKAGE_INDEX].symbols[0]  = strdup("T");
  packages[CORE_PACKAGE_INDEX].symbols[1]  = strdup("NIL");
  packages[CORE_PACKAGE_INDEX].symbols[2]  = strdup("QUOTE");
  packages[CORE_PACKAGE_INDEX].symbols[3]  = strdup("PRIM-ATOM");
  packages[CORE_PACKAGE_INDEX].symbols[4]  = strdup("PRIM-EQ");
  packages[CORE_PACKAGE_INDEX].symbols[5]  = strdup("PRIM-CAR");
  packages[CORE_PACKAGE_INDEX].symbols[6]  = strdup("PRIM-CDR");
  packages[CORE_PACKAGE_INDEX].symbols[7]  = strdup("PRIM-CONS");
  packages[CORE_PACKAGE_INDEX].symbols[8]  = strdup("LAMBDA");
  packages[CORE_PACKAGE_INDEX].symbols[9]  = strdup("SET");
  packages[CORE_PACKAGE_INDEX].symbols[10] = strdup("PRIM-ADD");
  packages[CORE_PACKAGE_INDEX].symbols[11] = strdup("PRIM-SUB");
  packages[CORE_PACKAGE_INDEX].symbols[12] = strdup("PRIM-MULT");
  packages[CORE_PACKAGE_INDEX].symbols[13] = strdup("PRIM-DIV");
  packages[CORE_PACKAGE_INDEX].symbols[14] = strdup("PROGN");
  packages[CORE_PACKAGE_INDEX].symbols[15] = strdup("PRIM-PRINT");
  packages[CORE_PACKAGE_INDEX].symbols[16] = strdup("PRIM-LIST");
  packages[CORE_PACKAGE_INDEX].symbols[17] = strdup("PRIM-LISTP");
  packages[CORE_PACKAGE_INDEX].symbols[18] = strdup("PRIM-SYMBOL-VALUE");
  packages[CORE_PACKAGE_INDEX].symbols[19] = strdup("BACKQUOTE");
  packages[CORE_PACKAGE_INDEX].symbols[20] = strdup("PRIM-GT");
  packages[CORE_PACKAGE_INDEX].symbols[21] = strdup("PRIM-GENSYM");
  packages[CORE_PACKAGE_INDEX].symbols[22] = strdup("PRIM-SETCAR");
  packages[CORE_PACKAGE_INDEX].symbols[23] = strdup("PRIM-SETCDR");
  packages[CORE_PACKAGE_INDEX].symbols[24] = strdup("PRIM-ERROR");
  packages[CORE_PACKAGE_INDEX].symbols[25] = strdup("PRIM-CREATE-PACKAGE");
  packages[CORE_PACKAGE_INDEX].symbols[26] = strdup("PRIM-IN-PACKAGE");
  packages[CORE_PACKAGE_INDEX].symbols[27] = strdup("COMMA");
  packages[CORE_PACKAGE_INDEX].symbols[28] = strdup("COMMA-AT");
  packages[CORE_PACKAGE_INDEX].symbols[29] = strdup("PRIM-EXPAND-MACRO");
  packages[CORE_PACKAGE_INDEX].symbols[30] = strdup("PRIM-APPLY");
  packages[CORE_PACKAGE_INDEX].symbols[31] = strdup("PRIM-STRING");
  packages[CORE_PACKAGE_INDEX].symbols[32] = strdup("PRIM-MAKE-ARRAY");
  packages[CORE_PACKAGE_INDEX].symbols[33] = strdup("PRIM-ARRAY-GET");
  packages[CORE_PACKAGE_INDEX].symbols[34] = strdup("PRIM-ARRAY-SET");
  packages[CORE_PACKAGE_INDEX].symbols[35] = strdup("PRIM-SUB-ARRAY");
  packages[CORE_PACKAGE_INDEX].symbols[36] = strdup("PRIM-ARRAY-LENGTH");
  packages[CORE_PACKAGE_INDEX].symbols[37] = strdup("PRIM-PRINT-STRING");
  packages[CORE_PACKAGE_INDEX].symbols[38] = strdup("LABELS");
  packages[CORE_PACKAGE_INDEX].symbols[39] = strdup("PRIM-CREATE-IMAGE");
  packages[CORE_PACKAGE_INDEX].symbols[40] = strdup("BREAK");
  packages[CORE_PACKAGE_INDEX].symbols[41] = strdup("PRIM-LOAD-FOREIGN-LIBRARY");
  packages[CORE_PACKAGE_INDEX].symbols[42] = strdup("CALL-FOREIGN-FUNCTION");
  packages[CORE_PACKAGE_INDEX].symbols[43] = strdup("PRIM-ENV");
  packages[CORE_PACKAGE_INDEX].symbols[44] = strdup("IF");
  packages[CORE_PACKAGE_INDEX].symbols[45] = strdup("PRIM-EVAL"); 
  packages[CORE_PACKAGE_INDEX].symbols[46] = strdup("CALL/CC"); 
  packages[CORE_PACKAGE_INDEX].symbols[47] = strdup("DEFINE"); 
  packages[CORE_PACKAGE_INDEX].symbols[48] = strdup("RESUME"); 
  packages[CORE_PACKAGE_INDEX].symbols[49] = strdup("BACKTRACE"); 
  packages[CORE_PACKAGE_INDEX].symbols[50] = strdup("PRIM-LOAD-FILE"); 

  packages[CORE_PACKAGE_INDEX].symbols[51] = strdup("PRIM-CONSP");
  packages[CORE_PACKAGE_INDEX].symbols[52] = strdup("PRIM-INTEGERP");
  packages[CORE_PACKAGE_INDEX].symbols[53] = strdup("PRIM-FLOATP");
  packages[CORE_PACKAGE_INDEX].symbols[54] = strdup("PRIM-CHARACTERP");
  packages[CORE_PACKAGE_INDEX].symbols[55] = strdup("PRIM-SYMBOLP");
  packages[CORE_PACKAGE_INDEX].symbols[56] = strdup("PRIM-STRINGP");
  packages[CORE_PACKAGE_INDEX].symbols[57] = strdup("PRIM-ARRAYP");
  packages[CORE_PACKAGE_INDEX].symbols[58] = strdup("PRIM-CLOSUREP");
  packages[CORE_PACKAGE_INDEX].symbols[59] = strdup("PRIM-MACROP");
  packages[CORE_PACKAGE_INDEX].symbols[60] = strdup("PRIM-CONTINUATIONP");
  packages[CORE_PACKAGE_INDEX].symbols[61] = strdup("LAMBDA-EXPRESSION");
  packages[CORE_PACKAGE_INDEX].symbols[62] = strdup("WHILE1"); //reverting to macro WHILE
  packages[CORE_PACKAGE_INDEX].symbols[63] = strdup("FORMAT");
  packages[CORE_PACKAGE_INDEX].symbols[64] = strdup("PRIM-CLONE");
  packages[CORE_PACKAGE_INDEX].symbols[65] = strdup("RETURN");
  packages[CORE_PACKAGE_INDEX].symbols[66] = strdup("COMPILE1");
  packages[CORE_PACKAGE_INDEX].symbols[67] = strdup("RETURN-FROM");
  packages[CORE_PACKAGE_INDEX].symbols[68] = strdup("PRIM-SYMBOL");
  packages[CORE_PACKAGE_INDEX].symbols[69] = strdup("PRIM-SYMBOL-NAME");
  packages[CORE_PACKAGE_INDEX].symbols[70] = strdup("PRIM-UNBIND");
  packages[CORE_PACKAGE_INDEX].symbols[71] = strdup("PRIM-NEWLINE");
  packages[CORE_PACKAGE_INDEX].symbols[72] = strdup("ABORT");
  packages[CORE_PACKAGE_INDEX].symbols[73] = strdup("PRIM-TIME");
  packages[CORE_PACKAGE_INDEX].symbols[74] = strdup("PRIM-PROFILE");

  packages[CORE_PACKAGE_INDEX].symbols[75] = strdup("PRIM-NOT");
  packages[CORE_PACKAGE_INDEX].symbols[76] = strdup("PRIM-LT");
  packages[CORE_PACKAGE_INDEX].symbols[77] = strdup("PRIM-LEQ");
  packages[CORE_PACKAGE_INDEX].symbols[78] = strdup("PRIM-GEQ");
  packages[CORE_PACKAGE_INDEX].symbols[79] = strdup("NEQ");

  packages[CORE_PACKAGE_INDEX].symbols[80] = strdup("PRIM-SAVE-OBJECT");
  packages[CORE_PACKAGE_INDEX].symbols[81] = strdup("PRIM-LOAD-OBJECT");

  packages[CORE_PACKAGE_INDEX].symbols[82] = strdup("COMPILE-FN");

  packages[CORE_PACKAGE_INDEX].symbols[83] = strdup("PRIM-EXPORT-PACKAGE");

  packages[CORE_PACKAGE_INDEX].symbols[84] = strdup("COMPILE-EXP");

  packages[CORE_PACKAGE_INDEX].symbols[85] = strdup("IMPORT-PACKAGE");

  /* symbols corresponding to assembler mnemonics */
  packages[CORE_PACKAGE_INDEX].symbols[86] =  strdup("HALT");
  packages[CORE_PACKAGE_INDEX].symbols[87] =  strdup("REFER");
  packages[CORE_PACKAGE_INDEX].symbols[88] =  strdup("CONSTANT");
  packages[CORE_PACKAGE_INDEX].symbols[89] =  strdup("CLOSE");
  packages[CORE_PACKAGE_INDEX].symbols[90] =  strdup("TEST");
  packages[CORE_PACKAGE_INDEX].symbols[91] =  strdup("ASSIGN");         
  packages[CORE_PACKAGE_INDEX].symbols[92] =  strdup("CONTI");
  packages[CORE_PACKAGE_INDEX].symbols[93] =  strdup("NUATE");
  packages[CORE_PACKAGE_INDEX].symbols[94] =  strdup("FRAME");
  packages[CORE_PACKAGE_INDEX].symbols[95] =  strdup("ARGUMENT");
  /* APPLY already defined as a special symbol */
  /* RETURN already defined as a special symbol */
  /* DEFINE already defined as a special symbol */
  packages[CORE_PACKAGE_INDEX].symbols[96] = strdup("MACRO");
  /* end symbols corresponding to assembler mnemonics */

  /* symbols for FFI */
  packages[CORE_PACKAGE_INDEX].symbols[97] = strdup("INTEGER");
  packages[CORE_PACKAGE_INDEX].symbols[98] = strdup("FLOAT");
  packages[CORE_PACKAGE_INDEX].symbols[99] = strdup("CHARACTER");
  packages[CORE_PACKAGE_INDEX].symbols[100] = strdup("VOID");
  packages[CORE_PACKAGE_INDEX].symbols[101] = strdup("INTEGER-POINTER");
  packages[CORE_PACKAGE_INDEX].symbols[102] = strdup("FLOAT-POINTER");
  packages[CORE_PACKAGE_INDEX].symbols[103] = strdup("CHARACTER-POINTER");
  /* end symbols for FFI */

  packages[CORE_PACKAGE_INDEX].symbols[104] = strdup("LET");
  packages[CORE_PACKAGE_INDEX].symbols[105] = strdup("COND");
  packages[CORE_PACKAGE_INDEX].symbols[106] = strdup("DOTIMES");
  packages[CORE_PACKAGE_INDEX].symbols[107] = strdup("DOLIST");

  packages[CORE_PACKAGE_INDEX].symbols[108] = strdup("LET2");
  packages[CORE_PACKAGE_INDEX].symbols[109] = strdup("DEFUN");
  packages[CORE_PACKAGE_INDEX].symbols[110] = strdup("DEFMACRO");

  packages[CORE_PACKAGE_INDEX].symbols[111] = strdup("NTH1");
  //packages[CORE_PACKAGE_INDEX].symbols[111] = strdup("CALL-CC");
  packages[CORE_PACKAGE_INDEX].symbols[112] = strdup("MY-CONT-VAR");
  packages[CORE_PACKAGE_INDEX].symbols[113] = strdup("SAVE-CONTINUATION");
  packages[CORE_PACKAGE_INDEX].symbols[114] = strdup("LETREC");
  packages[CORE_PACKAGE_INDEX].symbols[115] = strdup("EXTRACT-NATIVE-FN");
  packages[CORE_PACKAGE_INDEX].symbols[116] = strdup("CREATE-FN-CLOSURE");
  packages[CORE_PACKAGE_INDEX].symbols[117] = strdup("PRIM-CONCAT");
  packages[CORE_PACKAGE_INDEX].symbols[118] = strdup("GET-CONTINUATION");

  packages[CORE_PACKAGE_INDEX].symbols[119] = strdup("THROW");
  packages[CORE_PACKAGE_INDEX].symbols[120] = strdup("GET-EXCEPTION-HANDLER");
  packages[CORE_PACKAGE_INDEX].symbols[121] = strdup("ADD-EXCEPTION-HANDLER");

  packages[CORE_PACKAGE_INDEX].symbols[122] = strdup("PRIM-CALL-FF-INTERNAL");

  packages[CORE_PACKAGE_INDEX].symbols[123] = strdup("REPL-FUNCTION");

  packages[CORE_PACKAGE_INDEX].symbols[124] = strdup("SAVE-CONTINUATION-TO-RESUME");

  packages[CORE_PACKAGE_INDEX].symbols[125] = strdup("DISABLE-EXCEPTION-HANDLERS");
  packages[CORE_PACKAGE_INDEX].symbols[126] = strdup("ENABLE-EXCEPTION-HANDLERS");

  packages[CORE_PACKAGE_INDEX].symbols[127] = strdup("GET-SOURCE");

  packages[CORE_PACKAGE_INDEX].symbols[128] = strdup("INSPECT-OBJECT");
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

  int symbol_index;

  OBJECT_PTR retval;

  if(package_index == NOT_FOUND)
  {
    printf("Not found for %s\n", package_name);
    return CONS_NIL_NIL;
  }

  symbol_index = find_qualified_symbol(package_index, symbol_name);

  if(symbol_index != NOT_FOUND) //symbol exists in symbol table
    //retval = (OBJECT_PTR) ((package_index << (SYMBOL_BITS + OBJECT_SHIFT)) + (symbol_index << OBJECT_SHIFT) + SYMBOL_TAG);
    retval = build_symbol_object(package_index, symbol_index);
  else
    //retval = (OBJECT_PTR) ((package_index << (SYMBOL_BITS + OBJECT_SHIFT)) + (add_qualified_symbol(package_name, symbol_name) << OBJECT_SHIFT) + SYMBOL_TAG);
    retval = build_symbol_object(package_index, add_qualified_symbol(package_name, symbol_name));
    
  return cons(TRUE, retval);
}

void print_qualified_symbol(OBJECT_PTR ptr, char *buf)
{
  //int package_index = (int)ptr >> (SYMBOL_BITS + OBJECT_SHIFT);
  //int symbol_index =  ((int)ptr >> OBJECT_SHIFT) & TWO_RAISED_TO_SYMBOL_BITS_MINUS_1;
  int package_index = extract_package_index(ptr);
  int symbol_index = extract_symbol_index(ptr);
  
  assert(IS_SYMBOL_OBJECT(ptr));

  memset(buf,'\0',SYMBOL_STRING_SIZE);

  if(package_index < 0 || package_index >= nof_packages)
    assert(false);

  if(symbol_index < 0 || symbol_index >= packages[package_index].nof_symbols)
    assert(false);

  sprintf(buf, "%s:%s", packages[package_index].name, packages[package_index].symbols[symbol_index]);
}

void print_symbol(OBJECT_PTR ptr, char *buf)
{
  //int package_index = (int)ptr >> (SYMBOL_BITS + OBJECT_SHIFT);
  //int symbol_index =  ((int)ptr >> OBJECT_SHIFT) & TWO_RAISED_TO_SYMBOL_BITS_MINUS_1;
  int package_index = extract_package_index(ptr);
  int symbol_index = extract_symbol_index(ptr);
  
  log_function_entry("print_symbol");

  assert(IS_SYMBOL_OBJECT(ptr));

  memset(buf,'\0',SYMBOL_STRING_SIZE);

  if(package_index < 0 || package_index >= nof_packages)
    assert(false);

  if(symbol_index < 0 || symbol_index >= packages[package_index].nof_symbols)
    assert(false);

  /* if(package_index != 0) */
  /*   sprintf(buf, "%s:%s", packages[package_index].name, packages[package_index].symbols[symbol_index]); */
  /* else */
  if(package_index != CORE_PACKAGE_INDEX && package_index != print_context_pkg_index)
    sprintf(buf, "%s:%s", packages[package_index].name, packages[package_index].symbols[symbol_index]);
  else
    sprintf(buf, "%s", packages[package_index].symbols[symbol_index]);

  log_function_exit("print_symbol");
}

int add_qualified_symbol(char *package_name, char *sym)
{
  int package_index = find_package(package_name);

  char ** temp;

  log_function_entry("add_symbol");

  assert(package_index != NOT_FOUND);

  packages[package_index].nof_symbols++;

  temp = (char **)GC_REALLOC(packages[package_index].symbols, packages[package_index].nof_symbols * sizeof(char *));

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
  int package_index, symbol_index;

  if(!IS_SYMBOL_OBJECT(symbol_object))
    assert(false);

  //package_index = (int)symbol_object >> (SYMBOL_BITS + OBJECT_SHIFT);
  //symbol_index =  ((int)symbol_object >> OBJECT_SHIFT) & TWO_RAISED_TO_SYMBOL_BITS_MINUS_1;
  package_index = extract_package_index(symbol_object);
  symbol_index = extract_symbol_index(symbol_object);
  
  return packages[package_index].symbols[symbol_index];
}

char *get_qualified_symbol_name(OBJECT_PTR symbol_object)
{
  int package_index, symbol_index;

  if(!IS_SYMBOL_OBJECT(symbol_object))
    assert(false);

  package_index = extract_package_index(symbol_object);
  symbol_index = extract_symbol_index(symbol_object);

  char *package_name = packages[package_index].name;
  char *symbol_name = packages[package_index].symbols[symbol_index];

  int package_name_length = strlen(package_name);
  int symbol_name_length = strlen(symbol_name);

  char *ret = GC_MALLOC((package_name_length + symbol_name_length + 1) * sizeof(char));

  sprintf(ret, "%s:%s", package_name, symbol_name);

  ret[strlen(ret)] = '\0';

  return ret;
}

//given a key (e.g. 'a') and an arg list (e.g. (:a 1 :b 2 :c 3)
//this function returns the corresponding value (1 in this case)
OBJECT_PTR get_keyword_arg(OBJECT_PTR key, OBJECT_PTR arg_list)
{
  OBJECT_PTR ret;
  BOOLEAN found = false;

  OBJECT_PTR rest = arg_list;

  char *temp1, *temp2;

  log_function_entry("get_keyword_arg");

  assert(IS_SYMBOL_OBJECT(key));

  while(rest != NIL)
  {

    if(!(IS_SYMBOL_OBJECT(car(rest))))
    {
      rest = cdr(rest);
      continue;
    }

    temp1 = get_symbol_name(car(rest));

    assert(temp1[0] == ':');

    temp2 = substring(temp1, 1, strlen(temp1) - 1);

    if(!strcmp(get_symbol_name(key), temp2))
    {
      ret = CADR(rest);
      //free(temp2);
      found = true;
      break;
    }

    //free(temp2); //temp1 need not be freed
    rest = CDDR(rest); //need to skip the values and only consider :a, :b, ...
  }

  if(!found)
    ret = NIL;

  log_function_exit("get_keyword_arg");

  return ret;
}

BOOLEAN is_keyword_symbol(OBJECT_PTR symbol_object)
{
  BOOLEAN ret;

  log_function_entry("is_keyword_symbol");

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
  BOOLEAN ret = false;

  OBJECT_PTR rest = list;

  log_function_entry("contains_keyword_parameter");

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
  if(!IS_INTEGER_OBJECT(obj))
    assert(false);

  return *((int *)extract_ptr(obj));
}

OBJECT_PTR convert_int_to_object(int v)
{
  uintptr_t ptr = object_alloc(1, INTEGER_TAG);

  *((int *)ptr) = v;

  return ptr + INTEGER_TAG;
}

//float get_float_value(OBJECT_PTR obj)
double get_float_value(OBJECT_PTR obj)
{
  assert(IS_FLOAT_OBJECT(obj));
  //return *((float *)obj);
  //return *((float *)extract_ptr(obj));
  return *((double *)extract_ptr(obj));
}

//OBJECT_PTR convert_float_to_object(float v)
OBJECT_PTR convert_float_to_object(double v)
{
  uintptr_t ptr = object_alloc(1, FLOAT_TAG);

  //*((float *)ptr) = v;
  *((double *)ptr) = v;

  return ptr + FLOAT_TAG;
}

int print_array_object_to_string(OBJECT_PTR array, char *buf, int filled_buf_len)
{
  int len = 0;

  uintptr_t ptr = extract_ptr(array);

  //int length = get_int_value(get_heap(ptr, 0));
  int length = *((OBJECT_PTR *)ptr);

  int i;

  len += sprintf(buf+filled_buf_len, "[");

  for(i=0; i< length; i++)
  {
    len += print_object_to_string(get_heap(ptr, i + 1), buf, filled_buf_len+len);
    len += sprintf(buf+filled_buf_len+len, " ");
  }

  if(length > 0)
    len += sprintf(buf+filled_buf_len+len-1, "]") - 1;
  else
    len += sprintf(buf+filled_buf_len+len, "]");

  return len;
}

void print_array_object(OBJECT_PTR array)
{
  uintptr_t ptr;
  int length, i;

  log_function_entry("print_array_object");

  ptr = extract_ptr(array);

  if(!console_mode && !single_expression_mode && !pipe_mode)
  {
    print_to_transcript("[");

    //length = get_int_value(get_heap(ptr, 0));
    length = *((OBJECT_PTR *)ptr);

    for(i=0; i< length; i++)
    {
      print_object(get_heap(ptr, i + 1));
      print_to_transcript(" ");
    }

    if(length > 0)
      transcript_backspace();

    print_to_transcript("]");
  }
  else
  {
    fprintf(stdout, "[");

    //length = get_int_value(get_heap(ptr, 0));
    length = *((OBJECT_PTR *)ptr);

    for(i=0; i< length; i++)
    {
      print_object(get_heap(ptr, i + 1));
      fprintf(stdout, " ");
    }

    if(length > 0)
      fprintf(stdout, "\b");

    fprintf(stdout, "]");
  }

  log_function_exit("print_array_object");
}

int print_string_to_string(OBJECT_PTR string_object, char *buf, int filled_buf_len)
{
  uintptr_t ptr = extract_ptr(string_object);

  //int len = get_int_value(get_heap(ptr, 0));
  int len = *((OBJECT_PTR *)ptr);

  int i;

  int length = 0;

  length += sprintf(buf+filled_buf_len, "\"");

  for(i=1; i<=len; i++)
    length += sprintf(buf+filled_buf_len+length, "%c", (int)get_heap(ptr, i) >> OBJECT_SHIFT);

  length += sprintf(buf+filled_buf_len+length, "\"");

  return length;
}

void print_string(OBJECT_PTR string_object)
{
  uintptr_t ptr = extract_ptr(string_object);

  //int len = get_int_value(get_heap(ptr, 0));
  int len = *((OBJECT_PTR *)ptr);

  int i;

  char *buf;

  buf = (char *)GC_MALLOC(len*sizeof(char));
  
  int length;

  assert(is_string_object(string_object));

  if(!console_mode && !single_expression_mode && !pipe_mode)
  {
    memset(buf, '\0', len);

    length = 0;

    length = sprintf(buf+length, "\"");

    for(i=1; i<=len; i++)
      length += sprintf(buf+length, "%c", (int)get_heap(ptr, i) >> OBJECT_SHIFT);

    length += sprintf(buf+length, "\"");

    print_to_transcript(buf);
  }
  else
  {
    fprintf(stdout, "\"");

    for(i=1; i<=len; i++)
      fprintf(stdout, "%c", (int)get_heap(ptr, i) >> OBJECT_SHIFT);

    fprintf(stdout, "\"");
  }
}

BOOLEAN is_string_object(OBJECT_PTR obj)
{
  uintptr_t ptr;
  int len, i;

  if(!(IS_ARRAY_OBJECT(obj)))
    return false;

  ptr = extract_ptr(obj);

  //len = get_int_value(get_heap(ptr, 0));
  len = *((OBJECT_PTR *)ptr);

  for(i=1; i<=len; i++)
  {
    if(!(IS_CHAR_OBJECT(get_heap(ptr, i))))
      return false;
  }

  return true;
}

char *get_string(OBJECT_PTR string_object)
{
  uintptr_t ptr;
  int len, i;
  char *ret;

  if(!is_string_object(string_object))
    assert(false);

  ptr = extract_ptr(string_object);

  //len = get_int_value(get_heap(ptr, 0));
  len = *((OBJECT_PTR *)ptr);

  ret = (char *)GC_MALLOC(len * sizeof(char) + 1);

  for(i=1; i<=len; i++)
    ret[i-1] = (int)get_heap(ptr, i) >> OBJECT_SHIFT;

  ret[len] = '\0';

  return ret;
}

BOOLEAN is_valid_object(OBJECT_PTR obj)
{
  if(IS_CONS_OBJECT(obj)         ||
     IS_CLOSURE_OBJECT(obj)      ||
     IS_MACRO_OBJECT(obj)        ||
     IS_ARRAY_OBJECT(obj)        ||
     IS_CONTINUATION_OBJECT(obj) ||
     IS_INTEGER_OBJECT(obj)      ||
     IS_FLOAT_OBJECT(obj)        ||
     IS_NATIVE_FN_OBJECT(obj)    ||
     IS_FUNCTION2_OBJECT(obj)    ||
     IS_MACRO2_OBJECT(obj))
    return true;

  if(IS_STRING_LITERAL_OBJECT(obj) ||
     IS_CHAR_OBJECT(obj))
    return true;

  if(IS_SYMBOL_OBJECT(obj))
  {
    //int package_index = (int)obj >> (SYMBOL_BITS + OBJECT_SHIFT);
    //int symbol_index =  ((int)obj >> OBJECT_SHIFT) & TWO_RAISED_TO_SYMBOL_BITS_MINUS_1;
    int package_index = extract_package_index(obj);
    int symbol_index = extract_symbol_index(obj);
    
    return package_index >= 0 && 
           package_index < nof_packages &&
           symbol_index >= 0 &&
           symbol_index < packages[package_index].nof_symbols;
  }

  return false;
}

OBJECT_PTR get_symbol_from_value(OBJECT_PTR value_obj, OBJECT_PTR env_list)
{
  OBJECT_PTR rest = env_list;
  OBJECT_PTR ret;
  BOOLEAN found = false;

  log_function_entry("get_symbol_from_value");

  while(rest != NIL)
  {
    OBJECT_PTR result = get_symbol_from_value_from_env(value_obj, car(rest));

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
    OBJECT_PTR result = get_symbol_from_value_from_env(value_obj, top_level_env);

    if(car(result) == TRUE)
    {
       ret = cons(TRUE, cdr(result));
       found = true;
    }
  }

  if(!found)
    ret = CONS_NIL_NIL;

  log_function_exit("get_symbol_from_value");

  return ret;
}

OBJECT_PTR get_symbol_from_value_from_env(OBJECT_PTR value_obj, OBJECT_PTR env_obj)
{
  OBJECT_PTR rest = env_obj;
  OBJECT_PTR ret;
  BOOLEAN found = false;

  log_function_entry("get_symbol_from_value_from_env");

  while(rest != NIL)
  {
    if(equal(CDAR(rest), value_obj))
    {
      ret = cons(TRUE, CAAR(rest));
      found = true;
      break;
    }
    
    rest = cdr(rest);
  }

  if(!found)
    ret = CONS_NIL_NIL;

  log_function_exit("get_symbol_from_value_from_env");

  return ret;
}

OBJECT_PTR list(int count, ...)
{
  va_list ap;
  OBJECT_PTR ret;
  int i;

  if(!count)
    return NIL;

  va_start(ap, count);

  ret = cons((OBJECT_PTR)va_arg(ap, OBJECT_PTR), NIL);

  for(i=1; i<count; i++)
  {
    OBJECT_PTR val = (OBJECT_PTR)va_arg(ap, OBJECT_PTR);
    uintptr_t ptr = extract_ptr(last_cell(ret));
    set_heap(ptr, 1, cons(val, NIL));
  }

  va_end(ap);

  return ret;
}

OBJECT_PTR convert_symbol_to_core_package_symbol(OBJECT_PTR sym)
{
  char *symbol_name = get_symbol_name(sym);

  int i;

  for(i=0; i<packages[CORE_PACKAGE_INDEX].nof_symbols; i++)
  {
    if(!strcmp(packages[CORE_PACKAGE_INDEX].symbols[i], symbol_name))
      return cons((OBJECT_PTR)TRUE, (OBJECT_PTR)((i << OBJECT_SHIFT) + SYMBOL_TAG));
  }

  return CONS_NIL_NIL;
}

BOOLEAN is_language_symbol(OBJECT_PTR sym)
{
  if(!IS_SYMBOL_OBJECT(sym))
    return false;

  char *s = get_symbol_name(sym);

  return !strcmp(s, "LET")               ||
         !strcmp(s, "LET1")              ||
         !strcmp(s, "LETREC")            ||
         !strcmp(s, "IF")                ||
         !strcmp(s, "SET")               ||
         !strcmp(s, "LAMBDA")            ||
         !strcmp(s, "MACRO")             ||
#ifdef WIN32
         !strcmp(s, "ERROR1")            ||
#else
         !strcmp(s, "ERROR")             ||
#endif
         !strcmp(s, "CALL/CC")           ||
         !strcmp(s, "DEFINE")            ||
         !strcmp(s, "T")                 ||
         !strcmp(s, "NIL")               ||
         !strcmp(s, "COMMA")             ||
         !strcmp(s, "COMMA-AT")          ||

         !strcmp(s, "INTEGER")           ||
         !strcmp(s, "FLOAT")             ||
         !strcmp(s, "CHARACTER")         ||
         !strcmp(s, "VOID")              ||
         !strcmp(s, "CHARACTER-POINTER") ||
         !strcmp(s, "FLOAT-POINTER")     ||
         !strcmp(s, "INTEGER-POINTER");
}

BOOLEAN is_core_package_op(OBJECT_PTR sym)
{
  if(!IS_SYMBOL_OBJECT(sym))
    return false;

  char *symbol_name = get_symbol_name(sym);   

  int i;

  for(i=0; i<nof_global_vars; i++)
  {
    if(top_level_symbols[i].delete_flag)
      continue;

    OBJECT_PTR top_level_sym = top_level_symbols[i].sym;  

    //int package_index = (int)top_level_sym >> (SYMBOL_BITS + OBJECT_SHIFT);
    int package_index = extract_package_index(top_level_sym);
    
    if(package_index != 0)
      continue;

    if(!strcmp(symbol_name, get_symbol_name(top_level_sym)))
      return true;
  }

  return false;
}

BOOLEAN sym_eq(OBJECT_PTR sym, char *s)
{
  if(!IS_SYMBOL_OBJECT(sym))
    return false;

  return !strcmp(get_symbol_name(sym), s);
}

BOOLEAN is_valid_binding_exp(OBJECT_PTR exp)
{
  if(!IS_CONS_OBJECT(exp))
    return false;    

  OBJECT_PTR rest = exp;

  while(rest != NIL)
  {
    if(!IS_CONS_OBJECT(car(rest)))
      return false;    

    if(!IS_SYMBOL_OBJECT(CAAR(rest)))
      return false;    

    rest = cdr(rest);
  }

  return true;
}

BOOLEAN is_valid_param_list(OBJECT_PTR exp)
{
  if(!IS_CONS_OBJECT(exp))
    return false;    

  OBJECT_PTR rest = exp;

  while(rest != NIL)
  {
    if(!IS_SYMBOL_OBJECT(car(rest)))
      return false;    

    rest = cdr(rest);
  }

  return true;
}

OBJECT_PTR rewrite_symbols(OBJECT_PTR exp)
{
  if(current_package == 0)
    return exp;

  if(is_atom(exp))
  {
    if(!IS_SYMBOL_OBJECT(exp))
      return exp;
    else
    {
      char *symbol_name = get_symbol_name(exp);

      if(!strcmp(symbol_name, "NIL") || !strcmp(symbol_name, "T"))
        return exp;

      //int package_index = (int)exp >> (SYMBOL_BITS + OBJECT_SHIFT);
      int package_index = extract_package_index(exp);
      
      if(package_index != 0 && package_index != current_package)
        return exp;
      else
      {
        int index = find_symbol(symbol_name, current_package);
    
        if(index != NOT_FOUND) //symbol exists in symbol table
          //return (OBJECT_PTR) ((current_package << (SYMBOL_BITS + OBJECT_SHIFT)) + (index << OBJECT_SHIFT) + SYMBOL_TAG);
          return build_symbol_object(current_package, index);
        else
          //return (OBJECT_PTR) ((current_package << (SYMBOL_BITS + OBJECT_SHIFT)) + (add_symbol(symbol_name) << OBJECT_SHIFT) + SYMBOL_TAG);
          return build_symbol_object(current_package, add_symbol(symbol_name));
      }
    }
  }

  OBJECT_PTR car_exp = car(exp);
  
  if(IS_SYMBOL_OBJECT(car_exp) && !strcmp(get_symbol_name(car_exp), "QUOTE"))
  {
    if(cons_length(exp) == 2 && IS_SYMBOL_OBJECT(second(exp)))
      return exp;
    else
      return cons(car_exp, rewrite_symbols(cdr(exp)));
  }
  else if((IS_SYMBOL_OBJECT(car_exp) && !strcmp(get_symbol_name(car_exp), "LET"))  || 
          (IS_SYMBOL_OBJECT(car_exp) && !strcmp(get_symbol_name(car_exp), "LET1")) || 
          (IS_SYMBOL_OBJECT(car_exp) && !strcmp(get_symbol_name(car_exp), "LETREC")))
  {
    OBJECT_PTR rest = second(exp), ret = NIL;

    //since we do not know whether
    //the LET is a valid construction,
    //as we are processing raw
    //s-expressions from the parser
    //if(IS_CONS_OBJECT(rest))
    if(is_valid_binding_exp(rest))
    {
      while(rest != NIL)
      {
        //if(IS_CONS_OBJECT(car(rest)))
          ret = cons(map(rewrite_symbols, car(rest)), ret);
          //else
          //  ret = cons(rewrite_symbols(car(rest)), ret);

        rest = cdr(rest);
      }

      if(CDDR(exp) != NIL)
        return concat(3, 
                      list(1, car_exp),
                      list(1, reverse(ret)),
                      map(rewrite_symbols, CDDR(exp)));
      else
        return concat(2, 
                      list(1, car_exp),
                      list(1, reverse(ret)));
    }
    else
      return cons(car_exp, map(rewrite_symbols, cdr(exp)));

  }
  else if((IS_SYMBOL_OBJECT(car_exp) && !strcmp(get_symbol_name(car_exp), "LAMBDA")) || 
          (IS_SYMBOL_OBJECT(car_exp) && !strcmp(get_symbol_name(car_exp), "MACRO"))  || 
          (IS_SYMBOL_OBJECT(car_exp) && !strcmp(get_symbol_name(car_exp), "CATCH")))
  {
    OBJECT_PTR params = second(exp);

    //if(IS_CONS_OBJECT(params))
    if(is_valid_param_list(params))
    {
      if(CDDR(exp) != NIL)
        return concat(3,
                    list(1, car_exp),
                    list(1, map(rewrite_symbols, params)),
                    map(rewrite_symbols, CDDR(exp)));
      else
        return concat(2,
                      list(1, car_exp),
                      list(1, map(rewrite_symbols, params)));
    }
    else
      return cons(car_exp, map(rewrite_symbols, cdr(exp)));
                    
  }
  else if((IS_SYMBOL_OBJECT(car_exp) && !strcmp(get_symbol_name(car_exp), "DEFUN")) || 
          (IS_SYMBOL_OBJECT(car_exp) && !strcmp(get_symbol_name(car_exp), "DEFMACRO")))
  {
    OBJECT_PTR params = third(exp);

    //if(IS_CONS_OBJECT(params))
    if(is_valid_param_list(params))
    {
      if(CDDDR(exp) != NIL)
        return concat(4,
                      list(1, car_exp),
                      list(1, rewrite_symbols(second(exp))),
                      list(1, map(rewrite_symbols, params)),
                      map(rewrite_symbols, CDDDR(exp)));
      else
        return concat(3,
                      list(1, car_exp),
                      list(1, rewrite_symbols(second(exp))),
                      list(1, map(rewrite_symbols, params)));
    }
    else
      return cons(car_exp, map(rewrite_symbols, cdr(exp)));
  }
  else if((IS_SYMBOL_OBJECT(car_exp) && !strcmp(get_symbol_name(car_exp), "APPLY"))   || 
          (IS_SYMBOL_OBJECT(car_exp) && !strcmp(get_symbol_name(car_exp), "FUNCALL")) ||
          (IS_SYMBOL_OBJECT(car_exp) && !strcmp(get_symbol_name(car_exp), "MAP"))     ||
          (IS_SYMBOL_OBJECT(car_exp) && !strcmp(get_symbol_name(car_exp), "MAPCAR"))  ||
          (IS_SYMBOL_OBJECT(car_exp) && !strcmp(get_symbol_name(car_exp), "MAPCAN")))
  {
    if(is_core_package_op(second(exp)))
      return cons(car_exp, cons(second(exp), map(rewrite_symbols,CDDR(exp))));
    else
      return cons(car_exp, map(rewrite_symbols, cdr(exp)));
  }
  else if(is_language_symbol(car_exp) || primop(car_exp) || is_core_package_op(car_exp))
    return cons(car_exp, map(rewrite_symbols, cdr(exp)));
  else
    return map(rewrite_symbols, exp);
}

#ifdef WIN32
//from http://stackoverflow.com/questions/735126/are-there-alternate-implementations-of-gnu-getline-interface/735472#735472
size_t getline(char **lineptr, size_t *n, FILE *stream) {
    char *bufptr = NULL;
    char *p = bufptr;
    size_t size;
    int c;

    if (lineptr == NULL) {
    	return -1;
    }
    if (stream == NULL) {
    	return -1;
    }
    if (n == NULL) {
    	return -1;
    }
    bufptr = *lineptr;
    size = *n;

    c = fgetc(stream);
    if (c == EOF) {
    	return -1;
    }
    if (bufptr == NULL) {
    	bufptr = GC_MALLOC(128);
    	if (bufptr == NULL) {
    		return -1;
    	}
    	size = 128;
    }
    p = bufptr;
    while(c != EOF) {
    	if ((p - bufptr) > (size - 1)) {
    		size = size + 128;
    		bufptr = GC_REALLOC(bufptr, size);
    		if (bufptr == NULL) {
    			return -1;
    		}
    	}
    	*p++ = c;
    	if (c == '\n') {
    		break;
    	}
    	c = fgetc(stream);
    }

    *p++ = '\0';
    *lineptr = bufptr;
    *n = size;

    return p - bufptr - 1;
}
#endif

extern OBJECT_PTR cps_transform(OBJECT_PTR);
extern OBJECT_PTR mcps_transform(OBJECT_PTR);

int main1(int argc, char **argv)
{
  OBJECT_PTR exp;

  console_mode = true;
  
  initialize();
  
  prompt();
  yyparse();

  convert_expression_to_object(g_expr, &exp);

  printf("Metacontinuation Passing Style:\n");
  print_object(mcps_transform(exp));
  printf("\n");

  printf("Simple Continuation Passing Style:\n");
  print_object(cps_transform(exp));
  printf("\n");
  
  return 0;
}

#ifdef __OSX_BUNDLE__
char full_exec[4096];
char exec_path[4096];
char path_buf[4096];
#endif

int main(int argc, char **argv)
{
  int opt, i;
  char *expression;

#ifdef __OSX_BUNDLE__

  int ret;
  pid_t pid; 

  pid = getpid();
  ret = proc_pidpath (pid, full_exec, sizeof(full_exec));

  assert(ret);

  for(i=strlen(full_exec)-1; i>=0; i--)
  {
    if(full_exec[i] == '/')
      break;
  }

  strncpy(exec_path, full_exec, i+1);

  chdir(getenv("HOME"));
#endif  
  
  while((opt = getopt(argc, argv, "i:rcnl:e:p")) != -1)
  {
    switch(opt)
    {
      case 'i':
	image_mode = true;
	loaded_image_file_name = strdup(optarg);
	break;
      case 'c':
	console_mode = true;
	break;
      case 'e':
	single_expression_mode = true;
	expression = strdup(optarg);
	break;
      case 'n':
        interpreter_mode = true;
        break;
      case 'p':
        pipe_mode = true;
        break;
      case 'l':
        core_library_file_name = strdup(optarg);
        break;
      case 'r':
        raw_mode = true;
        break;
      default:
	//fprintf(stderr, "Usage: %s [-i imagefile] [-l libfile] -n [-c | -e exp | -p]\n", argv[0]);
        fprintf(stderr, "Usage: %s [-i imagefile] [-c | -e exp | -p]\n", argv[0]);
	exit(EXIT_FAILURE);
    }
  }

  //by default load plisp.image unless
  //use specifies another image file using -i option
  if(!image_mode && !raw_mode)
  {
    if(access("plisp.image", F_OK) != -1)
    {
      image_mode = true;
      loaded_image_file_name = strdup("plisp.image");
    }
    else
    {
      fprintf(stdout, "-----------------------------INFO------------------------------\n");
      fprintf(stdout, "Default image file 'plisp.image' not found in the current\n");
      fprintf(stdout, "directory. Working with an image is faster and more convenient,\n");
      fprintf(stdout, "as the core library doesn't have to be recompiled each time\n");
      fprintf(stdout, "pLisp is invoked. You can create the image file by choosing\n");
      fprintf(stdout, "the 'Save image' option in the Transcript window, and invoking\n");
      fprintf(stdout, "plisp as 'plisp -i <image file>' thereafter. If you name your\n");
      fprintf(stdout, "image file as 'plisp.image', you do not have to specify it\n");
      fprintf(stdout, "during startup; pLisp will use this file automatically.\n");
      fprintf(stdout, "---------------------------------------------------------------\n\n");

      image_mode = false;
      raw_mode = true;
    }
  }

  if(console_mode && single_expression_mode ||
    console_mode && pipe_mode               ||
    single_expression_mode && pipe_mode)
  {
    fprintf(stderr, "-c, -p or -e options cannot be combined with each other\n");
    exit(EXIT_FAILURE);
  }

  if(!console_mode && !single_expression_mode && !pipe_mode)
    gtk_init(&argc, &argv);

  if(image_mode)
  {
    if(!single_expression_mode && !pipe_mode)
    {
      fprintf(stdout, "Loading image...");
      fflush(stdout);
    }

    core_library_loaded = true;

    initialize_memory();

    //initialize_tcc();

    if(load_from_image(loaded_image_file_name))
    {
      printf("Unable to load image from file %s\n", loaded_image_file_name);
      cleanup();
      exit(1);
    }

    if(!single_expression_mode && !pipe_mode)
      fprintf(stdout, "done\n");    
  }
  else
  {
    initialize();

    if(load_core_library())
    {
      cleanup();
      exit(1);
    }

    core_library_loaded = true;

    if(!console_mode && !single_expression_mode && !pipe_mode)
      create_transcript_window(DEFAULT_TRANSCRIPT_POSX,
			       DEFAULT_TRANSCRIPT_POSY,
			       DEFAULT_TRANSCRIPT_WIDTH,
			       DEFAULT_TRANSCRIPT_HEIGHT,
			       default_transcript_text);

    if(!console_mode && !single_expression_mode && !pipe_mode)
    {
      show_warning_dialog("Default image file 'plisp.image' not found in the current " \
                          "directory. Working with an image is faster and more convenient, " \
                          "as the core library doesn't have to be recompiled each time " \
                          "pLisp is invoked. You can create the image file by choosing " \
                          "the 'Save image' option in the Transcript window. " \
                          "If you name your image file as 'plisp.image', pLisp will use " \
                          "this file automatically.");
    }
    
  }

  build_autocomplete_words();

  if(!console_mode && !single_expression_mode && !pipe_mode)
  {
#ifdef WIN_BUILD
    if(build_help_entries("../share/help.json"))
#else
#ifdef __OSX_BUNDLE__
    if(build_help_entries(concat_strings(path_buf, exec_path, "../Resources/share/plisp/help.json")))
#else      
    if(build_help_entries( PLISPDATADIR "/help.json"))
#endif
#endif
    {
      fprintf(stderr, "Building help entries failed: %s\n", get_string(cdr(exception_object)));
      cleanup();
      exit(1);
    }
  }

  if(pipe_mode)
  {
    char *cmd = NULL;
    int nbytes = 1000;

    while(1)
    {
      //if(cmd)
      //  ifree(cmd);

      cmd = (char *)GC_MALLOC((nbytes + 1) * sizeof(char));
      getline(&cmd, &nbytes, stdin);

      if(!strcmp(cmd, "(quit)\n"))
	 break;

      yy_scan_string(cmd);
      yyparse();
      repl2();
    }

    //if(cmd)
    //  free(cmd);

    cleanup();
    exit(0);    
  }

  if(single_expression_mode)
  {
    yy_scan_string(expression);
    yyparse();
    repl2();
    printf("\n");
    cleanup();

    exit(0);    
  }

  if(console_mode)
  {
    print_copyright_notice();
    welcome();
    while(1)
    {
      prompt();
      yyparse();
      repl2();
    }
  }
  else
  {
    gtk_main();
  }

  return 0;
}

int load_core_library()
{
  //if(!console_mode && !single_expression_mode && !pipe_mode)
  //  print_to_transcript("Loading core library...");
  //else if(console_mode)
  //{
  if(!pipe_mode && !single_expression_mode)
  {
    fprintf(stdout, "Loading core library (this may take a while)...");
    fflush(stdout);
  }
  //}

  saved_continuations = NIL;
  continuations_for_return = NIL;
  most_recent_closure = NIL;

  exception_object = NIL;
  exception_handlers = NIL;

  debug_stack = NIL;
  debug_window_dbg_stack = NIL;

  OBJECT_PTR src = cons(LOAD_FILE, 
#ifdef WIN_BUILD
                        cons((OBJECT_PTR)get_string_object(core_library_file_name ? core_library_file_name : "../lib/plisp_full_monty_compiler.lisp"),
#else
#ifdef __OSX_BUNDLE__                             
                        cons((OBJECT_PTR)get_string_object(core_library_file_name ? core_library_file_name : concat_strings(path_buf, exec_path, "../Resources/share/plisp/plisp_full_monty_compiler.lisp")),
#else                             
                        cons((OBJECT_PTR)get_string_object(core_library_file_name ? core_library_file_name : PLISPDATADIR "/plisp_full_monty_compiler.lisp"),
#endif
#endif
                             NIL));

  OBJECT_PTR res = full_monty_eval(src);

  if(in_error)
  {
    handle_exception();
    fprintf(stdout, "Error loading core library\n");
    return 1;
  }

  in_error = false;

  core_library_loaded = true;

  //if(!console_mode && !single_expression_mode && !pipe_mode)
  //  print_to_transcript(" done\n");
  //else if(console_mode)
  //{
    //hack to prevent message being overwritten
    //fprintf(stdout, "Loading core library... done\n");
  if(!pipe_mode && !single_expression_mode)
  {
    fprintf(stdout, " done\n");
    fflush(stdout);
  }
  //}

  return 0;  
}
