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
#include <stdarg.h>
#include <setjmp.h>

#define TPL_NOLIB

#include "tpl/tpl.h"

#include "plisp.h"
#include "memory.h"

extern OBJECT_PTR top_level_env;
extern unsigned int current_package;
extern unsigned int nof_packages;
extern package_t *packages;
extern int gen_sym_count;

extern int nof_strings;
extern char **strings;

extern RAW_PTR *heap;

extern RAW_PTR free_list;
extern RAW_PTR last_segment;

extern BOOLEAN debug_mode;
extern OBJECT_PTR debug_continuation;
extern OBJECT_PTR debug_env;
extern OBJECT_PTR execution_stack;
extern OBJECT_PTR debug_execution_stack;
extern OBJECT_PTR reg_accumulator;
extern OBJECT_PTR reg_next_expression;
extern OBJECT_PTR reg_current_env;
extern OBJECT_PTR reg_current_value_rib;
extern OBJECT_PTR reg_current_stack;


jmp_buf env;
extern tpl_hook_t tpl_hook;

void create_image(char *file_name)
{

  /* 
    variables that are serialized
    -----------------------------
    char           **strings
    OBJECT_PTR     top_level_env
    RAW_PTR        free_list
    RAW_PTR        last_segment
    RAW_PTR        *heap
    unsigned int   current_package
    int            gen_sym_count
    package_t      *packages
    BOOLEAN        debug_mode
    OBJECT_PTR     debug_continuation
    OBJECT_PTR     debug_env
    OBJECT_PTR     execution_stack
    OBJECT_PTR     debug_execution_stack
    OBJECT_PTR     reg_accumulator
    OBJECT_PTR     reg_next_expression
    OBJECT_PTR     reg_current_env
    OBJECT_PTR     reg_current_value_rib
    OBJECT_PTR     reg_current_stack
  */

  int i,j;
  char *str;
  RAW_PTR val;

  struct pkg {
    char *name;
    int  nof_symbols;
  } package;

  char *sym;

  tpl_node *tn = tpl_map("A(s)uuuA(u)uiA(S(si)A(s))uuuuuuuuuu", 
			 &str, 
			 &top_level_env,
			 &free_list,
			 &last_segment,
			 &val,
			 &current_package,
			 &gen_sym_count,
			 &package,
			 &sym,
                         &debug_mode,
                         &debug_continuation,
                         &debug_env,
                         &execution_stack,
                         &debug_execution_stack,
                         &reg_accumulator,
                         &reg_next_expression,
                         &reg_current_env,
                         &reg_current_value_rib,
                         &reg_current_stack);

  i=0;

  for(str=strings[i]; i<nof_strings; str=strings[++i])
    tpl_pack(tn, 1);

  tpl_pack(tn, 0);

  for(i=0; i<HEAP_SIZE; i++)
  //for(i=0; i<heap_ptr; i++)
  {
    val = heap[i];
    tpl_pack(tn, 2);
  }

  for(i=0; i<nof_packages; i++)
  {
    package.name = packages[i].name;
    package.nof_symbols = packages[i].nof_symbols;

    for(j=0; j<package.nof_symbols; j++)
    {
      sym = packages[i].symbols[j];
      tpl_pack(tn, 4);
    }

    tpl_pack(tn, 3);
  }

  tpl_dump(tn, TPL_FILE, file_name);
  tpl_free(tn);
}

//for handline tpl-related errors
void catch_fatal(char *fmt, ...) {
  va_list ap;

  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
  longjmp(env,-1);                /* return to setjmp point */
}

void load_from_image(char *file_name)
{

  int err;

  int i,j;

  char *str;
  unsigned int val;

  struct pkg {
    char *name;
    int  nof_symbols;
  } package;

  char *sym;
 
  tpl_hook.fatal = catch_fatal;

  err = setjmp(env);
  if (err) {
    fprintf(stdout, "load_from_image() failed\n");
    cleanup();
    exit(1);
  }

  tpl_node *tn = tpl_map("A(s)uuuA(u)uiA(S(si)A(s))uuuuuuuuuu", 
			 &str, 
			 &top_level_env,
			 &free_list,
			 &last_segment,
			 &val,
			 &current_package,
			 &gen_sym_count,
			 &package,
			 &sym,
                         &debug_mode,
                         &debug_continuation,
                         &debug_env,
                         &execution_stack,
                         &debug_execution_stack,
                         &reg_accumulator,
                         &reg_next_expression,
                         &reg_current_env,
                         &reg_current_value_rib,
                         &reg_current_stack);

  tpl_load(tn, TPL_FILE, file_name);

  tpl_unpack(tn, 0);

  nof_strings = tpl_Alen(tn, 1);

  strings = (char **)malloc(nof_strings * sizeof(char *));

  for(i=0;i<nof_strings; i++)
  {
    tpl_unpack(tn, 1);
    strings[i] = strdup(str);
    free(str);
  }

  //heap_ptr = tpl_Alen(tn, 2);
  heap = (unsigned int *)malloc(HEAP_SIZE * sizeof(unsigned int));

  //for(i=0; i<heap_ptr; i++)
  for(i=0; i<HEAP_SIZE; i++)
  {
    tpl_unpack(tn, 2);
    heap[i] = val;
  }

  nof_packages = tpl_Alen(tn, 3);

  packages = (package_t *)malloc(nof_packages * sizeof(package_t));

  for(i=0; i<nof_packages; i++)
  {
    tpl_unpack(tn, 3);
 
    packages[i].name = strdup(package.name);
    free(package.name);
    packages[i].nof_symbols = package.nof_symbols;

    packages[i].symbols = (char **)malloc(package.nof_symbols * sizeof(char *));

    for(j=0; j<package.nof_symbols; j++)
    {
      tpl_unpack(tn, 4);
      packages[i].symbols[j] = strdup(sym);
      free(sym);
    }
   }

  tpl_free(tn);
}
