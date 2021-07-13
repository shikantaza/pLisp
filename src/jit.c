/**
  Copyright 2011-2021 Rajesh Jayaprakash <rajesh.jayaprakash@pm.me>

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

#include "libtcc.h"

#include "jit.h"
#include "plisp.h"
#include "util.h"
#include "memory.h"

#define MAX_C_SOURCE_SIZE 524288

extern OBJECT_PTR NIL;
extern OBJECT_PTR idclo;
extern unsigned int nof_json_native_fns;
extern json_native_fn_src_mapping_t *json_native_fns;
extern unsigned int nof_global_vars;
extern global_var_mapping_t *top_level_symbols;
extern OBJECT_PTR saved_continuations;
extern OBJECT_PTR continuation_to_resume;

extern unsigned int build_fn_prototypes(char *, unsigned int);
extern unsigned int build_c_string(OBJECT_PTR, char *, BOOLEAN);
extern void throw_exception1(char *, char *);
extern BOOLEAN IS_NATIVE_FN_OBJECT(OBJECT_PTR);
extern char *get_native_fn_source(nativefn);
extern OBJECT_PTR identity_function(OBJECT_PTR, OBJECT_PTR);
extern void add_native_fn_source(nativefn, char *);
extern BOOLEAN IS_FUNCTION2_OBJECT(OBJECT_PTR);
extern BOOLEAN IS_MACRO2_OBJECT(OBJECT_PTR);
extern OBJECT_PTR cons_equivalent(OBJECT_PTR);
extern BOOLEAN IS_CONS_OBJECT(OBJECT_PTR);
extern OBJECT_PTR create_closure(unsigned int, BOOLEAN, OBJECT_PTR, ...);
extern OBJECT_PTR convert_native_fn_to_object(nativefn);
extern char *get_json_native_fn_source(OBJECT_PTR);
extern BOOLEAN is_dynamic_memory_object(OBJECT_PTR);


extern OBJECT_PTR nth1(OBJECT_PTR, OBJECT_PTR);
extern void save_continuation(OBJECT_PTR);
extern nativefn extract_native_fn(OBJECT_PTR);
extern OBJECT_PTR create_fn_closure(OBJECT_PTR, nativefn, ...);
extern OBJECT_PTR primitive_add(OBJECT_PTR, ...);
extern OBJECT_PTR primitive_sub(OBJECT_PTR, ...);
extern OBJECT_PTR primitive_car(OBJECT_PTR);
extern OBJECT_PTR primitive_cdr(OBJECT_PTR);
extern OBJECT_PTR cdr(OBJECT_PTR);
extern OBJECT_PTR quote(OBJECT_PTR, OBJECT_PTR);
extern OBJECT_PTR primitive_error(OBJECT_PTR);
extern OBJECT_PTR primitive_lt(OBJECT_PTR, OBJECT_PTR);
extern OBJECT_PTR primitive_gt(OBJECT_PTR, OBJECT_PTR);
extern OBJECT_PTR primitive_leq(OBJECT_PTR, OBJECT_PTR);
extern OBJECT_PTR primitive_geq(OBJECT_PTR, OBJECT_PTR);
extern OBJECT_PTR primitive_if(OBJECT_PTR, OBJECT_PTR, OBJECT_PTR);
extern int in_error_condition();
extern OBJECT_PTR primitive_print(OBJECT_PTR);
extern OBJECT_PTR cons(OBJECT_PTR, OBJECT_PTR);
extern OBJECT_PTR primitive_setcar(OBJECT_PTR, OBJECT_PTR);
extern OBJECT_PTR primitive_setcdr(OBJECT_PTR, OBJECT_PTR);
extern OBJECT_PTR primitive_list(OBJECT_PTR, ...);
extern OBJECT_PTR primitive_mult(OBJECT_PTR, ...);
extern OBJECT_PTR primitive_div(OBJECT_PTR, ...);
extern OBJECT_PTR primitive_equal(OBJECT_PTR);
extern OBJECT_PTR primitive_concat(OBJECT_PTR, ...);
extern OBJECT_PTR primitive_not(OBJECT_PTR);
extern OBJECT_PTR gensym();

extern OBJECT_PTR primitive_atom(OBJECT_PTR);
extern OBJECT_PTR prim_symbol_value(OBJECT_PTR);
extern OBJECT_PTR primitive_apply(OBJECT_PTR);
extern OBJECT_PTR primitive_symbol(OBJECT_PTR);
extern OBJECT_PTR prim_symbol_name(OBJECT_PTR);
extern OBJECT_PTR primitive_format(OBJECT_PTR, OBJECT_PTR, OBJECT_PTR, ...);
extern OBJECT_PTR primitive_clone(OBJECT_PTR);
extern OBJECT_PTR primitive_unbind(OBJECT_PTR);
extern OBJECT_PTR primitive_newline(OBJECT_PTR);

extern OBJECT_PTR primitive_consp(OBJECT_PTR);
extern OBJECT_PTR primitive_listp(OBJECT_PTR);
extern OBJECT_PTR primitive_integerp(OBJECT_PTR);
extern OBJECT_PTR primitive_floatp(OBJECT_PTR);
extern OBJECT_PTR prim_characterp(OBJECT_PTR);
extern OBJECT_PTR primitive_symbolp(OBJECT_PTR);
extern OBJECT_PTR primitive_stringp(OBJECT_PTR);
extern OBJECT_PTR primitive_arrayp(OBJECT_PTR);
extern OBJECT_PTR primitive_closurep(OBJECT_PTR);
extern OBJECT_PTR primitive_macrop(OBJECT_PTR);
extern OBJECT_PTR primitive_contp(OBJECT_PTR);

extern OBJECT_PTR primitive_string(OBJECT_PTR);
extern OBJECT_PTR prim_make_array(OBJECT_PTR, ...);
extern OBJECT_PTR prim_array_set(OBJECT_PTR, OBJECT_PTR, OBJECT_PTR);
extern OBJECT_PTR prim_array_get(OBJECT_PTR, OBJECT_PTR);
extern OBJECT_PTR prim_sub_array(OBJECT_PTR, OBJECT_PTR, OBJECT_PTR);
extern OBJECT_PTR prim_array_length(OBJECT_PTR);
extern OBJECT_PTR prim_print_string(OBJECT_PTR);

extern OBJECT_PTR prim_load_fgn_lib(OBJECT_PTR);
extern OBJECT_PTR prim_call_fgn_func(OBJECT_PTR, OBJECT_PTR, OBJECT_PTR);

extern OBJECT_PTR prim_create_pkg(OBJECT_PTR);
extern OBJECT_PTR prim_in_package(OBJECT_PTR);
extern OBJECT_PTR prim_export_pkg(OBJECT_PTR, OBJECT_PTR);
extern OBJECT_PTR prim_import_package(OBJECT_PTR);

extern OBJECT_PTR prim_create_image(OBJECT_PTR);
extern OBJECT_PTR prim_serialize(OBJECT_PTR, OBJECT_PTR);
extern OBJECT_PTR prim_deserialize(OBJECT_PTR);

extern OBJECT_PTR prim_load_file(OBJECT_PTR);

extern OBJECT_PTR primitive_time(OBJECT_PTR);
extern OBJECT_PTR primitive_profile(OBJECT_PTR);

extern OBJECT_PTR primitive_env(OBJECT_PTR);
extern OBJECT_PTR prim_expand_macro(OBJECT_PTR);

extern OBJECT_PTR full_monty_eval(OBJECT_PTR);

extern OBJECT_PTR convert_int_to_object_for_full_monty(int);
extern OBJECT_PTR conv_float_to_obj_for_fm(unsigned long long);

extern void set_most_recent_closure(OBJECT_PTR);
extern OBJECT_PTR get_continuation(OBJECT_PTR);

extern OBJECT_PTR handle_exception();
extern OBJECT_PTR add_exception_handler(OBJECT_PTR);
extern OBJECT_PTR get_exception_handler();
extern OBJECT_PTR primitive_throw(OBJECT_PTR);

extern void disable_exception_handlers();
extern void enable_exception_handlers();

extern void prim_inspect_object(OBJECT_PTR);

extern void push_into_debug_stack(OBJECT_PTR);
extern OBJECT_PTR save_cont_to_resume(OBJECT_PTR);

extern OBJECT_PTR step(OBJECT_PT);

nativefn get_function(void *state, const char *fname)
{
  return (nativefn)tcc_get_symbol((TCCState *)state, fname);
}

TCCState *create_tcc_state1()
{
  TCCState *tcc_state = tcc_new();

  if (!tcc_state)
  {
    fprintf(stderr, "Could not create tcc state\n");
    return NULL;
  }

  tcc_set_output_type(tcc_state, TCC_OUTPUT_MEMORY);

  tcc_add_symbol(tcc_state, "nth1",               nth1);
  tcc_add_symbol(tcc_state, "save_continuation",  save_continuation);
  tcc_add_symbol(tcc_state, "extract_native_fn",  extract_native_fn);
  //tcc_add_symbol(tcc_state, "call_cc1",           call_cc1);
  tcc_add_symbol(tcc_state, "create_fn_closure",  create_fn_closure);
  tcc_add_symbol(tcc_state, "primitive_add",      primitive_add);
  tcc_add_symbol(tcc_state, "primitive_sub",      primitive_sub);
  tcc_add_symbol(tcc_state, "primitive_car",      primitive_car);
  tcc_add_symbol(tcc_state, "primitive_cdr",      primitive_cdr);
  tcc_add_symbol(tcc_state, "cdr",                cdr);
  tcc_add_symbol(tcc_state, "quote",              quote);
  tcc_add_symbol(tcc_state, "primitive_error",    primitive_error);
  tcc_add_symbol(tcc_state, "primitive_lt",       primitive_lt);
  tcc_add_symbol(tcc_state, "primitive_gt",       primitive_gt);
  tcc_add_symbol(tcc_state, "primitive_leq",      primitive_leq);
  tcc_add_symbol(tcc_state, "primitive_geq",      primitive_geq);
  tcc_add_symbol(tcc_state, "primitive_if",       primitive_if);
  tcc_add_symbol(tcc_state, "in_error_condition", in_error_condition);
  tcc_add_symbol(tcc_state, "primitive_print",    primitive_print);
  tcc_add_symbol(tcc_state, "cons",               cons);
  tcc_add_symbol(tcc_state, "primitive_setcar",   primitive_setcar);
  tcc_add_symbol(tcc_state, "primitive_setcdr",   primitive_setcdr);
  tcc_add_symbol(tcc_state, "primitive_list",     primitive_list);
  tcc_add_symbol(tcc_state, "primitive_mult",     primitive_mult);
  tcc_add_symbol(tcc_state, "primitive_div",      primitive_div);
  tcc_add_symbol(tcc_state, "primitive_equal",    primitive_equal);
  tcc_add_symbol(tcc_state, "primitive_concat",   primitive_concat);

  tcc_add_symbol(tcc_state, "primitive_not",      primitive_not);

  tcc_add_symbol(tcc_state, "gensym",             gensym);
  tcc_add_symbol(tcc_state, "primitive_atom",     primitive_atom);
  tcc_add_symbol(tcc_state, "prim_symbol_value",  prim_symbol_value);
  tcc_add_symbol(tcc_state, "primitive_apply",    primitive_apply);
  tcc_add_symbol(tcc_state, "primitive_symbol",   primitive_symbol);
  tcc_add_symbol(tcc_state, "prim_symbol_name",   prim_symbol_name);
  tcc_add_symbol(tcc_state, "primitive_format",   primitive_format);
  tcc_add_symbol(tcc_state, "primitive_clone",    primitive_clone);
  tcc_add_symbol(tcc_state, "primitive_unbind",   primitive_unbind);
  tcc_add_symbol(tcc_state, "primitive_newline",  primitive_newline);

  tcc_add_symbol(tcc_state, "primitive_consp",    primitive_consp);
  tcc_add_symbol(tcc_state, "primitive_listp",    primitive_listp);
  tcc_add_symbol(tcc_state, "primitive_integerp", primitive_integerp);
  tcc_add_symbol(tcc_state, "primitive_floatp",   primitive_floatp);
  tcc_add_symbol(tcc_state, "prim_characterp",    prim_characterp);
  tcc_add_symbol(tcc_state, "primitive_symbolp",  primitive_symbolp);
  tcc_add_symbol(tcc_state, "primitive_stringp",  primitive_stringp);
  tcc_add_symbol(tcc_state, "primitive_arrayp",   primitive_arrayp);
  tcc_add_symbol(tcc_state, "primitive_closurep", primitive_closurep);
  tcc_add_symbol(tcc_state, "primitive_macrop",   primitive_macrop);
  tcc_add_symbol(tcc_state, "primitive_contp",    primitive_contp);

  tcc_add_symbol(tcc_state, "primitive_string",   primitive_string);
  tcc_add_symbol(tcc_state, "prim_make_array",    prim_make_array);
  tcc_add_symbol(tcc_state, "prim_array_set",     prim_array_set);
  tcc_add_symbol(tcc_state, "prim_array_get",     prim_array_get);
  tcc_add_symbol(tcc_state, "prim_sub_array",     prim_sub_array);
  tcc_add_symbol(tcc_state, "prim_array_length",  prim_array_length);
  tcc_add_symbol(tcc_state, "prim_print_string",  prim_print_string);

  tcc_add_symbol(tcc_state, "prim_load_fgn_lib",  prim_load_fgn_lib);
  tcc_add_symbol(tcc_state, "prim_call_fgn_func", prim_call_fgn_func);

  tcc_add_symbol(tcc_state, "prim_create_pkg",        prim_create_pkg);
  tcc_add_symbol(tcc_state, "prim_in_package",        prim_in_package);
  tcc_add_symbol(tcc_state, "prim_export_pkg",        prim_export_pkg);
  tcc_add_symbol(tcc_state, "prim_import_package",    prim_import_package);

  tcc_add_symbol(tcc_state, "prim_create_image",  prim_create_image);
  tcc_add_symbol(tcc_state, "prim_serialize",     prim_serialize);
  tcc_add_symbol(tcc_state, "prim_deserialize",   prim_deserialize);

  tcc_add_symbol(tcc_state, "prim_load_file",     prim_load_file);
  
  tcc_add_symbol(tcc_state, "primitive_time",     primitive_time);

  tcc_add_symbol(tcc_state, "primitive_profile",  primitive_profile);

  tcc_add_symbol(tcc_state, "primitive_env",      primitive_env);
  tcc_add_symbol(tcc_state, "prim_expand_macro",  prim_expand_macro);

  tcc_add_symbol(tcc_state, "primitive_eval",     full_monty_eval);

  tcc_add_symbol(tcc_state, "convert_int_to_object",   convert_int_to_object);
  tcc_add_symbol(tcc_state, "conv_float_to_obj_for_fm", conv_float_to_obj_for_fm);

  tcc_add_symbol(tcc_state, "set_most_recent_closure",  set_most_recent_closure);
  tcc_add_symbol(tcc_state, "get_continuation",         get_continuation);

  tcc_add_symbol(tcc_state, "handle_exception",         handle_exception);
  tcc_add_symbol(tcc_state, "add_exception_handler",    add_exception_handler);
  tcc_add_symbol(tcc_state, "get_exception_handler",    get_exception_handler);
  tcc_add_symbol(tcc_state, "primitive_throw",          primitive_throw);

  tcc_add_symbol(tcc_state, "push_into_debug_stack",    push_into_debug_stack);

  tcc_add_symbol(tcc_state, "save_cont_to_resume",      save_cont_to_resume);

  tcc_add_symbol(tcc_state, "disable_exception_handlers", disable_exception_handlers);
  tcc_add_symbol(tcc_state, "enable_exception_handlers",  enable_exception_handlers);

  tcc_add_symbol(tcc_state, "prim_inspect_object",      prim_inspect_object);

  tcc_add_symbol(tcc_state, "step",                     step);
  
  /* tcc_add_symbol(tcc_state, "insert_node",              insert_node); */
  /* tcc_add_symbol(tcc_state, "gc",                       gc); */
  /* tcc_add_symbol(tcc_state, "is_dynamic_memory_object", is_dynamic_memory_object); */

  //uncomment for debugging
  //tcc_add_symbol(tcc_state, "print_object",             print_object);

  return tcc_state;
}

void *compile_functions_from_string(const char *str)
{
  TCCState *tcc_state1 = create_tcc_state1();
  assert(tcc_state1);

  /* FILE *out = fopen("debug.c", "a"); */
  /* fprintf(out, "%s\n", str); */
  /* fclose(out); */

  if(tcc_compile_string(tcc_state1, str) == -1)
    //assert(false);
  {
    throw_exception1("COMPILE-ERROR", "Error compiling expression to native code (1)");
    return NULL;
  }

  if(tcc_relocate(tcc_state1, TCC_RELOCATE_AUTO) < 0)
    //assert(false);
  {
    throw_exception1("COMPILE-ERROR", "Error compiling expression to native code (2)");
    return NULL;
  }

  return (void *)tcc_state1;
  
}
