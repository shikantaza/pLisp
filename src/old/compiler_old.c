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
#include <stdint.h>

#include <stdlib.h>
#include <unistd.h>

#include <gtk/gtk.h>

#include "plisp.h"

#include "memory.h"

extern GtkWindow *transcript_window;

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

extern OBJECT_PTR CADR(OBJECT_PTR);
extern OBJECT_PTR CDDR(OBJECT_PTR);
extern OBJECT_PTR CADDR(OBJECT_PTR);
extern OBJECT_PTR CADDDR(OBJECT_PTR);

extern OBJECT_PTR NIL;
extern OBJECT_PTR QUOTE;
extern OBJECT_PTR LAMBDA;
extern OBJECT_PTR IF;
extern OBJECT_PTR SET;
extern OBJECT_PTR CALL_CC;
extern OBJECT_PTR PROGN;
extern OBJECT_PTR LOAD_FILE;

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
extern OBJECT_PTR WHILE;

#ifdef WIN32
extern OBJECT_PTR ERROR1;
#else
extern OBJECT_PTR ERROR;
#endif

extern expression_t *g_expr;

extern OBJECT_PTR reg_accumulator;
extern OBJECT_PTR reg_next_expression;
extern OBJECT_PTR reg_current_env;
extern OBJECT_PTR reg_current_value_rib;
extern OBJECT_PTR reg_current_stack;

extern OBJECT_PTR top_level_env;

extern FILE *yyin;

extern BOOLEAN debug_mode;

OBJECT_PTR debug_execution_stack;

BOOLEAN in_error;

OBJECT_PTR continuations_map;

BOOLEAN core_library_loaded = false;

char *loaded_image_file_name = NULL;

BOOLEAN console_mode = false;
BOOLEAN image_mode = false;
BOOLEAN single_expression_mode = false;
BOOLEAN interpreter_mode = false;
BOOLEAN pipe_mode = false;

BOOLEAN raw_mode = false;

extern unsigned int current_package;

extern OBJECT_PTR CONS_APPLY_NIL;
extern OBJECT_PTR CONS_HALT_NIL;
extern OBJECT_PTR CONS_NIL_NIL;
extern OBJECT_PTR CONS_RETURN_NIL;

char *core_library_file_name = NULL;

extern OBJECT_PTR saved_continuations;
extern OBJECT_PTR continuations_for_return;
extern OBJECT_PTR most_recent_closure;
extern OBJECT_PTR exception_object;
extern OBJECT_PTR exception_handlers;

extern OBJECT_PTR debug_stack;
extern OBJECT_PTR debug_window_dbg_stack;

extern OBJECT_PTR idclo;
extern OBJECT_PTR identity_function(OBJECT_PTR, OBJECT_PTR);

extern void build_autocomplete_words();

extern OBJECT_PTR nth1(OBJECT_PTR, OBJECT_PTR);

OBJECT_PTR compile_loop(OBJECT_PTR args, OBJECT_PTR c, OBJECT_PTR next)
{
  if(args == NIL)
  {
    if(car(next) == RETURN)
      return c;
    else
      return cons(FRAME, 
                       cons(next,
                            cons(c, NIL)));
  }
  else
  {
    OBJECT_PTR temp = compile(car(args), 
                              cons(cons(ARGUMENT, 
                                        cons(c, NIL)),
                                   cdr(c)));

#ifdef WIN32
    if(temp == ERROR1)
#else
    if(temp == ERROR)
#endif
    {
      printf("compile_loop() failed\n");
#ifdef WIN32
      return ERROR1;
#else
      return ERROR;
#endif
    }
    else
      return compile_loop(cdr(args), temp, next);
  }
}

OBJECT_PTR compile(OBJECT_PTR exp, OBJECT_PTR next)
{
  if(IS_SYMBOL_OBJECT(exp))
  {
    return cons(cons(REFER,
                     cons(exp, 
                          cons(next, NIL))),
                exp);
  }

  if(IS_CONS_OBJECT(exp))
  {
    OBJECT_PTR car_obj = car(exp);

    if(car_obj == QUOTE)
      return cons(cons(CONSTANT,
                       cons(CADR(exp), 
                            cons(next, NIL))),
                  exp);

    if(car_obj == BACKQUOTE)
    {

      //basically (FRAME next (CONSTANT cadr(exp) (ARGUMENT (CONSTANT BACKQUOTE (APPLY)))))
      //decorated with exp (cons'd to all the instructions recursively)

      return cons(cons(FRAME, cons(next, cons(cons(cons(CONSTANT, cons(CADR(exp), cons(cons(cons(ARGUMENT, cons(cons(cons(CONSTANT, cons(BACKQUOTE, cons(cons(CONS_APPLY_NIL, NIL), exp))), NIL), exp)), NIL), exp))), NIL), exp))), exp);

    }

    if(car_obj == LAMBDA)
    {
      OBJECT_PTR temp = compile(cons(PROGN, CDDR(exp)), cons(cons(RETURN, NIL), exp));

#ifdef WIN32
      if(temp == ERROR1)
#else
      if(temp == ERROR)
#endif
      {
        printf("Compiling lambda expression failed\n");
#ifdef WIN32
        return ERROR1;
#else
        return ERROR;
#endif
      }
      else
        /* return cons(cons(CLOSE, cons(CADR(exp), cons(temp, cons(next, NIL)))), */
        /*             exp); */
        /* return cons(cons(CLOSE, cons(CADR(exp), cons(temp, cons(CADDR(exp), cons(next, NIL))  ))), */
        /*             exp); */
        return cons(list(5, CLOSE, second(exp), temp, CDDR(exp), next),
                    exp);
      
    }

    if(car_obj == MACRO)
    {
      OBJECT_PTR temp = compile(cons(PROGN, CDDR(exp)), cons(cons(RETURN, NIL), exp));

#ifdef WIN32
      if(temp == ERROR1)
#else
      if(temp == ERROR)
#endif
      {
        printf("Compiling macro expression failed\n");
#ifdef WIN32
        return ERROR1;
#else
        return ERROR;
#endif
      }
      else
        /* return cons(cons(MACRO, cons(CADR(exp), cons(temp, cons(next, NIL)))), */
        /*             exp); */
        /* return cons(cons(MACRO, cons(CADR(exp), cons(temp, cons(CADDR(exp), cons(next, NIL))  ))), */
        /*             exp); */
        return cons(list(5, MACRO, second(exp), temp, CDDR(exp), next),
                    exp);
    }

    if(car_obj == IF)
    {
      OBJECT_PTR thenc = compile(CADDR(exp), next);
#ifdef WIN32
      if(thenc == ERROR1)
#else
      if(thenc == ERROR)
#endif
      {
        printf("Compiling then clause of IF expression failed\n");
#ifdef WIN32
        return ERROR1;
#else
        return ERROR;
#endif
      }

      OBJECT_PTR elsec = compile(CADDDR(exp), next);
#ifdef WIN32
      if(elsec == ERROR1)
#else
      if(elsec == ERROR)
#endif
      {
        printf("Compiling else clause of IF expression failed\n");
#ifdef WIN32
        return ERROR1;
#else
        return ERROR;
#endif
      }

      return compile(CADR(exp), cons(cons(TEST, cons(thenc, cons(elsec, NIL))), exp));
    }

    if(car_obj == WHILE)
    {
      OBJECT_PTR cond = compile(CADR(exp), cons(CONS_HALT_NIL, exp));
#ifdef WIN32
      if(cond == ERROR1)
#else
      if(cond == ERROR)
#endif
      {
        printf("Compiling condition in WHILE failed\n");
#ifdef WIN32
        return ERROR1;
#else
        return ERROR;
#endif
      }

      OBJECT_PTR body = compile(cons(PROGN, CDDR(exp)), 
                                cons(CONS_HALT_NIL, exp));
#ifdef WIN32
      if(body == ERROR1)
#else
      if(body == ERROR)
#endif
      {
        printf("Compiling body of WHILE failed\n");
#ifdef WIN32
        return ERROR1;
#else
        return ERROR;
#endif
      }

      return cons(cons(WHILE, cons(cond, cons(body, cons(next, NIL)))),
                  exp);
    }

    if(car_obj == SET)
      return compile(CADDR(exp), cons(cons(ASSIGN, cons(CADR(exp), cons(next, NIL))), exp));

    if(car_obj == DEFINE)
      return compile(CADDR(exp), cons(cons(DEFINE, cons(CADR(exp), cons(next, NIL))), exp) );

    if(car_obj == CALL_CC)
    {
      OBJECT_PTR temp = compile(CADR(exp), cons(CONS_APPLY_NIL, exp));
#ifdef WIN32
      if(temp == ERROR1)
#else
      if(temp == ERROR)
#endif
      {
        printf("Compiling CALL-CC failed\n");
#ifdef WIN32
        return ERROR1;
#else
        return ERROR;
#endif
      }

      OBJECT_PTR c = cons(CONTI,
                          cons(cons(cons(ARGUMENT, 
                                         cons(temp, 
                                              NIL)),
                                    exp),
                               NIL));

      if(car(next) == RETURN)
        return cons(c,exp);
      else
        return cons(cons(FRAME, cons(next, cons(cons(c, NIL), exp))),
                    exp);
    }

    if(car_obj == PROGN)
      return compile_progn(cdr(exp), next);

    //macros
    if(IS_SYMBOL_OBJECT(car_obj))
    {
      OBJECT_PTR res = get_symbol_value_from_env(car_obj, top_level_env);

      if(car(res) != NIL)
      {
        if(IS_MACRO_OBJECT(cdr(res)))
        {
          reg_next_expression = cons(cons(FRAME,
                                          cons(cons(CONS_HALT_NIL, car_obj),
                                               cons(cons(CONS_APPLY_NIL, car_obj),
                                                    NIL))),
                                     car_obj);

          in_error = false;

          //TODO: check this;
          //existing continuations_map needs
          //to be preserved (if macro 
          //invoked in the middle of an evaluation?
          continuations_map = NIL;

          eval(false);

          if(in_error)
          {
            printf("Compiling macro failed(1)\n");
#ifdef WIN32
            return ERROR1;
#else
            return ERROR;
#endif
          }

          reg_current_value_rib = NIL;

          OBJECT_PTR args = cdr(exp);

          //build the value rib with the unevaluated
          //arguments
          while(args != NIL)
          {
            if(reg_current_value_rib == NIL)
              reg_current_value_rib = cons(car(args), NIL);
            else
            {
              uintptr_t ptr = extract_ptr(last_cell(reg_current_value_rib));
              set_heap(ptr, 1, 
                       cons(car(args), NIL));         
            }
            args = cdr(args);
          }

          //place the macro object in the accumulator (to invoke APPLY)
          reg_accumulator = cdr(res);
          /* reg_next_expression = cons(APPLY, NIL); */
          
          in_error = false;

          //TODO: check this;
          //existing continuations_map needs
          //to be preserved (if macro 
          //invoked in the middle of an evaluation?
          continuations_map = NIL;

          //evaluate the macro invocation
          while(car(reg_next_expression) != NIL)
          {
            eval(false);
            if(in_error)
            {
              printf("Compiling macro failed (2)\n");
#ifdef WIN32
              return ERROR1;
#else
              return ERROR;
#endif
            }
          }

          //compile the output of the macro invocation
          //we return the original source expression
          //that was compiled to facilitate debugging

          //return compile(reg_accumulator, next);
          return cons(car(compile(reg_accumulator, next)), exp);

        } /* end of if(IS_MACRO_OBJECT(cdr(res))) */
      }
    } /* end of if(IS_SYMBOL_OBJECT(car_obj)) */

    //it's an application
    return cons(compile_loop(cdr(exp), compile(car(exp), cons(CONS_APPLY_NIL, exp)), next), exp);
  }

  //it's a constant object (integer, float, char, string, array) 
  return cons(cons(CONSTANT, cons (exp, cons(next, NIL))),
              exp);
}

OBJECT_PTR compile_progn(OBJECT_PTR exps, OBJECT_PTR next)
{
  if(cdr(exps) == NIL)
    return compile(car(exps), next);
  else
    return compile(car(exps), compile_progn(cdr(exps), next));
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

int main(int argc, char **argv)
{
  int opt, i;
  char *expression;

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
      fprintf(stdout, "plisp is invoked. You can create the image file by choosing\n");
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

    initialize_tcc();

    if(load_from_image(loaded_image_file_name))
    {
      printf("Unable to load image from file %s\n", loaded_image_file_name);
      cleanup();
      exit(1);
    }

#ifdef INTERPRETER_MODE
    CONS_NIL_NIL = cons(NIL, NIL);
    CONS_APPLY_NIL = cons(APPLY, NIL);
    CONS_HALT_NIL = cons(HALT, NIL);
    CONS_RETURN_NIL = cons(RETURN, NIL);
#endif

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

  }

  build_autocomplete_words();

  if(!console_mode && !single_expression_mode && !pipe_mode)
  {
//#ifdef WIN32
//    if(build_help_entries("../share/help.json"))
//#else
    if(build_help_entries( PLISPDATADIR "/help.json"))
//#endif
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
#ifdef INTERPRETER_MODE
      repl(1);
#else
      repl2();
#endif
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
#ifdef INTERPRETER_MODE
    repl(1);
#else
    repl2();
#endif
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
#ifdef INTERPRETER_MODE
      repl(1);
#else
      repl2();
#endif
    }
  }
  else
  {
    gtk_main();
  }

  return 0;
}

int repl(int mode)
{
  if(!g_expr)
    return 0;

  if(g_expr->type == LIST && g_expr->elements[0]->type == SYMBOL && !strcmp(g_expr->elements[0]->atom_value,"QUIT"))
  {
    if(console_mode)
    {
      fprintf(stdout, "Bye.\n");
      cleanup();
      exit(0);
    }
    else
      quit_application();
  }
  else if(debug_mode)
  {
    OBJECT_PTR out;
    int val = convert_expression_to_object(g_expr, &out);

    if(val != 0)
      return 1;

    if(!is_permitted_in_debug_mode(out))
    {
      fprintf(stdout, "Expression not permitted in debug mode\n");
      return 1;
    }

    reg_next_expression = compile(out, cons(CONS_HALT_NIL, out));

#ifdef WIN32
    if(reg_next_expression == ERROR1)
#else
    if(reg_next_expression == ERROR)
#endif
    {
      fprintf(stdout, "Compilation of expression failed\n");
      return 1;
    }

    reg_current_env = NIL;

    reg_current_value_rib = NIL;
    reg_current_stack = NIL;

    continuations_map = NIL;

    in_error = false;

    while(car(reg_next_expression) != NIL)
    {
      eval(mode == 1);
      if(in_error)
      {
        fprintf(stdout, "Eval failed\n");
        return 1;
      }
    }

    if(!console_mode)
    {
      if(!in_error)
	print_object(reg_accumulator);
    }
    else
    {
      if(yyin == stdin && !in_error)
	print_object(reg_accumulator);
    }
  }
  else
  {
    reg_accumulator = NIL;

    OBJECT_PTR exp;
    int val = convert_expression_to_object(g_expr, &exp);

    assert(is_valid_object(exp));

    if(val != 0)
      return 1;

    reg_next_expression = compile(exp, cons(CONS_HALT_NIL, exp));

#ifdef WIN32
    if(reg_next_expression == ERROR1)
#else
    if(reg_next_expression == ERROR)
#endif
      return 1;

    reg_current_env = NIL;

    reg_current_value_rib = NIL;
    reg_current_stack = NIL;

    continuations_map = NIL;

    in_error = false;

    //the expression entered at the REPL is
    //converted into a compiled closure
    //and the native code corresponding to
    //the closure is executed. since FRAME
    //instructions are only partly converted
    //to native code, eval() needs to be called
    //to complete the evaluation after
    //the native function call returns

    while(car(reg_next_expression) != NIL)
    {
      eval(mode == 1);
      if(in_error)
        return 1;
    }

    /* char buf[500]; */
    /* memset(buf, 500, '\0'); */

    /* cmpfn fn = compile_closure(create_closure_object(NIL, NIL, reg_next_expression, exp), buf); */

    /* if(!fn) */
    /* { */
    /*   raise_error("Unable to compile expression to native code"); */
    /*   return 1; */
    /* } */
    
    /* if(fn()) */
    /* { */
    /*   char str[100]; */
    /*   char buf1[500]; */

    /*   memset(str, 100, '\0'); */
    /*   memset(buf1, 500, '\0'); */

    /*   print_object_to_string(cdr(car(reg_current_value_rib)), str, 0); */
    /*   sprintf(buf, "%s: %s", get_symbol_name(car(car(reg_current_value_rib))), substring(str,1, strlen(str)-2)); */

    /*   raise_error(buf); */

    /*   return 1; */
    /* } */

    //this is to complete the evaluation left over (popped frame's code)
    /* while(car(reg_next_expression) != NIL) */
    /* { */
    /*   eval(mode == 1); */
    /*   if(in_error) */
    /*     return 1; */
    /* } */

    if((console_mode || single_expression_mode || pipe_mode) && core_library_loaded)
    {
      char buf[500];
      memset(buf, 500, '\0');

      print_object_to_string(reg_accumulator, buf, 0);

      fprintf(stdout, "%s\n", buf);
      fflush(stdout);
    }
    else
    {
      if(!console_mode && !single_expression_mode && !pipe_mode)
      {
	if(!in_error && core_library_loaded && !debug_mode)
	{
	  print_object(reg_accumulator);
	  print_to_transcript("\n");
	}
      }
      else
      {
	if(yyin == stdin && !in_error  && !debug_mode)
	  print_object(reg_accumulator);
      }
    }
    //reset the EXCEPTION-HANDLERS variable
    update_environment(cons(top_level_env, NIL),
                       get_symbol_object("EXCEPTION-HANDLERS"),
                       NIL);

    //if(!debug_mode)
    //  gc(false, true);
  }

  delete_expression(g_expr);
  g_expr = NULL;

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

#ifdef INTERPRETER_MODE

  reg_accumulator       = NIL;

  OBJECT_PTR src = cons(LOAD_FILE, 
//#ifdef WIN32
//                        cons((OBJECT_PTR)get_string_object(core_library_file_name ? core_library_file_name : "./plisp.lisp"),
//#else
                        cons((OBJECT_PTR)get_string_object(core_library_file_name ? core_library_file_name : PLISPDATADIR "/plisp.lisp"),
//#endif
                             NIL));


  OBJECT_PTR temp = compile(src, cons(CONS_HALT_NIL, src));

#ifdef WIN32
  if(temp == ERROR1)
#else
  if(temp == ERROR)
#endif
  {
    fprintf(stdout, "Compile failed while loading core library\n");
    return 1;
  }

  reg_next_expression   = temp;

  reg_current_env = NIL;

  reg_current_value_rib = NIL;
  reg_current_stack     = NIL;

  continuations_map = NIL;

  while(car(reg_next_expression) != NIL)
  {
    eval(false);
    if(in_error)
    {
      fprintf(stdout, "Eval failed while loading core library\n");
      return 1;
    }
  }

  //gc(false, true);

#else

  saved_continuations = NIL;
  continuations_for_return = NIL;
  most_recent_closure = NIL;

  exception_object = NIL;
  exception_handlers = NIL;

  debug_stack = NIL;
  debug_window_dbg_stack = NIL;

  OBJECT_PTR src = cons(LOAD_FILE, 
//#ifdef WIN32
//                        cons((OBJECT_PTR)get_string_object(core_library_file_name ? core_library_file_name : "../lib/plisp_full_monty_compiler_windows.lisp"),
//#else
                        cons((OBJECT_PTR)get_string_object(core_library_file_name ? core_library_file_name : PLISPDATADIR "/plisp_full_monty_compiler.lisp"),
//#endif
                             NIL));

  OBJECT_PTR res = full_monty_eval(src);

  if(in_error)
  {
    handle_exception();
    fprintf(stdout, "Error loading core library\n");
    return 1;
  }

#endif

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
