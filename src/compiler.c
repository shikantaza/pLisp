/**
  Copyright 2011-2017 Rajesh Jayaprakash <rajesh.jayaprakash@gmail.com>

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

char *default_transcript_text =  "Copyright 2011-2017 Rajesh Jayaprakash <rajesh.jayaprakash@gmail.com>\n\n"
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

    //initialize_tcc();

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
