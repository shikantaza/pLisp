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

%option noyywrap

%{
#include "plisp.h"
#include "util.h"
#include "plisp.tab.h"
int yyerror(char *s);

#define MAX_INCLUDE_DEPTH 10
YY_BUFFER_STATE include_stack[MAX_INCLUDE_DEPTH];
int include_stack_ptr = 0;

extern BOOLEAN console_mode;

int call_repl(char *expression)
{
  YY_BUFFER_STATE buf = yy_scan_string(expression);

  while(yyparse() == 0)
  {
#ifdef INTERPRETER_MODE
    if(repl(1))
#else
    if(repl2())
#endif
    {
      yy_delete_buffer(buf);
      return -1;
    }
  }

  yy_delete_buffer(buf);
  return 0;
}

//for handling (load-file ...)
int set_up_new_yyin(FILE *fp)
{
  //allow flex to maintain the buffers, stack, etc. internally
  //yypush_buffer_state(yy_create_buffer(fp, YY_BUF_SIZE));

  if (include_stack_ptr >= MAX_INCLUDE_DEPTH)
  {
    fprintf(stderr, "LOAD-FILEs nested too deeply");
    return 1;
  }

  include_stack[include_stack_ptr] = YY_CURRENT_BUFFER;

  include_stack_ptr++;

  yy_switch_to_buffer(yy_create_buffer(fp, YY_BUF_SIZE));

  return 0;
}

void pop_yyin()
{
  //allow flex to maintain the buffers, stack, etc. internally
  /*
  yypop_buffer_state();

  if(!YY_CURRENT_BUFFER)
    yyin = stdin;
  */

  if(include_stack_ptr == 0)
  {
    if(console_mode)
    {
      yyin = stdin;
      yyrestart(yyin);
    }
  }
  else
  {
    include_stack_ptr--;
    yy_delete_buffer(YY_CURRENT_BUFFER);
    yy_switch_to_buffer(include_stack[include_stack_ptr]);
  }
}

int open_parens = 0;

int open_square_brackets = 0;

%}

%x string

%%

\"                      BEGIN(string);

<string>(\\.|[^\\"])*\"       { yylval.atom_value = substring(yytext, 0, strlen(yytext)-1); BEGIN(INITIAL); return T_STRING_LITERAL; }

[>]|[<]|(<=)|(>=)|[+]|[\-]|[\*]|[\/]|\:[a-zA-Z&][a-zA-Z0-9\-]*|[a-zA-Z&][a-zA-Z0-9\-]*|[a-zA-Z&][a-zA-Z0-9\-]*:[a-zA-Z][a-zA-Z0-9\-]* { yylval.atom_value = strdup(yytext); return T_SYMBOL; }
[-+]?[0-9]+             { yylval.integer_value = atoi(yytext); return T_INTEGER; }
[-+]?[0-9]*\.?[0-9]*    { yylval.float_value = atof(yytext); return T_FLOAT; }

#\\[a-zA-Z0-9!$"'(),_\-./:;?+<=>#%&*@\[\\\]{|}`\^~] { yylval.atom_value = yytext; return T_CHAR; }
\(                      { open_parens++; return T_LEFT_PAREN; }
\)                      { open_parens--; return T_RIGHT_PAREN; }
\[                      { open_square_brackets++; return T_LEFT_SQUARE_BRACKET; }
\]                      { open_square_brackets--; return T_RIGHT_SQUARE_BRACKET; }
\'                      return T_QUOTE;
\`                      return T_BACKQUOTE;
\,                      return T_COMMA;
\,@                     return T_COMMA_AT;

[ \t]+                  /* ignore whitespace */
\n                      /* ignore newlines */
"#|"(.|\n)*"|#"         /* ignore multiline comments */
^;(.)*                  /* ignore single line comments */

"\033[A"                { printf("\n"); } /* ignore up arrow; the printf prevents overwriting the prompt  */
"\033[B"                /* ignore down arrow */
"\033[C"                /* ignore right arrow */
"\033[D"                /* ignore left arrow */

<string><<EOF>>         { 
                          YY_FLUSH_BUFFER; 
                          BEGIN(INITIAL);
                          yyterminate();
                        }

<<EOF>>                 {
                          if(open_parens) {
                            YY_FLUSH_BUFFER;
                            BEGIN(INITIAL);
                            open_parens = 0;
                            open_square_brackets = 0;
                            yyterminate();
                          }
                          else if(open_square_brackets) {
                            YY_FLUSH_BUFFER;
                            BEGIN(INITIAL);
                            open_parens = 0;
                            open_square_brackets = 0;
                            yyterminate();
                          }
                          else
                          {
                            open_parens = 0;
                            open_square_brackets = 0;
                            return END_OF_FILE;
                          }
                        }

%%
