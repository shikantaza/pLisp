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

  include_stack[include_stack_ptr++] = YY_CURRENT_BUFFER;

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

  if(--include_stack_ptr == 0)
  {
#ifndef GUI
    yyin = stdin;
    yyrestart(yyin);
#endif
  }
  else
  {
    yy_delete_buffer(YY_CURRENT_BUFFER);
    yy_switch_to_buffer(include_stack[include_stack_ptr]);
  }
}

%}

%%
[>]|[<]|(<=)|(>=)|[+]|[\-]|[\*]|[\/]|\:[a-zA-Z&][a-zA-Z0-9\-]*|[a-zA-Z&][a-zA-Z0-9\-]*|[a-zA-Z&][a-zA-Z0-9\-]*:[a-zA-Z][a-zA-Z0-9\-]* { yylval.atom_value = strdup(yytext); return T_SYMBOL; }
[-+]?[0-9]+             { yylval.integer_value = atoi(yytext); return T_INTEGER; }
[-+]?[0-9]*\.?[0-9]*    { yylval.float_value = atof(yytext); return T_FLOAT; }
\"(\\.|[^\\"])*\"       { yylval.atom_value = substring(yytext, 1, strlen(yytext)-2); return T_STRING_LITERAL; }
#\\[a-zA-Z0-9!$"'(),_\-./:;?+<=>#%&*@\[\\\]{|}`\^~] { yylval.atom_value = yytext; return T_CHAR; }
\(                      return T_LEFT_PAREN;
\)                      return T_RIGHT_PAREN;
\'                      return T_QUOTE;
\`                      return T_BACKQUOTE;
\,                      return T_COMMA;
\,@                     return T_COMMA_AT;

[ \t]+                  /* ignore whitespace */
\n                      /* ignore newline */
"#|"(.|\n)*"|#"         /* ignore multiline comments */
^;(.)*                  /* ignore single line comments */

"\033[A"                { printf("\n"); } /* ignore up arrow; the printf prevents overwriting the prompt  */
"\033[B"                /* ignore down arrow */
"\033[C"                /* ignore right arrow */
"\033[D"                /* ignore left arrow */

<<EOF>>                 return END_OF_FILE;

%%
