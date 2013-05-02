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
    yyin = stdin;
    yyrestart(yyin);
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
