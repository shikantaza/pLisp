%{
#include "plisp.h"
#include "util.h"
#include "plisp.tab.h"
int yyerror(char *s);
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
%%
