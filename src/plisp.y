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

%{
#include <stdio.h>
#include <assert.h>

#include "plisp.h"
#include "util.h"

int yyerror(char *s);

extern expression_t *g_expr;

int parens = 0;

int square_brackets = 0;

extern void prompt();

extern FILE *yyin;

extern void print_to_workspace();

extern BOOLEAN console_mode, single_expression_mode;

%}

%union{
  char                   *atom_value;
  int                    integer_value;
  expression_t           *expr_value;
  float                  float_value;
}

%start	expression
%token  <atom_value>             T_SYMBOL
%token  <integer_value>          T_INTEGER
%token  <float_value>            T_FLOAT
%token                           T_LEFT_PAREN 
%token                           T_RIGHT_PAREN
%token                           T_LEFT_SQUARE_BRACKET 
%token                           T_RIGHT_SQUARE_BRACKET
%token                           T_QUOTE
%token  <atom_value>             T_STRING_LITERAL
%token  <atom_value>             T_CHAR
%token                           T_BACKQUOTE
%token                           T_COMMA
%token                           T_COMMA_AT

%token                           END_OF_FILE

%type   <expr_value>             atom
%type   <expr_value>             list
%type   <expr_value>             expressions_in_parens
%type   <expr_value>             quoted_expression
%type   <expr_value>             backquoted_expression
%type   <expr_value>             comma_expression
%type   <expr_value>             comma_at_expression
%type   <expr_value>             array_reference
%type	<expr_value>             expression
%type   <expr_value>             expressions

%%

expression:
    atom 
    {
      if(parens == 0 && square_brackets == 0)
      {
	g_expr = $$;
	YYACCEPT;
      }
    }
    | list
    {
      if(parens == 0 && square_brackets == 0)
      {
	g_expr = $$;
	YYACCEPT;
      }
    };

atom:
    END_OF_FILE { return -1; }
    |
    T_INTEGER
    {
      $$ = create_expression(INTEGER, NULL, $1, 0, 0);
    }
    |
    T_FLOAT
    {
      $$ = create_expression(FLOAT, NULL, 0, $1, 0);
    }
    |
    T_STRING_LITERAL
    {
      $$ = create_expression(STRING_LITERAL, $1, 0, 0, 0);
    }
    |
    T_CHAR
    {
      $$ = create_expression(CHARACTER, $1, 0, 0, 0);
    }
    |
    T_SYMBOL
    {
      $$ = create_expression(SYMBOL, convert_to_upper_case($1), 0, 0, 0);
    };

list:
    expressions_in_parens
    | quoted_expression
    | backquoted_expression
    | comma_expression
    | comma_at_expression
    | array_reference;

expressions_in_parens:
    T_LEFT_PAREN {parens++;} expressions T_RIGHT_PAREN {parens--;}
    {
      $$ = $3;
    };

quoted_expression:
    T_QUOTE {parens++;} expression {parens--;}
    {
      expression_t *e = create_expression(LIST, NULL, 0, 0, 2);

      expression_t *quote = create_expression(SYMBOL, "QUOTE", 0, 0, 0);
      e->elements[0] = quote;

      expression_t *exp = $3;
      e->elements[1] = exp;

      $$ = e;
      
    };

backquoted_expression:
    T_BACKQUOTE {parens++;} expression {parens--;}
    {
      expression_t *e = create_expression(LIST, NULL, 0, 0, 2);

      expression_t *quote = create_expression(SYMBOL, "BACKQUOTE", 0, 0, 0);
      e->elements[0] = quote;

      expression_t *exp = $3;
      e->elements[1] = exp;

      $$ = e;
      
    };

comma_expression:
    T_COMMA {parens++;} expression {parens--;}
    {
      expression_t *e = create_expression(LIST, NULL, 0, 0, 2);

      expression_t *comma = create_expression(SYMBOL, "COMMA", 0, 0, 0);
      e->elements[0] = comma;

      expression_t *exp = $3;
      e->elements[1] = exp;

      $$ = e;
      
    };

comma_at_expression:
    T_COMMA_AT {parens++;} expression {parens--;}
    {
      expression_t *e = create_expression(LIST, NULL, 0, 0, 2);

      expression_t *comma_at = create_expression(SYMBOL, "COMMA-AT", 0, 0, 0);
      e->elements[0] = comma_at;

      expression_t *exp = $3;
      e->elements[1] = exp;

      $$ = e;
   
    };

array_reference:
    T_SYMBOL T_LEFT_SQUARE_BRACKET {square_brackets++;} expression expressions T_RIGHT_SQUARE_BRACKET {square_brackets--;}
    {
      expression_t *exps = $5;

      expression_t *e = create_expression(LIST, NULL, 0, 0, exps->nof_elements + 3);

      expression_t *aref = create_expression(SYMBOL, "AREF", 0, 0, 0);
      e->elements[0] = aref;

      e->elements[1] = create_expression(SYMBOL, convert_to_upper_case($1), 0, 0, 0);
      e->elements[2] = $4;

      int i;

      for(i=0; i<exps->nof_elements; i++)
	e->elements[i+3] = exps->elements[i];

      //we shouldn't call delete_expression(exps) because
      //this will delete the expression_t objects
      //referred to by e too.
      free(exps);

      $$ = e;
      
    };

expressions:
    /* empty */
    {
      $$ = create_expression(LIST, NULL, 0, 0, 0);
    }
    | expressions expression
    {
      $1->nof_elements++;

      $1->elements = (expression_t**)realloc($1->elements, $1->nof_elements * sizeof(expression_t *));

      $1->elements[$1->nof_elements - 1] = $2;

      $$ = $1;
    }
    ;
%%

int yyerror(char *s)
{
  if(!console_mode && !single_expression_mode)
    print_to_workspace("Syntax error in expression");
  else
    fprintf(stdout, "Syntax error in expression\n");

  parens = 0;

  square_brackets = 0;

  return 1;
  //prompt();
  //assert(false);
}


/* int yywrap() */
/* { */
/*   printf("EOF file reached\n"); */

/*   pop_yyin(); */

/*   return 1; */
/* } */

