/**
  Copyright 2011-2024 Rajesh Jayaprakash <rajesh.jayaprakash@pm.me>

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
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "gc.h"
  
#include "json.h"

int yyerror(char *s);

int jsonlex();

extern struct JSONObject *root_obj;

%}

%union{
  char                    *string_value;
  long long               integer_value;
  struct JSONObject       *object_value;
  double                  float_value;
  struct name_value_pairs *pairs;
  struct JSONObject       *array;
  struct JSONArray        *values;
  struct name_value_pair  *pair;
}

%start	root
%token  <integer_value>          T_INTEGER
%token  <float_value>            T_FLOAT
%token  <string_value>           T_STRING_LITERAL
%token                           T_LEFT_CURLY_BRACE 
%token                           T_RIGHT_CURLY_BRACE
%token                           T_LEFT_SQUARE_BRACE
%token                           T_RIGHT_SQUARE_BRACE
%token                           T_COMMA
%token                           T_COLON

%token                           END_OF_FILE

%type   <object_value>           value
%type   <object_value>           object
%type   <object_value>           root
%type   <pairs>                  name_value_pairs
%type   <array>                  array
%type   <values>                 values
%type   <pair>                   name_value_pair

%%

root:
    object
    {
      root_obj = $1;
      YYACCEPT;
    };

object:
    T_LEFT_CURLY_BRACE name_value_pairs T_RIGHT_CURLY_BRACE
    {
      $$ = JSON_create_pairs_object($2);
    };

array:
    T_LEFT_SQUARE_BRACE values T_RIGHT_SQUARE_BRACE
    {
      $$ = JSON_create_array_object($2);
    };

name_value_pairs:
    /* empty */
    {
      $$ = (struct name_value_pairs *)GC_MALLOC(sizeof(struct name_value_pairs));
      $$->count = 0;
    }
    |
    name_value_pair
    {
      $$ = (struct name_value_pairs *)GC_MALLOC(sizeof(struct name_value_pairs));
      $$->count = 1;
      $$->elements = (struct name_value_pair **)GC_MALLOC(sizeof(struct name_value_pair *));
      $$->elements[0] = $1;
    }
    |
    name_value_pairs T_COMMA name_value_pair
    {
      $1->count++;

      $1->elements = (struct name_value_pair **)GC_REALLOC($1->elements,
                                              $1->count * sizeof(struct name_value_pair *));

      $1->elements[$1->count - 1] = $3;

      $$ = $1;    
    };

name_value_pair:
    T_STRING_LITERAL T_COLON value
    {
      $$ = JSON_create_name_value_pair($1, $3);
      //free($1);
    };

value:
    T_STRING_LITERAL 
    {
      $$ = JSON_create_string_object($1);
      //free($1);
    }
    |
    T_INTEGER
    {
      $$ = JSON_create_int_object($1);
    }
    | 
    T_FLOAT
    {
      $$ = JSON_create_float_object($1);
    }
    | 
    object 
    {
      $$ = $1;
    }
    | 
    array
    {
      $$ = $1;
    };

values:
    /* empty */
    {
      $$ = (struct JSONArray *)GC_MALLOC(sizeof(struct JSONArray));
      $$->count = 0;
    }
    |
    value
    {
      $$ = (struct JSONArray *)GC_MALLOC(sizeof(struct JSONArray));
      $$->count = 1;
      $$->elements = (struct JSONObject **)GC_MALLOC(sizeof(struct JSONObject *));
      $$->elements[0] = $1;
    }
    |
    values T_COMMA value
    {
      $1->count++;

      $1->elements = (struct JSONObject **)GC_REALLOC($1->elements,
                                            $1->count * sizeof(struct JSONObject *));

      $1->elements[$1->count - 1] = $3;

      $$ = $1;
    };

%%

int yyerror(char *s)
{
  printf("%s\n", s);
  return 1;
}
