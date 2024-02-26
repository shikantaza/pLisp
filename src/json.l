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

%option noyywrap

%{
#include "json_parser.h"
#include "util.h"
%}

%%
[-+]?[0-9]+             { jsonlval.integer_value = atoll(yytext); return T_INTEGER; }
[-+]?[0-9]*\.?[0-9]*    { jsonlval.float_value = atof(yytext); return T_FLOAT; }
\"(\\.|[^\\"])*\"       { jsonlval.string_value = (char *)substring(yytext, 1, strlen(yytext)-2); return T_STRING_LITERAL; }
\{                      return T_LEFT_CURLY_BRACE;
\}                      return T_RIGHT_CURLY_BRACE;
\[                      return T_LEFT_SQUARE_BRACE;
\]                      return T_RIGHT_SQUARE_BRACE;
\,                      return T_COMMA;
\:                      return T_COLON;
[ \t]+                  /* ignore whitespace */
\n                      /* ignore newline */

%%
