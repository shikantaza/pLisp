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

#ifndef UTIL_H
#define UTIL_H 

typedef uintptr_t OBJECT_PTR;

char *convert_to_upper_case(char *);
char *convert_to_lower_case(char *);

void log_function_entry(char *);
void log_function_exit(char *);

char *substring(char *, size_t, size_t);

size_t trim_whitespace(char *, size_t, const char *);

double get_wall_time();

char *conv_to_lower_case_preserve_strings(char *);

char *get_file_contents(char *);

char *convert_identifier(char *);

unsigned int file_exists(char *);

void *open_library(char *);

int extract_package_index(OBJECT_PTR);
int extract_symbol_index(OBJECT_PTR);

OBJECT_PTR build_symbol_object(int, int);
#endif
