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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char *convert_to_upper_case(char *str)
{
  char *ptr = NULL;

  for(ptr=str;*ptr;ptr++) 
  { 
    *ptr=toupper(*ptr); 
  } 

  return str;
}

void log_function_entry(char *fn)
{
#ifdef DEBUG
  fprintf(stdout,"Entering %s()\n", fn);
#endif
}

void log_function_exit(char *fn)
{
#ifdef DEBUG
  fprintf(stdout,"Exiting %s()\n", fn);
#endif
}

char* substring(const char* str, size_t begin, size_t len) 
{ 
  if (str == 0 || strlen(str) == 0 || strlen(str) < begin || strlen(str) < (begin+len)) 
    return 0; 

  return strndup(str + begin, len); 
}
