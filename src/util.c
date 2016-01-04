/**
  Copyright 2011-2016 Rajesh Jayaprakash <rajesh.jayaprakash@gmail.com>

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
#include <sys/time.h>

char *convert_to_upper_case(char *str)
{
  char *ptr = NULL;

  for(ptr=str;*ptr;ptr++) 
  { 
    *ptr=toupper(*ptr); 
  } 

  return str;
}

char *convert_to_lower_case(char *str)
{
  char *ptr = NULL;

  for(ptr=str;*ptr;ptr++) 
  { 
    *ptr=tolower(*ptr); 
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

//http://stackoverflow.com/questions/122616/how-do-i-trim-leading-trailing-whitespace-in-a-standard-way
size_t trim_whitespace(char *out, size_t len, const char *str)
{
  if(len == 0)
    return 0;

  const char *end;
  size_t out_size;

  // Trim leading space
  while(isspace(*str)) str++;

  if(*str == 0)  // All spaces?
  {
    *out = 0;
    return 1;
  }

  // Trim trailing space
  end = str + strlen(str) - 1;
  while(end > str && isspace(*end)) end--;
  end++;

  // Set output size to minimum of trimmed string length and buffer size minus 1
  out_size = (end - str) < len-1 ? (end - str) : len-1;

  // Copy trimmed string and add null terminator
  memcpy(out, str, out_size);
  out[out_size] = 0;

  return out_size;
}

//http://stackoverflow.com/questions/17432502/how-can-i-measure-cpu-time-and-wall-clock-time-on-both-linux-windows
double get_wall_time()
{
  struct timeval time;
  if (gettimeofday(&time,NULL)){
    //  Handle error
    printf("error in get_wall_time()\n");
    return 0;
  }
  return (double)time.tv_sec + (double)time.tv_usec * .000001;
}

char *conv_to_lower_case_preserve_strings(char *str)
{
  char *ptr = NULL;

  int in_quote = 0;

  for(ptr=str;*ptr;ptr++) 
  {
    if(*ptr == '"')
    {
      if(ptr == str || (ptr != str && *(ptr-1) != '\\'))
	in_quote = !in_quote;
    }
    else
    {
      if(!in_quote)
	*ptr=tolower(*ptr); 
    }
  }

  return str;
}
