/**
  Copyright 2011-2018 Rajesh Jayaprakash <rajesh.jayaprakash@gmail.com>

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
#include <unistd.h>

#ifdef WIN32
#include <windows.h>
#else
#include <dlfcn.h>
#endif

#include "plisp.h"

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

#ifdef WIN32
char *strndup(const char *s, size_t n)
{
    char *result;
    size_t len = strlen (s);
    if (n < len) len = n;
    result = (char *) GC_MALLOC (len + 1);
    if (!result) return 0;
    result[len] = '\0';
    return (char *) strncpy (result, s, len);
}
#endif

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

//http://stackoverflow.com/questions/3747086/reading-the-whole-text-file-into-a-char-array-in-c
char *get_file_contents(char *file_name)
{
  FILE *fp;
  long lSize;
  char *buffer;

  fp = fopen(file_name, "r" );
  if(!fp)
    return NULL;

  fseek(fp, 0L, SEEK_END);
  lSize = ftell(fp);

  //to handle zero-byte files
  if(!lSize)
  {
    fclose(fp);
    return -1;
  }

  rewind(fp);

  /* allocate memory for entire content */
  //buffer = calloc(1, lSize+1);
  buffer = GC_MALLOC(lSize+1);
  
  if(!buffer)
  {
    fclose(fp);
    return NULL;
  }

  /* copy the file into the buffer */
  size_t bytes_read = fread(buffer, lSize, 1, fp);
  if(bytes_read != 1)
  {
    fclose(fp);
    //free(buffer);
    return NULL;
  }

  fclose(fp);

  return(buffer);
}

char convert_special_char(char c)
{
  switch(c)
  {
    case '\\':
      return '0';
    case '^':
      return '1';
    case '$':
      return '2';
    case '.':
      return '3';
    case '|':
      return '4';
    case '?':
      return '5';
    case '*':
      return '6';
    case '+':
      return '7';
    case '{':
      return '8';
    case '!':
      return '9';
    case '_':
      return 'a';
    case '-':
      return 'b';
    case '/':
      return 'c';
    case '<':
      return 'd';
    case '=':
      return 'e';
    case '>':
      return 'f';
    case '#':
      return 'g';
    case '%':
      return 'h';
    case '&':
      return 'i';
    case '}':
      return 'j';
    case '~':
      return 'k';
    case ':':
      return 'l';
    default:
      return c;
  }
}

#define MAX_IDENTIFIER_LENGTH 100

char *convert_identifier(char *id)
{
  int i;

  int len = strlen(id);

  if(len > MAX_IDENTIFIER_LENGTH)
  {
    printf("Max identifier length exceeded\n");
    return NULL;
  }

  char *s = (char *)GC_MALLOC((MAX_IDENTIFIER_LENGTH + 1) * sizeof(char));

  memset(s, MAX_IDENTIFIER_LENGTH + 1, '\0');

  s[0] = '_';

  for(i=0; i<len; i++)
    s[i+1] = convert_special_char(id[i]);

  s[i+1] = '\0';

  return s;
}

unsigned int file_exists(char *fname)
{
  return access(fname, F_OK) != -1;
}

void *open_library(char *fname)
{
  void *ret;
  
#ifdef WIN32
  unsigned int len = strlen(fname) + 5; //four characters for ".dll"

  char *buf = (char *)GC_MALLOC(len); 
  memset(buf,'\0', len);

  strcat(buf, fname);
  strcat(buf, ".dll");

  ret= LoadLibrary(fname);

  //free(buf);
#else
#ifdef __APPLE__
  unsigned int len = strlen(fname) + 7; //six characters for ".dylib"

  char *buf = (char *)GC_MALLOC(len); 
  memset(buf,'\0', len);

  strcat(buf, fname);
  strcat(buf, ".dylib");

  ret = dlopen(buf, RTLD_LAZY);

  //free(buf);
#else
  unsigned int len = strlen(fname) + 4; //three characters for ".so"

  char *buf = (char *)GC_MALLOC(len); 
  memset(buf,'\0', len);

  strcat(buf, fname);
  strcat(buf, ".so");

  ret = dlopen(buf, RTLD_LAZY);

  //free(buf);  
#endif
#endif
  
  return ret;
}

int extract_package_index(OBJECT_PTR symbol_obj)
{
  return symbol_obj >> (SYMBOL_BITS + OBJECT_SHIFT);
}

int extract_symbol_index(OBJECT_PTR symbol_obj)
{
  //return ((symbol_obj >> OBJECT_SHIFT) << (PACKAGE_BITS + OBJECT_SHIFT)) >> (PACKAGE_BITS + OBJECT_SHIFT);
  return (symbol_obj >> OBJECT_SHIFT) & TWO_RAISED_TO_SYMBOL_BITS_MINUS_1;
}

OBJECT_PTR build_symbol_object(int package_index, int symbol_index)
{
  return (OBJECT_PTR)(((OBJECT_PTR)package_index << (SYMBOL_BITS + OBJECT_SHIFT)) + (symbol_index << OBJECT_SHIFT) + SYMBOL_TAG);
}

unsigned int is_valid_symbol_char(char c)
{
  return (c >= 48 && c <= 57)  || //0-9
         (c >= 65 && c <= 90)  || //A-Z
         (c >= 97 && c <= 122) || //a-z
         c == '\\' || c == '^' || c == '$' || c == '.' || c == '|' || c == '?' || c == '*' || c == '+' || c == '{' || c == '!' ||
         c == '_' ||  c == '-' || c == '/' || c == '<' || c == '=' || c == '>' || c == '#' || c == '%' || c == '&' || c == '}' || c == '~';
}

unsigned int is_valid_symbol_name(char *str)
{
  int len = strlen(str);

  int i;
  
  for(i=0; i<len; i++)
  {
    if(is_valid_symbol_char(str[i]))
      continue;
    else
      return 0;
  }

  return 1;
}
