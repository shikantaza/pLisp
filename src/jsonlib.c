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

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "gc.h"

#include "json.h"

extern void jsonparse();

struct JSONObject *root_obj;

struct JSONObject *JSON_create_string_object(char *str)
{
  struct JSONObject *obj = (struct JSONObject *)GC_MALLOC(sizeof(struct JSONObject));
  obj->type = JSON_STRING;
  obj->strvalue = GC_strdup(str);

  return obj;
}

#if __x86_64__
struct JSONObject *JSON_create_int_object(long long ival)
#else
#ifdef __APPLE__
struct JSONObject *JSON_create_int_object(long long ival)
#else
struct JSONObject *JSON_create_int_object(int ival)  
#endif
#endif
{
  struct JSONObject *obj = (struct JSONObject *)GC_MALLOC(sizeof(struct JSONObject));
  obj->type = JSON_INT;
  obj->ivalue = ival;

  return obj;
}

struct JSONObject *JSON_create_float_object(double fval)
{
  struct JSONObject *obj = (struct JSONObject *)GC_MALLOC(sizeof(struct JSONObject));
  obj->type = JSON_FLOAT;
  obj->fvalue = fval;

  return obj;
}

struct JSONObject *JSON_create_pairs_object(struct name_value_pairs *pairs)
{
  struct JSONObject *obj = (struct JSONObject *)GC_MALLOC(sizeof(struct JSONObject));
  obj->type = JSON_NAME_VALUE_PAIRS;
  obj->pairs = pairs;

  return obj;
}

struct name_value_pair *JSON_create_name_value_pair(char *name, struct JSONObject * obj)
{
  struct name_value_pair *pair = (struct name_value_pair *)GC_MALLOC(sizeof(struct name_value_pair));

  pair->name = GC_strdup(name);
  pair->value = obj;

  return pair;
}

struct JSONObject *JSON_create_array_object(struct JSONArray *array)
{
  struct JSONObject *obj = (struct JSONObject *)GC_MALLOC(sizeof(struct JSONObject));
  obj->type = JSON_ARRAY;
  obj->array = array;

  return obj;
}

void JSON_print_object(struct JSONObject *obj)
{
  struct JSONArray *array;
  struct name_value_pairs *pairs;
  int i;

  switch(obj->type)
  {
    case JSON_INT:
      printf("Type: INTEGER; value: %d\n", (int)obj->ivalue);
      break;
    case JSON_FLOAT:
      printf("Type: FLOAT; value: %lf\n", obj->fvalue);
      break;
    case JSON_STRING:
      printf("Type: STRING; value: %s\n", obj->strvalue);
      break;
    case JSON_ARRAY:
      array = obj->array;
      printf("Type: ARRAY; value: ");
      for(i=0; i<array->count; i++)
      {
        JSON_print_object(array->elements[i]);
        if(i != array->count-1) printf(", ");
      }
      printf("\n");
      break;
    case JSON_NAME_VALUE_PAIRS:
      printf("Type: NAME_VALUE_PAIRS; value: ");
      pairs = obj->pairs;
      for(i=0; i<pairs->count; i++)
      {
        printf("%s : ", pairs->elements[i]->name);
        JSON_print_object(pairs->elements[i]->value);
        if(i != pairs->count-1) printf(", ");
      }
      printf("\n");
      break;
  }
}

void JSON_delete_object(struct JSONObject *obj)
{
  /* if(obj->type == JSON_NAME_VALUE_PAIRS) */
  /* { */
  /*   struct name_value_pairs *pairs = obj->pairs; */
  /*   int i; */
  /*   for(i=0; i<pairs->count; i++) */
  /*   { */
  /*     free(pairs->elements[i]->name); */
  /*     JSON_delete_object(pairs->elements[i]->value); */
  /*     free(pairs->elements[i]); */
  /*   } */

  /*   if(pairs->count > 0) */
  /*     free(pairs->elements); */

  /*   free(pairs); */
  /* } */
  /* else if(obj->type == JSON_ARRAY) */
  /* { */
  /*   struct JSONArray *array = obj->array; */
  /*   int i; */
  /*   for(i=0; i<array->count; i++) */
  /*     JSON_delete_object(array->elements[i]); */

  /*   if(array->count > 0) */
  /*     free(array->elements); */

  /*   free(array); */
  /* } */
  /* else if(obj->type == JSON_STRING) */
  /*   free(obj->strvalue); */

  /* free(obj); */
}

struct JSONObject *JSON_get_object_item(struct JSONObject *obj, char *name)
{
  struct name_value_pairs *pairs = obj->pairs;
  int i;
  for(i=0; i<pairs->count; i++)
  {
    if(!strcmp(pairs->elements[i]->name, name))
      return pairs->elements[i]->value;
  }
  return NULL;
}

struct JSONObject *JSON_get_array_item(struct JSONObject *obj, int index)
{
  return obj->array->elements[index];
}

int JSON_get_array_size(struct JSONObject *obj)
{
  return obj->array->count;
}

extern FILE *jsonin;

struct JSONObject *JSON_parse(char *file_name)
{
  jsonin = fopen(file_name, "r");

  if(!jsonin)
  {
    printf("Unable to open file %s\n", file_name);
    return NULL;
  }

  jsonparse();

  fclose(jsonin);

  return root_obj;
}

#ifdef TEST_JSON
int main(int argc, char **argv)
{
  yyin = fopen(argv[1], "r");

  if(!yyin)
  {
    printf("Unable to open file %s\n", argv[1]);
    exit(1);
  }

  yyparse();

  //JSON_print_object(root_obj);

  struct JSONObject *t = JSON_get_object_item(root_obj, "top_level_env");
  
  JSON_print_object(t);

  struct JSONObject *heap = JSON_get_object_item(root_obj, "heap");

  JSON_print_object(JSON_get_array_item(heap,t->ivalue ));

  fclose(yyin);

  JSON_delete_object(root_obj);

  return 0;
}
#endif
