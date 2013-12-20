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
#include <assert.h>
#include <dlfcn.h>

#include "plisp.h"
#include "memory.h"

#include "cJSON/cJSON.h"

#include "queue.h"
#include "hashtable.h"

extern OBJECT_PTR top_level_env;
extern unsigned int current_package;
extern unsigned int nof_packages;
extern package_t *packages;
extern int gen_sym_count;

extern int nof_strings;
extern char **strings;

extern BOOLEAN debug_mode;
extern OBJECT_PTR debug_continuation;
extern OBJECT_PTR debug_env;
extern OBJECT_PTR debug_execution_stack;
extern OBJECT_PTR reg_accumulator;
extern OBJECT_PTR reg_next_expression;
extern OBJECT_PTR reg_current_env;
extern OBJECT_PTR reg_current_value_rib;
extern OBJECT_PTR reg_current_stack;

extern OBJECT_PTR NIL;

extern int nof_dl_handles;
extern char *foreign_library_names[];
extern void **dl_handles;

extern unsigned int POINTER_MASK;

//forward declarations
BOOLEAN is_dynamic_reference(unsigned int);
void add_to_deserialization_queue(cJSON *, queue_t *, unsigned int, uintptr_t, unsigned int);
OBJECT_PTR deserialize(cJSON *, unsigned int, hashtable_t *, queue_t *);
void convert_heap(cJSON *, hashtable_t *, queue_t *);

struct slot
{
  unsigned int ref;
  OBJECT_PTR ptr;
  unsigned int index;
};

void add_obj_to_print_list(queue_t *print_queue, OBJECT_PTR obj, hashtable_t *printed_objects)
{
  //this search is O(n), but this is OK because
  //the queue keeps growing and shrinking, so its
  //size at any point in time is quite small (<10)
  if(queue_item_exists(print_queue, (void *)obj) || hashtable_get(printed_objects, (void *)obj))
    return;

  assert(is_dynamic_memory_object(obj));
  queue_enqueue(print_queue, (void *)obj);
}

void print_json_object(FILE *fp, OBJECT_PTR obj, queue_t *print_queue, unsigned int *obj_count, hashtable_t *hashtable, hashtable_t *printed_objects)
{
  if(!is_valid_object(obj))
    assert(false);

  if(is_dynamic_memory_object(obj))
  {
    hashtable_entry_t *e = hashtable_get(hashtable, (void *)obj);

    if(e)
      fprintf(fp, "%d", (int)e->value);
    else
    {
      fprintf(fp, "%d",  ((*obj_count) << OBJECT_SHIFT) + (obj & BIT_MASK));
      hashtable_put(hashtable, (void *)obj, (void *)  ((*obj_count) << OBJECT_SHIFT) + (obj & BIT_MASK) );
      (*obj_count)++;
    }

    add_obj_to_print_list(print_queue, obj, printed_objects);
  }
  else
    fprintf(fp, "%d", (unsigned int)obj);
}

void print_heap_representation(FILE *fp, 
                               OBJECT_PTR obj, 
                               queue_t *print_queue, 
                               unsigned int *obj_count, 
                               hashtable_t *hashtable, 
                               hashtable_t *printed_objects)
{
  if(!is_dynamic_memory_object(obj))
  {
    printf("%d\n", (int)obj);
    assert(false);
  }

  if(IS_CONS_OBJECT(obj))
  {
    OBJECT_PTR car_obj = car(obj);
    OBJECT_PTR cdr_obj = cdr(obj);

    fprintf(fp, "[");
    print_json_object(fp, car_obj, print_queue, obj_count, hashtable, printed_objects);
    fprintf(fp, ", ");
    print_json_object(fp, cdr_obj, print_queue, obj_count, hashtable, printed_objects);
    fprintf(fp, "] ");
  }
  else if(IS_ARRAY_OBJECT(obj))
  {
    fprintf(fp, "[ ");

    int len = *((unsigned int *)(obj & POINTER_MASK));

    int i;

    for(i=1; i<=len; i++)
    {
      print_json_object(fp, get_heap(obj & POINTER_MASK, i), print_queue, obj_count, hashtable, printed_objects);

      if(i != len)     fprintf(fp, ", ");
      else             fprintf(fp, " ");
    }

    fprintf(fp, "] ");
  }
  else if(IS_CLOSURE_OBJECT(obj) || IS_MACRO_OBJECT(obj))
  {
    OBJECT_PTR env    = get_env_list(obj);
    OBJECT_PTR params = get_params_object(obj);
    OBJECT_PTR body   = get_body_object(obj);
    OBJECT_PTR source = get_source_object(obj);

    fprintf(fp, "[");
    print_json_object(fp, env, print_queue, obj_count, hashtable, printed_objects);
    fprintf(fp, ", ");
    print_json_object(fp, params, print_queue, obj_count, hashtable, printed_objects);
    fprintf(fp, ", ");
    print_json_object(fp, body, print_queue, obj_count, hashtable, printed_objects);
    fprintf(fp, ", ");
    print_json_object(fp, source, print_queue, obj_count, hashtable, printed_objects);
    fprintf(fp, "] ");
  }
  else if(IS_CONTINUATION_OBJECT(obj))
  {
    print_json_object(fp, get_heap(obj & POINTER_MASK, 0), print_queue, obj_count, hashtable, printed_objects);
  }
  else if(IS_INTEGER_OBJECT(obj))
    fprintf(fp, "%d", get_int_value(obj));
  else if(IS_FLOAT_OBJECT(obj))
    fprintf(fp, "%f", get_float_value(obj));
  else
    assert(false);

  hashtable_put(printed_objects, (void *)obj, (void *)1);
}

//#ifdef DEBUG
void doit(char *text)
{
  char *out;cJSON *json;
	
  json=cJSON_Parse(text);
  if (!json) {printf("Error before: [%s]\n",cJSON_GetErrorPtr()); getchar();}
  else
  {
    out=cJSON_Print(json);
    cJSON_Delete(json);
    printf("%s\n",out);
    free(out);
  }
}

/* Read a file, parse, render back, etc. */
void dofile(char *filename)
{
  FILE *f=fopen(filename,"rb");fseek(f,0,SEEK_END);long len=ftell(f);fseek(f,0,SEEK_SET);
  char *data=(char*)malloc(len+1);fread(data,1,len,f);fclose(f);
  doit(data);
  free(data);
}
//#endif

void create_image(char *file_name)
{
  int i;

  FILE *fp = fopen(file_name, "w");  

  unsigned int *obj_count = (unsigned int *)malloc(sizeof(unsigned int));

  *obj_count = 0;

  queue_t *print_queue = queue_create();
  hashtable_t *hashtable = hashtable_create(1000001);
  hashtable_t *printed_objects = hashtable_create(1000001);

  fprintf(fp, "{ ");

  fprintf(fp, "\"debug_mode\" : \"%s\"",       debug_mode ? "true" : "false");                                             fprintf(fp, ", ");

  fprintf(fp, "\"top_level_env\" : "        ); print_json_object(fp, top_level_env,        print_queue, obj_count, hashtable, printed_objects); fprintf(fp, ", ");

  fprintf(fp, "\"debug_continuation\" : "   ); print_json_object(fp,debug_continuation,    print_queue, obj_count, hashtable, printed_objects); fprintf(fp, ", ");
  fprintf(fp, "\"debug_env\" : "            ); print_json_object(fp,debug_env,             print_queue, obj_count, hashtable, printed_objects); fprintf(fp, ", ");
  fprintf(fp, "\"debug_execution_stack\" : "); print_json_object(fp,debug_execution_stack, print_queue, obj_count, hashtable, printed_objects); fprintf(fp, ", ");
  fprintf(fp, "\"reg_accumulator\" : "      ); print_json_object(fp,reg_accumulator,       print_queue, obj_count, hashtable, printed_objects); fprintf(fp, ", ");
  fprintf(fp, "\"reg_next_expression\" : "  ); print_json_object(fp,reg_next_expression,   print_queue, obj_count, hashtable, printed_objects); fprintf(fp, ", ");
  fprintf(fp, "\"reg_current_env\" : "      ); print_json_object(fp,reg_current_env,       print_queue, obj_count, hashtable, printed_objects); fprintf(fp, ", ");
  fprintf(fp, "\"reg_current_value_rib\" : "); print_json_object(fp,reg_current_value_rib, print_queue, obj_count, hashtable, printed_objects); fprintf(fp, ", ");
  fprintf(fp, "\"reg_current_stack\" : "    ); print_json_object(fp,reg_current_stack,     print_queue, obj_count, hashtable, printed_objects); fprintf(fp, ", ");

  fprintf(fp, "\"current_package\" : %d ",     current_package);                                                           fprintf(fp, ", ");
  fprintf(fp, "\"gen_sym_count\" : %d ",       gen_sym_count);                                                             fprintf(fp, ", ");

  fprintf(fp,               "\"strings\" : [ ");
  for(i=0; i<nof_strings; i++)
    fprintf(fp, "\"%s\"%s", strings[i], (i == (nof_strings-1)) ? " " : ", ");
  fprintf(fp,               "]");
  fprintf(fp, ", ");

  fprintf(fp,               "\"foreign_libraries\" : [ ");
  for(i=0; i<nof_dl_handles; i++)
    fprintf(fp, "\"%s\"%s", foreign_library_names[i], (i == (nof_dl_handles-1)) ? " " : ", ");
  fprintf(fp,               "]");
  fprintf(fp, ", ");

  fprintf(fp,               "\"packages\" : [ ");

  for(i=0; i<nof_packages; i++)
  {
    fprintf(fp, "{\"name\" : \"%s\",", packages[i].name);
    fprintf(fp, "\"symbols\" : ");

    fprintf(fp, "[ ");
    int j;
    for(j=0; j<packages[i].nof_symbols; j++)
      fprintf(fp, "\"%s\"%s", packages[i].symbols[j], (j == (packages[i].nof_symbols-1)) ? " " : ", ");
    fprintf(fp, "]");

    fprintf(fp, "}");

    if(i != nof_packages-1)     fprintf(fp, ", ");
    else                        fprintf(fp, " ");
  }

  fprintf(fp, "]");

  fprintf(fp, ", ");

  fprintf(fp, "\"heap\" : [");

  while(!queue_is_empty(print_queue))
  {
    OBJECT_PTR obj = (OBJECT_PTR)((queue_dequeue(print_queue))->data);
    assert(is_dynamic_memory_object(obj));
    print_heap_representation(fp, obj, print_queue, obj_count, hashtable, printed_objects);
    if(!queue_is_empty(print_queue))fprintf(fp, ", ");
  }

  queue_delete(print_queue);
  hashtable_delete(hashtable);
  hashtable_delete(printed_objects);

  fprintf(fp, "]}");

  fclose(fp);

  free(obj_count);

  //#ifdef DEBUG
  //dofile(file_name);
  //#endif

}

/* OBJECT_PTR convert_to_plisp_obj(cJSON *root, cJSON *heap, cJSON * obj, hashtable_t *hashtable) */
/* { */
/*   int str_type = obj->valueint & BIT_MASK; */

/*   int i; */

/*   OBJECT_PTR ret; */

/*   if(str_type < 1 || str_type > 10) */
/*     assert(false); */

/*   hashtable_entry_t *e = hashtable_get(hashtable, (void *)(obj->valueint)); */

/*   if(str_type == SYMBOL_TAG         || */
/*      str_type == STRING_LITERAL_TAG || */
/*      str_type == CHAR_TAG) */
/*     return (OBJECT_PTR)(obj->valueint); */

/*   if(e) */
/*   { */
/*     assert(is_valid_object((OBJECT_PTR)(e->value))); */
/*     return (OBJECT_PTR)(e->value); */
/*   } */

/*   cJSON *value = cJSON_GetArrayItem(heap, obj->valueint >> OBJECT_SHIFT); */

/*   if(str_type == CONS_TAG) */
/*   { */
/*     uintptr_t ptr = object_alloc(2, CONS_TAG); */

/*     set_heap(ptr, 0, convert_to_plisp_obj(root, heap, cJSON_GetArrayItem(value, 0), hashtable)); */
/*     set_heap(ptr, 1, convert_to_plisp_obj(root, heap, cJSON_GetArrayItem(value, 1), hashtable)); */

/*     ret = ptr + CONS_TAG; */
/*   } */
/*   else if(str_type == ARRAY_TAG) */
/*   { */
/*     int size = cJSON_GetArraySize(value); */
    
/*     uintptr_t ptr = object_alloc(size + 1, ARRAY_TAG); */

/*     *((unsigned int *)(ptr & POINTER_MASK)) = size; */

/*     for(i=0; i<size; i++) */
/*       set_heap(ptr, i+1, convert_to_plisp_obj(root, heap, cJSON_GetArrayItem(value, i), hashtable)); */

/*     ret = ptr + ARRAY_TAG; */
/*   } */
/*   else if(str_type == CLOSURE_TAG || str_type == MACRO_TAG) */
/*   { */
/*     uintptr_t ptr = object_alloc(4, str_type); */

/*     set_heap(ptr, 0, convert_to_plisp_obj(root, heap, cJSON_GetArrayItem(value, 0), hashtable)); */
/*     set_heap(ptr, 1, convert_to_plisp_obj(root, heap, cJSON_GetArrayItem(value, 1), hashtable)); */
/*     set_heap(ptr, 2, convert_to_plisp_obj(root, heap, cJSON_GetArrayItem(value, 2), hashtable)); */
/*     set_heap(ptr, 3, convert_to_plisp_obj(root, heap, cJSON_GetArrayItem(value, 3), hashtable)); */

/*     ret = ptr + str_type; */
/*   } */
/*   else if(str_type == CONTINUATION_TAG) */
/*   { */
/*     uintptr_t ptr = object_alloc(1, CONTINUATION_TAG); */
/*     set_heap(ptr, 0, convert_to_plisp_obj(root, heap, value, hashtable)); */
/*     ret = ptr + CONTINUATION_TAG; */
/*   } */
/*   else if(str_type == INTEGER_TAG) */
/*   { */
/*     ret = convert_int_to_object(value->valueint); */
/*   } */
/*   else if(str_type == FLOAT_TAG) */
/*   { */
/*     ret = convert_float_to_object((float)value->valuedouble); */
/*   } */
/*   else */
/*   { */
/*     printf("%d\n", str_type); */
/*     assert(false); */
/*   } */

/*   if(!is_valid_object(ret)) */
/*   { */
/*     printf("%d\n", ret); */
/*     assert(false); */
/*   } */

/*   hashtable_put(hashtable, (void *)(obj->valueint),(void *)ret); */

/*   return ret; */
/* } */

void load_from_image(char *file_name)
{
  int i, j;

  FILE *f=fopen(file_name,"rb");fseek(f,0,SEEK_END);long len=ftell(f);fseek(f,0,SEEK_SET);
  char *data=(char*)malloc(len+1);fread(data,1,len,f);fclose(f);

  cJSON *root = cJSON_Parse(data);
  cJSON *heap = cJSON_GetObjectItem(root, "heap");

  free(data);

  cJSON *temp;

  temp = cJSON_GetObjectItem(root, "current_package");
  current_package = temp->valueint;

  temp = cJSON_GetObjectItem(root, "gen_sym_count");
  gen_sym_count = temp->valueint;

  temp = cJSON_GetObjectItem(root, "strings");
  nof_strings = cJSON_GetArraySize(temp);

  strings = (char **)malloc(nof_strings * sizeof(char *));

  for(i=0; i<nof_strings; i++)
  {
    cJSON *strobj = cJSON_GetArrayItem(temp, i);
    strings[i] = strdup(strobj->valuestring);
  }

  temp = cJSON_GetObjectItem(root, "foreign_libraries");
  nof_dl_handles = cJSON_GetArraySize(temp);

  dl_handles = (void **)malloc(nof_dl_handles * sizeof(void *));

  for(i=0; i<nof_dl_handles; i++)
  {
    cJSON *strobj = cJSON_GetArrayItem(temp, i);
    foreign_library_names[i] = strdup(strobj->valuestring);
    dl_handles[i] = dlopen(strobj->valuestring, RTLD_LAZY);
  }

  temp = cJSON_GetObjectItem(root, "packages");
  nof_packages = cJSON_GetArraySize(temp);

  packages = (package_t *)malloc(nof_packages * sizeof(package_t));

  for(i=0; i<nof_packages; i++)
  {
    cJSON *pkgobj = cJSON_GetArrayItem(temp, i);

    cJSON *pkg_name = cJSON_GetObjectItem(pkgobj, "name");
    cJSON *symbols  = cJSON_GetObjectItem(pkgobj, "symbols");

    int nof_symbols = cJSON_GetArraySize(symbols);

    packages[i].name = strdup(pkg_name->valuestring);
    packages[i].nof_symbols = nof_symbols;

    packages[i].symbols = (char **)malloc(packages[i].nof_symbols * sizeof(char *));

    for(j=0; j<nof_symbols; j++)
    {
      cJSON *symbol_obj = cJSON_GetArrayItem(symbols, j);
      packages[i].symbols[j] = strdup(symbol_obj->valuestring);
    }
  }

  hashtable_t *hashtable = hashtable_create(1001);

  temp = cJSON_GetObjectItem(root, "debug_mode");
  debug_mode = strcmp(temp->valuestring, "true") ? false : true;

  /* temp = cJSON_GetObjectItem(root, "top_level_env"); */
  /* top_level_env = convert_to_plisp_obj(root, heap, temp, hashtable); */

  /* temp = cJSON_GetObjectItem(root, "debug_continuation"); */
  /* debug_continuation = convert_to_plisp_obj(root, heap, temp, hashtable); */

  /* temp = cJSON_GetObjectItem(root, "debug_env"); */
  /* debug_env = convert_to_plisp_obj(root, heap, temp, hashtable); */

  /* temp = cJSON_GetObjectItem(root, "debug_execution_stack"); */
  /* debug_execution_stack = convert_to_plisp_obj(root, heap, temp, hashtable); */

  /* temp = cJSON_GetObjectItem(root, "reg_accumulator"); */
  /* reg_accumulator = convert_to_plisp_obj(root, heap, temp, hashtable); */

  /* temp = cJSON_GetObjectItem(root, "reg_next_expression"); */
  /* reg_next_expression = convert_to_plisp_obj(root, heap, temp, hashtable); */

  /* temp = cJSON_GetObjectItem(root, "reg_current_env"); */
  /* reg_current_env = convert_to_plisp_obj(root, heap, temp, hashtable); */

  /* temp = cJSON_GetObjectItem(root, "reg_current_value_rib"); */
  /* reg_current_value_rib = convert_to_plisp_obj(root, heap, temp, hashtable); */

  /* temp = cJSON_GetObjectItem(root, "reg_current_stack"); */
  /* reg_current_stack = convert_to_plisp_obj(root, heap, temp, hashtable); */

  queue_t *q = queue_create();

  top_level_env         = deserialize(heap, cJSON_GetObjectItem(root, "top_level_env")->valueint,         hashtable, q);
  debug_continuation    = deserialize(heap, cJSON_GetObjectItem(root, "debug_continuation")->valueint,    hashtable, q);
  debug_env             = deserialize(heap, cJSON_GetObjectItem(root, "debug_env")->valueint,             hashtable, q);
  debug_execution_stack = deserialize(heap, cJSON_GetObjectItem(root, "debug_execution_stack")->valueint, hashtable, q);
  reg_accumulator       = deserialize(heap, cJSON_GetObjectItem(root, "reg_accumulator")->valueint,       hashtable, q);
  reg_next_expression   = deserialize(heap, cJSON_GetObjectItem(root, "reg_next_expression")->valueint,   hashtable, q);
  reg_current_env       = deserialize(heap, cJSON_GetObjectItem(root, "reg_current_env")->valueint,       hashtable, q);
  reg_current_value_rib = deserialize(heap, cJSON_GetObjectItem(root, "reg_current_value_rib")->valueint, hashtable, q);
  reg_current_stack     = deserialize(heap, cJSON_GetObjectItem(root, "reg_current_stack")->valueint,     hashtable, q);

  convert_heap(heap, hashtable, q);

  hashtable_delete(hashtable);
  queue_delete(q);

  cJSON_Delete(root);
}

BOOLEAN is_dynamic_reference(unsigned int ref)
{
  unsigned int type = ref & BIT_MASK;

  return type == CONS_TAG         ||
         type == ARRAY_TAG        ||
         type == CLOSURE_TAG      ||
         type == MACRO_TAG        ||
         type == CONTINUATION_TAG ||
         type == INTEGER_TAG      ||
         type == FLOAT_TAG;
}

void add_to_deserialization_queue(cJSON *heap, queue_t *q, unsigned int ref, uintptr_t ptr, unsigned int index)
{
  struct slot *s = (struct slot *)malloc(sizeof(struct slot));
  s->ref = ref;
  s->ptr = ptr;
  s->index = index;
  queue_enqueue(q, s);
}

OBJECT_PTR deserialize(cJSON *heap, unsigned int ref, hashtable_t *ht, queue_t *q)
{
  unsigned int object_type = ref & BIT_MASK;

  if(object_type == SYMBOL_TAG ||
     object_type == STRING_LITERAL_TAG ||
     object_type == CHAR_TAG)
    return (OBJECT_PTR)ref;

  hashtable_entry_t *e = hashtable_get(ht, (void *)ref);

  if(e)
    return (OBJECT_PTR)e->value;

  cJSON *heap_obj = cJSON_GetArrayItem(heap, ref >> OBJECT_SHIFT);

  uintptr_t ptr;

  if(object_type == CONS_TAG)
  {
    ptr = object_alloc(2, CONS_TAG);

    unsigned int car_ref = cJSON_GetArrayItem(heap_obj, 0)->valueint;
    unsigned int cdr_ref = cJSON_GetArrayItem(heap_obj, 1)->valueint;

    if(is_dynamic_reference(car_ref))
    {
       hashtable_entry_t *e = hashtable_get(ht, (void *)car_ref);
      if(e)
        set_heap(ptr, 0, (OBJECT_PTR)(e->value));
      else
        add_to_deserialization_queue(heap, q, car_ref, ptr, 0);
    }
    else
      set_heap(ptr, 0, car_ref);

    if(is_dynamic_reference(cdr_ref))
    {
      hashtable_entry_t *e = hashtable_get(ht, (void *)cdr_ref);
      if(e)
        set_heap(ptr, 1, (OBJECT_PTR)(e->value));
      else
        add_to_deserialization_queue(heap, q, cdr_ref, ptr, 1);
    }
    else
      set_heap(ptr, 1, cdr_ref);

  }
  else if(object_type == ARRAY_TAG)
  {
    int i;
    int len = cJSON_GetArraySize(heap_obj);

    ptr = object_alloc(len + 1, ARRAY_TAG);

    (*(unsigned int *)ptr) = len;

    for(i=0; i<len; i++)
    {
      unsigned int elem_ref = cJSON_GetArrayItem(heap_obj, i)->valueint;
      if(is_dynamic_reference(elem_ref))
      {
        hashtable_entry_t *e = hashtable_get(ht, (void *)elem_ref);
        if(e)
          set_heap(ptr, i+1, (OBJECT_PTR)(e->value));
        else
          add_to_deserialization_queue(heap, q, elem_ref, ptr, i+1);
      }
      else
        set_heap(ptr, i+1, elem_ref);
    }
  }
  else if(object_type == CLOSURE_TAG || object_type == MACRO_TAG)
  {
    ptr = object_alloc(4, object_type);

    assert(cJSON_GetArraySize(heap_obj) == 4);

    unsigned int env_ref = cJSON_GetArrayItem(heap_obj, 0)->valueint;
    if(is_dynamic_reference(env_ref))
    {
      hashtable_entry_t *e = hashtable_get(ht, (void *)env_ref);
      if(e)
        set_heap(ptr, 0, (OBJECT_PTR)(e->value));
      else
        add_to_deserialization_queue(heap, q, env_ref, ptr, 0);
    }
    else
      set_heap(ptr, 0, env_ref);

    unsigned int params_ref = cJSON_GetArrayItem(heap_obj, 1)->valueint;
    if(is_dynamic_reference(params_ref))
    {
      hashtable_entry_t *e = hashtable_get(ht, (void *)params_ref);
      if(e)
        set_heap(ptr, 1, (OBJECT_PTR)(e->value));
      else
        add_to_deserialization_queue(heap, q, params_ref, ptr, 1);
    }
    else
      set_heap(ptr, 1, params_ref);

    unsigned int body_ref = cJSON_GetArrayItem(heap_obj, 2)->valueint;
    if(is_dynamic_reference(body_ref))
    {
      hashtable_entry_t *e = hashtable_get(ht, (void *)body_ref);
      if(e)
        set_heap(ptr, 2, (OBJECT_PTR)(e->value));
      else
        add_to_deserialization_queue(heap, q, body_ref, ptr, 2);
    }
    else
      set_heap(ptr, 2, body_ref);

    unsigned int source_ref = cJSON_GetArrayItem(heap_obj, 3)->valueint;
    if(is_dynamic_reference(source_ref))
    {
      hashtable_entry_t *e = hashtable_get(ht, (void *)source_ref);
      if(e)
        set_heap(ptr, 3, (OBJECT_PTR)(e->value));
      else
        add_to_deserialization_queue(heap, q, source_ref, ptr, 3);
    }
    else
      set_heap(ptr, 3, source_ref);
  }
  else if(object_type == CONTINUATION_TAG)
  {
    ptr = object_alloc(1, CONTINUATION_TAG);

    unsigned int stack_ref = heap_obj->valueint;
    if(is_dynamic_reference(stack_ref))
    {
      hashtable_entry_t *e = hashtable_get(ht, (void *)stack_ref);
      if(e)
        set_heap(ptr, 0, (OBJECT_PTR)(e->value));
      else
        add_to_deserialization_queue(heap, q, stack_ref, ptr, 0);
    }
    else
      set_heap(ptr, 0, stack_ref);
  }
  else if(object_type == INTEGER_TAG)
  {
    ptr = object_alloc(1, INTEGER_TAG);
    (*(unsigned int *)ptr) = heap_obj->valueint;
  }
  else if(object_type == FLOAT_TAG)
  {
    ptr = object_alloc(1, FLOAT_TAG);
    (*(float *)ptr) = (float)heap_obj->valuedouble;
  }

  hashtable_put(ht, (void *)ref, (void *)(ptr + object_type));

  return ptr + object_type;
}

void convert_heap(cJSON *heap, hashtable_t *ht, queue_t *q)
{
  while(!queue_is_empty(q))
  {
    struct slot *slot_obj = (struct slot *)queue_dequeue(q)->data;

    unsigned int ref = slot_obj->ref;

    hashtable_entry_t *e = hashtable_get(ht, (void *)ref);

    if(e)
      set_heap(slot_obj->ptr, slot_obj->index, (OBJECT_PTR)e->value);
    else
      set_heap(slot_obj->ptr, slot_obj->index, deserialize(heap, ref, ht, q));

    free(slot_obj);
  }
}
