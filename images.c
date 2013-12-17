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

static const int REF          = 11;

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

inline void print_ref_json_object(FILE *fp, OBJECT_PTR obj, unsigned int *obj_count, hashtable_t *hashtable)
{
  if(obj == NIL)
    fprintf(fp, "{ \"t\" : %d, \"v\" : %d }", SYMBOL, (int)obj);
  else
  {
    hashtable_entry_t *e = hashtable_get(hashtable, (void *)obj);

    if(e)
      fprintf(fp, "{ \"t\" : %d, \"v\" : %d }", REF, (int)e->value);
    else
    {
      fprintf(fp, "{ \"t\" : %d, \"v\" : %d }", REF, *obj_count);
      hashtable_put(hashtable, (void *)obj, (void *)(*obj_count));
      (*obj_count)++;
    }
  }
}

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

void print_immediate_object(FILE *fp, OBJECT_PTR obj)
{
  assert(IS_CHAR_OBJECT(obj)    ||
         IS_SYMBOL_OBJECT(obj)  ||
         IS_STRING_LITERAL_OBJECT(obj));

  if(IS_CHAR_OBJECT(obj))
    fprintf(fp, "{ \"t\" : %d, \"v\" : %d }", CHAR_TAG, (unsigned int)obj);
  else if(IS_STRING_LITERAL_OBJECT(obj))
    fprintf(fp, "{ \"t\" : %d, \"v\" : %d }", STRING_LITERAL_TAG, (unsigned int)obj);
  else if(IS_SYMBOL_OBJECT(obj))
    fprintf(fp, "{ \"t\" : %d, \"v\" : %d }", SYMBOL_TAG, (unsigned int)obj);
}

void print_object1(FILE *fp, OBJECT_PTR obj, queue_t *print_queue, unsigned int *obj_count, hashtable_t *hashtable, hashtable_t *printed_objects)
{
  if(is_dynamic_memory_object(obj))
  {
    print_ref_json_object(fp, obj, obj_count, hashtable);
    add_obj_to_print_list(print_queue, obj, printed_objects);
  }
  else
    print_immediate_object(fp, obj);
}

void print_json_object(FILE *fp, 
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

  fprintf(fp, "{ ");
  //fprintf(fp, "{ \"ptr\" : %d, ", obj);

  if(IS_CONS_OBJECT(obj))
  {
    OBJECT_PTR car_obj = car(obj);
    OBJECT_PTR cdr_obj = cdr(obj);

    fprintf(fp, "\"t\" : %d ,", CONS_TAG);
    fprintf(fp, "\"v\" : [");
    print_object1(fp, car_obj, print_queue, obj_count, hashtable, printed_objects);
    fprintf(fp, ", ");
    print_object1(fp, cdr_obj, print_queue, obj_count, hashtable, printed_objects);
    fprintf(fp, "] ");
  }
  else if(IS_ARRAY_OBJECT(obj))
  {
    fprintf(fp, "\"t\" : %d,", ARRAY_TAG);
    fprintf(fp, "\"v\" : [ ");

    //int len = get_int_value(get_heap(obj & POINTER_MASK, 0));
    int len = *((unsigned int *)(obj & POINTER_MASK));

    int i;

    for(i=1; i<=len; i++)
    {
      print_object1(fp, get_heap(obj & POINTER_MASK, i), print_queue, obj_count, hashtable, printed_objects);

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

    fprintf(fp, "\"t\" : %d,", IS_CLOSURE_OBJECT(obj) ? CLOSURE_TAG : MACRO_TAG);
    fprintf(fp, "\"v\" : [");
    print_object1(fp, env, print_queue, obj_count, hashtable, printed_objects);
    fprintf(fp, ", ");
    print_object1(fp, params, print_queue, obj_count, hashtable, printed_objects);
    fprintf(fp, ", ");
    print_object1(fp, body, print_queue, obj_count, hashtable, printed_objects);
    fprintf(fp, ", ");
    print_object1(fp, source, print_queue, obj_count, hashtable, printed_objects);
    fprintf(fp, "] ");
  }
  else if(IS_CONTINUATION_OBJECT(obj))
  {
    fprintf(fp, "\"t\" : %d,", CONTINUATION_TAG);
    fprintf(fp, "\"v\" : ");
    print_object1(fp, get_heap(obj & POINTER_MASK, 0), print_queue, obj_count, hashtable, printed_objects);
  }
  else if(IS_INTEGER_OBJECT(obj))
    fprintf(fp, "\"t\" : %d, \"v\" : %d ", INTEGER_TAG, get_int_value(obj));
  else if(IS_FLOAT_OBJECT(obj))
    fprintf(fp, "\"t\" : %d, \"v\" : %d ", FLOAT_TAG,   get_float_value(obj));
  else
    assert(false);

  fprintf(fp, "}");

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

  fprintf(fp, "\"top_level_env\" : "        ); print_object1(fp, top_level_env,        print_queue, obj_count, hashtable, printed_objects); fprintf(fp, ", ");

  fprintf(fp, "\"debug_continuation\" : "   ); print_object1(fp,debug_continuation,    print_queue, obj_count, hashtable, printed_objects); fprintf(fp, ", ");
  fprintf(fp, "\"debug_env\" : "            ); print_object1(fp,debug_env,             print_queue, obj_count, hashtable, printed_objects); fprintf(fp, ", ");
  fprintf(fp, "\"debug_execution_stack\" : "); print_object1(fp,debug_execution_stack, print_queue, obj_count, hashtable, printed_objects); fprintf(fp, ", ");
  fprintf(fp, "\"reg_accumulator\" : "      ); print_object1(fp,reg_accumulator,       print_queue, obj_count, hashtable, printed_objects); fprintf(fp, ", ");
  fprintf(fp, "\"reg_next_expression\" : "  ); print_object1(fp,reg_next_expression,   print_queue, obj_count, hashtable, printed_objects); fprintf(fp, ", ");
  fprintf(fp, "\"reg_current_env\" : "      ); print_object1(fp,reg_current_env,       print_queue, obj_count, hashtable, printed_objects); fprintf(fp, ", ");
  fprintf(fp, "\"reg_current_value_rib\" : "); print_object1(fp,reg_current_value_rib, print_queue, obj_count, hashtable, printed_objects); fprintf(fp, ", ");
  fprintf(fp, "\"reg_current_stack\" : "    ); print_object1(fp,reg_current_stack,     print_queue, obj_count, hashtable, printed_objects); fprintf(fp, ", ");

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

  //add_obj_to_print_list(print_queue, top_level_env);

  while(!queue_is_empty(print_queue))
  {
    OBJECT_PTR obj = (OBJECT_PTR)((queue_dequeue(print_queue))->data);
    assert(is_dynamic_memory_object(obj));
    print_json_object(fp, obj, print_queue, obj_count, hashtable, printed_objects);
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

OBJECT_PTR convert_to_plisp_obj(cJSON *root, cJSON *heap, cJSON * obj, hashtable_t *hashtable)
{
  /* cJSON *type = cJSON_GetObjectItem(obj, "t"); */
  /* cJSON *value = cJSON_GetObjectItem(obj, "v"); */
  cJSON *type = obj->child;
  cJSON *value = obj->child->next;
  
  int str_type = type->valueint;

  int i;

  OBJECT_PTR ret;

  BOOLEAN obj_to_be_created = false;

  if(str_type == REF)
  {
    hashtable_entry_t *e = hashtable_get(hashtable, (void *)(value->valueint));
    if(e)
      ret = (OBJECT_PTR)(e->value);
    else
    {
      cJSON *refobj = cJSON_GetArrayItem(heap, value->valueint);

      if(!refobj)
      {
        printf("%d\n", value->valueint);
        printf("%d\n", cJSON_GetArraySize(heap));
        assert(false);
      }

      ret = convert_to_plisp_obj(root, heap, refobj, hashtable);
      hashtable_put(hashtable, (void *)(value->valueint),(void *)ret);
    }
  }
  else if(str_type == CONS_TAG)
  {
    uintptr_t ptr = object_alloc(2, CONS_TAG);

    set_heap(ptr, 0, convert_to_plisp_obj(root, heap, cJSON_GetArrayItem(value, 0), hashtable));
    set_heap(ptr, 1, convert_to_plisp_obj(root, heap, cJSON_GetArrayItem(value, 1), hashtable));

    ret = ptr + CONS_TAG;
  }
  else if(str_type == ARRAY_TAG)
  {
    int size = cJSON_GetArraySize(value);
    
    //see comment in main.c for why we're not using object_alloc()
    //unsigned int *raw_ptr;
    //posix_memalign((void **)&raw_ptr, 16, sizeof(unsigned int *));
    //*((int *)raw_ptr) = size;

    uintptr_t ptr = object_alloc(size + 1, ARRAY_TAG);

    //set_heap(ptr, 0, convert_int_to_object(size));
    //set_heap(ptr, 0, (uintptr_t)raw_ptr + INTEGER_TAG);
    *((unsigned int *)ptr) = size;

    for(i=0; i<size; i++)
      set_heap(ptr, i+1, convert_to_plisp_obj(root, heap, cJSON_GetArrayItem(value, i), hashtable));

    ret = ptr + ARRAY_TAG;
  }
  else if(str_type == CLOSURE_TAG || str_type == MACRO_TAG)
  {
    uintptr_t ptr = object_alloc(4, str_type);

    set_heap(ptr, 0, convert_to_plisp_obj(root, heap, cJSON_GetArrayItem(value, 0), hashtable));
    set_heap(ptr, 1, convert_to_plisp_obj(root, heap, cJSON_GetArrayItem(value, 1), hashtable));
    set_heap(ptr, 2, convert_to_plisp_obj(root, heap, cJSON_GetArrayItem(value, 2), hashtable));
    set_heap(ptr, 3, convert_to_plisp_obj(root, heap, cJSON_GetArrayItem(value, 3), hashtable));

    ret = ptr + str_type;
  }
  else if(str_type == CONTINUATION_TAG)
  {
    uintptr_t ptr = object_alloc(1, CONTINUATION_TAG);
    set_heap(ptr, 0, convert_to_plisp_obj(root, heap, value, hashtable));
    ret = ptr + CONTINUATION_TAG;
  }
  else if(str_type == INTEGER_TAG)
  {
    ret = convert_int_to_object(value->valueint);
  }
  else if(str_type == FLOAT_TAG)
  {
    ret = convert_float_to_object((float)value->valuedouble);
  }
  else if(str_type == SYMBOL_TAG         ||
          str_type == STRING_LITERAL_TAG ||
          str_type == CHAR_TAG)
  {
    ret = (OBJECT_PTR)(value->valueint);
  }
  else
  {
    printf("%d\n", str_type);
    assert(false);
  }

  if(!is_valid_object(ret))
  {
    printf("%d\n", ret);
    assert(false);
  }

  return ret;
}

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

  hashtable_t *hashtable = hashtable_create(1000001);

  temp = cJSON_GetObjectItem(root, "debug_mode");
  debug_mode = strcmp(temp->valuestring, "true") ? false : true;

  temp = cJSON_GetObjectItem(root, "top_level_env");
  top_level_env = convert_to_plisp_obj(root, heap, temp, hashtable);

  temp = cJSON_GetObjectItem(root, "debug_continuation");
  debug_continuation = convert_to_plisp_obj(root, heap, temp, hashtable);

  temp = cJSON_GetObjectItem(root, "debug_env");
  debug_env = convert_to_plisp_obj(root, heap, temp, hashtable);

  temp = cJSON_GetObjectItem(root, "debug_execution_stack");
  debug_execution_stack = convert_to_plisp_obj(root, heap, temp, hashtable);

  temp = cJSON_GetObjectItem(root, "reg_accumulator");
  reg_accumulator = convert_to_plisp_obj(root, heap, temp, hashtable);

  temp = cJSON_GetObjectItem(root, "reg_next_expression");
  reg_next_expression = convert_to_plisp_obj(root, heap, temp, hashtable);

  temp = cJSON_GetObjectItem(root, "reg_current_env");
  reg_current_env = convert_to_plisp_obj(root, heap, temp, hashtable);

  temp = cJSON_GetObjectItem(root, "reg_current_value_rib");
  reg_current_value_rib = convert_to_plisp_obj(root, heap, temp, hashtable);

  temp = cJSON_GetObjectItem(root, "reg_current_stack");
  reg_current_stack = convert_to_plisp_obj(root, heap, temp, hashtable);

  hashtable_delete(hashtable);

  cJSON_Delete(root);
}

