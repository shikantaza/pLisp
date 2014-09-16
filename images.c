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
#include <gtk/gtk.h>

#include "plisp.h"
#include "memory.h"

#include "json.h"

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

extern BOOLEAN in_break;

extern OBJECT_PTR NIL;

extern int nof_dl_handles;
extern char *foreign_library_names[];
extern void **dl_handles;

extern unsigned int POINTER_MASK;

extern GtkWindow *workspace_window;
extern GtkTextBuffer *workspace_buffer;

extern GtkWindow *transcript_window;
extern GtkTextBuffer *transcript_buffer;

extern GtkWindow *debugger_window;

extern GtkWindow *system_browser_window;
extern GtkTextBuffer *system_browser_buffer;
extern GtkTextView *system_browser_textview;
extern GtkStatusbar *system_browser_statusbar;

extern GtkWindow *profiler_window;

extern hashtable_t *profiling_tab;

extern GtkTreeView *packages_list;
extern GtkTreeView *symbols_list;

extern OBJECT_PTR DEFUN, DEFMACRO;

//forward declarations
BOOLEAN is_dynamic_reference(unsigned int);
void add_to_deserialization_queue(struct JSONObject *, queue_t *, unsigned int, uintptr_t, unsigned int);
OBJECT_PTR deserialize_internal(struct JSONObject *, unsigned int, hashtable_t *, queue_t *, BOOLEAN);
void convert_heap(struct JSONObject *, hashtable_t *, queue_t *, BOOLEAN);

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

  //assert(is_dynamic_memory_object(obj));
  queue_enqueue(print_queue, (void *)obj);
}

void print_json_object(FILE *fp, 
                       OBJECT_PTR obj, 
                       queue_t *print_queue, 
                       unsigned int *obj_count,
                       hashtable_t *hashtable, 
                       hashtable_t *printed_objects,
                       BOOLEAN single_object)
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
  {
    if(single_object)
    {
      fprintf(fp, "%d",  ((*obj_count) << OBJECT_SHIFT) + (obj & BIT_MASK));
      (*obj_count)++;

      add_obj_to_print_list(print_queue, obj, printed_objects);
    }
    else
      fprintf(fp, "%d", (unsigned int)obj);
  }
}

void print_heap_representation(FILE *fp, 
                               OBJECT_PTR obj, 
                               queue_t *print_queue, 
                               unsigned int *obj_count, 
                               hashtable_t *hashtable, 
                               hashtable_t *printed_objects,
                               BOOLEAN single_object)
{
  if(single_object)
  {
    if(IS_SYMBOL_OBJECT(obj))
    {
      char s[SYMBOL_STRING_SIZE];
      print_qualified_symbol(obj, s);
      fprintf(fp, "\"%s\" ", s);
      return;
    }
    else if(IS_STRING_LITERAL_OBJECT(obj))
      fprintf(fp, "\"%s\"", strings[obj >> OBJECT_SHIFT]);
    else if(IS_CHAR_OBJECT(obj))
      fprintf(fp, "%d ", obj >> OBJECT_SHIFT);
  }

  if(!single_object && !is_dynamic_memory_object(obj))
  {
    printf("%d\n", (int)obj);
    assert(false);
  }

  if(IS_CONS_OBJECT(obj))
  {
    OBJECT_PTR car_obj = car(obj);
    OBJECT_PTR cdr_obj = cdr(obj);

    fprintf(fp, "[");
    print_json_object(fp, car_obj, print_queue, obj_count, hashtable, printed_objects, single_object);
    fprintf(fp, ", ");
    print_json_object(fp, cdr_obj, print_queue, obj_count, hashtable, printed_objects, single_object);
    fprintf(fp, "] ");
  }
  else if(IS_ARRAY_OBJECT(obj))
  {
    fprintf(fp, "[ ");

    int len = *((unsigned int *)(obj & POINTER_MASK));

    int i;

    for(i=1; i<=len; i++)
    {
      print_json_object(fp, get_heap(obj & POINTER_MASK, i), print_queue, obj_count, hashtable, printed_objects, single_object);

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
    print_json_object(fp, env, print_queue, obj_count, hashtable, printed_objects, single_object);
    fprintf(fp, ", ");
    print_json_object(fp, params, print_queue, obj_count, hashtable, printed_objects, single_object);
    fprintf(fp, ", ");
    print_json_object(fp, body, print_queue, obj_count, hashtable, printed_objects, single_object);
    fprintf(fp, ", ");
    print_json_object(fp, source, print_queue, obj_count, hashtable, printed_objects, single_object);
    fprintf(fp, "] ");
  }
  else if(IS_CONTINUATION_OBJECT(obj))
  {
    print_json_object(fp, get_heap(obj & POINTER_MASK, 0), print_queue, obj_count, hashtable, printed_objects, single_object);
  }
  else if(IS_INTEGER_OBJECT(obj))
    fprintf(fp, "%d", get_int_value(obj));
  else if(IS_FLOAT_OBJECT(obj))
    fprintf(fp, "%f", get_float_value(obj));
  else
    if(!single_object)assert(false);

  hashtable_put(printed_objects, (void *)obj, (void *)1);
}

//#ifdef DEBUG
/* void doit(char *text) */
/* { */
/*   char *out;cJSON *json; */
	
/*   json=cJSON_Parse(text); */
/*   if (!json) {printf("Error before: [%s]\n",cJSON_GetErrorPtr()); getchar();} */
/*   else */
/*   { */
/*     out=cJSON_Print(json); */
/*     cJSON_Delete(json); */
/*     printf("%s\n",out); */
/*     free(out); */
/*   } */
/* } */

/* Read a file, parse, render back, etc. */
/* void dofile(char *filename) */
/* { */
/*   FILE *f=fopen(filename,"rb");fseek(f,0,SEEK_END);long len=ftell(f);fseek(f,0,SEEK_SET); */
/*   char *data=(char*)malloc(len+1);fread(data,1,len,f);fclose(f); */
/*   doit(data); */
/*   free(data); */
/* } */
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
  fprintf(fp, "\"in_break\" : \"%s\"",       in_break ? "true" : "false");                                             fprintf(fp, ", ");

  fprintf(fp, "\"top_level_env\" : "        ); print_json_object(fp, top_level_env,        print_queue, obj_count, hashtable, printed_objects, false); fprintf(fp, ", ");

  fprintf(fp, "\"debug_continuation\" : "   ); print_json_object(fp,debug_continuation,    print_queue, obj_count, hashtable, printed_objects, false); fprintf(fp, ", ");
  fprintf(fp, "\"debug_env\" : "            ); print_json_object(fp,debug_env,             print_queue, obj_count, hashtable, printed_objects, false); fprintf(fp, ", ");
  fprintf(fp, "\"debug_execution_stack\" : "); print_json_object(fp,debug_execution_stack, print_queue, obj_count, hashtable, printed_objects, false); fprintf(fp, ", ");
  fprintf(fp, "\"reg_accumulator\" : "      ); print_json_object(fp,reg_accumulator,       print_queue, obj_count, hashtable, printed_objects, false); fprintf(fp, ", ");
  fprintf(fp, "\"reg_next_expression\" : "  ); print_json_object(fp,reg_next_expression,   print_queue, obj_count, hashtable, printed_objects, false); fprintf(fp, ", ");
  fprintf(fp, "\"reg_current_env\" : "      ); print_json_object(fp,reg_current_env,       print_queue, obj_count, hashtable, printed_objects, false); fprintf(fp, ", ");
  fprintf(fp, "\"reg_current_value_rib\" : "); print_json_object(fp,reg_current_value_rib, print_queue, obj_count, hashtable, printed_objects, false); fprintf(fp, ", ");
  fprintf(fp, "\"reg_current_stack\" : "    ); print_json_object(fp,reg_current_stack,     print_queue, obj_count, hashtable, printed_objects, false); fprintf(fp, ", ");

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
  
  if(profiler_window)
  {
    int posx, posy, width, height;

    gtk_window_get_position(profiler_window, &posx, &posy);
    gtk_window_get_size(profiler_window, &width, &height);

    fprintf(fp, 
            "\"profiler\" : [ %d, %d, %d, %d, [", 
            posx, posy, width, height);

    hashtable_entry_t **entries = profiling_tab->entries;

    int i, count = profiling_tab->hash_size;

    BOOLEAN first_printed = false;

    for(i=0; i<count; i++)
    {
      if(profiling_tab->entries[i])
      {
        if(first_printed)
          fprintf(fp, ", ");

        hashtable_entry_t *e = profiling_tab->entries[i];

        while(e)
        {
          OBJECT_PTR operator = (OBJECT_PTR)e->ptr;

          profiling_datum_t *pd = (profiling_datum_t *)e->value;

          fprintf(fp, "[ ");
          print_json_object(fp, operator, print_queue, obj_count, hashtable, printed_objects, false);
          fprintf(fp, ", %d, %f, %f, %d, %d", 
                  pd->count,
                  (float)pd->elapsed_wall_time,
                  (float)pd->elapsed_cpu_time,
                  pd->mem_allocated,
                  pd->mem_deallocated);
          fprintf(fp, " ]");

          e = e->next;

          if(e)
            fprintf(fp, ",");
        }

        if(!first_printed)
          first_printed = true;
      }
    }
    fprintf(fp, "]], ");
  }

  if(system_browser_window)
  {
    int posx, posy, width, height;

    gtk_window_get_position(system_browser_window, &posx, &posy);
    gtk_window_get_size(system_browser_window, &width, &height);

    GtkListStore *store1 = GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(packages_list)));
    GtkTreeModel *model1 = gtk_tree_view_get_model (GTK_TREE_VIEW (packages_list));
    GtkTreeIter  iter1;
    gint id = -1;

    if(gtk_tree_selection_get_selected(gtk_tree_view_get_selection(GTK_TREE_VIEW(packages_list)), &model1, &iter1))
      gtk_tree_model_get(model1, &iter1, 1, &id, -1);

    GtkListStore *store2 = GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(symbols_list)));
    GtkTreeModel *model2 = gtk_tree_view_get_model (GTK_TREE_VIEW (symbols_list));
    GtkTreeIter  iter2;
    gint ptr = -1;

    if(gtk_tree_selection_get_selected(gtk_tree_view_get_selection(GTK_TREE_VIEW(symbols_list)), &model2, &iter2))
      gtk_tree_model_get(model2, &iter2, 1, &ptr, -1);

    fprintf(fp, 
            "\"system_browser\" : [ %d, %d, %d, %d, %d, ", 
            posx, posy, width, height, id);

    if(ptr != -1)
      print_json_object(fp, ptr, print_queue, obj_count, hashtable, printed_objects, false);
    else
      fprintf(fp, "-1");

    fprintf(fp, "], ");
  }

  fprintf(fp, "\"heap\" : [");

  while(!queue_is_empty(print_queue))
  {
    OBJECT_PTR obj = (OBJECT_PTR)((queue_dequeue(print_queue))->data);
    assert(is_dynamic_memory_object(obj));
    print_heap_representation(fp, obj, print_queue, obj_count, hashtable, printed_objects, false);
    if(!queue_is_empty(print_queue))fprintf(fp, ", ");
  }

  queue_delete(print_queue);
  hashtable_delete(hashtable);
  hashtable_delete(printed_objects);

  fprintf(fp, "]");

  if(workspace_window)
  {
    int posx, posy, width, height;
    GtkTextIter start, end;

    char text[MAX_STRING_LENGTH];

    char *workspace_text;

    gtk_text_buffer_get_start_iter(workspace_buffer, &start);
    gtk_text_buffer_get_end_iter(workspace_buffer, &end);

    int len;

    int i=0, j=0;

    workspace_text = strdup(gtk_text_buffer_get_text(workspace_buffer, &start, &end, FALSE));

    len = strlen(workspace_text);

    memset(text, '\0', MAX_STRING_LENGTH);

    while(i<len)
    {
      if(workspace_text[i] == '\"')
      {
        text[j] = '\\';
        text[j+1] = '\"';
        j += 2;
      }
      else
      {
        text[j] = workspace_text[i];
        j++;
      }
      i++;
    }

    gtk_window_get_position(workspace_window, &posx, &posy);
    gtk_window_get_size(workspace_window, &width, &height);

    fprintf(fp, 
            ", \"workspace\" : [ %d, %d, %d, %d, \"%s\" ]", 
            posx, posy, width, height, text);
  }

  if(debugger_window)
  {
    int posx, posy, width, height;

    gtk_window_get_position(debugger_window, &posx, &posy);
    gtk_window_get_size(debugger_window, &width, &height);

    fprintf(fp, 
            ", \"debugger\" : [ %d, %d, %d, %d ]", 
            posx, posy, width, height);
  }

  //begin transcript window serialization
  int posx, posy, width, height;
  GtkTextIter start, end;

  char text[MAX_STRING_LENGTH];

  char *transcript_text;

  gtk_text_buffer_get_start_iter(transcript_buffer, &start);
  gtk_text_buffer_get_end_iter(transcript_buffer, &end);

  int len;

  int ii=0, jj=0;

  transcript_text = strdup(gtk_text_buffer_get_text(transcript_buffer, &start, &end, FALSE));

  len = strlen(transcript_text);

  memset(text, '\0', MAX_STRING_LENGTH);

  while(ii<len)
  {
    if(transcript_text[ii] == '\"')
    {
      text[jj] = '\\';
      text[jj+1] = '\"';
      jj += 2;
    }
    else
    {
      text[jj] = transcript_text[ii];
      jj++;
    }
    ii++;
  }

  gtk_window_get_position(transcript_window, &posx, &posy);
  gtk_window_get_size(transcript_window, &width, &height);

  fprintf(fp, 
          ", \"transcript\" : [ %d, %d, %d, %d, \"%s\" ]", 
          posx, posy, width, height, text);
  //end transcript window serialization

  fprintf(fp, "}");

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

  struct JSONObject *root = JSON_parse(file_name);
  struct JSONObject *heap = JSON_get_object_item(root, "heap");

  struct JSONObject *temp;

  temp = JSON_get_object_item(root, "current_package");
  current_package = temp->ivalue;

  temp = JSON_get_object_item(root, "gen_sym_count");
  gen_sym_count = temp->ivalue;

  temp = JSON_get_object_item(root, "strings");
  nof_strings = JSON_get_array_size(temp);

  strings = (char **)malloc(nof_strings * sizeof(char *));

  for(i=0; i<nof_strings; i++)
  {
    struct JSONObject *strobj = JSON_get_array_item(temp, i);
    strings[i] = strdup(strobj->strvalue);
  }

  temp = JSON_get_object_item(root, "foreign_libraries");
  nof_dl_handles = JSON_get_array_size(temp);

  dl_handles = (void **)malloc(nof_dl_handles * sizeof(void *));

  for(i=0; i<nof_dl_handles; i++)
  {
    struct JSONObject *strobj = JSON_get_array_item(temp, i);
    foreign_library_names[i] = strdup(strobj->strvalue);
    dl_handles[i] = dlopen(strobj->strvalue, RTLD_LAZY);
  }

  temp = JSON_get_object_item(root, "packages");
  nof_packages = JSON_get_array_size(temp);

  packages = (package_t *)malloc(nof_packages * sizeof(package_t));

  for(i=0; i<nof_packages; i++)
  {
    struct JSONObject *pkgobj = JSON_get_array_item(temp, i);

    struct JSONObject *pkg_name = JSON_get_object_item(pkgobj, "name");
    struct JSONObject *symbols  = JSON_get_object_item(pkgobj, "symbols");

    int nof_symbols = JSON_get_array_size(symbols);

    packages[i].name = strdup(pkg_name->strvalue);
    packages[i].nof_symbols = nof_symbols;

    packages[i].symbols = (char **)malloc(packages[i].nof_symbols * sizeof(char *));

    for(j=0; j<nof_symbols; j++)
    {
      struct JSONObject *symbol_obj = JSON_get_array_item(symbols, j);
      packages[i].symbols[j] = strdup(symbol_obj->strvalue);
    }
  }

  hashtable_t *hashtable = hashtable_create(1001);

  temp = JSON_get_object_item(root, "debug_mode");
  debug_mode = strcmp(temp->strvalue, "true") ? false : true;

  temp = JSON_get_object_item(root, "in_break");
  in_break = strcmp(temp->strvalue, "true") ? false : true;

  queue_t *q = queue_create();

  top_level_env         = deserialize_internal(heap, JSON_get_object_item(root, "top_level_env")->ivalue,         hashtable, q, false);
  debug_continuation    = deserialize_internal(heap, JSON_get_object_item(root, "debug_continuation")->ivalue,    hashtable, q, false);
  debug_env             = deserialize_internal(heap, JSON_get_object_item(root, "debug_env")->ivalue,             hashtable, q, false);
  debug_execution_stack = deserialize_internal(heap, JSON_get_object_item(root, "debug_execution_stack")->ivalue, hashtable, q, false);
  reg_accumulator       = deserialize_internal(heap, JSON_get_object_item(root, "reg_accumulator")->ivalue,       hashtable, q, false);
  reg_next_expression   = deserialize_internal(heap, JSON_get_object_item(root, "reg_next_expression")->ivalue,   hashtable, q, false);
  reg_current_env       = deserialize_internal(heap, JSON_get_object_item(root, "reg_current_env")->ivalue,       hashtable, q, false);
  reg_current_value_rib = deserialize_internal(heap, JSON_get_object_item(root, "reg_current_value_rib")->ivalue, hashtable, q, false);
  reg_current_stack     = deserialize_internal(heap, JSON_get_object_item(root, "reg_current_stack")->ivalue,     hashtable, q, false);

  struct JSONObject *profiler = JSON_get_object_item(root, "profiler");

  if(profiler)
  {
    profiling_tab = hashtable_create(1000001);

    struct JSONObject *entries = JSON_get_array_item(profiler, 4);

    int size = JSON_get_array_size(entries);

    int i;

    for(i=0; i<size; i++)
    {
      struct JSONObject *entry = JSON_get_array_item(entries, i);
      OBJECT_PTR operator = deserialize_internal(heap, JSON_get_array_item(entry, 0)->ivalue, hashtable, q, false);

      profiling_datum_t *pd = (profiling_datum_t *)malloc(sizeof(profiling_datum_t));

      pd->count             = JSON_get_array_item(entry, 1)->ivalue;
      pd->elapsed_wall_time = JSON_get_array_item(entry, 2)->fvalue;
      pd->elapsed_cpu_time  = JSON_get_array_item(entry, 3)->fvalue;
      pd->mem_allocated     = JSON_get_array_item(entry, 4)->ivalue;
      pd->mem_deallocated   = JSON_get_array_item(entry, 5)->ivalue;

      hashtable_put(profiling_tab, (void *)operator, (void *)pd);
      
    }
  }

  struct JSONObject *system_browser = JSON_get_object_item(root, "system_browser");
  OBJECT_PTR obj1;

  int sym_ptr;

  if(system_browser)
  {
    sym_ptr = JSON_get_array_item(system_browser, 5)->ivalue;
    if(sym_ptr != -1)
      obj1 = deserialize_internal(heap, sym_ptr, hashtable, q, false);
  }

  convert_heap(heap, hashtable, q, false);

  if(system_browser_window)
    close_application_window(&system_browser_window);

  if(system_browser)
  {
    create_system_browser_window(JSON_get_array_item(system_browser, 0)->ivalue,
                                 JSON_get_array_item(system_browser, 1)->ivalue,
                                 JSON_get_array_item(system_browser, 2)->ivalue,
                                 JSON_get_array_item(system_browser, 3)->ivalue);

    int id = JSON_get_array_item(system_browser, 4)->ivalue;

    if(id != -1)
    {
      char s[4];
      memset(s, '\0', 4);
      sprintf(s,"%d", id);

      //select the selected package
      gtk_tree_view_set_cursor(packages_list, gtk_tree_path_new_from_string(s), NULL, FALSE);

      remove_all_from_list(symbols_list);

      GtkListStore *store2;
      GtkTreeIter  iter2;

      store2 = GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(symbols_list)));

      OBJECT_PTR rest = top_level_env;

      while(rest != NIL)
      {
        OBJECT_PTR sym = CAAR(rest);

        if(((int)sym >> (SYMBOL_BITS + OBJECT_SHIFT)) == id)
        {
          gtk_list_store_append(store2, &iter2);
          gtk_list_store_set(store2, &iter2, 0, get_symbol_name(sym), -1);  
          gtk_list_store_set(store2, &iter2, 1, sym, -1);
        }

        rest = cdr(rest);
      }
    }

    if(sym_ptr != -1)
    {
      GtkTreeModel *model = gtk_tree_view_get_model(GTK_TREE_VIEW(symbols_list));
      gboolean is_not_empty;
      GtkTreeIter iter;
      is_not_empty = gtk_tree_model_get_iter_first(model, &iter); 

      int i=0;
      char s1[4];

      while(is_not_empty == TRUE)
      {
        gchar *s;
        gtk_tree_model_get(model, &iter, 0, &s, -1);

        if(!strcmp(s, get_symbol_name(obj1)))
        {
          memset(s1, '\0', 4);
          sprintf(s1,"%d", i);
          gtk_tree_view_set_cursor(symbols_list, gtk_tree_path_new_from_string(s1), NULL, FALSE);
          break;
        }
        is_not_empty = gtk_tree_model_iter_next(model, &iter);
        i++;
      }

      char buf[MAX_STRING_LENGTH];
      memset(buf, '\0', MAX_STRING_LENGTH);

      OBJECT_PTR obj = cdr(get_symbol_value_from_env(obj1, top_level_env));

      gtk_text_buffer_set_text(system_browser_buffer, buf, -1);

      gtk_text_view_set_editable(system_browser_textview, FALSE);

      if(IS_CLOSURE_OBJECT(obj))
      {
        memset(buf, '\0', MAX_STRING_LENGTH);
        OBJECT_PTR temp = cons(DEFUN, 
                               cons(obj1,
                                    cons(get_params_object(obj),
                                         get_source_object(obj))));

        print_object_to_string(temp, buf, 0);

        gtk_text_buffer_insert_at_cursor(system_browser_buffer, (char *)convert_to_lower_case(buf), -1);

        gtk_text_view_set_editable(system_browser_textview, TRUE);
      }
      else if(IS_MACRO_OBJECT(obj))
      {
        memset(buf, '\0', MAX_STRING_LENGTH);
        OBJECT_PTR temp = cons(DEFMACRO, 
                               cons(obj1,
                                    cons(get_params_object(obj),
                                         get_source_object(obj))));

        print_object_to_string(temp, buf, 0);

        gtk_text_buffer_insert_at_cursor(system_browser_buffer, (char *)convert_to_lower_case(buf), -1);
        gtk_text_view_set_editable(system_browser_textview, TRUE);
      }
      else if(IS_CONTINUATION_OBJECT(obj))
      {
        memset(buf, '\0', MAX_STRING_LENGTH);
        print_object_to_string(obj, buf, 0);
        gtk_text_buffer_insert_at_cursor(system_browser_buffer, (char *)convert_to_lower_case(buf), -1);
      }
      else
      {
        memset(buf, '\0', MAX_STRING_LENGTH);
        print_object_to_string(obj, buf, 0);
        gtk_text_buffer_insert_at_cursor(system_browser_buffer, (char *)convert_to_lower_case(buf), -1);
        gtk_text_view_set_editable(system_browser_textview, FALSE);
      }

      gtk_text_buffer_insert_at_cursor(system_browser_buffer, "\n", -1);
    }
  }

  if(profiler_window)
    close_application_window(&profiler_window);

  if(profiler)
    create_profiler_window(JSON_get_array_item(profiler, 0)->ivalue,
                           JSON_get_array_item(profiler, 1)->ivalue,
                           JSON_get_array_item(profiler, 2)->ivalue,
                           JSON_get_array_item(profiler, 3)->ivalue);

  hashtable_delete(hashtable);
  queue_delete(q);

  struct JSONObject *workspace = JSON_get_object_item(root, "workspace");

  if(workspace_window)
    close_application_window(&workspace_window);

  if(workspace)
  {
    char text[MAX_STRING_LENGTH];
    memset(text, '\0', MAX_STRING_LENGTH);
    int i=0, j=0, len;
    char *json_text = JSON_get_array_item(workspace, 4)->strvalue;
    
    len = strlen(json_text);

    while(i<len)
    {
      if(json_text[i] != '\\')
      {
        text[j] = json_text[i];
        j++;
      }
      i++;
    }

    create_workspace_window(JSON_get_array_item(workspace, 0)->ivalue,
                            JSON_get_array_item(workspace, 1)->ivalue,
                            JSON_get_array_item(workspace, 2)->ivalue,
                            JSON_get_array_item(workspace, 3)->ivalue,
                            text);
  }

  struct JSONObject *debugger = JSON_get_object_item(root, "debugger");

  if(debugger_window)
    close_application_window(&debugger_window);

  if(debugger)
    create_debug_window(JSON_get_array_item(debugger, 0)->ivalue,
                        JSON_get_array_item(debugger, 1)->ivalue,
                        JSON_get_array_item(debugger, 2)->ivalue,
                        JSON_get_array_item(debugger, 3)->ivalue);

  struct JSONObject *transcript = JSON_get_object_item(root, "transcript");

  assert(transcript);

  char text[MAX_STRING_LENGTH];
  memset(text, '\0', MAX_STRING_LENGTH);
  int ii=0, jj=0, len;
  char *json_text = JSON_get_array_item(transcript, 4)->strvalue;
    
  len = strlen(json_text);

  while(ii<len)
  {
    if(json_text[ii] != '\\')
    {
      text[jj] = json_text[ii];
      jj++;
    }
    ii++;
  }

  if(transcript_window)
  {
    gtk_window_set_default_size(transcript_window,
				JSON_get_array_item(transcript, 2)->ivalue,
				JSON_get_array_item(transcript, 3)->ivalue);

    gtk_window_move(transcript_window,
		    JSON_get_array_item(transcript, 0)->ivalue,
		    JSON_get_array_item(transcript, 1)->ivalue); 
    
    gtk_text_buffer_set_text(transcript_buffer, "", 0);
    print_to_transcript(text);
    update_transcript_title();
  }
  else
  {
    create_transcript_window(JSON_get_array_item(transcript, 0)->ivalue,
			     JSON_get_array_item(transcript, 1)->ivalue,
			     JSON_get_array_item(transcript, 2)->ivalue,
			     JSON_get_array_item(transcript, 3)->ivalue,
			     text);
  }

  JSON_delete_object(root);
}

/* void load_from_image(char *file_name) */
/* { */
/*   int i, j; */

/*   FILE *f=fopen(file_name,"rb");fseek(f,0,SEEK_END);long len=ftell(f);fseek(f,0,SEEK_SET); */
/*   char *data=(char*)malloc(len+1);fread(data,1,len,f);fclose(f); */

/*   cJSON *root = cJSON_Parse(data); */
/*   cJSON *heap = cJSON_GetObjectItem(root, "heap"); */

/*   free(data); */

/*   cJSON *temp; */

/*   temp = cJSON_GetObjectItem(root, "current_package"); */
/*   current_package = temp->valueint; */

/*   temp = cJSON_GetObjectItem(root, "gen_sym_count"); */
/*   gen_sym_count = temp->valueint; */

/*   temp = cJSON_GetObjectItem(root, "strings"); */
/*   nof_strings = cJSON_GetArraySize(temp); */

/*   strings = (char **)malloc(nof_strings * sizeof(char *)); */

/*   for(i=0; i<nof_strings; i++) */
/*   { */
/*     cJSON *strobj = cJSON_GetArrayItem(temp, i); */
/*     strings[i] = strdup(strobj->valuestring); */
/*   } */

/*   temp = cJSON_GetObjectItem(root, "foreign_libraries"); */
/*   nof_dl_handles = cJSON_GetArraySize(temp); */

/*   dl_handles = (void **)malloc(nof_dl_handles * sizeof(void *)); */

/*   for(i=0; i<nof_dl_handles; i++) */
/*   { */
/*     cJSON *strobj = cJSON_GetArrayItem(temp, i); */
/*     foreign_library_names[i] = strdup(strobj->valuestring); */
/*     dl_handles[i] = dlopen(strobj->valuestring, RTLD_LAZY); */
/*   } */

/*   temp = cJSON_GetObjectItem(root, "packages"); */
/*   nof_packages = cJSON_GetArraySize(temp); */

/*   packages = (package_t *)malloc(nof_packages * sizeof(package_t)); */

/*   for(i=0; i<nof_packages; i++) */
/*   { */
/*     cJSON *pkgobj = cJSON_GetArrayItem(temp, i); */

/*     cJSON *pkg_name = cJSON_GetObjectItem(pkgobj, "name"); */
/*     cJSON *symbols  = cJSON_GetObjectItem(pkgobj, "symbols"); */

/*     int nof_symbols = cJSON_GetArraySize(symbols); */

/*     packages[i].name = strdup(pkg_name->valuestring); */
/*     packages[i].nof_symbols = nof_symbols; */

/*     packages[i].symbols = (char **)malloc(packages[i].nof_symbols * sizeof(char *)); */

/*     for(j=0; j<nof_symbols; j++) */
/*     { */
/*       cJSON *symbol_obj = cJSON_GetArrayItem(symbols, j); */
/*       packages[i].symbols[j] = strdup(symbol_obj->valuestring); */
/*     } */
/*   } */

/*   hashtable_t *hashtable = hashtable_create(1001); */

/*   temp = cJSON_GetObjectItem(root, "debug_mode"); */
/*   debug_mode = strcmp(temp->valuestring, "true") ? false : true; */

/*   /\* temp = cJSON_GetObjectItem(root, "top_level_env"); *\/ */
/*   /\* top_level_env = convert_to_plisp_obj(root, heap, temp, hashtable); *\/ */

/*   /\* temp = cJSON_GetObjectItem(root, "debug_continuation"); *\/ */
/*   /\* debug_continuation = convert_to_plisp_obj(root, heap, temp, hashtable); *\/ */

/*   /\* temp = cJSON_GetObjectItem(root, "debug_env"); *\/ */
/*   /\* debug_env = convert_to_plisp_obj(root, heap, temp, hashtable); *\/ */

/*   /\* temp = cJSON_GetObjectItem(root, "debug_execution_stack"); *\/ */
/*   /\* debug_execution_stack = convert_to_plisp_obj(root, heap, temp, hashtable); *\/ */

/*   /\* temp = cJSON_GetObjectItem(root, "reg_accumulator"); *\/ */
/*   /\* reg_accumulator = convert_to_plisp_obj(root, heap, temp, hashtable); *\/ */

/*   /\* temp = cJSON_GetObjectItem(root, "reg_next_expression"); *\/ */
/*   /\* reg_next_expression = convert_to_plisp_obj(root, heap, temp, hashtable); *\/ */

/*   /\* temp = cJSON_GetObjectItem(root, "reg_current_env"); *\/ */
/*   /\* reg_current_env = convert_to_plisp_obj(root, heap, temp, hashtable); *\/ */

/*   /\* temp = cJSON_GetObjectItem(root, "reg_current_value_rib"); *\/ */
/*   /\* reg_current_value_rib = convert_to_plisp_obj(root, heap, temp, hashtable); *\/ */

/*   /\* temp = cJSON_GetObjectItem(root, "reg_current_stack"); *\/ */
/*   /\* reg_current_stack = convert_to_plisp_obj(root, heap, temp, hashtable); *\/ */

/*   queue_t *q = queue_create(); */

/*   top_level_env         = deserialize_internal(heap, cJSON_GetObjectItem(root, "top_level_env")->valueint,         hashtable, q); */
/*   debug_continuation    = deserialize_internal(heap, cJSON_GetObjectItem(root, "debug_continuation")->valueint,    hashtable, q); */
/*   debug_env             = deserialize_internal(heap, cJSON_GetObjectItem(root, "debug_env")->valueint,             hashtable, q); */
/*   debug_execution_stack = deserialize_internal(heap, cJSON_GetObjectItem(root, "debug_execution_stack")->valueint, hashtable, q); */
/*   reg_accumulator       = deserialize_internal(heap, cJSON_GetObjectItem(root, "reg_accumulator")->valueint,       hashtable, q); */
/*   reg_next_expression   = deserialize_internal(heap, cJSON_GetObjectItem(root, "reg_next_expression")->valueint,   hashtable, q); */
/*   reg_current_env       = deserialize_internal(heap, cJSON_GetObjectItem(root, "reg_current_env")->valueint,       hashtable, q); */
/*   reg_current_value_rib = deserialize_internal(heap, cJSON_GetObjectItem(root, "reg_current_value_rib")->valueint, hashtable, q); */
/*   reg_current_stack     = deserialize_internal(heap, cJSON_GetObjectItem(root, "reg_current_stack")->valueint,     hashtable, q); */

/*   convert_heap(heap, hashtable, q); */

/*   hashtable_delete(hashtable); */
/*   queue_delete(q); */

/*   cJSON_Delete(root); */
/* } */

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

void add_to_deserialization_queue(struct JSONObject *heap, queue_t *q, unsigned int ref, uintptr_t ptr, unsigned int index)
{
  struct slot *s = (struct slot *)malloc(sizeof(struct slot));
  s->ref = ref;
  s->ptr = ptr;
  s->index = index;
  queue_enqueue(q, s);
}

OBJECT_PTR deserialize_internal(struct JSONObject *heap, unsigned int ref, hashtable_t *ht, queue_t *q, BOOLEAN single_object)
{
  unsigned int object_type = ref & BIT_MASK;

  if(object_type == SYMBOL_TAG ||
     object_type == STRING_LITERAL_TAG ||
     object_type == CHAR_TAG)
  {
    if(single_object)
    {
      if(object_type == STRING_LITERAL_TAG)
        return get_string_object(JSON_get_array_item(heap, ref >> OBJECT_SHIFT)->strvalue);
      else if(object_type == SYMBOL_TAG)
      {
        char *symbol_name = JSON_get_array_item(heap, ref >> OBJECT_SHIFT)->strvalue;
        int colon_location = 0;
        int i;

        for(i=0; i<strlen(symbol_name); i++)
          if(symbol_name[i] == ':') { colon_location = i; break; }

        if(colon_location != 0) //qualified symbol_name
        {
          char *package_name = (char *)substring(symbol_name, 0, colon_location);
          char *sym_name = (char *)substring(symbol_name, colon_location + 1, strlen(symbol_name) - colon_location - 1);

          int package_index = find_package(package_name);

          if(package_index == NOT_FOUND)
          {
            create_package(package_name);
            packages[nof_packages-1].nof_symbols = 1;
            packages[nof_packages-1].symbols = (char **)malloc(sizeof(char *));
            packages[nof_packages-1].symbols[0] = strdup(sym_name);
          }            
            
          return cdr(get_qualified_symbol_object(package_name, sym_name));
        }
        else
          return get_symbol_object(symbol_name);
      }
      else
      {
        return ((JSON_get_array_item(heap, ref >> OBJECT_SHIFT)->ivalue) << OBJECT_SHIFT) + CHAR_TAG;
      }
    }
    else
      return (OBJECT_PTR)ref;
  }

  hashtable_entry_t *e = hashtable_get(ht, (void *)ref);

  if(e)
    return (OBJECT_PTR)e->value;

  struct JSONObject *heap_obj = JSON_get_array_item(heap, ref >> OBJECT_SHIFT);

  uintptr_t ptr;

  if(object_type == CONS_TAG)
  {
    ptr = object_alloc(2, CONS_TAG);

    unsigned int car_ref = JSON_get_array_item(heap_obj, 0)->ivalue;
    unsigned int cdr_ref = JSON_get_array_item(heap_obj, 1)->ivalue;

    if(is_dynamic_reference(car_ref))
    {
       hashtable_entry_t *e = hashtable_get(ht, (void *)car_ref);
      if(e)
        set_heap(ptr, 0, (OBJECT_PTR)(e->value));
      else
        add_to_deserialization_queue(heap, q, car_ref, ptr, 0);
    }
    else
      if(single_object) add_to_deserialization_queue(heap, q, car_ref, ptr, 0); else set_heap(ptr, 0, car_ref);

    if(is_dynamic_reference(cdr_ref))
    {
      hashtable_entry_t *e = hashtable_get(ht, (void *)cdr_ref);
      if(e)
        set_heap(ptr, 1, (OBJECT_PTR)(e->value));
      else
        add_to_deserialization_queue(heap, q, cdr_ref, ptr, 1);
    }
    else
      if(single_object) add_to_deserialization_queue(heap, q, cdr_ref, ptr, 1); else set_heap(ptr, 1, cdr_ref);

  }
  else if(object_type == ARRAY_TAG)
  {
    int i;
    int len = JSON_get_array_size(heap_obj);

    ptr = object_alloc(len + 1, ARRAY_TAG);

    (*(unsigned int *)ptr) = len;

    for(i=0; i<len; i++)
    {
      unsigned int elem_ref = JSON_get_array_item(heap_obj, i)->ivalue;
      if(is_dynamic_reference(elem_ref))
      {
        hashtable_entry_t *e = hashtable_get(ht, (void *)elem_ref);
        if(e)
          set_heap(ptr, i+1, (OBJECT_PTR)(e->value));
        else
          add_to_deserialization_queue(heap, q, elem_ref, ptr, i+1);
      }
      else
        if(single_object) add_to_deserialization_queue(heap, q, elem_ref, ptr, i+1); else set_heap(ptr, i+1, elem_ref);
    }
  }
  else if(object_type == CLOSURE_TAG || object_type == MACRO_TAG)
  {
    ptr = object_alloc(4, object_type);

    unsigned int env_ref = JSON_get_array_item(heap_obj, 0)->ivalue;
    if(is_dynamic_reference(env_ref))
    {
      hashtable_entry_t *e = hashtable_get(ht, (void *)env_ref);
      if(e)
        set_heap(ptr, 0, (OBJECT_PTR)(e->value));
      else
        add_to_deserialization_queue(heap, q, env_ref, ptr, 0);
    }
    else
      if(single_object) add_to_deserialization_queue(heap, q, env_ref, ptr, 0); else set_heap(ptr, 0, env_ref);

    unsigned int params_ref = JSON_get_array_item(heap_obj, 1)->ivalue;
    if(is_dynamic_reference(params_ref))
    {
      hashtable_entry_t *e = hashtable_get(ht, (void *)params_ref);
      if(e)
        set_heap(ptr, 1, (OBJECT_PTR)(e->value));
      else
        add_to_deserialization_queue(heap, q, params_ref, ptr, 1);
    }
    else
      if(single_object) add_to_deserialization_queue(heap, q, params_ref, ptr, 1); else set_heap(ptr, 1, params_ref);

    unsigned int body_ref = JSON_get_array_item(heap_obj, 2)->ivalue;
    if(is_dynamic_reference(body_ref))
    {
      hashtable_entry_t *e = hashtable_get(ht, (void *)body_ref);
      if(e)
        set_heap(ptr, 2, (OBJECT_PTR)(e->value));
      else
        add_to_deserialization_queue(heap, q, body_ref, ptr, 2);
    }
    else
      if(single_object) add_to_deserialization_queue(heap, q, body_ref, ptr, 2); else set_heap(ptr, 2, body_ref);

    unsigned int source_ref = JSON_get_array_item(heap_obj, 3)->ivalue;
    if(is_dynamic_reference(source_ref))
    {
      hashtable_entry_t *e = hashtable_get(ht, (void *)source_ref);
      if(e)
        set_heap(ptr, 3, (OBJECT_PTR)(e->value));
      else
        add_to_deserialization_queue(heap, q, source_ref, ptr, 3);
    }
    else
      if(single_object) add_to_deserialization_queue(heap, q, source_ref, ptr, 3); else set_heap(ptr, 3, source_ref);
  }
  else if(object_type == CONTINUATION_TAG)
  {
    ptr = object_alloc(1, CONTINUATION_TAG);

    unsigned int stack_ref = heap_obj->ivalue;

    if(is_dynamic_reference(stack_ref))
    {
      hashtable_entry_t *e = hashtable_get(ht, (void *)stack_ref);
      if(e)
        set_heap(ptr, 0, (OBJECT_PTR)(e->value));
      else
        add_to_deserialization_queue(heap, q, stack_ref, ptr, 0);
    }
    else
      if(single_object) add_to_deserialization_queue(heap, q, stack_ref, ptr, 0); else set_heap(ptr, 0, stack_ref);
  }
  else if(object_type == INTEGER_TAG)
  {
    ptr = object_alloc(1, INTEGER_TAG);
    (*(unsigned int *)ptr) = heap_obj->ivalue;
  }
  else if(object_type == FLOAT_TAG)
  {
    ptr = object_alloc(1, FLOAT_TAG);
    (*(float *)ptr) = heap_obj->fvalue;
  }

  hashtable_put(ht, (void *)ref, (void *)(ptr + object_type));

  return ptr + object_type;
}

void convert_heap(struct JSONObject *heap, hashtable_t *ht, queue_t *q, BOOLEAN single_object)
{
  while(!queue_is_empty(q))
  {
    struct slot *slot_obj = (struct slot *)queue_dequeue(q)->data;

    unsigned int ref = slot_obj->ref;

    hashtable_entry_t *e = hashtable_get(ht, (void *)ref);

    if(e)
      set_heap(slot_obj->ptr, slot_obj->index, (OBJECT_PTR)e->value);
    else
      set_heap(slot_obj->ptr, slot_obj->index, deserialize_internal(heap, ref, ht, q, single_object));

    free(slot_obj);
  }
}

int serialize(OBJECT_PTR obj, char *file_name)
{
  FILE *fp = fopen(file_name, "w");  

  if(!fp)
  {
    printf("Unable to open file %s\n", file_name);
    return -1;
  }

  unsigned int *obj_count = (unsigned int *)malloc(sizeof(unsigned int));

  *obj_count = 0;

  queue_t *print_queue = queue_create();
  hashtable_t *hashtable = hashtable_create(1001);
  hashtable_t *printed_objects = hashtable_create(1001);

  fprintf(fp, "{ ");
  fprintf(fp, "\"object\" : " ); 
  print_json_object(fp, obj, print_queue, obj_count, hashtable, printed_objects, true); 
  fprintf(fp, ", ");

  fprintf(fp, "\"heap\" : [");

  while(!queue_is_empty(print_queue))
  {
    OBJECT_PTR obj1 = (OBJECT_PTR)((queue_dequeue(print_queue))->data);
    print_heap_representation(fp, obj1, print_queue, obj_count, hashtable, printed_objects, true);
    if(!queue_is_empty(print_queue))fprintf(fp, ", ");
  }

  fprintf(fp, "]}");

  fclose(fp);

  free(obj_count);

  queue_delete(print_queue);
  hashtable_delete(hashtable);
  hashtable_delete(printed_objects);

  return 0;
}

int deserialize(char *file_name)
{
  struct JSONObject *root = JSON_parse(file_name);

  if(!root)
  {
    printf("Unable to parse file %s\n", file_name);
    return -1;
  }

  struct JSONObject *heap = JSON_get_object_item(root, "heap");

  if(!heap)
  {
    printf("No heap found in file\n");
    JSON_delete_object(root);
    return -1;
  }

  queue_t *q = queue_create();
  hashtable_t *hashtable = hashtable_create(1001);

  OBJECT_PTR object = deserialize_internal(heap, JSON_get_object_item(root, "object")->ivalue, hashtable, q, true);

  convert_heap(heap, hashtable, q, true);

  hashtable_delete(hashtable);
  queue_delete(q);

  JSON_delete_object(root);

  return object;

}
