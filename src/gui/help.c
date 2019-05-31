/**
  Copyright 2011-2019 Rajesh Jayaprakash <rajesh.jayaprakash@gmail.com>

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

#include "../plisp.h"
#include "../json.h"

enum {FUNCTION, MACRO, SPECIAL_OPERATOR};

help_entry_t *help_entries = NULL;
unsigned int nof_help_entries = 0;

int build_help_entries(char *file_name)
{
  struct JSONObject *root = JSON_parse(file_name);

  if(!root)
  {
    throw_exception1("Unable to parse file %s\n", file_name);
    return 1;
  }

  struct JSONObject *entries = JSON_get_object_item(root, "entries");

  if(!entries)
  {
    throw_exception1("EXCEPTION", "No entries found in file");
    JSON_delete_object(root);
    return 1;
  }
  
  nof_help_entries = JSON_get_array_size(entries);

  help_entries = (help_entry_t *)GC_MALLOC(nof_help_entries * sizeof(help_entry_t));

  if(!help_entries)
  {
    throw_exception1("EXCEPTION", "Unable to allocate memory for help entries");
    JSON_delete_object(root);
    return 1;
  }

  int i,j;

  for(i=0; i<nof_help_entries; i++)
  {
    struct JSONObject *entry = JSON_get_array_item(entries, i);

    unsigned int type = JSON_get_object_item(entry, "type")->ivalue;

    if(type != FUNCTION && type != MACRO && type != SPECIAL_OPERATOR)
    {
      throw_exception1("EXCEPTION", "Invalid help entry type");
      JSON_delete_object(root);
      //free(help_entries);
      return 1;      
    }
    
    help_entries[i].type       = type;
    help_entries[i].name       = strdup(JSON_get_object_item(entry, "name")->strvalue);
    help_entries[i].syntax     = strdup(JSON_get_object_item(entry, "syntax")->strvalue);
    help_entries[i].args       = strdup(JSON_get_object_item(entry, "args")->strvalue);
    help_entries[i].desc       = strdup(JSON_get_object_item(entry, "desc")->strvalue);
    help_entries[i].exceptions = strdup(JSON_get_object_item(entry, "exceptions")->strvalue);


    help_entries[i].examples_count = JSON_get_array_size(JSON_get_object_item(entry, "examples"));

    if(help_entries[i].examples_count > 0)
    {
      help_entries[i].examples = (char **)GC_MALLOC(help_entries[i].examples_count * sizeof(char *));

      if(!help_entries[i].examples)
      {
        throw_exception1("EXCEPTION", "Unable to allocate memory for examples");
        JSON_delete_object(root);
        //free(help_entries);
        return 1;      
      }
    }

    for(j=0; j<help_entries[i].examples_count; j++)
      help_entries[i].examples[j] = strdup(JSON_get_array_item(JSON_get_object_item(entry, "examples"), j)->strvalue);

    help_entries[i].see_also_count = JSON_get_array_size(JSON_get_object_item(entry, "see-also"));

    if(help_entries[i].see_also_count > 0)
    {
      help_entries[i].see_also = (char **)GC_MALLOC(help_entries[i].see_also_count * sizeof(char *));

      if(!help_entries[i].see_also)
      {
        throw_exception1("EXCEPTION", "Unable to allocate memory for see-also-count");
        JSON_delete_object(root);
        //free(help_entries);
        return 1;      
      }
    }

    for(j=0; j<help_entries[i].see_also_count; j++)
      help_entries[i].see_also[j] = strdup(JSON_get_array_item(JSON_get_object_item(entry, "see-also"), j)->strvalue);
  }

  JSON_delete_object(root);

  return 0;
}

help_entry_t *find_help_entry(char *name)
{
  int i;

  for(i=0; i<nof_help_entries; i++)
  {
    if(!strcmp(name, help_entries[i].name))
      return help_entries+i;
  }

  return NULL;
}

void cleanup_help_entries()
{
  /* int i,j; */

  /* for(i=0; i<nof_help_entries; i++) */
  /* { */
  /*   free(help_entries[i].name); */
  /*   free(help_entries[i].syntax); */
  /*   free(help_entries[i].args); */
  /*   free(help_entries[i].desc); */
  /*   free(help_entries[i].exceptions); */

  /*   for(j=0; j<help_entries[i].examples_count; j++) */
  /*     free(help_entries[i].examples[j]); */

  /*   if(help_entries[i].examples_count > 0) */
  /*     free(help_entries[i].examples); */

  /*   for(j=0; j<help_entries[i].see_also_count; j++) */
  /*     free(help_entries[i].see_also[j]); */

  /*   if(help_entries[i].see_also_count > 0) */
  /*     free(help_entries[i].see_also); */
  /* } */

  /* free(help_entries); */
}
