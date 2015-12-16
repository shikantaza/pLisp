/**
  Copyright 2011-2015 Rajesh Jayaprakash <rajesh.jayaprakash@gmail.com>

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
#include <string.h>
#include <stdlib.h>

#include "../src/json.h"

typedef struct help_entry
{
  unsigned int type;
  char *name;
  char *syntax;
  char *args;
  char *desc;
  char *exceptions;
  unsigned int examples_count;
  char **examples;
  unsigned int see_also_count;
  char **see_also;
} help_entry_t;

enum {FUNCTION, MACRO, SPECIAL_OPERATOR};

help_entry_t *help_entries = NULL;
unsigned int nof_help_entries = 0;

int build_help_entries(char *file_name)
{
  struct JSONObject *root = JSON_parse(file_name);

  if(!root)
  {
    printf("Unable to parse file %s\n", file_name);
    return 1;
  }

  struct JSONObject *entries = JSON_get_object_item(root, "entries");

  if(!entries)
  {
    printf("EXCEPTION", "No entries found in file");
    JSON_delete_object(root);
    return 1;
  }
  
  nof_help_entries = JSON_get_array_size(entries);

  help_entries = (help_entry_t *)malloc(nof_help_entries * sizeof(help_entry_t));

  if(!help_entries)
  {
    printf("EXCEPTION", "Unable to allocate memory for help entries");
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
      printf("EXCEPTION", "Invalid help entry type");
      JSON_delete_object(root);
      free(help_entries);
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
      help_entries[i].examples = (char **)malloc(help_entries[i].examples_count * sizeof(char *));

      if(!help_entries[i].examples)
      {
        printf("EXCEPTION", "Unable to allocate memory for examples");
        JSON_delete_object(root);
        free(help_entries);
        return 1;      
      }
    }

    for(j=0; j<help_entries[i].examples_count; j++)
      help_entries[i].examples[j] = strdup(JSON_get_array_item(JSON_get_object_item(entry, "examples"), j)->strvalue);

    help_entries[i].see_also_count = JSON_get_array_size(JSON_get_object_item(entry, "see-also"));

    if(help_entries[i].see_also_count > 0)
    {
      help_entries[i].see_also = (char **)malloc(help_entries[i].see_also_count * sizeof(char *));

      if(!help_entries[i].see_also)
      {
        printf("EXCEPTION", "Unable to allocate memory for see-also-count");
        JSON_delete_object(root);
        free(help_entries);
        return 1;      
      }
    }

    for(j=0; j<help_entries[i].see_also_count; j++)
      help_entries[i].see_also[j] = strdup(JSON_get_array_item(JSON_get_object_item(entry, "see-also"), j)->strvalue);
  }

  JSON_delete_object(root);

  return 0;
}

void cleanup_help_entries()
{
  int i,j;

  for(i=0; i<nof_help_entries; i++)
  {
    free(help_entries[i].name);
    free(help_entries[i].syntax);
    free(help_entries[i].args);
    free(help_entries[i].desc);
    free(help_entries[i].exceptions);

    for(j=0; j<help_entries[i].examples_count; j++)
      free(help_entries[i].examples[j]);

    if(help_entries[i].examples_count > 0)
      free(help_entries[i].examples);

    for(j=0; j<help_entries[i].see_also_count; j++)
      free(help_entries[i].see_also[j]);

    if(help_entries[i].see_also_count > 0)
      free(help_entries[i].see_also);
  }

  free(help_entries);
}

int main(int argc, char **argv)
{
  char *s = NULL;

  if(argc == 1)
    s = "doc/help.html";
  else
    strcpy(s, argv[1]);

  FILE *out = fopen(s, "w");

  if(!out)
  {
    printf("Unable to open file %s\n", s);
    if(argc > 1)free(s);
    exit(1);
  }

  fprintf(out,"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\">\n");
  fprintf(out, "<html>\n");
  fprintf(out, "<head></head>");
  fprintf(out, "<body>\n");
  fprintf(out, "<center><h1>pLisp Language Reference</h1></center><br/>");

  if(build_help_entries("doc/help.json"))
  {
    if(argc > 1)free(s);
    fclose(out);
    exit(1);
  }

  int i, j, k;

  char prev_entry_start = ' ';

  for(i=0; i<nof_help_entries; i++)
  {
    if(help_entries[i].name[0] >= 97 && help_entries[i].name[0] <= 122)
    {
      if(help_entries[i].name[0] != prev_entry_start)
        fprintf(out, "<b><a href=\"#%c\">%c</a> | </b>\n", help_entries[i].name[0] - 32, help_entries[i].name[0] - 32);

      prev_entry_start = help_entries[i].name[0];
    }
  }

  fprintf(out, "<b><a href=\"#Non-Alphabetic\">Non-Alphabetic</a></b>");

  fprintf(out, "<br/><br/>");

  prev_entry_start = ' ';

  for(i=0; i<nof_help_entries; i++)
  {
    if(help_entries[i].name[0] >= 97 && help_entries[i].name[0] <= 122)
    {
      if(help_entries[i].name[0] != prev_entry_start)
      {
        if(prev_entry_start != ' ')
          fprintf(out, "</ul>\n");
        fprintf(out, "<br/><b><a name=\"%c\">%c</a></b><br/>\n", help_entries[i].name[0] - 32, help_entries[i].name[0] - 32);
        fprintf(out, "<ul style=\"padding-left:40px\">\n");
      }

      fprintf(out, "<li><a href=\"#%s\">%s</a></li><br/>\n", help_entries[i].name, help_entries[i].name);

      prev_entry_start = help_entries[i].name[0];
    }
  }

  fprintf(out, "</ul>\n");

  fprintf(out, "<br/><b><a name=\"Non-Alphabetic\">Non-Alphabetic</a></b><br/>\n");

  fprintf(out, "<ul style=\"padding-left:40px\">\n");

  for(i=0; i<nof_help_entries; i++)
  {
    if(help_entries[i].name[0] < 97 || help_entries[i].name[0] > 122)
      fprintf(out, "<li><a href=\"#%s\">%s</a></li><br/>\n", help_entries[i].name, help_entries[i].name);
  }

  fprintf(out, "</ul>\n");

  fprintf(out, "<hr/>\n");

  for(i=0; i<nof_help_entries; i++)
  {
    fprintf(out, "<a name=\"%s\"></a>\n", help_entries[i].name);

    if(help_entries[i].type == FUNCTION)
      fprintf(out, "<i>Function</i>");
    else if(help_entries[i].type == MACRO)
      fprintf(out, "<i>Macro</i>");
    else if(help_entries[i].type == SPECIAL_OPERATOR)
      fprintf(out, "<i>Special Operator</i>");

    fprintf(out, "<b> %s</b><br/><br/>\n", help_entries[i].name);

    fprintf(out, "<b>Syntax:</b> %s<br/><br/>\n", help_entries[i].syntax);
    fprintf(out, "<b>Arguments:</b> %s<br/><br/>\n", help_entries[i].args);

    fprintf(out, "<b>Description: ");

    for(j=0; help_entries[i].desc[j] != ' '; j++)
      fprintf(out, "%c", help_entries[i].desc[j]);

    fprintf(out, "</b>");

    int first_space = j;

    int toggle = 0;

    for(j=first_space; j<strlen(help_entries[i].desc); j++)
    {
      char c = help_entries[i].desc[j];
      if(c == '\'')
      {
        if(toggle == 0)
          fprintf(out, "<i>");
        else
          fprintf(out, "</i>");
        toggle = !toggle;
      }
      else
        fprintf(out, "%c", help_entries[i].desc[j]);
    }

    fprintf(out, "<br/><br/>\n");

    fprintf(out, "<b>Exceptions: </b>");

    toggle = 0;

    for(j=0; j<strlen(help_entries[i].exceptions); j++)
    {
      char c = help_entries[i].exceptions[j];
      if(c == '\'')
      {
        if(toggle == 0)
          fprintf(out, "<i>");
        else
          fprintf(out, "</i>");
        toggle = !toggle;
      }
      else
        fprintf(out, "%c", help_entries[i].exceptions[j]);
    }

    fprintf(out, "<br/><br/>\n");

    fprintf(out, "<b>Examples:</b><br/><br/>\n");

    for(j=0; j<help_entries[i].examples_count; j++)
    {
      if(strlen(help_entries[i].examples[j]) == 0)
        continue;

      for(k=0; k<strlen(help_entries[i].examples[j])-1; k++)
      {
        if(help_entries[i].examples[j][k] == '\\' && (help_entries[i].examples[j][k+1] == '"' || help_entries[i].examples[j][k+1] == '\\'))
          continue;
        else
          fprintf(out, "%c", help_entries[i].examples[j][k]);
      }
      fprintf(out, "%c", help_entries[i].examples[j][k]);

      fprintf(out, "<br/><br/>");
    }

    fprintf(out, "<b>See Also:</b> \n");

    int first_time = 1;

    for(j=0; j<help_entries[i].see_also_count; j++)
    {
      if(!first_time)
        fprintf(out, ", ");

      fprintf(out, "<a href=\"#%s\">%s</a>", help_entries[i].see_also[j], help_entries[i].see_also[j]);

      first_time = 0;
    }

    fprintf(out, "<hr/>\n");
  }

  fprintf(out, "</body>\n");
  fprintf(out, "</html>\n");
  cleanup_help_entries();

  fclose(out);

  return 0;
}
