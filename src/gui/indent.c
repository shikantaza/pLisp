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

#include <gtk/gtk.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

#include "../plisp.h"

#define FORM     0
#define LET      1
#define DEFUN    2
#define IF       3
#define WHILE    4
#define DEFMACRO 5
#define PROGN    6
#define LAMBDA   7
#define MACRO    8
#define LET1     9
#define DOLIST   10
#define DOTIMES  11
#define LETREC   12

/* determinant form = the characters from the leftmost open paren
   to the current cursor position */

//structure that contains the constituent forms in a determinant
//form. e.g. 
//     '(x 10' breaks down into [(FORM, 1), (FORM, 3)]
//     '(let ((x 10))' breaks down to [(LET, 1), (FORM, 5]
//     '(defun (f)' breaks down to [(DEFUN, 1), (FORM, 7)]
typedef struct form_position
{
  unsigned int form_type;
  unsigned int pos;
} form_position_t;

extern in_code(GtkTextBuffer *, GtkTextIter *);

form_position_t *convert_to_form_position(char *str, unsigned int *count)
{
  form_position_t *fp = NULL, *temp;

  BOOLEAN in_string_literal = false;
  BOOLEAN in_single_line_comment = false;
  BOOLEAN in_multi_line_comment = false;
  BOOLEAN in_compound_form = false;

  BOOLEAN new_form_encountered = false;

  BOOLEAN newline_char = false;

  unsigned int i=1;
  unsigned int len = strlen(str);

  unsigned int form_type;
  unsigned int pos;

  unsigned int imbalance = 0;

  unsigned int corrected_i=1;

  *count = 0;

  while(i < len)
  {
    //need to handle these: newline space #| |# ; ( ) "

    char c = str[i];

    if(c == '\n')
    {
      if(!in_string_literal && !in_multi_line_comment)
      {
        in_single_line_comment = false;
        corrected_i = 0;
        i++;
      }
      else
      {
        i++;
        corrected_i = i;
      }

      newline_char = true;
    }
    else if(c == '#')
    {
      if((i <= len-2)            && 
         str[i+1] == '|'        &&
         !in_string_literal      &&
         !in_single_line_comment &&
         !in_multi_line_comment)
        in_multi_line_comment = true;
      else if(i > 0                   &&
              str[i-1] == '|'        &&
              !in_string_literal      &&
              !in_single_line_comment &&
              in_multi_line_comment)
        in_multi_line_comment = false;      

      i++;
      corrected_i++;

      newline_char = false;
    }
    else if(c == ';')
    {
      if(!in_string_literal && !in_multi_line_comment)
        in_single_line_comment = true;

      i++;
      corrected_i++;

      newline_char = false;
    }
    else if(c == '"')
    {
      if(in_string_literal)
        in_string_literal = false;
      else if(!in_single_line_comment && !in_multi_line_comment && !in_compound_form)
      {
        in_string_literal = true;

        //capture form and pos
        form_type = FORM;
        pos = corrected_i;

        new_form_encountered = true;
      }

      i++;
      corrected_i++;

      newline_char = false;
    }
    else if(c == '(')
    {
      if(!in_string_literal && !in_single_line_comment && !in_multi_line_comment)
      {
        if(imbalance == 0)
        {
          in_compound_form = true;

          //capture form and pos
          form_type = FORM;

          //if(i != corrected_i)
          if(corrected_i == 0)
            pos = fp[(*count)-1].pos;
          else
            pos = corrected_i;

          new_form_encountered = true;
        }
        imbalance++;
      }
      i++;
      corrected_i++;

      newline_char = false;
    }
    else if(c == ')')
    {
      if(!in_string_literal && !in_single_line_comment && !in_multi_line_comment)
      {
        imbalance--;
        if(imbalance == 0)
          in_compound_form = false;
      }
      i++;
      corrected_i++;

      newline_char = false;
    }
    else if(c != ' ') //all other characters except space
    {
      if(!in_string_literal      && 
         !in_single_line_comment &&
         !in_multi_line_comment  &&
         !in_compound_form)
      {
        if(i == 1 ||
           (i > 1 && (str[i-1] == ' ' || str[i-1] == '\n' || str[i-1] == '"' || str[i-1] == '#')))
        {
          //capture form and pos -- need to capture the full form here, to
          //get keywords (LET, DEFUN, IF, WHILE, ...)

          if(corrected_i == 0)
            pos = fp[(*count)-1].pos;
          else
            pos = corrected_i;

          while(str[i] != ' ' && str[i] != '\n' && str[i] != '(' && str[i] != '"' && str[i] != ';' && str[i] != '#' && i < len)
          {
            i++;
            corrected_i++;
          }

          char *tempstr = GC_MALLOC((i-pos+1) * sizeof(char));
          memset(tempstr, '\0', i-pos+1);
      
          strncpy(tempstr, str+pos, i-pos);
      
          convert_to_upper_case(tempstr);

          if(!strncmp(tempstr, "LET", (i-pos)))
            form_type = LET;
          else if(!strncmp(tempstr, "DEFUN", (i-pos)))
            form_type = DEFUN;
          else if(!strncmp(tempstr, "IF", (i-pos)))
            form_type = IF;
          else if(!strncmp(tempstr, "WHILE", (i-pos)))
            form_type = WHILE;
          else if(!strncmp(tempstr, "DEFMACRO", (i-pos)))
            form_type = DEFMACRO;
          else if(!strncmp(tempstr, "PROGN", (i-pos)))
            form_type = PROGN;
          else if(!strncmp(tempstr, "LAMBDA", (i-pos)))
            form_type = LAMBDA;
          else if(!strncmp(tempstr, "MACRO", (i-pos)))
            form_type = MACRO;
          else if(!strncmp(tempstr, "LET*", (i-pos)))
            form_type = LET1;
          else if(!strncmp(tempstr, "DOLIST", (i-pos)))
            form_type = DOLIST;
          else if(!strncmp(tempstr, "DOTIMES", (i-pos)))
            form_type = DOTIMES;
          else if(!strncmp(tempstr, "LETREC", (i-pos)))
            form_type = LETREC;
          else
            form_type = FORM;

          //free(tempstr);

          new_form_encountered = true;
        }
      }
      else
      {
        i++;
        corrected_i++;
      }

      newline_char = false;
    }
    else
    {
      assert(c == ' ');
      i++;
      if(!newline_char)
        corrected_i++;
    }

    if(new_form_encountered)
    {
      (*count)++;
      if(!fp)
        fp = (form_position_t *)GC_MALLOC(sizeof(form_position_t));
      else
      {
        temp = GC_REALLOC(fp, (*count) * (sizeof(form_position_t)));
        fp = temp;
      }

      fp[(*count)-1].form_type = form_type;
      fp[(*count)-1].pos       = pos;
    }
    new_form_encountered = false;

  } //end of while(i < len)

  /* for(i=0; i< *count; i++) */
  /*   printf("%d %d\n", fp[i].form_type, fp[i].pos); */
  /* printf("---\n"); */

  return fp;
}

//convert the string representation of the determinant form
//into form_position_t values
form_position_t *convert_to_form_position_old(char *str, unsigned int *count)
{
  printf("%s\n", str);

  form_position_t *fp = NULL, *temp;
  int index=1, i;

  *count = 0;

  BOOLEAN newline_encountered_before_id_start;
  BOOLEAN in_string_literal = false;

  while(index < strlen(str))
  {
    //if in the middle of a string literal,
    //simply consume the characters
    if(str[index] != '"' && in_string_literal == true)
    {
      index++;
      continue;
    }

    if(str[index] == '"')
      in_string_literal = !in_string_literal;

    if(str[index] != ' ' && str[index] != '\n' && str[index] != '(')
    {

      if(newline_encountered_before_id_start == true)
      {
        if((*count == 0))
          i = 1;
        else
          i = fp[(*count)-1].pos;
      }
      else
        i = index;

      newline_encountered_before_id_start = false;

      (*count)++;
      if(!fp)
        fp = (form_position_t *)GC_MALLOC(sizeof(form_position_t));
      else
      {
        temp = GC_REALLOC(fp, (*count) * (sizeof(form_position_t)));
        fp = temp;
      }

      while(str[index] != ' ' && str[index] != '\n' && str[index] != '(' && index < strlen(str))
        index++;

      char *tempstr = GC_MALLOC((index-i+1) * sizeof(char));
      memset(tempstr, '\0', index-i+1);
      
      strncpy(tempstr, str+i, index-i);
      
      convert_to_upper_case(tempstr);

      if(!strncmp(tempstr, "LET", (index-i)))
        fp[(*count)-1].form_type = LET;
      else if(!strncmp(tempstr, "DEFUN", (index-i)))
        fp[(*count)-1].form_type = DEFUN;
      else if(!strncmp(tempstr, "IF", (index-i)))
        fp[(*count)-1].form_type = IF;
      else if(!strncmp(tempstr, "WHILE", (index-i)))
        fp[(*count)-1].form_type = WHILE;
      else if(!strncmp(tempstr, "LETREC", (index-i)))
        fp[(*count)-1].form_type = LETREC;
      else
        fp[(*count)-1].form_type = FORM;
        
      fp[(*count)-1].pos = i;

      //free(tempstr);
    }
    else if(str[index] == '(' && !in_string_literal)
    {
      i = index;

      (*count)++;
      if(!fp)
        fp = (form_position_t *)GC_MALLOC(sizeof(form_position_t));
      else
      {
        temp = GC_REALLOC(fp, (*count) * (sizeof(form_position_t)));
        fp = temp;
      }

      int imbalance = 1;
      index++;

      //while(index < strlen(str))
      while(imbalance != 0)
      {
        if(str[index] == ')')
          imbalance--;
        else if(str[index] == '(')
          imbalance++;

        if(imbalance == 0 && str[index] == '\n')
          newline_encountered_before_id_start = true;

        index++;
      }

      fp[(*count)-1].form_type = FORM;

      if(newline_encountered_before_id_start == true)
        fp[(*count)-1].pos = fp[(*count)-2].pos;
      else
        fp[(*count)-1].pos = i;

    }
    else
    {
      if(str[index] == '\n')
        newline_encountered_before_id_start = true;
      index++;
    }
  }

  for(i=0; i< *count; i++)
    printf("%d %d\n", fp[i].form_type, fp[i].pos);
  printf("---\n");

  return fp;
}

//returns the number of indents that should be printed for the
//given determinant form
unsigned int get_indent_count(form_position_t *fp, unsigned int count)
{
  if(count == 0)
    return 1;

  if(fp[0].form_type == LET || fp[0].form_type == LET1 || fp[0].form_type == LETREC)
  {
    if(count == 1)
      return 5;
    else if(count >= 2)
      return 2;
    /* else */
    /*   return fp[1].pos; */
  }
  else if(fp[0].form_type == DEFUN    || 
          fp[0].form_type == DEFMACRO ||
          fp[0].form_type == LAMBDA   ||
          fp[0].form_type == MACRO)
    return 2;
  else if(fp[0].form_type == IF)
  {
    if(count == 1)
      return 4;
    else if(count == 2)
      return 4;
    else
      return 2;
  }
  else if(fp[0].form_type == WHILE  || 
          fp[0].form_type == DOLIST ||
          fp[0].form_type == DOTIMES)
  {
    if(count == 1)
      return 4;
    else
      return 2;
  }
  else if(fp[0].form_type == PROGN)
  {
    if(count == 1)
      return 2;
    else
      return fp[1].pos;
  }
  else //other forms
  {
    if(count == 1)
      return fp[0].pos;
    else
      return fp[1].pos;
  }
}

//indent the current line in the buffer by index_count
//(handles presence of already existing indents)
void introduce_indents(GtkTextBuffer *buffer, unsigned int indent_count)
{
  GtkTextIter iter,line_start_iter, line_end_iter;

  //get the iter at the current location
  gtk_text_buffer_get_iter_at_mark(buffer, &iter, gtk_text_buffer_get_insert(buffer));

  //get the line number for the current location
  gint line_number = gtk_text_iter_get_line(&iter);

  //get the start iter for the current line
  gtk_text_buffer_get_iter_at_line(buffer, &line_start_iter, line_number);

  gint line_count = gtk_text_buffer_get_line_count(buffer);

  if(line_number < line_count - 1) //not the last line
    gtk_text_buffer_get_iter_at_line(buffer, &line_end_iter, line_number+1);
  else
    gtk_text_buffer_get_end_iter(buffer, &line_end_iter);

  //get the full text for the line
  gchar *text = gtk_text_buffer_get_text(buffer, &line_start_iter, &line_end_iter, FALSE);

  unsigned int existing_indent_count = 0;
  int i=0;

  while(i<strlen(text) && text[i] == ' ')
  {
    existing_indent_count++;
    i++;
  }

  //remove existing indents
  gtk_text_buffer_get_iter_at_line_offset(buffer, &iter, line_number, existing_indent_count);
  gtk_text_buffer_delete(buffer, &line_start_iter, &iter);

  //re-get the start iter for the current line
  gtk_text_buffer_get_iter_at_line(buffer, &line_start_iter, line_number);

  //add required indents
  for(i=0; i<indent_count; i++)
    gtk_text_buffer_insert(buffer, &line_start_iter, " ", -1);
}

//indent the current line in the buffer
//according to the indentation rules
void indent(GtkTextBuffer *buffer)
{
  GtkTextIter iter, line_start;
  gtk_text_buffer_get_iter_at_mark(buffer, &iter, gtk_text_buffer_get_insert(buffer));
  gint line_number = gtk_text_iter_get_line(&iter);

  //we are at the first line, so no
  //indentation needed
  if(line_number == 0)
    return;

  GtkTextIter curr_iter;
  GtkTextIter start_match, end_match;

  //get the iter at the beginning of the current line
  gtk_text_buffer_get_iter_at_line(buffer, &curr_iter, line_number);

  GtkTextIter saved_iter = curr_iter;

  gchar *str;

  while(1)
  {
    if(gtk_text_iter_backward_search(&curr_iter, 
                                     "(", 
                                     GTK_TEXT_SEARCH_TEXT_ONLY | GTK_TEXT_SEARCH_VISIBLE_ONLY, 
                                     &start_match,
                                     &end_match, 
                                     NULL))
    {

      str = gtk_text_buffer_get_text(buffer, &start_match, &saved_iter, FALSE);

      if(no_unmatched_left_parens(str) || !in_code(buffer, &start_match))
      {
        //continue searching
        int offset = gtk_text_iter_get_offset(&start_match);
        gtk_text_buffer_get_iter_at_offset(buffer,
                                           &curr_iter, 
                                           offset);
      }
      else
      {
        //there is an unmatched '(', so we're done
        curr_iter = start_match;
	break;
      }
    }
    else
    {
      //there are no more left parens left
      return;
      //break;
    }
  }

  //get the line number corresponding to the unmatched '('
  gint line_no = gtk_text_iter_get_line(&curr_iter);

  //get the start iter for this line
  GtkTextIter line_start_iter;
  gtk_text_buffer_get_iter_at_line(buffer, &line_start_iter, line_no);

  //get the text between the line_star_iter and current_iter
  gchar *text = gtk_text_buffer_get_text(buffer, &line_start_iter, &curr_iter, FALSE);

  unsigned int carried_over_indents = strlen(text);

  unsigned int count;

  form_position_t *fp = convert_to_form_position(str, &count);

  introduce_indents(buffer, carried_over_indents + get_indent_count(fp, count));

  g_free(text);
  g_free(str);
  //free(fp);
}

