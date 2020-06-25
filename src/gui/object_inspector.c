/**
  Copyright 2011-2020 Rajesh Jayaprakash <rajesh.jayaprakash@gmail.com>

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
#include <gdk/gdkkeysyms.h>
#include <stdlib.h>
#include <string.h>

#include "../plisp.h"
#include "../util.h"
#include "../memory.h"

void create_object_inspector_window(OBJECT_PTR);

//extern void set_triggering_window(GtkWidget *, gpointer);
extern void show_error_dialog(char *);
extern int print_object_to_string(OBJECT_PTR, char *, int);
extern void close_application_window(GtkWidget **);
extern OBJECT_PTR prim_get_source(OBJECT_PTR);
extern char *get_current_word(GtkTextBuffer *);
extern BOOLEAN is_valid_object(OBJECT_PTR);
extern void print_object(OBJECT_PTR);

extern OBJECT_PTR NIL;

#ifdef __OSX_BUNDLE__
extern char exec_path[512];
extern char path_buf[1024];
#endif

//GtkWindow *object_inspector_window = NULL;
//GtkTextBuffer *object_inspector_buffer = NULL;

gboolean delete_obj_inspector( GtkWidget *widget,
                               GdkEvent  *event,
                               gpointer   data )
{
  close_application_window(&widget);
}

gboolean handle_obj_insp_key_press_events(GtkWidget *widget, GdkEventKey *event, gpointer user_data)
{
  if(event->keyval == GDK_KEY_F3)
  {
    GtkTextBuffer *buffer = (GtkTextBuffer *)user_data;
    
    char *str = get_current_word(buffer);
    char *ptr_str = substring(str, 1, strlen(str)-2);
    OBJECT_PTR obj = (OBJECT_PTR)strtol(ptr_str, NULL, 16);
    //free(ptr_str);

    if(is_valid_object(obj))
      create_object_inspector_window(obj);
  }
  else if(event->keyval == GDK_KEY_Escape)
    close_application_window(&widget);
  
  return TRUE;
}

int print_array_object_to_string_for_insp(OBJECT_PTR array, char *buf, int filled_buf_len)
{
  int len = 0;

  uintptr_t ptr = extract_ptr(array);

  //int length = get_int_value(get_heap(ptr, 0));
  int length = *((OBJECT_PTR *)ptr);

  int i;

  len += sprintf(buf+filled_buf_len, "[");

  for(i=0; i< length; i++)
  {
    OBJECT_PTR obj = get_heap(ptr, i + 1);
    if(is_atom(obj))
      len += print_object_to_string(obj, buf, filled_buf_len+len);
    else
      len += sprintf(buf+filled_buf_len+len, "<%p>", obj);
    
    len += sprintf(buf+filled_buf_len+len, " ");
  }

  if(length > 0)
    len += sprintf(buf+filled_buf_len+len-1, "]") - 1;
  else
    len += sprintf(buf+filled_buf_len+len, "]");

  return len;
}

int print_cons_object_to_string_for_insp(OBJECT_PTR obj, char *buf, int filled_buf_len)
{
  int len = 0;
  
  OBJECT_PTR rest = obj;

  len += sprintf(buf+filled_buf_len+len, "(");
  
  while(rest != NIL && 
        !(IS_ARRAY_OBJECT(rest)        || 
          is_atom(rest)                || 
          IS_CLOSURE_OBJECT(rest)      || 
          IS_MACRO_OBJECT(rest)        || 
          IS_CONTINUATION_OBJECT(rest) ||
          IS_FUNCTION2_OBJECT(rest)    ||
          IS_MACRO2_OBJECT(rest)))
  {
    if(is_atom(car(rest)))
      len += print_object_to_string(car(rest), buf, filled_buf_len+len);
    else
      len += sprintf(buf+filled_buf_len+len, "<%p>", car(rest));
    len += sprintf(buf+filled_buf_len+len, " ");
    rest = cdr(rest);
  }

  if((IS_ARRAY_OBJECT(rest)        || 
      is_atom(rest)                || 
      IS_CLOSURE_OBJECT(rest)      || 
      IS_MACRO_OBJECT(rest)        || 
      IS_CONTINUATION_OBJECT(rest) ||
      IS_FUNCTION2_OBJECT(rest)    ||
      IS_MACRO2_OBJECT(rest)) 
     && rest != NIL)
  {
    len += sprintf(buf+filled_buf_len+len, ". ");
    if(is_atom(rest))
      len += print_object_to_string(rest, buf, filled_buf_len+len);
    else
      len += sprintf(buf+filled_buf_len+len, "<%p>", rest);
    len += sprintf(buf+filled_buf_len+len, ")");
  }
  else
    len += sprintf(buf+filled_buf_len+len-1, ")") - 1;

  return len;
}

void apply_link_tag(GtkTextBuffer *object_inspector_buffer)
{
  GtkTextIter curr_iter;
  GtkTextIter start_match, end_match;

  gtk_text_buffer_get_start_iter(object_inspector_buffer, &curr_iter);
  
  while(1)
  {
#ifdef WIN32
    if(gtk_text_iter_forward_search(&curr_iter, 
                                     "<0", 
                                     GTK_TEXT_SEARCH_TEXT_ONLY | GTK_TEXT_SEARCH_VISIBLE_ONLY, 
                                     &start_match,
                                     &end_match, 
                                     NULL))
#else
    if(gtk_text_iter_forward_search(&curr_iter, 
                                     "<0x", 
                                     GTK_TEXT_SEARCH_TEXT_ONLY | GTK_TEXT_SEARCH_VISIBLE_ONLY, 
                                     &start_match,
                                     &end_match, 
                                     NULL))
#endif
    {
      GtkTextIter start_match1;
      
      gtk_text_iter_forward_search(&end_match, 
                                   ">", 
                                   GTK_TEXT_SEARCH_TEXT_ONLY | GTK_TEXT_SEARCH_VISIBLE_ONLY, 
                                   &start_match1,
                                   &end_match, 
                                   NULL);
      
      gtk_text_buffer_apply_tag_by_name(object_inspector_buffer, "bold_blue_foreground", &start_match, &end_match);
      curr_iter = end_match;
    }
    else
      break;
  }
}

void print_object_to_inspector(OBJECT_PTR obj, GtkTextBuffer *object_inspector_buffer)
{
  gtk_text_buffer_insert_at_cursor(object_inspector_buffer, "", -1);

  char buf[MAX_STRING_LENGTH];

  memset(buf, '\0', MAX_STRING_LENGTH);

  GtkTextIter start_iter, curr_iter;
  
  gtk_text_buffer_insert_at_cursor(object_inspector_buffer, "Object Type :", -1);

  gtk_text_buffer_get_iter_at_line(object_inspector_buffer, &start_iter, 0);
  gtk_text_buffer_get_iter_at_mark(object_inspector_buffer, &curr_iter, gtk_text_buffer_get_insert(object_inspector_buffer));
  gtk_text_buffer_apply_tag_by_name(object_inspector_buffer, "bold", &start_iter, &curr_iter);
   
  if(IS_INTEGER_OBJECT(obj))
    gtk_text_buffer_insert_at_cursor(object_inspector_buffer, " INTEGER\n\n", -1);
  else if(IS_FLOAT_OBJECT(obj))
    gtk_text_buffer_insert_at_cursor(object_inspector_buffer, " FLOAT\n\n", -1);
  else if(IS_STRING_LITERAL_OBJECT(obj) || is_string_object(obj))
    gtk_text_buffer_insert_at_cursor(object_inspector_buffer, " STRING\n\n", -1);
  else if(IS_SYMBOL_OBJECT(obj))
    gtk_text_buffer_insert_at_cursor(object_inspector_buffer, " SYMBOL\n\n", -1);
  else if(IS_CHAR_OBJECT(obj))
    gtk_text_buffer_insert_at_cursor(object_inspector_buffer, " CHARACTER\n\n", -1);
  else if(IS_FUNCTION2_OBJECT(obj))
    gtk_text_buffer_insert_at_cursor(object_inspector_buffer, " FUNCTION\n\n", -1);
  else if(IS_MACRO2_OBJECT(obj))
    gtk_text_buffer_insert_at_cursor(object_inspector_buffer, " MACRO\n\n", -1);
  else if(IS_ARRAY_OBJECT(obj))
    gtk_text_buffer_insert_at_cursor(object_inspector_buffer, " ARRAY\n\n", -1);  
  else if(IS_CONS_OBJECT(obj))
    gtk_text_buffer_insert_at_cursor(object_inspector_buffer, " CONS\n\n", -1);  
  else
  {
    show_error_dialog("Object type not yet implemented");
    return;
  }

  gtk_text_buffer_insert_at_cursor(object_inspector_buffer, "Object Value:", -1);

  gtk_text_buffer_get_iter_at_line(object_inspector_buffer, &start_iter, 2);
  gtk_text_buffer_get_iter_at_mark(object_inspector_buffer, &curr_iter, gtk_text_buffer_get_insert(object_inspector_buffer));
  gtk_text_buffer_apply_tag_by_name(object_inspector_buffer, "bold", &start_iter, &curr_iter);
  
  if(IS_FUNCTION2_OBJECT(obj) || IS_MACRO2_OBJECT(obj))
    gtk_text_buffer_insert_at_cursor(object_inspector_buffer, "\n\n", -1);
  else
    gtk_text_buffer_insert_at_cursor(object_inspector_buffer, " ", -1);
  
  if(IS_INTEGER_OBJECT(obj) || IS_FLOAT_OBJECT(obj) || is_string_object(obj) ||
     IS_STRING_LITERAL_OBJECT(obj) || IS_SYMBOL_OBJECT(obj) || IS_CHAR_OBJECT(obj))
    print_object_to_string(obj, buf, 0);
  else if(IS_FUNCTION2_OBJECT(obj) || IS_MACRO2_OBJECT(obj))
      print_object_to_string(prim_get_source(obj), buf, 0);
  else if(IS_ARRAY_OBJECT(obj))
    print_array_object_to_string_for_insp(obj, buf, 0);
  else if(IS_CONS_OBJECT(obj))
    print_cons_object_to_string_for_insp(obj, buf, 0);

  if(IS_FUNCTION2_OBJECT(obj) || IS_MACRO2_OBJECT(obj))
    gtk_text_buffer_insert_at_cursor(object_inspector_buffer, (char *)conv_to_lower_case_preserve_strings(buf), -1);
  else
    gtk_text_buffer_insert_at_cursor(object_inspector_buffer, buf, -1);
  
  apply_link_tag(object_inspector_buffer);

  gtk_text_buffer_insert_at_cursor(object_inspector_buffer, "\n\n(To drill down into child objects (indicated by <0x..>), place the cursor on the object and press F3)", -1);
}

void create_object_inspector_window(OBJECT_PTR obj)
{
  GtkWidget *win = gtk_window_new(GTK_WINDOW_TOPLEVEL);

  gtk_window_set_title((GtkWindow *)win, "pLisp Object Inspector");

#ifdef WIN_BUILD
  gtk_window_set_icon_from_file(win, "../share/icons/evaluate.png", NULL);
#else
#ifdef __OSX_BUNDLE__
  gtk_window_set_icon_from_file(win, concat_strings(path_buf, exec_path, "../Resources/share/plisp/icons/evaluate.png"), NULL);
#else
  gtk_window_set_icon_from_file(win, PLISPDATADIR "/icons/evaluate.png", NULL);
#endif
#endif  

  //gtk_window_set_decorated((GtkWindow *)win, FALSE);

  gtk_window_set_default_size((GtkWindow *)win, 600, 280);
  gtk_window_set_position(GTK_WINDOW(win), GTK_WIN_POS_CENTER);

  GtkWidget *scrolled_win, *vbox;
  GtkWidget *textview = gtk_text_view_new ();

  gtk_text_view_set_wrap_mode((GtkTextView *)textview, GTK_WRAP_WORD);

  gtk_text_view_set_editable((GtkTextView *)textview, FALSE);

  GtkTextBuffer *buffer = gtk_text_view_get_buffer((GtkTextView *)textview);

  gtk_text_buffer_create_tag(buffer, "bold", 
                             "weight", PANGO_WEIGHT_BOLD, 
                             NULL);

  gtk_text_buffer_create_tag(buffer, "bold_blue_foreground",
                             "weight", PANGO_WEIGHT_BOLD, "foreground", "blue", "underline", PANGO_UNDERLINE_SINGLE, NULL);  

#ifndef __APPLE__  
  gtk_widget_override_font(GTK_WIDGET(textview), pango_font_description_from_string("Monospace Normal 9"));
#else
  gtk_widget_override_font(GTK_WIDGET(textview), pango_font_description_from_string("Menlo Regular 11"));
#endif  
  
  scrolled_win = gtk_scrolled_window_new (NULL, NULL);
  gtk_container_add (GTK_CONTAINER (scrolled_win), textview);

  vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
  gtk_box_pack_start (GTK_BOX (vbox), scrolled_win, TRUE, TRUE, 0);
  
  gtk_container_add (GTK_CONTAINER (win), vbox);

  g_signal_connect (win, "delete-event",
                    G_CALLBACK (delete_obj_inspector), NULL);

  g_signal_connect(G_OBJECT(win), 
                   "key_press_event", 
                   G_CALLBACK (handle_obj_insp_key_press_events), 
                   buffer);

  print_object_to_inspector(obj, buffer);

  gtk_widget_show_all((GtkWidget *)win);
}
