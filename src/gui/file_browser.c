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

#include <stdlib.h>
#include <assert.h>
#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>

#include <gtksourceview/gtksource.h>

#include "../plisp.h"
#include "../util.h"
#include "../json.h"

#ifdef __APPLE__
#define FONT "Menlo Bold 12"
#else
#ifdef WIN32
//#define FONT "Courier Regular 10"
#define FONT "Monospace Normal 10"
#else
#define FONT "DejaVu Sans Mono Bold 9"
#endif
#endif

typedef struct file_name_and_widget
{
  GtkWidget *widget;
  char *file_name;
  GtkTextBuffer *buffer;
  GtkTextView *view;
} file_name_and_widget_t;

GtkWindow *file_browser_window;
GtkStatusbar *file_browser_statusbar;
GtkWidget *fb_notebook;

extern gboolean delete_event(GtkWidget *,
                             GdkEvent *,
                             gpointer);
extern void eval_expression(GtkWidget *, gpointer);
extern void close_window(GtkWidget *, gpointer);
extern gboolean handle_key_press_events(GtkWidget *, 
                                        GdkEventKey *,
                                        gpointer);

extern void handle_cursor_move(GtkWidget *, gpointer);

extern void set_triggering_window(GtkWidget *, gpointer);

extern void show_error_dialog(char *);

extern GtkSourceLanguage *source_language;

extern GtkWindow *action_triggering_window;

unsigned int nof_open_files;
char *current_file_name;
GtkWidget *current_scrolled_win;

unsigned int nof_fnws;
file_name_and_widget_t **fnws;
GtkTextBuffer *curr_file_browser_buffer;
GtkTextView *curr_file_browser_text_view;

//for find functionality
char search_string[MAX_STRING_LENGTH];
GtkTextBuffer *find_buffer;
GtkTextIter last_find_iter;
//end for find functionality

void cleanup_file_name_widgets()
{
  int i;
  /* for(i=0; i<nof_fnws; i++) */
  /*   free(fnws[i]); */
  nof_fnws = 0;
}

void add_file_name_and_widget(file_name_and_widget_t *fnw)
{
  if(!fnws)
  {
    fnws = (file_name_and_widget_t **)GC_MALLOC(sizeof(file_name_and_widget_t *));
    assert(fnws);
    fnws[0] = fnw;
  }
  else
  {
    file_name_and_widget_t **temp = (file_name_and_widget_t **)GC_REALLOC(fnws, (nof_fnws+1) * sizeof(file_name_and_widget_t *));
    assert(temp);
    fnws = temp;
    fnws[nof_fnws] = fnw;
  }
  nof_fnws++;
}

BOOLEAN quit_file_browser()
{
  if(any_buffers_modified())
  {
    GtkWidget *dialog1 = gtk_message_dialog_new ((GtkWindow *)file_browser_window,
                                                 GTK_DIALOG_DESTROY_WITH_PARENT,
                                                 GTK_MESSAGE_QUESTION,
                                                 GTK_BUTTONS_YES_NO,
                                                 "There are one or more unsaved file(s), do you still want to close the File Browser?");

    gtk_widget_grab_focus(gtk_dialog_get_widget_for_response(GTK_DIALOG(dialog1), GTK_RESPONSE_NO));

    if(gtk_dialog_run(GTK_DIALOG (dialog1)) == GTK_RESPONSE_YES)
    {
      gtk_widget_destroy((GtkWidget *)dialog1);    

      /* if(current_file_name) */
      /*   free(current_file_name); */

      cleanup_file_name_widgets();

      close_application_window(&file_browser_window);
      return true;
    }
    else
    {
      gtk_widget_destroy((GtkWidget *)dialog1);
      return false;
    }
  }
  else
  {
    close_application_window(&file_browser_window);
    return true;
  }
}

gboolean fb_delete_event( GtkWidget *widget,
                      GdkEvent  *event,
                      gpointer  data)
{
  if(quit_file_browser())
    return FALSE;
  else
    return TRUE;
}

void fb_select_file(GtkWidget *widget, gpointer data)
{
  file_name_and_widget_t *fnw = (file_name_and_widget_t *)data;

  //free(current_file_name);
  current_file_name = (char *)GC_MALLOC(strlen(fnw->file_name) + 1);
  memset(current_file_name, '\0', strlen(current_file_name) + 1);
  strcpy(current_file_name, fnw->file_name);

  current_scrolled_win = fnw->widget;
  curr_file_browser_buffer = fnw->buffer;
  curr_file_browser_text_view = fnw->view;
}

void find_next_occurrence()
{
  GtkTextIter start_match, end_match;
  GtkTextBuffer *buffer = curr_file_browser_buffer;

  if(find_buffer != curr_file_browser_buffer)
    gtk_text_buffer_get_start_iter(buffer, &last_find_iter);
  else
  {
    GtkTextMark *last_pos;

    last_pos = gtk_text_buffer_get_mark (buffer, "last_pos");
    if (last_pos == NULL)
      return;

    gtk_text_buffer_get_iter_at_mark (buffer, &last_find_iter, last_pos);
  }

  find_buffer = buffer;

  if(gtk_text_iter_forward_search(&last_find_iter, search_string, 
                                  GTK_TEXT_SEARCH_TEXT_ONLY | 
                                  GTK_TEXT_SEARCH_CASE_INSENSITIVE |
                                  GTK_TEXT_SEARCH_VISIBLE_ONLY, 
                                  &start_match, &end_match, NULL))
  {
    //remove all tags
    ///
    GtkTextIter start_find, end_find;
    gtk_text_buffer_get_start_iter(buffer, &start_find);
    gtk_text_buffer_get_end_iter(buffer, &end_find);
      
    gtk_text_buffer_remove_tag_by_name(buffer, "gray_bg", 
                                       &start_find, &end_find); 
    ///

    gtk_text_view_scroll_to_iter(curr_file_browser_text_view, &start_match, 0, TRUE, 0.5, 0.5);

    gtk_text_buffer_apply_tag_by_name(buffer, "gray_bg", 
                                      &start_match, &end_match);

    /* gint offset = gtk_text_iter_get_offset(&end_match); */
    /* gtk_text_buffer_get_iter_at_offset(buffer,  */
    /*                                    &last_find_iter, offset); */

    gtk_text_buffer_create_mark (buffer, "last_pos", &end_match, FALSE);

    gtk_widget_grab_focus(curr_file_browser_text_view);

  }
  else
    show_error_dialog("No further matches found");  
}

gboolean fb_handle_key_press_events(GtkWidget *widget, GdkEventKey *event, gpointer user_data)
{
  if(event->keyval == GDK_KEY_Tab)
  {
    GtkTextBuffer *buffer = gtk_text_view_get_buffer((GtkTextView *)widget);
    indent(buffer);
    do_auto_complete(buffer);
    return TRUE;  
  }
  else if((event->state & GDK_CONTROL_MASK) && event->keyval == GDK_KEY_g)
  {
    find_next_occurrence();
    return TRUE;
  }

  return FALSE;
}

void new_source_file()
{
  char *opened_source_file_name = "";

  GtkWidget *scrolled_win = gtk_scrolled_window_new (NULL, NULL);
  GtkSourceBuffer *buffer = gtk_source_buffer_new_with_language(source_language);
  GtkWidget *view = gtk_source_view_new_with_buffer(buffer);

  //for search functionality
  gtk_text_buffer_create_tag(buffer, "gray_bg", 
                             "background", "lightgray", NULL);

  g_signal_connect(G_OBJECT(buffer), 
                   "notify::cursor-position", 
                   G_CALLBACK (handle_cursor_move), 
                   NULL);

  g_signal_connect(G_OBJECT(view), 
                   "key_press_event", 
                   G_CALLBACK (fb_handle_key_press_events), 
                   NULL);

  gtk_container_add (GTK_CONTAINER (scrolled_win), view);

  gtk_widget_override_font(GTK_WIDGET(view), pango_font_description_from_string(FONT));

  GtkWidget *label = gtk_label_new(opened_source_file_name);

  file_name_and_widget_t *fnw = (file_name_and_widget_t *)GC_MALLOC(sizeof(file_name_and_widget_t));
  fnw->widget = scrolled_win;
  fnw->file_name = opened_source_file_name;
  fnw->buffer = buffer;
  fnw->view = view;

  add_file_name_and_widget(fnw);

  //to work around focus not being set to the newly-created view/scrolled window
  //free(current_file_name);
  current_file_name = (char *)GC_MALLOC(strlen(fnw->file_name) + 1);
  memset(current_file_name, '\0', strlen(current_file_name) + 1);
  strcpy(current_file_name, fnw->file_name);
  current_scrolled_win = fnw->widget;    
  curr_file_browser_buffer = fnw->buffer;
  curr_file_browser_text_view = view;

  g_signal_connect(view, "grab-focus", G_CALLBACK(fb_select_file), (void *)fnw);

  gtk_text_buffer_set_text(buffer, "", -1);

  gtk_notebook_append_page(fb_notebook, scrolled_win, label);
  gtk_widget_show_all(file_browser_window);

  gtk_text_buffer_set_modified(curr_file_browser_buffer, FALSE);
}

void fb_new_source_file(GtkWidget *widget, gpointer data)
{
  new_source_file();
}

void fb_load_source_file()
{
  action_triggering_window = file_browser_window;

  GtkWidget *dialog;

  dialog = gtk_file_chooser_dialog_new ("Open pLisp source file",
                                        (GtkWindow *)file_browser_window,
                                        GTK_FILE_CHOOSER_ACTION_OPEN,
                                        "Cancel", GTK_RESPONSE_CANCEL,
                                        "Open", GTK_RESPONSE_ACCEPT,
                                        NULL);

  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
  {

    char *opened_source_file_name = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));

    gtk_widget_destroy (dialog);

    GtkWidget *scrolled_win = gtk_scrolled_window_new (NULL, NULL);
    GtkSourceBuffer *buffer = gtk_source_buffer_new_with_language(source_language);
    GtkWidget *view = gtk_source_view_new_with_buffer(buffer);

    //for search functionality
    gtk_text_buffer_create_tag(buffer, "gray_bg", 
                               "background", "lightgray", NULL);

    g_signal_connect(G_OBJECT(buffer), 
                     "notify::cursor-position", 
                     G_CALLBACK (handle_cursor_move), 
                     NULL);

    g_signal_connect(G_OBJECT(view), 
                     "key_press_event", 
                     G_CALLBACK (fb_handle_key_press_events), 
                     NULL);

    gtk_container_add (GTK_CONTAINER (scrolled_win), view);

    gtk_widget_override_font(GTK_WIDGET(view), pango_font_description_from_string(FONT));

    GtkWidget *label = gtk_label_new(opened_source_file_name);

    file_name_and_widget_t *fnw = (file_name_and_widget_t *)GC_MALLOC(sizeof(file_name_and_widget_t));
    fnw->widget = scrolled_win;
    fnw->file_name = opened_source_file_name;
    fnw->buffer = buffer;
    fnw->view = view;

    add_file_name_and_widget(fnw);

    //to work around focus not being set to the newly-created view/scrolled window
    //free(current_file_name);
    current_file_name = (char *)GC_MALLOC(strlen(fnw->file_name) + 1);
    memset(current_file_name, '\0', strlen(current_file_name) + 1);
    strcpy(current_file_name, fnw->file_name);
    current_scrolled_win = fnw->widget;    
    curr_file_browser_buffer = fnw->buffer;
    curr_file_browser_text_view = view;

    g_signal_connect(view, "grab-focus", G_CALLBACK(fb_select_file), (void *)fnw);

    char *file_contents = get_file_contents(opened_source_file_name);

    if(!file_contents)
    {
      show_error_dialog("Unable to open file");
      return;
    }

    gtk_text_buffer_set_text(buffer, file_contents == -1 ? "" : file_contents, -1);
    /* if(file_contents && file_contents != -1) */
    /*   free(file_contents); */

    gtk_notebook_append_page(fb_notebook, scrolled_win, label);
    gtk_widget_show_all(file_browser_window);

    gtk_text_buffer_set_modified(curr_file_browser_buffer, FALSE);

    //TODO: set focus to the newly-opened file
  }
  else
    gtk_widget_destroy (dialog);  
}

void fb_open_source_file(GtkWidget *widget, gpointer data)
{
  fb_load_source_file();
}

void save_file()
{
  if(gtk_text_buffer_get_modified(curr_file_browser_buffer) == FALSE)
  {
    gtk_statusbar_push(file_browser_statusbar, 0, "No changes to save");
    return;
  }

  if(strlen(current_file_name) == 0)
  {
    action_triggering_window = file_browser_window;

    GtkWidget *dialog;

    dialog = gtk_file_chooser_dialog_new ("Save pLisp source file",
                                          (GtkWindow *)file_browser_window,
                                          GTK_FILE_CHOOSER_ACTION_SAVE,
                                          "Cancel", GTK_RESPONSE_CANCEL,
                                          "Save", GTK_RESPONSE_ACCEPT,
                                          NULL);

    if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
    {
      current_file_name = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));

      gtk_widget_destroy (dialog);

      if(file_exists(current_file_name))
      {
        GtkWidget *dialog1 = gtk_message_dialog_new ((GtkWindow *)file_browser_window,
                                                     GTK_DIALOG_DESTROY_WITH_PARENT,
                                                     GTK_MESSAGE_QUESTION,
                                                     GTK_BUTTONS_YES_NO,
                                                     "File exists, do you want to overwite it?");

        gtk_widget_grab_focus(gtk_dialog_get_widget_for_response(GTK_DIALOG(dialog1), GTK_RESPONSE_NO));

        if(gtk_dialog_run(GTK_DIALOG (dialog1)) == GTK_RESPONSE_NO)
        {
          gtk_widget_destroy((GtkWidget *)dialog1);
          g_free(current_file_name);
          current_file_name = "";
          return;
        }
        else
        {
          gtk_widget_destroy((GtkWidget *)dialog1);
        }
      }

      //gint page_no = gtk_notebook_get_current_page(fb_notebook);
      gtk_notebook_set_tab_label_text(fb_notebook, gtk_container_get_focus_child(fb_notebook), current_file_name);
    }
    else
      return;
  }

  /* FILE *out = fopen(current_file_name, "w"); */
  
  /* if(!out) */
  /* { */
  /*   show_error_dialog("Unable to open file"); */
  /*   return; */
  /* } */

  /* GtkTextIter start, end;   */

  /* gtk_text_buffer_get_start_iter(curr_file_browser_buffer, &start); */
  /* gtk_text_buffer_get_end_iter (curr_file_browser_buffer, &end); */

  /* fprintf(out, "%s", gtk_text_buffer_get_text(curr_file_browser_buffer, &start, &end, FALSE)); */

  /* fclose(out); */

  GtkTextIter start, end;

  gtk_text_buffer_get_start_iter(curr_file_browser_buffer, &start);
  gtk_text_buffer_get_end_iter (curr_file_browser_buffer, &end);

  GError *err = NULL;

  gboolean result = g_file_set_contents(current_file_name,
                                        gtk_text_buffer_get_text(curr_file_browser_buffer, &start, &end, FALSE),
                                        -1,
                                        &err);

  if(result == FALSE)
  {
    show_error_dialog(err->message);
  }
  else
  {
    gtk_statusbar_push(file_browser_statusbar, 0, "File saved successfully");
    gtk_text_buffer_set_modified(curr_file_browser_buffer, FALSE);
  }
}

void fb_save_source_file(GtkWidget *widget, gpointer data)
{
  save_file();
}

void close_file()
{
  if(gtk_notebook_get_n_pages(fb_notebook) == 0)
    return;

  if(gtk_text_buffer_get_modified(curr_file_browser_buffer) == TRUE)
  {
      GtkWidget *dialog1 = gtk_message_dialog_new ((GtkWindow *)file_browser_window,
                                                   GTK_DIALOG_DESTROY_WITH_PARENT,
                                                   GTK_MESSAGE_QUESTION,
                                                   GTK_BUTTONS_YES_NO,
                                                   "File has been modified; save it before closing?");

      gtk_widget_grab_focus(gtk_dialog_get_widget_for_response(GTK_DIALOG(dialog1), GTK_RESPONSE_YES));

      if(gtk_dialog_run(GTK_DIALOG (dialog1)) == GTK_RESPONSE_YES)
        save_file();

      gtk_widget_destroy((GtkWidget *)dialog1);
  }

  gtk_container_remove(fb_notebook, current_scrolled_win);

  if(gtk_notebook_get_n_pages(fb_notebook) > 0)
    current_scrolled_win = gtk_notebook_get_nth_page(fb_notebook, gtk_notebook_get_current_page(fb_notebook));
}

void fb_close_source_file(GtkWidget *widget, gpointer data)
{
  close_file();
}

void reload_file()
{
  if(current_file_name == NULL)
    return;

  if(strlen(current_file_name) > 0)
  {
    if(gtk_text_buffer_get_modified(curr_file_browser_buffer) == TRUE)
    {
      GtkWidget *dialog1 = gtk_message_dialog_new ((GtkWindow *)file_browser_window,
                                                   GTK_DIALOG_DESTROY_WITH_PARENT,
                                                   GTK_MESSAGE_QUESTION,
                                                   GTK_BUTTONS_YES_NO,
                                                   "File has been modified; reload it?");

      gtk_widget_grab_focus(gtk_dialog_get_widget_for_response(GTK_DIALOG(dialog1), GTK_RESPONSE_NO));

      if(gtk_dialog_run(GTK_DIALOG (dialog1)) == GTK_RESPONSE_YES)
      {
        char *file_contents = get_file_contents(current_file_name);

        if(!file_contents)
        {
          show_error_dialog("Unable to open file");
          return;
        }

        gtk_text_buffer_set_text(curr_file_browser_buffer, file_contents, -1);
        //free(file_contents);

        gtk_text_buffer_set_modified(curr_file_browser_buffer, FALSE);

      }

      gtk_widget_destroy((GtkWidget *)dialog1);
    }
  }
}

void fb_reload_source_file(GtkWidget *widget, gpointer data)
{
  reload_file();
}

int get_text_to_find(char *buf)
{
  GtkWidget *dialog;
  GtkWidget *entry;
  GtkWidget *content_area;

  action_triggering_window = file_browser_window;

  dialog = gtk_dialog_new_with_buttons("Find Text",
                                       action_triggering_window,
                                       GTK_DIALOG_DESTROY_WITH_PARENT, 
                                       //GTK_STOCK_OK,
                                       "Find",
                                       GTK_RESPONSE_ACCEPT,
                                       //GTK_STOCK_CANCEL,
                                       "Cancel",
                                       GTK_RESPONSE_REJECT,
                                       NULL);

  gtk_window_set_resizable((GtkWindow *)dialog, FALSE);

  //gtk_window_set_position(GTK_WINDOW(dialog), GTK_WIN_POS_CENTER);

  gtk_dialog_set_default_response(dialog, GTK_RESPONSE_ACCEPT);

  content_area = gtk_dialog_get_content_area(GTK_DIALOG(dialog));
  entry = gtk_entry_new();
  gtk_entry_set_activates_default(entry, TRUE);
  gtk_container_add(GTK_CONTAINER(content_area), entry);

  gtk_widget_show_all(dialog);
  gint result = gtk_dialog_run(GTK_DIALOG(dialog));

  if(result == GTK_RESPONSE_ACCEPT)
    strcpy(buf, gtk_entry_get_text(GTK_ENTRY(entry)));

  gtk_widget_destroy(dialog);
  
  return result;
    
}

void find_text()
{
  if(curr_file_browser_buffer == NULL)
  {
    show_error_dialog("No file open\n");
    return;
  }

  memset(search_string, '\0', MAX_STRING_LENGTH);

  int result = GTK_RESPONSE_ACCEPT;

  while(result == GTK_RESPONSE_ACCEPT && strlen(search_string) == 0)
  {
    result = get_text_to_find(search_string);
    if(strlen(search_string) == 0 && result == GTK_RESPONSE_ACCEPT)
      show_error_dialog("Please enter search string\n");
  }

  if(result == GTK_RESPONSE_ACCEPT)
  {
    GtkTextIter start_match, end_match;
    GtkTextBuffer *buffer = curr_file_browser_buffer;

    find_buffer = buffer;

    gtk_text_buffer_get_start_iter(buffer, &last_find_iter);

    if(gtk_text_iter_forward_search(&last_find_iter, search_string, 
                                    GTK_TEXT_SEARCH_TEXT_ONLY | 
                                    GTK_TEXT_SEARCH_CASE_INSENSITIVE |
                                    GTK_TEXT_SEARCH_VISIBLE_ONLY, 
                                    &start_match, &end_match, NULL))
    {
      //remove all tags
      ///
      GtkTextIter start_find, end_find;
      gtk_text_buffer_get_start_iter(buffer, &start_find);
      gtk_text_buffer_get_end_iter(buffer, &end_find);
      
      gtk_text_buffer_remove_tag_by_name(buffer, "gray_bg", 
                                         &start_find, &end_find); 
      ///

      gtk_text_view_scroll_to_iter(curr_file_browser_text_view, &start_match, 0, TRUE, 0.5, 0.5);

      gtk_text_buffer_apply_tag_by_name(buffer, "gray_bg", 
                                        &start_match, &end_match);

      /* gint offset = gtk_text_iter_get_offset(&end_match); */
      /* gtk_text_buffer_get_iter_at_offset(buffer,  */
      /*                                    &last_find_iter, offset); */

      gtk_text_buffer_create_mark (buffer, "last_pos", &end_match, FALSE);

      gtk_widget_grab_focus(curr_file_browser_text_view);
    }
    else
      show_error_dialog("No matches found");
  }
}

void fb_find_text(GtkWidget *widget, gpointer data)
{
  find_text();
}

GtkToolbar *create_file_browser_toolbar()
{
  GtkWidget *toolbar;

#ifdef WIN_BUILD
  GtkWidget *new_icon     = gtk_image_new_from_file ("../share/icons/new_file.png");
  GtkWidget *open_icon    = gtk_image_new_from_file ("../share/icons/open_file.png");
  GtkWidget *save_icon    = gtk_image_new_from_file ("../share/icons/save_file.png");
  GtkWidget *close_icon   = gtk_image_new_from_file ("../share/icons/close_file.png");
  GtkWidget *refresh_icon = gtk_image_new_from_file ("../share/icons/refresh.png");
  GtkWidget *find_icon    = gtk_image_new_from_file ("../share/icons/find.png");
  GtkWidget *eval_icon    = gtk_image_new_from_file ("../share/icons/evaluate.png");
  GtkWidget *exit_icon    = gtk_image_new_from_file ("../share/icons/exit32x32.png");
#else
  GtkWidget *new_icon     = gtk_image_new_from_file (PLISPDATADIR "/icons/new_file.png");
  GtkWidget *open_icon    = gtk_image_new_from_file (PLISPDATADIR "/icons/open_file.png");
  GtkWidget *save_icon    = gtk_image_new_from_file (PLISPDATADIR "/icons/save_file.png");
  GtkWidget *close_icon   = gtk_image_new_from_file (PLISPDATADIR "/icons/close_file.png");
  GtkWidget *refresh_icon = gtk_image_new_from_file (PLISPDATADIR "/icons/refresh.png");
  GtkWidget *find_icon    = gtk_image_new_from_file (PLISPDATADIR "/icons/find.png");
  GtkWidget *eval_icon    = gtk_image_new_from_file (PLISPDATADIR "/icons/evaluate.png");
  GtkWidget *exit_icon    = gtk_image_new_from_file (PLISPDATADIR "/icons/exit32x32.png");
#endif

  toolbar = gtk_toolbar_new ();
  gtk_orientable_set_orientation (GTK_ORIENTABLE (toolbar), GTK_ORIENTATION_HORIZONTAL);
  gtk_toolbar_set_style (GTK_TOOLBAR (toolbar), GTK_TOOLBAR_ICONS);
  gtk_container_set_border_width (GTK_CONTAINER (toolbar), 5);

  GtkToolItem *new_button = gtk_tool_button_new(new_icon, NULL);
  gtk_tool_item_set_tooltip_text(new_button, "New file (Ctrl-N)");
  g_signal_connect (new_button, "clicked", G_CALLBACK (fb_new_source_file), file_browser_window);
  gtk_toolbar_insert((GtkToolbar *)toolbar, new_button, 0);

  GtkToolItem *open_button = gtk_tool_button_new(open_icon, NULL);
  gtk_tool_item_set_tooltip_text(open_button, "Open file (Ctrl-O)");
  g_signal_connect (open_button, "clicked", G_CALLBACK (fb_open_source_file), file_browser_window);
  gtk_toolbar_insert((GtkToolbar *)toolbar, open_button, 1);

  GtkToolItem *save_button = gtk_tool_button_new(save_icon, NULL);
  gtk_tool_item_set_tooltip_text(save_button, "Save file (Ctrl-S)");
  g_signal_connect (save_button, "clicked", G_CALLBACK (fb_save_source_file), file_browser_window);
  gtk_toolbar_insert((GtkToolbar *)toolbar, save_button, 2);

  GtkToolItem *close_button = gtk_tool_button_new(close_icon, NULL);
  gtk_tool_item_set_tooltip_text(close_button, "Close file (Ctrl-W)");
  g_signal_connect (close_button, "clicked", G_CALLBACK (fb_close_source_file), file_browser_window);
  gtk_toolbar_insert((GtkToolbar *)toolbar, close_button, 3);

  GtkToolItem *refresh_button = gtk_tool_button_new(refresh_icon, NULL);
  gtk_tool_item_set_tooltip_text(refresh_button, "Reload file (F5)");
  g_signal_connect (refresh_button, "clicked", G_CALLBACK (fb_reload_source_file), file_browser_window);
  gtk_toolbar_insert((GtkToolbar *)toolbar, refresh_button, 4);

  GtkToolItem *find_button = gtk_tool_button_new(find_icon, NULL);
  gtk_tool_item_set_tooltip_text(find_button, "Find (Ctrl+F)");
  g_signal_connect (find_button, "clicked", G_CALLBACK (fb_find_text), file_browser_window);
  gtk_toolbar_insert((GtkToolbar *)toolbar, find_button, 5);

  GtkToolItem *eval_button = gtk_tool_button_new(eval_icon, NULL);
  gtk_tool_item_set_tooltip_text(eval_button, "Evaluate (Ctrl+Enter)");
  g_signal_connect (eval_button, "clicked", G_CALLBACK (eval_expression), file_browser_window);
  gtk_toolbar_insert((GtkToolbar *)toolbar, eval_button, 6);

  GtkToolItem *exit_button = gtk_tool_button_new(exit_icon, NULL);
  gtk_tool_item_set_tooltip_text(exit_button, "Exit (Ctrl-Q)");
  g_signal_connect (exit_button, "clicked", G_CALLBACK (close_window), file_browser_window);
  gtk_toolbar_insert((GtkToolbar *)toolbar, exit_button, 7);

  return (GtkToolbar *)toolbar;
}

void create_file_browser_window(int posx, int posy, int width, int height)
{
  GtkWidget *win = gtk_window_new (GTK_WINDOW_TOPLEVEL);

  file_browser_window = (GtkWindow *)win;

#ifdef WIN_BUILD
  gtk_window_set_icon_from_file(file_browser_window, "../share/icons/evaluate.png", NULL);
#else
  gtk_window_set_icon_from_file(file_browser_window, PLISPDATADIR "/icons/evaluate.png", NULL);
#endif

  gtk_window_set_title(file_browser_window, "pLisp File Browser");

  gtk_window_set_default_size((GtkWindow *)win, width, height);
  gtk_window_move((GtkWindow *)win, posx, posy); 

  g_signal_connect (win, "delete-event",
                    G_CALLBACK (fb_delete_event), NULL);

  g_signal_connect (win, "focus",
                    G_CALLBACK (set_triggering_window), NULL);

  gtk_container_set_border_width (GTK_CONTAINER (win), 10);

  GtkWidget *vbox;

  //setting up source buffers should happen when the
  //corresponding tab is created

  g_signal_connect(G_OBJECT(win), 
                   "key_press_event", 
                   G_CALLBACK (handle_key_press_events), 
                   NULL);

  vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
  gtk_box_pack_start (GTK_BOX (vbox), (GtkWidget *)create_file_browser_toolbar(), FALSE, FALSE, 0);


  fb_notebook = gtk_notebook_new();
  gtk_box_pack_start (GTK_BOX (vbox), fb_notebook, TRUE, TRUE, 0);

  file_browser_statusbar = (GtkStatusbar *)gtk_statusbar_new();
  gtk_box_pack_start (GTK_BOX (vbox), (GtkWidget *)file_browser_statusbar, FALSE, FALSE, 0);  
  
  gtk_container_add (GTK_CONTAINER (win), vbox);

  nof_open_files = 0;
  fnws = NULL;

  gtk_widget_show_all(win);
}

void serialize_file_browser_window(FILE *fp)
{
  if(!file_browser_window)
    return;

  int posx, posy, width, height;

  gtk_window_get_position(file_browser_window, &posx, &posy);
  gtk_window_get_size(file_browser_window, &width, &height);

  fprintf(fp, 
          ", \"file-browser\" : [ %d, %d, %d, %d, [", 
          posx, posy, width, height);
  
  gint nof_pages = gtk_notebook_get_n_pages(fb_notebook);

  int i=0;

  for(i=0; i<nof_pages; i++)
  {
    GtkWidget *page = gtk_notebook_get_nth_page(fb_notebook, i);

    if(i != 0)
      fprintf(fp, ", ");

    fprintf(fp, "\"%s\"", gtk_notebook_get_tab_label_text(fb_notebook, page));
  }

  fprintf(fp, "]]");
}

void add_file_to_file_browser(char *file_name)
{
  GtkWidget *scrolled_win = gtk_scrolled_window_new (NULL, NULL);
  GtkSourceBuffer *buffer = gtk_source_buffer_new_with_language(source_language);
  GtkWidget *view = gtk_source_view_new_with_buffer(buffer);

  g_signal_connect(G_OBJECT(buffer), 
                   "notify::cursor-position", 
                   G_CALLBACK (handle_cursor_move), 
                   NULL);

  g_signal_connect(G_OBJECT(view), 
                   "key_press_event", 
                   G_CALLBACK (fb_handle_key_press_events), 
                   NULL);

  gtk_container_add (GTK_CONTAINER (scrolled_win), view);

  gtk_widget_override_font(GTK_WIDGET(view), pango_font_description_from_string(FONT));

  GtkWidget *label = gtk_label_new(file_name);

  file_name_and_widget_t *fnw = (file_name_and_widget_t *)GC_MALLOC(sizeof(file_name_and_widget_t));
  fnw->widget = scrolled_win;
  fnw->file_name = file_name;
  fnw->buffer = buffer;
  fnw->view = view;

  add_file_name_and_widget(fnw);

  //to work around focus not being set to the newly-created view/scrolled window
  //free(current_file_name);
  current_file_name = (char *)GC_MALLOC(strlen(fnw->file_name) + 1);
  memset(current_file_name, '\0', strlen(current_file_name) + 1);
  strcpy(current_file_name, fnw->file_name);
  current_scrolled_win = fnw->widget;    
  curr_file_browser_buffer = fnw->buffer;
  curr_file_browser_text_view = view;

  g_signal_connect(view, "grab-focus", G_CALLBACK(fb_select_file), (void *)fnw);

  char *file_contents = get_file_contents(file_name);

  if(!file_contents)
  {
    char buf[200];
    memset(buf, 200, '\0');
    sprintf(buf, "Unable to open file %s", file_name);
    show_error_dialog(buf);
    return;
  }

  gtk_text_buffer_set_text(buffer, file_contents == -1 ? "" : file_contents, -1);
  /* if(file_contents && file_contents != -1) */
  /*   free(file_contents); */

  gtk_notebook_append_page(fb_notebook, scrolled_win, label);
  gtk_text_buffer_set_modified(curr_file_browser_buffer, FALSE);  
}

void deserialize_file_browser_window(struct JSONObject *root)
{
  struct JSONObject *file_browser = JSON_get_object_item(root, "file-browser");

  if(file_browser_window)
    close_application_window(&file_browser_window);

  if(file_browser)
  {
    create_file_browser_window(JSON_get_array_item(file_browser, 0)->ivalue,
                               JSON_get_array_item(file_browser, 1)->ivalue,
                               JSON_get_array_item(file_browser, 2)->ivalue,
                               JSON_get_array_item(file_browser, 3)->ivalue);

    
    struct JSONObject *file_names = JSON_get_array_item(file_browser, 4);

    unsigned int nof_files = JSON_get_array_size(file_names);

    int i=0;

    for(i=0; i < nof_files; i++)
      add_file_to_file_browser(JSON_get_array_item(file_names, i)->strvalue);

    gtk_widget_show_all(file_browser_window);
  }
}

BOOLEAN any_buffers_modified()
{
  gint nof_pages = gtk_notebook_get_n_pages(fb_notebook);

  int i=0;

  for(i=0; i<nof_pages; i++)
  {
    GtkWidget *page = gtk_notebook_get_nth_page(fb_notebook, i);

    GList *children = gtk_container_get_children((GtkContainer *)page);

    //the assumption is that the page (the scrolled window)
    //contains only one child, the text view.
    GtkTextView *view = (GtkTextView *)children->data;

    GtkTextBuffer *buffer = gtk_text_view_get_buffer(view);

    if(gtk_text_buffer_get_modified(buffer) == TRUE)
      return true;
  }

  return false;
}
