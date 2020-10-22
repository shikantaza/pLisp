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
#include <gtksourceview/gtksource.h>
#include <assert.h>
#include <stdint.h>

#include "../plisp.h"

#include "../util.h"

#define DEFAULT_STEPPER_WINDOW_POSX 650
#define DEFAULT_STEPPER_WINDOW_POSY 200
#define DEFAULT_STEPPER_WINDOW_WIDTH 600
#define DEFAULT_STEPPER_WINDOW_HEIGHT 400

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

extern gboolean delete_event(GtkWidget *,
                             GdkEvent *,
                             gpointer);
extern gboolean handle_key_press_events(GtkWidget *, 
                                        GdkEventKey *,
                                        gpointer);
extern void set_triggering_window(GtkWidget *, gpointer);

extern GtkSourceLanguage *source_language;

extern void highlight_text(GtkTextBuffer *, char *);

extern GtkWindow *action_triggering_window;
extern GtkWindow *transcript_window;

GtkWindow *stepper_window;
GtkSourceBuffer *fn_source_buffer;

GtkTreeView *env_symbols_list;

BOOLEAN abrt_stepper;
BOOLEAN run_to_completion;
BOOLEAN step_over;
extern continuation_t *step_over_continuation;
continuation_t *dummy;

#ifdef __OSX_BUNDLE__
extern char exec_path[512];
extern char path_buf[1024];
#endif

extern OBJECT_PTR NIL;

extern void close_application_window(GtkWidget **);

void close_stepper_window()
{
  close_application_window((GtkWidget **)&stepper_window);
}

void continue_stepper(GtkWidget *widget,
                      gpointer data)
{
  gtk_widget_hide((GtkWidget *)stepper_window);
  run_to_completion = false;
  step_over = false;
  gtk_main_quit();
}

void run_complete(GtkWidget *widget,
                  gpointer data)
{
  gtk_widget_hide((GtkWidget *)stepper_window);
  gtk_main_quit();
  run_to_completion = true;
}

void abort_stepper(GtkWidget *widget,
                   gpointer data)
{
  gtk_widget_hide((GtkWidget *)stepper_window);  
  gtk_main_quit();
  abrt_stepper = true;
}

void step_over_cb(GtkWidget *widget,
                  gpointer data)
{
  gtk_widget_hide((GtkWidget *)stepper_window);  
  gtk_main_quit();
  step_over = true;
  step_over_continuation = dummy;
}

GtkToolbar *create_stepper_toolbar()
{
  GtkWidget *toolbar;

#ifdef WIN_BUILD
  GtkWidget *step_icon = gtk_image_new_from_file ("../share/icons/step.png");
  GtkWidget *step_over_icon = gtk_image_new_from_file ("../share/icons/step_over.png");
  GtkWidget *resume_icon = gtk_image_new_from_file ("../share/icons/resume.png");  
  GtkWidget *abort_icon = gtk_image_new_from_file ("../share/icons/abort.png");
#else
#ifdef __OSX_BUNDLE__
  GtkWidget *step_icon = gtk_image_new_from_file (concat_strings(path_buf, exec_path, "../Resources/share/plisp/icons/step.png"));
  GtkWidget *step_over_icon = gtk_image_new_from_file (concat_strings(path_buf, exec_path, "../Resources/share/plisp/icons/step_over.png"));
  GtkWidget *resume_icon = gtk_image_new_from_file (concat_strings(path_buf, exec_path, "../Resources/share/plisp/icons/resume.png"));
  GtkWidget *abort_icon = gtk_image_new_from_file (concat_strings(path_buf, exec_path, "../Resources/share/plisp/icons/abort.png"));
#else
  GtkWidget *step_icon = gtk_image_new_from_file (PLISPDATADIR "/icons/step.png");
  GtkWidget *step_over_icon = gtk_image_new_from_file (PLISPDATADIR "/icons/step_over.png");
  GtkWidget *resume_icon = gtk_image_new_from_file (PLISPDATADIR "/icons/resume.png");
  GtkWidget *abort_icon = gtk_image_new_from_file (PLISPDATADIR "/icons/abort.png");
#endif
#endif  

  toolbar = gtk_toolbar_new ();
  gtk_orientable_set_orientation (GTK_ORIENTABLE (toolbar), GTK_ORIENTATION_HORIZONTAL);
  gtk_toolbar_set_style (GTK_TOOLBAR (toolbar), GTK_TOOLBAR_ICONS);
  gtk_container_set_border_width (GTK_CONTAINER (toolbar), 5);

  GtkToolItem *step_button = gtk_tool_button_new(step_icon, NULL);
  //commenting out the tooltip because it's too distracting
  //when the user wants to continiously step through the code
  //gtk_tool_item_set_tooltip_text(step_button, "Step");
  g_signal_connect (step_button, "clicked", G_CALLBACK (continue_stepper), stepper_window);
  gtk_toolbar_insert((GtkToolbar *)toolbar, step_button, 0);

  GtkToolItem *step_over_button = gtk_tool_button_new(step_over_icon, NULL);
  gtk_tool_item_set_tooltip_text(step_over_button, "Step over");
  g_signal_connect (step_over_button, "clicked", G_CALLBACK (step_over_cb), stepper_window);
  gtk_toolbar_insert((GtkToolbar *)toolbar, step_over_button, 1);
  
  GtkToolItem *resume_button = gtk_tool_button_new(resume_icon, NULL);
  gtk_tool_item_set_tooltip_text(resume_button, "Run to completion");
  g_signal_connect (resume_button, "clicked", G_CALLBACK (run_complete), stepper_window);
  gtk_toolbar_insert((GtkToolbar *)toolbar, resume_button, 2);
  
  GtkToolItem *abort_button = gtk_tool_button_new(abort_icon, NULL);
  gtk_tool_item_set_tooltip_text(abort_button, "Abort");
  g_signal_connect (abort_button, "clicked", G_CALLBACK (abort_stepper), stepper_window);
  gtk_toolbar_insert((GtkToolbar *)toolbar, abort_button, 3);

  
  return (GtkToolbar *)toolbar;
}

void initialize_env_symbols_list(GtkTreeView *list)
{
  GtkCellRenderer    *renderer;
  GtkTreeViewColumn  *column1, *column2;
  GtkListStore       *store;

  renderer = gtk_cell_renderer_text_new();

  column1 = gtk_tree_view_column_new_with_attributes("Symbol",
                                                     renderer, "text", 0, NULL);
  gtk_tree_view_append_column(GTK_TREE_VIEW (list), column1);

  column2 = gtk_tree_view_column_new_with_attributes("Value",
                                                     renderer, "text", 1, NULL);
  gtk_tree_view_append_column(GTK_TREE_VIEW (list), column2);

  store = gtk_list_store_new (2, G_TYPE_STRING, G_TYPE_STRING);

  gtk_tree_view_set_model(GTK_TREE_VIEW (list), 
                          GTK_TREE_MODEL(store));

  g_object_unref(store);  
}

extern OBJECT_PTR first(OBJECT_PTR);
extern OBJECT_PTR second(OBJECT_PTR);

void populate_env_symbols_list(GtkTreeView *list, OBJECT_PTR env)
{
  GtkListStore *store;
  GtkTreeIter  iter;

  store = GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(list)));

  gtk_list_store_clear(store);
  
  OBJECT_PTR rest = env;

  while(rest != NIL)
  {
    OBJECT_PTR binding = car(rest);

    char buf[MAX_STRING_LENGTH];
    memset(buf, '\0', MAX_STRING_LENGTH);

    print_object_to_string(first(binding), buf, 0);

    gtk_list_store_append(store, &iter);
    gtk_list_store_set(store, &iter, 0, buf, -1);

    memset(buf, '\0', MAX_STRING_LENGTH);
    print_object_to_string(second(binding), buf, 0);

    gtk_list_store_set(store, &iter, 1, buf, -1);

    rest = cdr(rest);
  }
}

unsigned int is_space_char(char c)
{
  return c == ' ' || c == '\n';
}

//TODO: handle strings (double quotes)
//using goto's as this seems more natural than
//shoehorning a for loop
int find_substring(char *str, char *substr, unsigned int *end)
{
  unsigned int str_ptr, substr_ptr, saved_str_ptr;

  unsigned int str_len = strlen(str);
  unsigned int substr_len = strlen(substr);

  unsigned int i = 0;

  if(str_len < substr_len)
    return -1;

  if(!strcmp(str, substr))
  {
    *end = str_len;
    return 0;
  }
  
label1:

  str_ptr = i;
  i++;

  if(i > str_len - substr_len + 1)
    return -1;
    
  substr_ptr = 0;

  saved_str_ptr = str_ptr;
  
label2:

  while((str[str_ptr] == substr[substr_ptr]) && !is_space_char(str[str_ptr]) && !is_space_char(substr[substr_ptr]))
  {
    str_ptr++;
    substr_ptr++;

    if(substr_ptr == substr_len)
    {
      *end = str_ptr;
      return saved_str_ptr;
    }
  }
    
  if(!is_space_char(str[str_ptr]) || !is_space_char(substr[substr_ptr]))
    goto label1;

  if(is_space_char(str[str_ptr]))
  {
    while(is_space_char(str[str_ptr]))
      str_ptr++;
  }

  if(is_space_char(substr[substr_ptr]))
  {
    while(is_space_char(substr[substr_ptr]))
      substr_ptr++;
  }

  goto label2;    
}

void highlight_text_stepper(GtkTextBuffer *buffer, char *text)
{
  GtkTextIter start_buffer, end_buffer, start_highlight, end_highlight;

  unsigned int end;
  
  gtk_text_buffer_get_start_iter(buffer, &start_buffer);
  gtk_text_buffer_get_end_iter(buffer, &end_buffer);
  
  gchar *s = gtk_text_buffer_get_text(buffer, &start_buffer, &end_buffer, FALSE);

  unsigned int offset = find_substring(s, text, &end);

  if(offset == -1)
    return;
  
  gtk_text_buffer_get_iter_at_offset(buffer, &start_highlight, offset);
  gtk_text_buffer_get_iter_at_offset(buffer, &end_highlight, end);

  gtk_text_buffer_apply_tag_by_name(buffer, "gray_bg", &start_highlight, &end_highlight);
  
}

void populate_stepper_fn_source(OBJECT_PTR fn_source, OBJECT_PTR exp)
{
  char buf[MAX_STRING_LENGTH];
  memset(buf, '\0', MAX_STRING_LENGTH);

  print_object_to_string(fn_source, buf, 0);

  gtk_text_buffer_set_text((GtkTextBuffer *)fn_source_buffer, convert_to_lower_case(buf), -1);

  memset(buf, '\0', MAX_STRING_LENGTH);

  print_object_to_string(exp, buf, 0);  

  highlight_text_stepper((GtkTextBuffer *)fn_source_buffer, convert_to_lower_case(buf));
}

void create_stepper_window()
{
  GtkWidget *win = gtk_window_new (GTK_WINDOW_TOPLEVEL);

  stepper_window = (GtkWindow *)win;

#ifdef WIN_BUILD
  gtk_window_set_icon_from_file(stepper_window, "../share/icons/evaluate.png", NULL);
#else
#ifdef __OSX_BUNDLE__
  gtk_window_set_icon_from_file(stepper_window, concat_strings(path_buf, exec_path, "../Resources/share/plisp/icons/evaluate.png"), NULL);
#else
  gtk_window_set_icon_from_file(stepper_window, PLISPDATADIR "/icons/evaluate.png", NULL);
#endif
#endif  

  gtk_window_set_modal(stepper_window, TRUE);
  
  GtkWidget *scrolled_win1;
  GtkWidget *vbox, *hbox1;

  gtk_window_set_title((GtkWindow *)win, "pLisp Stepper");

  gtk_window_set_default_size(stepper_window, DEFAULT_STEPPER_WINDOW_WIDTH, DEFAULT_STEPPER_WINDOW_HEIGHT);
  gtk_window_move(stepper_window, DEFAULT_STEPPER_WINDOW_POSX, DEFAULT_STEPPER_WINDOW_POSY); 

  g_signal_connect (win, "delete-event",
                    G_CALLBACK (delete_event), NULL);

  g_signal_connect (win, "focus",
                    G_CALLBACK (set_triggering_window), NULL);

  g_signal_connect(G_OBJECT(win), 
                   "key_press_event", 
                   G_CALLBACK (handle_key_press_events), 
                   NULL);

  gtk_container_set_border_width (GTK_CONTAINER (win), 10);

  vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);

  gtk_box_pack_start (GTK_BOX (vbox), (GtkWidget *)create_stepper_toolbar(), FALSE, FALSE, 0);

  //env
  scrolled_win1 = gtk_scrolled_window_new(NULL, NULL);

  env_symbols_list = (GtkTreeView *)gtk_tree_view_new();
  gtk_tree_view_set_headers_visible(GTK_TREE_VIEW(env_symbols_list), TRUE);
  gtk_widget_override_font(GTK_WIDGET(env_symbols_list), pango_font_description_from_string(FONT));

  initialize_env_symbols_list((GtkTreeView *)env_symbols_list);

  gtk_tree_view_column_set_sort_column_id(gtk_tree_view_get_column(env_symbols_list, 0), 0); 

  gtk_container_add(GTK_CONTAINER (scrolled_win1), (GtkWidget *)env_symbols_list);

  hbox1 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);

  gtk_box_pack_start(GTK_BOX (hbox1), scrolled_win1, TRUE, TRUE, 0);
  //end env
  
  //function source
  GtkWidget *scrolled_win;

  fn_source_buffer = gtk_source_buffer_new_with_language(source_language);
  GtkSourceView *fn_source_view = (GtkSourceView *)gtk_source_view_new_with_buffer(fn_source_buffer);

  gtk_widget_override_font(GTK_WIDGET(fn_source_view), pango_font_description_from_string(FONT));
  gtk_text_view_set_editable((GtkTextView *)fn_source_view, FALSE);

  gtk_text_buffer_create_tag((GtkTextBuffer *)fn_source_buffer, "gray_bg", 
                             "background", "lightgray", NULL); 
  
  scrolled_win = gtk_scrolled_window_new (NULL, NULL);

  //to provide sufficient height to avoid scrolling through the code where possible
  gtk_widget_set_size_request(scrolled_win, DEFAULT_STEPPER_WINDOW_WIDTH, 200);

  gtk_container_add (GTK_CONTAINER (scrolled_win), (GtkWidget *)fn_source_view);

  //end function source

  gtk_box_pack_start (GTK_BOX (vbox), hbox1, TRUE, TRUE, 0);
  gtk_box_pack_start (GTK_BOX (vbox), scrolled_win, TRUE, TRUE, 0);

  gtk_container_add (GTK_CONTAINER (win), vbox);
  
  //gtk_widget_grab_focus((GtkWidget *)env_symbols_list);
}

void show_stepper_window(OBJECT_PTR exp, OBJECT_PTR env, continuation_t *k, OBJECT_PTR fn_source)
{
  populate_env_symbols_list((GtkTreeView *)env_symbols_list, env);
  populate_stepper_fn_source(fn_source, exp);

  gtk_widget_show_all((GtkWidget *)stepper_window);

  gtk_main();

  //since the stepper_window will already be closed when an
  //exception dialog is displayed, the fool-proof option
  //is to center the dialog against the transcript window
  //(the '(step ..)' expression might have been evaluated
  //from the workspace, file browser, etc.).
  action_triggering_window = transcript_window;

  dummy = k;
}
