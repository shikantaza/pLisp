#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>

#include "../plisp.h"

#define FONT "Courier Bold 9"

#define MAX_STRING_LENGTH 8096

GtkTextBuffer *transcript_buffer;
GtkTextBuffer *workspace_buffer;
GtkTextBuffer *system_browser_buffer;

GtkWindow *transcript_window;
GtkWindow *workspace_window;
GtkWindow *system_browser_window;
GtkWindow *debugger_window;

GtkTreeView *packages_list;
GtkTreeView *symbols_list;

GtkTextView *transcript_textview;
GtkTextView *system_browser_textview;

BOOLEAN new_symbol_being_created;

GtkWindow *action_triggering_window;

GtkTreeView *frames_list;
GtkTreeView *variables_list;

GtkStatusbar *system_browser_statusbar;

extern unsigned int nof_packages;
extern package_t *packages;

extern OBJECT_PTR debug_execution_stack;
extern OBJECT_PTR NIL;

/* event handler function definitions begin */
extern gboolean delete_event(GtkWidget *,
                             GdkEvent *,
                             gpointer);
extern void quit(GtkWidget *, gpointer);
extern void new_package(GtkWidget *, gpointer);
extern void new_symbol(GtkWidget *, gpointer);
extern void accept(GtkWidget *, gpointer);
extern void delete_pkg_or_sym(GtkWidget *, gpointer);
extern void refresh_sys_browser(GtkWidget *, gpointer);
extern void close_window(GtkWidget *, gpointer);
extern gboolean handle_key_press_events(GtkWidget *, 
                                        GdkEventKey *,
                                        gpointer);
extern void load_source_file(GtkWidget *, gpointer);
extern void show_workspace_win(GtkWidget *, gpointer);
extern void load_image_file(GtkWidget *, gpointer);
extern void save_image_file(GtkWidget *, gpointer);
extern void show_system_browser_win(GtkWidget *, gpointer);
extern void fetch_package_members(GtkWidget *, gpointer);
extern void fetch_symbol_value(GtkWidget *, gpointer);
extern void eval_expression(GtkWidget *, gpointer);
extern void fetch_variables(GtkWidget *, gpointer);
/* event handler function definitions end */

typedef struct
{
  GtkWidget *textview;
} Widgets;

void set_workspace_window_title(char *title)
{
  gtk_window_set_title(workspace_window, title);
}

void transcript_backspace()
{
  GtkTextIter endIter;
  gtk_text_buffer_get_iter_at_offset(transcript_buffer, &endIter, -1);
  gtk_text_buffer_backspace(transcript_buffer, &endIter, FALSE, TRUE);
}

void print_to_transcript(char * str)
{
  gtk_text_buffer_insert_at_cursor(transcript_buffer, str, -1);

  //scroll to the end of the text
  GtkTextIter iter;
  gtk_text_buffer_get_end_iter (transcript_buffer, &iter);
  gtk_text_view_scroll_to_iter(transcript_textview,
                               &iter, 0.0, FALSE, 0, 0);
}

void print_to_workspace(char * str)
{
  gtk_text_buffer_insert_at_cursor(workspace_buffer, str, -1);
}

void print_ui_copyright_notice()
{
  //print_to_transcript("pLisp is an interpreter for a Lisp-1 dialect.\n\n");
  print_to_transcript("Copyright 2011-2013 Rajesh Jayaprakash <rajesh.jayaprakash@gmail.com>\n\n");
  print_to_transcript("pLisp is free software: you can redistribute it and/or modify\n");
  print_to_transcript("it under the terms of the GNU General Public License as published by\n");
  print_to_transcript("the Free Software Foundation, either version 3 of the License, or\n");
  print_to_transcript("(at your option) any later version.\n\n");

  print_to_transcript("pLisp is distributed in the hope that it will be useful,\n");
  print_to_transcript("but WITHOUT ANY WARRANTY; without even the implied warranty of\n");
  print_to_transcript("MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n");
  print_to_transcript("GNU General Public License for more details.\n\n");

  print_to_transcript("You should have received a copy of the GNU General Public License\n");
  print_to_transcript("along with pLisp.  If not, see <http://www.gnu.org/licenses/>.\n\n");

  print_to_transcript("This is the transcript window. Results of evaluating expressions\n");
  print_to_transcript("(entered in the workspace window) will be displayed here.\n\n");
}

GtkToolbar *create_workspace_toolbar()
{
  GtkWidget *toolbar;

  GtkWidget *load_icon = gtk_image_new_from_file ("icons/load_file.png");
  GtkWidget *eval_icon = gtk_image_new_from_file ("icons/evaluate.png");
  GtkWidget *exit_icon = gtk_image_new_from_file ("icons/exit32x32.png");

  toolbar = gtk_toolbar_new ();
  gtk_toolbar_set_orientation (GTK_TOOLBAR (toolbar), GTK_ORIENTATION_HORIZONTAL);
  gtk_toolbar_set_style (GTK_TOOLBAR (toolbar), GTK_TOOLBAR_BOTH);
  gtk_container_set_border_width (GTK_CONTAINER (toolbar), 5);

  gtk_toolbar_append_item (GTK_TOOLBAR (toolbar),                   
                           NULL,                                   /* button label */
                           "Load file (Ctrl-O)",                   /* button's tooltip */
                           "Private",                              /* tooltip private info */
                           load_icon,                              /* icon widget */
                           GTK_SIGNAL_FUNC(load_source_file),      /* a signal */
                           NULL);

  gtk_toolbar_append_item (GTK_TOOLBAR (toolbar),                   
                           NULL,                                   /* button label */
                           "Evaluate (F5)",                        /* button's tooltip */
                           "Private",                              /* tooltip private info */
                           eval_icon,                              /* icon widget */
                           GTK_SIGNAL_FUNC(eval_expression),       /* a signal */
                           (GtkWidget *)workspace_window);

  gtk_toolbar_append_item (GTK_TOOLBAR (toolbar),                   
                           NULL,                                   /* button label */
                           "Close (Ctrl-W)",                       /* button's tooltip */
                           "Private",                              /* tooltip private info */
                           exit_icon,                              /* icon widget */
                           GTK_SIGNAL_FUNC(close_window),          /* a signal */
                           (GtkWidget *)workspace_window);

  return (GtkToolbar *)toolbar;
}

void create_workspace_window()
{
  GtkWidget *win = gtk_window_new (GTK_WINDOW_TOPLEVEL);

  workspace_window = (GtkWindow *)win;

  gtk_window_set_default_size((GtkWindow *)win, 600, 400);
  gtk_window_move((GtkWindow *)win, 650, 200); 

  g_signal_connect (win, "delete-event",
                    G_CALLBACK (delete_event), NULL);

  gtk_container_set_border_width (GTK_CONTAINER (win), 10);

  //Widgets *w = g_slice_new (Widgets);
  GtkWidget *scrolled_win, *vbox;

  GtkWidget *textview = gtk_text_view_new ();

  gtk_widget_modify_font(GTK_WIDGET(textview), pango_font_description_from_string(FONT));

  workspace_buffer = gtk_text_view_get_buffer((GtkTextView *)textview);

  print_to_workspace("; This is the workspace; type pLisp expressions here.\n");
  print_to_workspace("; Select an expression and press F5 to evaluate it.\n");
  print_to_workspace("; Alternately, if the expression fits in a single line,\n");
  print_to_workspace("; place the cursor anywhere on the line and press F5.\n");

  g_signal_connect(G_OBJECT(win), 
                   "key_press_event", 
                   G_CALLBACK (handle_key_press_events), 
                   NULL);

  scrolled_win = gtk_scrolled_window_new (NULL, NULL);
  gtk_container_add (GTK_CONTAINER (scrolled_win), textview);

  vbox = gtk_vbox_new (FALSE, 5);
  gtk_box_pack_start (GTK_BOX (vbox), (GtkWidget *)create_workspace_toolbar(), FALSE, FALSE, 0);
  gtk_box_pack_start (GTK_BOX (vbox), scrolled_win, TRUE, TRUE, 0);
  
  gtk_container_add (GTK_CONTAINER (win), vbox);

  gtk_widget_show_all(win);

  prompt();

  gtk_widget_grab_focus(textview);
}

void show_error_dialog_for_window(char *msg, GtkWindow *win)
{
  GtkWidget *dialog = gtk_message_dialog_new ((GtkWindow *)win,
                                              GTK_DIALOG_DESTROY_WITH_PARENT,
                                              GTK_MESSAGE_ERROR,
                                              GTK_BUTTONS_CLOSE,
                                              msg);
  gtk_dialog_run(GTK_DIALOG (dialog));
  gtk_widget_destroy((GtkWidget *)dialog);
}

void initialize_packages_list(GtkTreeView *list)
{
  GtkCellRenderer    *renderer;
  GtkTreeViewColumn  *column1, *column2;
  GtkListStore       *store;

  renderer = gtk_cell_renderer_text_new();

  column1 = gtk_tree_view_column_new_with_attributes("Packages",
                                                     renderer, "text", 0, NULL);
  gtk_tree_view_append_column(GTK_TREE_VIEW (list), column1);

  /* column2 = gtk_tree_view_column_new_with_attributes("ID", */
  /*                                                    renderer, "text", 1, NULL); */
  /* gtk_tree_view_append_column(GTK_TREE_VIEW (list), column2); */

  store = gtk_list_store_new (2, G_TYPE_STRING, G_TYPE_INT);

  gtk_tree_view_set_model(GTK_TREE_VIEW (list), 
                          GTK_TREE_MODEL(store));

  g_object_unref(store);  
}

void initialize_symbols_list(GtkTreeView *list)
{
  GtkCellRenderer    *renderer;
  GtkTreeViewColumn  *column1, *column2;
  GtkListStore       *store;

  renderer = gtk_cell_renderer_text_new();

  column1 = gtk_tree_view_column_new_with_attributes("Symbols",
                                                     renderer, "text", 0, NULL);
  gtk_tree_view_append_column(GTK_TREE_VIEW (list), column1);

  /* column2 = gtk_tree_view_column_new_with_attributes("ID", */
  /*                                                    renderer, "text", 1, NULL); */
  /* gtk_tree_view_append_column(GTK_TREE_VIEW (list), column2); */

  store = gtk_list_store_new (2, G_TYPE_STRING, G_TYPE_INT);

  gtk_tree_view_set_model(GTK_TREE_VIEW (list), 
                          GTK_TREE_MODEL(store));

  g_object_unref(store);  
}

void populate_packages_list()
{
  GtkListStore *store;
  GtkTreeIter  iter;

  store = GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(packages_list)));

  int i;

  for(i=0; i<nof_packages; i++)
  {
    gtk_list_store_append(store, &iter);
    gtk_list_store_set(store, &iter, 0, strdup(packages[i].name), -1);  
    gtk_list_store_set(store, &iter, 1, i, -1);
  }

}

void remove_all_from_list(GtkTreeView *list)
{
  GtkListStore *store;
  GtkTreeModel *model;
  GtkTreeIter  iter;

  model = gtk_tree_view_get_model (GTK_TREE_VIEW (list));
  store = GTK_LIST_STORE(model);

  if(gtk_tree_model_get_iter_first(model, &iter) == FALSE) 
      return;
  gtk_list_store_clear(store);
}


GtkToolbar *create_system_browser_toolbar()
{
  GtkWidget *toolbar;

  GtkWidget *new_package_icon = gtk_image_new_from_file ("icons/new_package.png");
  GtkWidget *new_symbol_icon = gtk_image_new_from_file ("icons/new_symbol.png");
  GtkWidget *accept_icon = gtk_image_new_from_file ("icons/accept.png");
  GtkWidget *delete_icon = gtk_image_new_from_file ("icons/delete.png");
  GtkWidget *refresh_icon = gtk_image_new_from_file ("icons/refresh.png");
  GtkWidget *exit_icon = gtk_image_new_from_file ("icons/exit32x32.png");

  toolbar = gtk_toolbar_new ();
  gtk_toolbar_set_orientation (GTK_TOOLBAR (toolbar), GTK_ORIENTATION_HORIZONTAL);
  gtk_toolbar_set_style (GTK_TOOLBAR (toolbar), GTK_TOOLBAR_BOTH);
  gtk_container_set_border_width (GTK_CONTAINER (toolbar), 5);

  gtk_toolbar_append_item (GTK_TOOLBAR (toolbar),                   
                           NULL,                                   /* button label */
                           "New package (Ctrl-K)",                 /* button's tooltip */
                           "Private",                              /* tooltip private info */
                           new_package_icon,                       /* icon widget */
                           GTK_SIGNAL_FUNC(new_package),            /* a signal */
                           NULL);

  gtk_toolbar_append_item (GTK_TOOLBAR (toolbar),                   
                           NULL,                                   /* button label */
                           "New symbol (Ctrl-N)",                  /* button's tooltip */
                           "Private",                              /* tooltip private info */
                           new_symbol_icon,                        /* icon widget */
                           GTK_SIGNAL_FUNC(new_symbol),            /* a signal */
                           (GtkWidget *)system_browser_window);

  gtk_toolbar_append_item (GTK_TOOLBAR (toolbar),                   
                           NULL,                                   /* button label */
                           "Accept (Ctrl-S)",                      /* button's tooltip */
                           "Private",                              /* tooltip private info */
                           accept_icon,                            /* icon widget */
                           GTK_SIGNAL_FUNC(accept),                /* a signal */
                           (GtkWidget *)system_browser_window);

  gtk_toolbar_append_item (GTK_TOOLBAR (toolbar),                   
                           NULL,                                   /* button label */
                           "Delete symbol (Ctrl-X)",               /* button's tooltip */
                           "Private",                              /* tooltip private info */
                           delete_icon,                            /* icon widget */
                           GTK_SIGNAL_FUNC(delete_pkg_or_sym),     /* a signal */
                           NULL);

  gtk_toolbar_append_item (GTK_TOOLBAR (toolbar),                   
                           NULL,                                   /* button label */
                           "Refresh (F5)",                         /* button's tooltip */
                           "Private",                              /* tooltip private info */
                           refresh_icon,                           /* icon widget */
                           GTK_SIGNAL_FUNC(refresh_sys_browser),   /* a signal */
                           NULL);

  gtk_toolbar_append_item (GTK_TOOLBAR (toolbar),                   
                           NULL,                                   /* button label */
                           "Close (Ctrl-W)",                       /* button's tooltip */
                           "Private",                              /* tooltip private info */
                           exit_icon,                              /* icon widget */
                           GTK_SIGNAL_FUNC(close_window),          /* a signal */
                           (GtkWidget *)system_browser_window);

  return (GtkToolbar *)toolbar;
}

void create_system_browser_window()
{
  GtkWidget *win = gtk_window_new (GTK_WINDOW_TOPLEVEL);

  system_browser_window = (GtkWindow *)win;

  GtkWidget *scrolled_win1, *scrolled_win2;
  GtkWidget *vbox, *hbox;

  gtk_window_set_title((GtkWindow *)win, "pLisp System Browser");
  gtk_window_set_default_size((GtkWindow *)win, 600, 400);
  gtk_window_set_position(GTK_WINDOW(win), GTK_WIN_POS_CENTER);

  g_signal_connect (win, "delete-event",
                    G_CALLBACK (delete_event), NULL);

  g_signal_connect(G_OBJECT(win), 
                   "key_press_event", 
                   G_CALLBACK (handle_key_press_events), 
                   NULL);

  gtk_container_set_border_width (GTK_CONTAINER (win), 10);

  scrolled_win1 = gtk_scrolled_window_new(NULL, NULL);
  scrolled_win2 = gtk_scrolled_window_new(NULL, NULL);

  packages_list = (GtkTreeView *)gtk_tree_view_new();
  gtk_tree_view_set_headers_visible(GTK_TREE_VIEW(packages_list), TRUE);
  gtk_widget_modify_font(GTK_WIDGET(packages_list), pango_font_description_from_string(FONT));

  initialize_packages_list((GtkTreeView *)packages_list);

  gtk_tree_view_column_set_sort_column_id(gtk_tree_view_get_column(packages_list, 0), 0); 

  symbols_list = (GtkTreeView *)gtk_tree_view_new();
  gtk_tree_view_set_headers_visible(GTK_TREE_VIEW(symbols_list), TRUE);
  gtk_widget_modify_font(GTK_WIDGET(symbols_list), pango_font_description_from_string(FONT));

  initialize_symbols_list((GtkTreeView *)symbols_list);
  gtk_tree_view_column_set_sort_column_id(gtk_tree_view_get_column(symbols_list, 0), 0);

  populate_packages_list();

  g_signal_connect(G_OBJECT(packages_list), "cursor-changed",
          G_CALLBACK(fetch_package_members), NULL);

  g_signal_connect(G_OBJECT(symbols_list), "cursor-changed",
                   G_CALLBACK(fetch_symbol_value), NULL);

  gtk_container_add(GTK_CONTAINER (scrolled_win1), (GtkWidget *)packages_list);
  gtk_container_add(GTK_CONTAINER (scrolled_win2), (GtkWidget *)symbols_list);

  hbox = gtk_hbox_new (FALSE, 5);
  gtk_box_pack_start_defaults (GTK_BOX (hbox), scrolled_win1);
  gtk_box_pack_start_defaults (GTK_BOX (hbox), scrolled_win2);

  GtkWidget *scrolled_win;

  GtkWidget *textview = gtk_text_view_new ();

  system_browser_textview = (GtkTextView *)textview;

  gtk_widget_modify_font(GTK_WIDGET(textview), pango_font_description_from_string(FONT));
  //gtk_text_view_set_editable((GtkTextView *)textview, FALSE);

  system_browser_buffer = gtk_text_view_get_buffer((GtkTextView *)textview);

  scrolled_win = gtk_scrolled_window_new (NULL, NULL);
  gtk_container_add (GTK_CONTAINER (scrolled_win), textview);

  vbox = gtk_vbox_new (FALSE, 5);
  gtk_box_pack_start (GTK_BOX (vbox), (GtkWidget *)create_system_browser_toolbar(), FALSE, FALSE, 0);
  gtk_box_pack_start (GTK_BOX (vbox), hbox, TRUE, TRUE, 0);
  gtk_box_pack_start (GTK_BOX (vbox), scrolled_win, TRUE, TRUE, 0);

  system_browser_statusbar = (GtkStatusbar *)gtk_statusbar_new();
  gtk_box_pack_start (GTK_BOX (vbox), (GtkWidget *)system_browser_statusbar, FALSE, FALSE, 0);  

  gtk_container_add (GTK_CONTAINER (win), vbox);

  gtk_widget_show_all(win);

  gtk_widget_grab_focus((GtkWidget *)packages_list);

  new_symbol_being_created = false;
}

void refresh_system_browser()
{
  populate_packages_list();
  fetch_package_symbols();
  gtk_widget_grab_focus((GtkWidget *)packages_list);
}

GtkToolbar *create_transcript_toolbar()
{
  GtkWidget *toolbar;

  GtkWidget *load_icon = gtk_image_new_from_file ("icons/load_image.png");
  GtkWidget *save_icon = gtk_image_new_from_file ("icons/save_image.png");
  GtkWidget *workspace_icon = gtk_image_new_from_file ("icons/workspace.png");
  GtkWidget *browser_icon = gtk_image_new_from_file ("icons/browser.png");
  GtkWidget *exit_icon = gtk_image_new_from_file ("icons/exit.png");

  toolbar = gtk_toolbar_new ();
  gtk_toolbar_set_orientation (GTK_TOOLBAR (toolbar), GTK_ORIENTATION_HORIZONTAL);
  gtk_toolbar_set_style (GTK_TOOLBAR (toolbar), GTK_TOOLBAR_BOTH);
  gtk_container_set_border_width (GTK_CONTAINER (toolbar), 5);
  //gtk_toolbar_set_space_size (GTK_TOOLBAR (toolbar), 5);

  gtk_toolbar_append_item (GTK_TOOLBAR (toolbar),                   
                           NULL,                                   /* button label */
                           "Load image (Ctrl-L)",                  /* button's tooltip */
                           "Private",                              /* tooltip private info */
                           load_icon,                              /* icon widget */
                           GTK_SIGNAL_FUNC(load_image_file),       /* a signal */
                           NULL);

  gtk_toolbar_append_item (GTK_TOOLBAR (toolbar),                   
                           NULL,                                   /* button label */
                           "Save image (Ctrl-S)",                  /* button's tooltip */
                           "Private",                              /* tooltip private info */
                           save_icon,                              /* icon widget */
                           GTK_SIGNAL_FUNC(save_image_file),       /* a signal */
                           NULL);


  gtk_toolbar_append_item (GTK_TOOLBAR (toolbar),                   
                           NULL,                                   /* button label */
                           "Show workspace window (F7)",           /* button's tooltip */
                           "Private",                              /* tooltip private info */
                           workspace_icon,                         /* icon widget */
                           GTK_SIGNAL_FUNC(show_workspace_win),    /* a signal */
                           NULL);

  gtk_toolbar_append_item (GTK_TOOLBAR (toolbar),                   
                           NULL,                                        /* button label */
                           "System Browser (F9)",                       /* button's tooltip */
                           "Private",                                   /* tooltip private info */
                           browser_icon,                                /* icon widget */
                           GTK_SIGNAL_FUNC(show_system_browser_win),    /* a signal */
                           NULL);

  gtk_toolbar_append_item (GTK_TOOLBAR (toolbar),                   
                           NULL,                                   /* button label */
                           "Exit (Ctrl-W)",                        /* button's tooltip */
                           "Private",                              /* tooltip private info */
                           exit_icon,                              /* icon widget */
                           GTK_SIGNAL_FUNC(quit),                  /* a signal */
                           NULL);

  return (GtkToolbar *)toolbar;
}

void create_transcript_window()
{
  GtkWidget *scrolled_win, *vbox;

  transcript_window = (GtkWindow *)gtk_window_new (GTK_WINDOW_TOPLEVEL);

  //gtk_window_set_title((GtkWindow *)transcript_window, "pLisp Transcript");
  update_transcript_title();

  gtk_window_set_default_size((GtkWindow *)transcript_window, 600, 400);
  gtk_window_set_position(GTK_WINDOW(transcript_window), GTK_WIN_POS_CENTER);
    
  g_signal_connect (transcript_window, "delete-event",
                    G_CALLBACK (delete_event), NULL);
    
  g_signal_connect (transcript_window, "destroy",
		      G_CALLBACK (quit), NULL);

  g_signal_connect(transcript_window, 
                   "key_press_event", 
                   G_CALLBACK (handle_key_press_events), 
                   NULL);

    
  gtk_container_set_border_width (GTK_CONTAINER (transcript_window), 10);
  
  GtkWidget *textview = gtk_text_view_new ();

  transcript_textview = (GtkTextView *)textview;

  gtk_text_view_set_editable((GtkTextView *)textview, FALSE);
  gtk_text_view_set_cursor_visible((GtkTextView *)textview, FALSE);
  gtk_widget_set_sensitive(textview, FALSE);

  gtk_widget_modify_font(GTK_WIDGET(textview), pango_font_description_from_string(FONT));

  transcript_buffer = gtk_text_view_get_buffer((GtkTextView *)textview);

  print_ui_copyright_notice();

  scrolled_win = gtk_scrolled_window_new (NULL, NULL);
  gtk_container_add (GTK_CONTAINER (scrolled_win), textview);

  vbox = gtk_vbox_new (FALSE, 5);

  gtk_box_pack_start (GTK_BOX (vbox), (GtkWidget *)create_transcript_toolbar(), FALSE, FALSE, 0);
  gtk_box_pack_start (GTK_BOX (vbox), scrolled_win, TRUE, TRUE, 0);
  
  gtk_container_add (GTK_CONTAINER (transcript_window), vbox);
  
  gtk_widget_show_all((GtkWidget *)transcript_window);
}

void show_error_dialog(char *msg)
{
  GtkWidget *dialog = gtk_message_dialog_new (action_triggering_window,
                                              GTK_DIALOG_DESTROY_WITH_PARENT,
                                              GTK_MESSAGE_ERROR,
                                              GTK_BUTTONS_CLOSE,
                                              msg);
  gtk_dialog_run(GTK_DIALOG (dialog));
  gtk_widget_destroy((GtkWidget *)dialog);
}

//use this when we include a button to display the stack trace
void error_window(char *msg)
{
  GtkWidget *window, *scrolled_win, *hbox, *vbox, *ok;

  window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title (GTK_WINDOW (window), "Error");
  gtk_container_set_border_width (GTK_CONTAINER (window), 10);
  gtk_window_set_default_size((GtkWindow *)window, 400, 50);
  gtk_window_move((GtkWindow *)window, 500, 200); 

  GtkWidget *textview = gtk_text_view_new ();

  gtk_widget_modify_font(GTK_WIDGET(textview), pango_font_description_from_string(FONT));
  gtk_text_buffer_insert_at_cursor(gtk_text_view_get_buffer((GtkTextView *)textview), msg, -1);

  ok = gtk_button_new_with_label("OK");

  g_signal_connect_swapped (ok, "clicked",
                            G_CALLBACK (gtk_widget_destroy),
                            window);

  scrolled_win = gtk_scrolled_window_new (NULL, NULL);
  gtk_container_add (GTK_CONTAINER (scrolled_win), textview);

  hbox = gtk_hbox_new (FALSE, 5);
  gtk_box_pack_start_defaults (GTK_BOX (hbox), ok);

  vbox = gtk_vbox_new (FALSE, 5);
  gtk_box_pack_start (GTK_BOX (vbox), scrolled_win, TRUE, TRUE, 0);
  gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, TRUE, 0);

  gtk_container_add (GTK_CONTAINER (window), vbox);
  gtk_widget_show_all (window);
}

void initialize_frames_list(GtkTreeView *list)
{
  GtkCellRenderer    *renderer;
  GtkTreeViewColumn  *column1, *column2;
  GtkListStore       *store;

  renderer = gtk_cell_renderer_text_new();

  column1 = gtk_tree_view_column_new_with_attributes("Frame",
                                                     renderer, "text", 0, NULL);
  gtk_tree_view_append_column(GTK_TREE_VIEW (list), column1);

  column2 = gtk_tree_view_column_new_with_attributes("Function",
                                                     renderer, "text", 1, NULL);
  gtk_tree_view_append_column(GTK_TREE_VIEW (list), column2);

  /* column3 = gtk_tree_view_column_new_with_attributes("ID", */
  /*                                                    renderer, "text", 1, NULL); */
  /* gtk_tree_view_append_column(GTK_TREE_VIEW (list), column3); */

  store = gtk_list_store_new (3, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_INT);

  gtk_tree_view_set_model(GTK_TREE_VIEW (list), 
                          GTK_TREE_MODEL(store));

  g_object_unref(store);  
}

void initialize_variables_list(GtkTreeView *list)
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

void populate_frames_list(GtkTreeView *list)
{
  GtkListStore *store;
  GtkTreeIter  iter;

  store = GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(list)));

  /* int i; */

  /* for(i=0; i<nof_packages; i++) */
  /* { */
  /*   gtk_list_store_append(store, &iter); */
  /*   gtk_list_store_set(store, &iter, 0, strdup(packages[i].name), -1);   */
  /*   gtk_list_store_set(store, &iter, 1, i, -1); */
  /* } */

  OBJECT_PTR rest = debug_execution_stack;

  while(rest != NIL)
  {
    OBJECT_PTR frame = car(rest);

    char buf[MAX_STRING_LENGTH];
    memset(buf, '\0', MAX_STRING_LENGTH);

    sprintf(buf, "#<Frame #x%08x> ", frame);

    OBJECT_PTR env = get_heap((frame >> OBJECT_SHIFT) + 2);
    OBJECT_PTR source_expression  = get_heap((frame >> OBJECT_SHIFT) + 4);    

    gtk_list_store_append(store, &iter);
    gtk_list_store_set(store, &iter, 0, buf, -1);

    memset(buf, '\0', MAX_STRING_LENGTH);
    print_object_to_string(car(source_expression), buf, 0);
    gtk_list_store_set(store, &iter, 1, buf, -1);

    gtk_list_store_set(store, &iter, 2, env, -1);

    rest = cdr(rest);
  }
}

void create_debug_window()
{
  GtkWidget *win = gtk_window_new (GTK_WINDOW_TOPLEVEL);

  debugger_window = (GtkWindow *)win;

  GtkWidget *scrolled_win1, *scrolled_win2;
  GtkWidget *vbox, *hbox1, *hbox2;

  gtk_window_set_title((GtkWindow *)win, "pLisp Debugger");
  gtk_window_set_default_size((GtkWindow *)win, 600, 400);
  gtk_window_set_position(GTK_WINDOW(win), GTK_WIN_POS_CENTER);

  g_signal_connect (win, "delete-event",
                    G_CALLBACK (delete_event), NULL);

  g_signal_connect(G_OBJECT(win), 
                   "key_press_event", 
                   G_CALLBACK (handle_key_press_events), 
                   NULL);

  gtk_container_set_border_width (GTK_CONTAINER (win), 10);

  scrolled_win1 = gtk_scrolled_window_new(NULL, NULL);
  scrolled_win2 = gtk_scrolled_window_new(NULL, NULL);

  frames_list = (GtkTreeView *)gtk_tree_view_new();
  gtk_tree_view_set_headers_visible(GTK_TREE_VIEW(frames_list), TRUE);
  gtk_widget_modify_font(GTK_WIDGET(frames_list), pango_font_description_from_string(FONT));

  initialize_frames_list((GtkTreeView *)frames_list);

  //frames should NOT be sorted
  //gtk_tree_view_column_set_sort_column_id(gtk_tree_view_get_column(frames_list, 0), 0); 

  variables_list = (GtkTreeView *)gtk_tree_view_new();
  gtk_tree_view_set_headers_visible(GTK_TREE_VIEW(variables_list), TRUE);
  gtk_widget_modify_font(GTK_WIDGET(variables_list), pango_font_description_from_string(FONT));

  initialize_variables_list((GtkTreeView *)variables_list);
  gtk_tree_view_column_set_sort_column_id(gtk_tree_view_get_column(variables_list, 0), 0);

  populate_frames_list((GtkTreeView *)frames_list);

  g_signal_connect(G_OBJECT(frames_list), "cursor-changed",
          G_CALLBACK(fetch_variables), NULL);

  gtk_container_add(GTK_CONTAINER (scrolled_win1), (GtkWidget *)frames_list);
  gtk_container_add(GTK_CONTAINER (scrolled_win2), (GtkWidget *)variables_list);

  hbox1 = gtk_hbox_new (FALSE, 5);
  hbox2 = gtk_hbox_new (FALSE, 5);

  gtk_box_pack_start_defaults (GTK_BOX (hbox1), scrolled_win1);
  gtk_box_pack_start_defaults (GTK_BOX (hbox2), scrolled_win2);

  vbox = gtk_vbox_new (FALSE, 5);
  //uncomment this when adding debugger toolbar
  //gtk_box_pack_start (GTK_BOX (vbox), (GtkWidget *)create_debugger_toolbar(), FALSE, FALSE, 0);
  gtk_box_pack_start (GTK_BOX (vbox), hbox1, TRUE, TRUE, 0);
  gtk_box_pack_start (GTK_BOX (vbox), hbox2, TRUE, TRUE, 0);

  //uncomment this to add status bar
  //gtk_box_pack_start (GTK_BOX (vbox), gtk_statusbar_new(), FALSE, FALSE, 0);  

  gtk_container_add (GTK_CONTAINER (win), vbox);

  gtk_widget_show_all(win);

  gtk_widget_grab_focus((GtkWidget *)frames_list);
}
