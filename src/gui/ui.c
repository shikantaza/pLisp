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

#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>

#include <gtksourceview/gtksource.h>

#include "../plisp.h"
#include "../memory.h"
#include "../hashtable.h"
#include "../util.h"

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

GtkTextBuffer *transcript_buffer;
GtkTextBuffer *workspace_buffer;
GtkTextBuffer *system_browser_buffer;
GtkSourceBuffer *callers_source_buffer;

GtkWindow *transcript_window;
GtkWindow *workspace_window;
GtkWindow *system_browser_window;
GtkWindow *debugger_window;
GtkWindow *profiler_window;

GtkWindow *callers_window;

GtkWindow *help_window = NULL;
GtkTextBuffer *help_buffer = NULL;

GtkTreeView *packages_list;
GtkTreeView *symbols_list;

GtkTextView *transcript_textview;
GtkTextView *system_browser_textview;

GtkTreeView *callers_symbols_list;
GtkSourceView *callers_source_view;

BOOLEAN new_symbol_being_created;

GtkWindow *action_triggering_window;

GtkTreeView *frames_list;
GtkTreeView *variables_list;

GtkStatusbar *system_browser_statusbar;
GtkStatusbar *workspace_statusbar;
GtkStatusbar *callers_statusbar;

GtkTreeView *operators_list;

OBJECT_PTR debug_window_dbg_stack;

extern unsigned int nof_packages;
extern package_t *packages;

extern OBJECT_PTR NIL;

extern OBJECT_PTR LAMBDA;

extern hashtable_t *profiling_tab;

/* event handler function definitions begin */
extern gboolean delete_event(GtkWidget *,
                             GdkEvent *,
                             gpointer);
extern void quit(GtkWidget *, gpointer);
extern void new_package(GtkWidget *, gpointer);
extern void new_symbol(GtkWidget *, gpointer);
extern void rename_symbol(GtkWidget *, gpointer);
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

extern void show_file_browser_win(GtkWidget *, gpointer);

extern void resume_from_debugger(GtkWidget *, gpointer);
extern void abort_debugger(GtkWidget *, gpointer);

extern void clear_transcript(GtkWidget *, gpointer);
extern void clear_workspace(GtkWidget *, gpointer);

extern void exp_pkg(GtkWidget *, gpointer);

extern void callers(GtkWidget *, gpointer);

extern void handle_cursor_move(GtkWidget *, gpointer);

extern void fetch_symbol_value_for_caller(GtkWidget *, gpointer);

extern void set_triggering_window(GtkWidget *, gpointer);
/* event handler function definitions end */

extern BOOLEAN in_break;

extern BOOLEAN console_mode, pipe_mode;
extern BOOLEAN image_mode;

extern OBJECT_PTR debug_stack;

extern BOOLEAN debug_mode;

GtkSourceLanguage *source_language;
GtkSourceLanguageManager *lm;

GtkSourceView *workspace_source_view;
GtkSourceBuffer *workspace_source_buffer;

GtkSourceView *system_browser_source_view;
GtkSourceBuffer *system_browser_source_buffer;

extern unsigned nof_global_vars;
extern global_var_mapping_t *top_level_symbols;

extern BOOLEAN IS_NATIVE_FN_OBJECT(OBJECT_PTR);
extern BOOLEAN IS_CONS_OBJECT(OBJECT_PTR);
extern BOOLEAN IS_FUNCTION2_OBJECT(OBJECT_PTR);
extern BOOLEAN IS_MACRO2_OBJECT(OBJECT_PTR);

extern OBJECT_PTR CADR(OBJECT_PTR);

OBJECT_PTR callers_sym;

extern OBJECT_PTR reverse_sym_lookup(OBJECT_PTR);

#ifdef __OSX_BUNDLE__
extern char exec_path[512];
extern char path_buf[1024];
#endif

void set_up_system_browser_source_buffer()
{
  system_browser_source_buffer = gtk_source_buffer_new_with_language(source_language);
  system_browser_source_view = gtk_source_view_new_with_buffer(system_browser_source_buffer);
}

void set_up_workspace_source_buffer()
{
  workspace_source_buffer = gtk_source_buffer_new_with_language(source_language);
  workspace_source_view = gtk_source_view_new_with_buffer(workspace_source_buffer);
}

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
  if(console_mode || pipe_mode)
    fprintf(stdout, "%s", str);
  else
  {
    //gtk_text_buffer_insert_at_cursor(transcript_buffer, str, -1);

    //scroll to the end of the text
    /* GtkTextIter iter; */
    /* gtk_text_buffer_get_end_iter (transcript_buffer, &iter); */
    /* gtk_text_view_scroll_to_iter(transcript_textview, */
    /*     			 &iter, 0.0, TRUE, 1.0, 1.0); */

    GtkTextMark *mark = gtk_text_buffer_get_insert(transcript_buffer);
    GtkTextIter iter;

    gtk_text_buffer_get_end_iter(transcript_buffer, &iter );
    gtk_text_buffer_move_mark(transcript_buffer, mark, &iter );
    gtk_text_buffer_insert_at_cursor(transcript_buffer, str, -1 );
    gtk_text_view_scroll_to_mark(transcript_textview, mark, 0.0, TRUE, 0.5, 1 );
  }
}

void print_to_workspace(char * str)
{
  gtk_text_buffer_insert_at_cursor(workspace_buffer, str, -1);
}

void print_ui_copyright_notice()
{
  //print_to_transcript("pLisp is an interpreter for a Lisp-1 dialect.\n\n");
  print_to_transcript("Copyright 2011-2019 Rajesh Jayaprakash <rajesh.jayaprakash@gmail.com>\n\n");
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

#ifdef WIN_BUILD
  GtkWidget *load_icon = gtk_image_new_from_file ("../share/icons/load_file.png");
  GtkWidget *fb_icon = gtk_image_new_from_file ("../share/icons/file_browser.png");
  GtkWidget *eval_icon = gtk_image_new_from_file ("../share/icons/evaluate.png");
  GtkWidget *clear_icon = gtk_image_new_from_file ("../share/icons/clear32x32.png");
  GtkWidget *exit_icon = gtk_image_new_from_file ("../share/icons/exit32x32.png");
#else
#ifdef __OSX_BUNDLE__
  GtkWidget *load_icon = gtk_image_new_from_file (concat_strings(path_buf, exec_path, "../Resources/share/plisp/icons/load_file.png"));
  GtkWidget *fb_icon = gtk_image_new_from_file (concat_strings(path_buf, exec_path, "../Resources/share/plisp/icons/file_browser.png"));
  GtkWidget *eval_icon = gtk_image_new_from_file (concat_strings(path_buf, exec_path, "../Resources/share/plisp/icons/evaluate.png"));
  GtkWidget *clear_icon = gtk_image_new_from_file (concat_strings(path_buf, exec_path, "../Resources/share/plisp/icons/clear32x32.png"));
  GtkWidget *exit_icon = gtk_image_new_from_file (concat_strings(path_buf, exec_path, "../Resources/share/plisp/icons/exit32x32.png"));
#else  
  GtkWidget *load_icon = gtk_image_new_from_file (PLISPDATADIR "/icons/load_file.png");
  GtkWidget *fb_icon = gtk_image_new_from_file (PLISPDATADIR "/icons/file_browser.png");
  GtkWidget *eval_icon = gtk_image_new_from_file (PLISPDATADIR "/icons/evaluate.png");
  GtkWidget *clear_icon = gtk_image_new_from_file (PLISPDATADIR "/icons/clear32x32.png");
  GtkWidget *exit_icon = gtk_image_new_from_file (PLISPDATADIR "/icons/exit32x32.png");
#endif
#endif

  toolbar = gtk_toolbar_new ();
  gtk_orientable_set_orientation (GTK_ORIENTABLE (toolbar), GTK_ORIENTATION_HORIZONTAL);
  gtk_toolbar_set_style (GTK_TOOLBAR (toolbar), GTK_TOOLBAR_ICONS);
  gtk_container_set_border_width (GTK_CONTAINER (toolbar), 5);

  /* gtk_toolbar_append_item (GTK_TOOLBAR (toolbar),                    */
  /*                          NULL,                                   /\* button label *\/ */
  /*                          "Load file (Ctrl-O)",                   /\* button's tooltip *\/ */
  /*                          "Private",                              /\* tooltip private info *\/ */
  /*                          load_icon,                              /\* icon widget *\/ */
  /*                          GTK_SIGNAL_FUNC(load_source_file),      /\* a signal *\/ */
  /*                          (GtkWidget *)workspace_window); */

  GtkToolItem *load_button = gtk_tool_button_new(load_icon, NULL);
  gtk_tool_item_set_tooltip_text(load_button, "Load file (Ctrl-L)");
  g_signal_connect (load_button, "clicked", G_CALLBACK (load_source_file), workspace_window);
  gtk_toolbar_insert((GtkToolbar *)toolbar, load_button, 0);

  /* gtk_toolbar_append_item (GTK_TOOLBAR (toolbar),                    */
  /*                          NULL,                                   /\* button label *\/ */
  /*                          "Evaluate (Ctrl+Enter)",                /\* button's tooltip *\/ */
  /*                          "Private",                              /\* tooltip private info *\/ */
  /*                          eval_icon,                              /\* icon widget *\/ */
  /*                          GTK_SIGNAL_FUNC(eval_expression),       /\* a signal *\/ */
  /*                          (GtkWidget *)workspace_window); */

  GtkToolItem *fb_button = gtk_tool_button_new(fb_icon, NULL);
  gtk_tool_item_set_tooltip_text(fb_button, "File Browser (Ctrl-B)");
  g_signal_connect (fb_button, "clicked", G_CALLBACK (show_file_browser_win), workspace_window);
  gtk_toolbar_insert((GtkToolbar *)toolbar, fb_button, 1);

  GtkToolItem *eval_button = gtk_tool_button_new(eval_icon, NULL);
  gtk_tool_item_set_tooltip_text(eval_button, "Evaluate (Ctrl+Enter)");
  g_signal_connect (eval_button, "clicked", G_CALLBACK (eval_expression), workspace_window);
  gtk_toolbar_insert((GtkToolbar *)toolbar, eval_button, 2);

  GtkToolItem *clear_button = gtk_tool_button_new(clear_icon, NULL);
  gtk_tool_item_set_tooltip_text(clear_button, "Clear Workspace");
  g_signal_connect (clear_button, "clicked", G_CALLBACK (clear_workspace), workspace_window);
  gtk_toolbar_insert((GtkToolbar *)toolbar, clear_button, 3);

  /* gtk_toolbar_append_item (GTK_TOOLBAR (toolbar),                    */
  /*                          NULL,                                   /\* button label *\/ */
  /*                          "Close (Ctrl-W)",                       /\* button's tooltip *\/ */
  /*                          "Private",                              /\* tooltip private info *\/ */
  /*                          exit_icon,                              /\* icon widget *\/ */
  /*                          GTK_SIGNAL_FUNC(close_window),          /\* a signal *\/ */
  /*                          (GtkWidget *)workspace_window); */

  GtkToolItem *close_button = gtk_tool_button_new(exit_icon, NULL);
  gtk_tool_item_set_tooltip_text(close_button, "Close (Ctrl-W)");
  g_signal_connect (close_button, "clicked", G_CALLBACK (close_window), workspace_window);
  gtk_toolbar_insert((GtkToolbar *)toolbar, close_button, 4);

  return (GtkToolbar *)toolbar;
}

void create_workspace_window(int posx, int posy, int width, int height, char *text)
{
  GtkWidget *win = gtk_window_new (GTK_WINDOW_TOPLEVEL);

  workspace_window = (GtkWindow *)win;

#ifdef WIN_BUILD
  gtk_window_set_icon_from_file(workspace_window, "../share/icons/evaluate.png", NULL);
#else
#ifdef __OSX_BUNDLE__
  gtk_window_set_icon_from_file(workspace_window, concat_strings(path_buf, exec_path, "../Resources/share/plisp/icons/evaluate.png"), NULL);
#else  
  gtk_window_set_icon_from_file(workspace_window, PLISPDATADIR "/icons/evaluate.png", NULL);
#endif
#endif  

  gtk_window_set_default_size((GtkWindow *)win, width, height);
  gtk_window_move((GtkWindow *)win, posx, posy); 

  g_signal_connect (win, "delete-event",
                    G_CALLBACK (delete_event), NULL);

  g_signal_connect (win, "focus",
                    G_CALLBACK (set_triggering_window), NULL);

  gtk_container_set_border_width (GTK_CONTAINER (win), 10);

  //Widgets *w = g_slice_new (Widgets);
  GtkWidget *scrolled_win, *vbox;

  set_up_workspace_source_buffer();

  //GtkWidget *textview = gtk_text_view_new ();
  GtkWidget *textview = workspace_source_view;

  gtk_widget_override_font(GTK_WIDGET(textview), pango_font_description_from_string(FONT));

  //workspace_buffer = gtk_text_view_get_buffer((GtkTextView *)textview);
  workspace_buffer = workspace_source_buffer;

  g_signal_connect(G_OBJECT(workspace_buffer), 
                   "notify::cursor-position", 
                   G_CALLBACK (handle_cursor_move), 
                   NULL);

  /* print_to_workspace("; This is the workspace; type pLisp expressions here.\n"); */
  /* print_to_workspace("; To evaluate an expression, enter the expression\n"); */
  /* print_to_workspace("; and press Ctrl+Enter when the expression is complete\n"); */
  /* print_to_workspace("; (indicated by the highlighted matching parens).\n"); */
  print_to_workspace(text);

  g_signal_connect(G_OBJECT(win), 
                   "key_press_event", 
                   G_CALLBACK (handle_key_press_events), 
                   NULL);

  scrolled_win = gtk_scrolled_window_new (NULL, NULL);
  gtk_container_add (GTK_CONTAINER (scrolled_win), textview);

  vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
  gtk_box_pack_start (GTK_BOX (vbox), (GtkWidget *)create_workspace_toolbar(), FALSE, FALSE, 0);
  gtk_box_pack_start (GTK_BOX (vbox), scrolled_win, TRUE, TRUE, 0);

  workspace_statusbar = (GtkStatusbar *)gtk_statusbar_new();
  gtk_box_pack_start (GTK_BOX (vbox), (GtkWidget *)workspace_statusbar, FALSE, FALSE, 0);  
  
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

#if __x86_64__
  store = gtk_list_store_new (2, G_TYPE_STRING, G_TYPE_INT64);
#else
#ifdef __APPLE__
  store = gtk_list_store_new (2, G_TYPE_STRING, G_TYPE_INT64);
#else
  store = gtk_list_store_new (2, G_TYPE_STRING, G_TYPE_INT);
#endif
#endif  
  
  gtk_tree_view_set_model(GTK_TREE_VIEW (list), 
                          GTK_TREE_MODEL(store));

  g_object_unref(store);  
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

void populate_packages_list()
{
  remove_all_from_list(packages_list);

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

GtkToolbar *create_system_browser_toolbar()
{
  GtkWidget *toolbar;

#ifdef WIN_BUILD
  GtkWidget *new_package_icon = gtk_image_new_from_file ("../share/icons/new_package.png");
  GtkWidget *new_symbol_icon = gtk_image_new_from_file ("../share/icons/new_symbol.png");
  GtkWidget *rename_icon = gtk_image_new_from_file ("../share/icons/rename.png");  
  GtkWidget *accept_icon = gtk_image_new_from_file ("../share/icons/accept.png");
  GtkWidget *delete_icon = gtk_image_new_from_file ("../share/icons/delete.png");
  GtkWidget *refresh_icon = gtk_image_new_from_file ("../share/icons/refresh.png");
  GtkWidget *export_pkg_icon = gtk_image_new_from_file ("../share/icons/export_package.png");
  GtkWidget *callers_icon = gtk_image_new_from_file ("../share/icons/callers.png");
  GtkWidget *exit_icon = gtk_image_new_from_file ("../share/icons/exit32x32.png");
#else
#ifdef __OSX_BUNDLE__
  GtkWidget *new_package_icon = gtk_image_new_from_file (concat_strings(path_buf, exec_path, "../Resources/share/plisp/icons/new_package.png"));
  GtkWidget *new_symbol_icon = gtk_image_new_from_file (concat_strings(path_buf, exec_path, "../Resources/share/plisp/icons/new_symbol.png"));
  GtkWidget *rename_icon = gtk_image_new_from_file (concat_strings(path_buf, exec_path, "../Resources/share/plisp/icons/rename.png"));
  GtkWidget *accept_icon = gtk_image_new_from_file (concat_strings(path_buf, exec_path, "../Resources/share/plisp/icons/accept.png"));
  GtkWidget *delete_icon = gtk_image_new_from_file (concat_strings(path_buf, exec_path, "../Resources/share/plisp/icons/delete.png"));
  GtkWidget *refresh_icon = gtk_image_new_from_file (concat_strings(path_buf, exec_path, "../Resources/share/plisp/icons/refresh.png"));
  GtkWidget *export_pkg_icon = gtk_image_new_from_file (concat_strings(path_buf, exec_path, "../Resources/share/plisp/icons/export_package.png"));
  GtkWidget *callers_icon = gtk_image_new_from_file (concat_strings(path_buf, exec_path, "../Resources/share/plisp/icons/callers.png"));
  GtkWidget *exit_icon = gtk_image_new_from_file (concat_strings(path_buf, exec_path, "../Resources/share/plisp/icons/exit32x32.png"));
#else  
  GtkWidget *new_package_icon = gtk_image_new_from_file (PLISPDATADIR "/icons/new_package.png");
  GtkWidget *new_symbol_icon = gtk_image_new_from_file (PLISPDATADIR "/icons/new_symbol.png");
  GtkWidget *rename_icon = gtk_image_new_from_file (PLISPDATADIR "/icons/rename.png");
  GtkWidget *accept_icon = gtk_image_new_from_file (PLISPDATADIR "/icons/accept.png");
  GtkWidget *delete_icon = gtk_image_new_from_file (PLISPDATADIR "/icons/delete.png");
  GtkWidget *refresh_icon = gtk_image_new_from_file (PLISPDATADIR "/icons/refresh.png");
  GtkWidget *export_pkg_icon = gtk_image_new_from_file (PLISPDATADIR "/icons/export_package.png");
  GtkWidget *callers_icon = gtk_image_new_from_file (PLISPDATADIR "/icons/callers.png");
  GtkWidget *exit_icon = gtk_image_new_from_file (PLISPDATADIR "/icons/exit32x32.png");
#endif
#endif  

  toolbar = gtk_toolbar_new ();
  gtk_orientable_set_orientation (GTK_ORIENTABLE (toolbar), GTK_ORIENTATION_HORIZONTAL);
  gtk_toolbar_set_style (GTK_TOOLBAR (toolbar), GTK_TOOLBAR_ICONS);
  gtk_container_set_border_width (GTK_CONTAINER (toolbar), 5);

  /* gtk_toolbar_append_item (GTK_TOOLBAR (toolbar),                    */
  /*                          NULL,                                   /\* button label *\/ */
  /*                          "New package (Ctrl-K)",                 /\* button's tooltip *\/ */
  /*                          "Private",                              /\* tooltip private info *\/ */
  /*                          new_package_icon,                       /\* icon widget *\/ */
  /*                          GTK_SIGNAL_FUNC(new_package),            /\* a signal *\/ */
  /*                          NULL); */
  GtkToolItem *new_package_button = gtk_tool_button_new(new_package_icon, NULL);
  gtk_tool_item_set_tooltip_text(new_package_button, "New package (Ctrl-K)");
  g_signal_connect (new_package_button, "clicked", G_CALLBACK (new_package), system_browser_window);
  gtk_toolbar_insert((GtkToolbar *)toolbar, new_package_button, 0);

  /* gtk_toolbar_append_item (GTK_TOOLBAR (toolbar),                    */
  /*                          NULL,                                   /\* button label *\/ */
  /*                          "New symbol (Ctrl-N)",                  /\* button's tooltip *\/ */
  /*                          "Private",                              /\* tooltip private info *\/ */
  /*                          new_symbol_icon,                        /\* icon widget *\/ */
  /*                          GTK_SIGNAL_FUNC(new_symbol),            /\* a signal *\/ */
  /*                          (GtkWidget *)system_browser_window); */
  GtkToolItem *new_symbol_button = gtk_tool_button_new(new_symbol_icon, NULL);
  gtk_tool_item_set_tooltip_text(new_symbol_button, "New symbol (Ctrl-N)");
  g_signal_connect (new_symbol_button, "clicked", G_CALLBACK (new_symbol), system_browser_window);
  gtk_toolbar_insert((GtkToolbar *)toolbar, new_symbol_button, 1);

  GtkToolItem *rename_button = gtk_tool_button_new(rename_icon, NULL);
  gtk_tool_item_set_tooltip_text(rename_button, "Rename symbol (F2)");
  g_signal_connect (rename_button, "clicked", G_CALLBACK (rename_symbol), system_browser_window);
  gtk_toolbar_insert((GtkToolbar *)toolbar, rename_button, 2);
  
  /* gtk_toolbar_append_item (GTK_TOOLBAR (toolbar),                    */
  /*                          NULL,                                   /\* button label *\/ */
  /*                          "Accept (Ctrl-S)",                      /\* button's tooltip *\/ */
  /*                          "Private",                              /\* tooltip private info *\/ */
  /*                          accept_icon,                            /\* icon widget *\/ */
  /*                          GTK_SIGNAL_FUNC(accept),                /\* a signal *\/ */
  /*                          (GtkWidget *)system_browser_window); */
  GtkToolItem *accept_button = gtk_tool_button_new(accept_icon, NULL);
  gtk_tool_item_set_tooltip_text(accept_button, "Accept (Ctrl-S)");
  g_signal_connect (accept_button, "clicked", G_CALLBACK (accept), system_browser_window);
  gtk_toolbar_insert((GtkToolbar *)toolbar, accept_button, 3);


  /* gtk_toolbar_append_item (GTK_TOOLBAR (toolbar),                    */
  /*                          NULL,                                   /\* button label *\/ */
  /*                          "Delete symbol (Ctrl-X)",               /\* button's tooltip *\/ */
  /*                          "Private",                              /\* tooltip private info *\/ */
  /*                          delete_icon,                            /\* icon widget *\/ */
  /*                          GTK_SIGNAL_FUNC(delete_pkg_or_sym),     /\* a signal *\/ */
  /*                          NULL); */
  GtkToolItem *delete_button = gtk_tool_button_new(delete_icon, NULL);
  gtk_tool_item_set_tooltip_text(delete_button, "Delete symbol (Ctrl-X)");
  g_signal_connect (delete_button, "clicked", G_CALLBACK (delete_pkg_or_sym), system_browser_window);
  gtk_toolbar_insert((GtkToolbar *)toolbar, delete_button, 4);

  /* gtk_toolbar_append_item (GTK_TOOLBAR (toolbar),                    */
  /*                          NULL,                                   /\* button label *\/ */
  /*                          "Refresh (F5)",                         /\* button's tooltip *\/ */
  /*                          "Private",                              /\* tooltip private info *\/ */
  /*                          refresh_icon,                           /\* icon widget *\/ */
  /*                          GTK_SIGNAL_FUNC(refresh_sys_browser),   /\* a signal *\/ */
  /*                          NULL); */
  GtkToolItem *refresh_button = gtk_tool_button_new(refresh_icon, NULL);
  gtk_tool_item_set_tooltip_text(refresh_button, "Refresh (F5)");
  g_signal_connect (refresh_button, "clicked", G_CALLBACK (refresh_sys_browser), system_browser_window);
  gtk_toolbar_insert((GtkToolbar *)toolbar, refresh_button, 5);

  GtkToolItem *export_pkg_button = gtk_tool_button_new(export_pkg_icon, NULL);
  gtk_tool_item_set_tooltip_text(export_pkg_button, "Export Package");
  g_signal_connect (export_pkg_button, "clicked", G_CALLBACK (exp_pkg), system_browser_window);
  gtk_toolbar_insert((GtkToolbar *)toolbar, export_pkg_button, 6);

  GtkToolItem *callers_button = gtk_tool_button_new(callers_icon, NULL);
  gtk_tool_item_set_tooltip_text(callers_button, "Referrers");
  g_signal_connect (callers_button, "clicked", G_CALLBACK (callers), system_browser_window);
  gtk_toolbar_insert((GtkToolbar *)toolbar, callers_button, 7);

  /* gtk_toolbar_append_item (GTK_TOOLBAR (toolbar),                    */
  /*                          NULL,                                   /\* button label *\/ */
  /*                          "Close (Ctrl-W)",                       /\* button's tooltip *\/ */
  /*                          "Private",                              /\* tooltip private info *\/ */
  /*                          exit_icon,                              /\* icon widget *\/ */
  /*                          GTK_SIGNAL_FUNC(close_window),          /\* a signal *\/ */
  /*                          (GtkWidget *)system_browser_window); */
  GtkToolItem *close_button = gtk_tool_button_new(exit_icon, NULL);
  gtk_tool_item_set_tooltip_text(close_button, "Close (Ctrl-W)");
  g_signal_connect (close_button, "clicked", G_CALLBACK (close_window), system_browser_window);
  gtk_toolbar_insert((GtkToolbar *)toolbar, close_button, 8);

  return (GtkToolbar *)toolbar;
}

void create_system_browser_window(int posx, int posy, int width, int height)
{
  GtkWidget *win = gtk_window_new (GTK_WINDOW_TOPLEVEL);

  system_browser_window = (GtkWindow *)win;

#ifdef WIN_BUILD
  gtk_window_set_icon_from_file(system_browser_window, "../share/icons/evaluate.png", NULL);
#else
#ifdef __OSX_BUNDLE__
  gtk_window_set_icon_from_file(system_browser_window, concat_strings(path_buf, exec_path, "../Resources/share/plisp/icons/evaluate.png"), NULL);
#else
  gtk_window_set_icon_from_file(system_browser_window, PLISPDATADIR "/icons/evaluate.png", NULL);
#endif
#endif  

  GtkWidget *scrolled_win1, *scrolled_win2;
  GtkWidget *vbox, *hbox;

  gtk_window_set_title((GtkWindow *)win, "pLisp System Browser");

  /* gtk_window_set_default_size((GtkWindow *)win, 600, 400); */
  /* gtk_window_set_position(GTK_WINDOW(win), GTK_WIN_POS_CENTER); */
  gtk_window_set_default_size(system_browser_window, width, height);
  gtk_window_move(system_browser_window, posx, posy); 

  g_signal_connect (win, "delete-event",
                    G_CALLBACK (delete_event), NULL);

  g_signal_connect (win, "focus",
                    G_CALLBACK (set_triggering_window), NULL);

  g_signal_connect(G_OBJECT(win), 
                   "key_press_event", 
                   G_CALLBACK (handle_key_press_events), 
                   NULL);

  gtk_container_set_border_width (GTK_CONTAINER (win), 10);

  scrolled_win1 = gtk_scrolled_window_new(NULL, NULL);
  scrolled_win2 = gtk_scrolled_window_new(NULL, NULL);

  packages_list = (GtkTreeView *)gtk_tree_view_new();
  gtk_tree_view_set_headers_visible(GTK_TREE_VIEW(packages_list), TRUE);
  gtk_widget_override_font(GTK_WIDGET(packages_list), pango_font_description_from_string(FONT));

  initialize_packages_list((GtkTreeView *)packages_list);

  gtk_tree_view_column_set_sort_column_id(gtk_tree_view_get_column(packages_list, 0), 0); 

  symbols_list = (GtkTreeView *)gtk_tree_view_new();
  gtk_tree_view_set_headers_visible(GTK_TREE_VIEW(symbols_list), TRUE);
  gtk_widget_override_font(GTK_WIDGET(symbols_list), pango_font_description_from_string(FONT));

  initialize_symbols_list((GtkTreeView *)symbols_list);
  gtk_tree_view_column_set_sort_column_id(gtk_tree_view_get_column(symbols_list, 0), 0);

  populate_packages_list();

  g_signal_connect(G_OBJECT(packages_list), "cursor-changed",
          G_CALLBACK(fetch_package_members), NULL);

  g_signal_connect(G_OBJECT(symbols_list), "cursor-changed",
                   G_CALLBACK(fetch_symbol_value), NULL);

  gtk_container_add(GTK_CONTAINER (scrolled_win1), (GtkWidget *)packages_list);
  gtk_container_add(GTK_CONTAINER (scrolled_win2), (GtkWidget *)symbols_list);

  hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
  gtk_box_pack_start(GTK_BOX (hbox), scrolled_win1, TRUE, TRUE, 0);
  gtk_box_pack_start(GTK_BOX (hbox), scrolled_win2, TRUE, TRUE, 0);

  GtkWidget *scrolled_win;

  set_up_system_browser_source_buffer();

  //GtkWidget *textview = gtk_text_view_new ();
  GtkWidget *textview = system_browser_source_view;

  system_browser_textview = (GtkTextView *)textview;

  gtk_widget_override_font(GTK_WIDGET(textview), pango_font_description_from_string(FONT));
  //gtk_text_view_set_editable((GtkTextView *)textview, FALSE);

  //system_browser_buffer = gtk_text_view_get_buffer((GtkTextView *)textview);
  system_browser_buffer = system_browser_source_buffer;

  g_signal_connect(G_OBJECT(system_browser_buffer), 
                   "notify::cursor-position", 
                   G_CALLBACK (handle_cursor_move), 
                   NULL);

  scrolled_win = gtk_scrolled_window_new (NULL, NULL);
  gtk_container_add (GTK_CONTAINER (scrolled_win), textview);

  vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
  gtk_box_pack_start (GTK_BOX (vbox), (GtkWidget *)create_system_browser_toolbar(), FALSE, FALSE, 0);
  gtk_box_pack_start (GTK_BOX (vbox), hbox, TRUE, TRUE, 0);
  gtk_box_pack_start (GTK_BOX (vbox), scrolled_win, TRUE, TRUE, 0);

  system_browser_statusbar = (GtkStatusbar *)gtk_statusbar_new();
  gtk_box_pack_start (GTK_BOX (vbox), (GtkWidget *)system_browser_statusbar, FALSE, FALSE, 0);  

  gtk_container_add (GTK_CONTAINER (win), vbox);

  gtk_widget_show_all(win);

  //commented out because if we're restoring the system browser from
  //an image, this will wrongly set focus to the first package,
  //though the symbols and definition will be that of the package
  //selected at the time the image was created
  //gtk_widget_grab_focus((GtkWidget *)packages_list);

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

#ifdef WIN_BUILD
  GtkWidget *load_icon = gtk_image_new_from_file ("../share/icons/load_image.png");
  GtkWidget *save_icon = gtk_image_new_from_file ("../share/icons/save_image.png");
  GtkWidget *workspace_icon = gtk_image_new_from_file ("../share/icons/workspace.png");
  GtkWidget *browser_icon = gtk_image_new_from_file ("../share/icons/browser.png");
  GtkWidget *clear_icon = gtk_image_new_from_file ("../share/icons/clear.png");
  GtkWidget *exit_icon = gtk_image_new_from_file ("../share/icons/exit.png");
#else
#ifdef __OSX_BUNDLE__
  GtkWidget *load_icon = gtk_image_new_from_file (concat_strings(path_buf, exec_path, "../Resources/share/plisp/icons/load_image.png"));
  GtkWidget *save_icon = gtk_image_new_from_file (concat_strings(path_buf, exec_path, "../Resources/share/plisp/icons/save_image.png"));
  GtkWidget *workspace_icon = gtk_image_new_from_file (concat_strings(path_buf, exec_path, "../Resources/share/plisp/icons/workspace.png"));
  GtkWidget *browser_icon = gtk_image_new_from_file (concat_strings(path_buf, exec_path, "../Resources/share/plisp/icons/browser.png"));
  GtkWidget *clear_icon = gtk_image_new_from_file (concat_strings(path_buf, exec_path, "../Resources/share/plisp/icons/clear.png"));
  GtkWidget *exit_icon = gtk_image_new_from_file (concat_strings(path_buf, exec_path, "../Resources/share/plisp/icons/exit.png"));
#else
  GtkWidget *load_icon = gtk_image_new_from_file (PLISPDATADIR "/icons/load_image.png");
  GtkWidget *save_icon = gtk_image_new_from_file (PLISPDATADIR "/icons/save_image.png");
  GtkWidget *workspace_icon = gtk_image_new_from_file (PLISPDATADIR "/icons/workspace.png");
  GtkWidget *browser_icon = gtk_image_new_from_file (PLISPDATADIR "/icons/browser.png");
  GtkWidget *clear_icon = gtk_image_new_from_file (PLISPDATADIR "/icons/clear.png");
  GtkWidget *exit_icon = gtk_image_new_from_file (PLISPDATADIR "/icons/exit.png");
#endif
#endif  

  toolbar = gtk_toolbar_new ();
  gtk_orientable_set_orientation (GTK_ORIENTABLE (toolbar), GTK_ORIENTATION_HORIZONTAL);
  gtk_toolbar_set_style (GTK_TOOLBAR (toolbar), GTK_TOOLBAR_ICONS);
  gtk_container_set_border_width (GTK_CONTAINER (toolbar), 5);
  //gtk_toolbar_set_space_size (GTK_TOOLBAR (toolbar), 5);

  /* gtk_toolbar_append_item (GTK_TOOLBAR (toolbar),                    */
  /*                          NULL,                                   /\* button label *\/ */
  /*                          "Load image (Ctrl-L)",                  /\* button's tooltip *\/ */
  /*                          "Private",                              /\* tooltip private info *\/ */
  /*                          load_icon,                              /\* icon widget *\/ */
  /*                          GTK_SIGNAL_FUNC(load_image_file),       /\* a signal *\/ */
  /*                          NULL); */
  GtkToolItem *load_button = gtk_tool_button_new(load_icon, NULL);
  gtk_tool_item_set_tooltip_text(load_button, "Load image (Ctrl-L)");
  g_signal_connect (load_button, "clicked", G_CALLBACK (load_image_file), transcript_window);
  gtk_toolbar_insert((GtkToolbar *)toolbar, load_button, 0);

  /* gtk_toolbar_append_item (GTK_TOOLBAR (toolbar),                    */
  /*                          NULL,                                   /\* button label *\/ */
  /*                          "Save image (Ctrl-S)",                  /\* button's tooltip *\/ */
  /*                          "Private",                              /\* tooltip private info *\/ */
  /*                          save_icon,                              /\* icon widget *\/ */
  /*                          GTK_SIGNAL_FUNC(save_image_file),       /\* a signal *\/ */
  /*                          NULL); */
  GtkToolItem *save_button = gtk_tool_button_new(save_icon, NULL);
  gtk_tool_item_set_tooltip_text(save_button, "Save image (Ctrl-S)");
  g_signal_connect (save_button, "clicked", G_CALLBACK (save_image_file), transcript_window);
  gtk_toolbar_insert((GtkToolbar *)toolbar, save_button, 1);

  /* gtk_toolbar_append_item (GTK_TOOLBAR (toolbar),                    */
  /*                          NULL,                                   /\* button label *\/ */
  /*                          "Show workspace window (F7)",           /\* button's tooltip *\/ */
  /*                          "Private",                              /\* tooltip private info *\/ */
  /*                          workspace_icon,                         /\* icon widget *\/ */
  /*                          GTK_SIGNAL_FUNC(show_workspace_win),    /\* a signal *\/ */
  /*                          NULL); */
  GtkToolItem *workspace_button = gtk_tool_button_new(workspace_icon, NULL);
  gtk_tool_item_set_tooltip_text(workspace_button, "Show workspace window (F7)");
  g_signal_connect (workspace_button, "clicked", G_CALLBACK (show_workspace_win), transcript_window);
  gtk_toolbar_insert((GtkToolbar *)toolbar, workspace_button, 2);

  /* gtk_toolbar_append_item (GTK_TOOLBAR (toolbar),                    */
  /*                          NULL,                                        /\* button label *\/ */
  /*                          "System Browser (F9)",                       /\* button's tooltip *\/ */
  /*                          "Private",                                   /\* tooltip private info *\/ */
  /*                          browser_icon,                                /\* icon widget *\/ */
  /*                          GTK_SIGNAL_FUNC(show_system_browser_win),    /\* a signal *\/ */
  /*                          NULL); */
  GtkToolItem *browser_button = gtk_tool_button_new(browser_icon, NULL);
  gtk_tool_item_set_tooltip_text(browser_button, "System Browser (F9)");
  g_signal_connect (browser_button, "clicked", G_CALLBACK (show_system_browser_win), transcript_window);
  gtk_toolbar_insert((GtkToolbar *)toolbar, browser_button, 3);

  GtkToolItem *clear_button = gtk_tool_button_new(clear_icon, NULL);
  gtk_tool_item_set_tooltip_text(clear_button, "Clear Transcript");
  g_signal_connect (clear_button, "clicked", G_CALLBACK (clear_transcript), transcript_window);
  gtk_toolbar_insert((GtkToolbar *)toolbar, clear_button, 4);

  /* gtk_toolbar_append_item (GTK_TOOLBAR (toolbar),                    */
  /*                          NULL,                                   /\* button label *\/ */
  /*                          "Exit (Ctrl-W)",                        /\* button's tooltip *\/ */
  /*                          "Private",                              /\* tooltip private info *\/ */
  /*                          exit_icon,                              /\* icon widget *\/ */
  /*                          GTK_SIGNAL_FUNC(quit),                  /\* a signal *\/ */
  /*                          NULL); */
  GtkToolItem *exit_button = gtk_tool_button_new(exit_icon, NULL);
  gtk_tool_item_set_tooltip_text(exit_button, "Exit (Ctrl-W)");
  g_signal_connect (exit_button, "clicked", G_CALLBACK (quit), transcript_window);
  gtk_toolbar_insert((GtkToolbar *)toolbar, exit_button, 5);

  return (GtkToolbar *)toolbar;
}

//https://gist.github.com/Gazer/130858
void setup_language_manager_path(GtkSourceLanguageManager *lm)
{
  gchar **lang_files;
  int i, lang_files_count;
  char **new_langs;
 
  lang_files = g_strdupv (gtk_source_language_manager_get_search_path (lm));
 
  lang_files_count = g_strv_length (lang_files);
  new_langs = g_new (char*, lang_files_count + 2);
 
  for (i = 0; lang_files[i]; i++)
    new_langs[i] = lang_files[i];
 
#ifdef WIN_BUILD
  new_langs[lang_files_count] = g_strdup ("../share/");
#else
#ifdef __OSX_BUNDLE__
  new_langs[lang_files_count] = g_strdup (concat_strings(path_buf, exec_path, "../Resources/share/plisp/"));
#else
  new_langs[lang_files_count] = g_strdup (PLISPDATADIR);
#endif
#endif  

  new_langs[lang_files_count+1] = NULL;
 
  g_free (lang_files);
 
  gtk_source_language_manager_set_search_path (lm, new_langs);

  g_free(new_langs);
} 

void create_transcript_window(int posx, int posy, int width, int height, char *text)
{
  GtkWidget *scrolled_win, *vbox;

  transcript_window = (GtkWindow *)gtk_window_new (GTK_WINDOW_TOPLEVEL);

#ifdef WIN_BUILD
  gtk_window_set_icon_from_file(transcript_window, "../share/icons/evaluate.png", NULL);
#else
#ifdef __OSX_BUNDLE__
  gtk_window_set_icon_from_file(transcript_window, concat_strings(path_buf, exec_path, "../Resources/share/plisp/icons/evaluate.png"), NULL);
#else  
  gtk_window_set_icon_from_file(transcript_window, PLISPDATADIR "/icons/evaluate.png", NULL);
#endif
#endif  

  //gtk_window_set_title((GtkWindow *)transcript_window, "pLisp Transcript");
  update_transcript_title();

  /* gtk_window_set_default_size((GtkWindow *)transcript_window, 600, 400); */
  /* gtk_window_set_position(GTK_WINDOW(transcript_window), GTK_WIN_POS_CENTER); */
  gtk_window_set_default_size(transcript_window, width, height);

  //gtk_window_set_position(GTK_WINDOW(transcript_window), GTK_WIN_POS_CENTER);
  gtk_window_move(transcript_window, posx, posy); 
      
  g_signal_connect (transcript_window, "delete-event",
                    G_CALLBACK (delete_event), NULL);

  g_signal_connect (transcript_window, "focus",
                    G_CALLBACK (set_triggering_window), NULL);
    
  /* g_signal_connect (transcript_window, "destroy", */
  /*                    G_CALLBACK (quit), NULL); */

  g_signal_connect(transcript_window, 
                   "key_press_event", 
                   G_CALLBACK (handle_key_press_events), 
                   NULL);
    
  gtk_container_set_border_width (GTK_CONTAINER (transcript_window), 10);
  
  GtkWidget *textview = gtk_text_view_new ();

  transcript_textview = (GtkTextView *)textview;

  gtk_text_view_set_editable((GtkTextView *)textview, FALSE);
  gtk_text_view_set_cursor_visible((GtkTextView *)textview, FALSE);
  //gtk_widget_set_sensitive(textview, FALSE);

  gtk_widget_override_font(GTK_WIDGET(textview), pango_font_description_from_string(FONT));

  transcript_buffer = gtk_text_view_get_buffer((GtkTextView *)textview);

  scrolled_win = gtk_scrolled_window_new (NULL, NULL);
  gtk_container_add (GTK_CONTAINER (scrolled_win), textview);

  vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);

  gtk_box_pack_start (GTK_BOX (vbox), (GtkWidget *)create_transcript_toolbar(), FALSE, FALSE, 0);
  gtk_box_pack_start (GTK_BOX (vbox), scrolled_win, TRUE, TRUE, 0);
  
  gtk_container_add (GTK_CONTAINER (transcript_window), vbox);
  
  gtk_widget_show_all((GtkWidget *)transcript_window);

  //print_ui_copyright_notice();
  print_to_transcript(text);

  if(!image_mode)
  {
    lm = gtk_source_language_manager_get_default();
    setup_language_manager_path(lm);
    source_language = gtk_source_language_manager_get_language(lm, "plisp");
  }

  //gtk_window_set_keep_above(transcript_window, TRUE);
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

void show_warning_dialog(char *msg)
{
  GtkWidget *dialog = gtk_message_dialog_new (action_triggering_window,
                                              GTK_DIALOG_DESTROY_WITH_PARENT,
                                              GTK_MESSAGE_WARNING,
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

#ifdef WIN_BUILD
  gtk_window_set_icon_from_file(window, "../share/icons/evaluate.png", NULL);
#else
#ifdef __OSX_BUNDLE__
  gtk_window_set_icon_from_file(window, concat_strings(path_buf, exec_path, "../Resources/share/plisp/icons/evaluate.png"), NULL);
#else
  gtk_window_set_icon_from_file(window, PLISPDATADIR "/icons/evaluate.png", NULL);
#endif
#endif  

  gtk_window_set_title (GTK_WINDOW (window), "Error");
  gtk_container_set_border_width (GTK_CONTAINER (window), 10);
  gtk_window_set_default_size((GtkWindow *)window, 400, 50);
  gtk_window_move((GtkWindow *)window, 500, 200); 

  GtkWidget *textview = gtk_text_view_new ();

  gtk_widget_override_font(GTK_WIDGET(textview), pango_font_description_from_string(FONT));
  gtk_text_buffer_insert_at_cursor(gtk_text_view_get_buffer((GtkTextView *)textview), msg, -1);

  ok = gtk_button_new_with_label("OK");

  g_signal_connect_swapped (ok, "clicked",
                            G_CALLBACK (gtk_widget_destroy),
                            window);

  scrolled_win = gtk_scrolled_window_new (NULL, NULL);
  gtk_container_add (GTK_CONTAINER (scrolled_win), textview);

  hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
  gtk_box_pack_start(GTK_BOX (hbox), ok, FALSE, FALSE, 0);

  vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
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

  column1 = gtk_tree_view_column_new_with_attributes("Function Call",
                                                     renderer, "text", 0, NULL);
  gtk_tree_view_append_column(GTK_TREE_VIEW (list), column1);

  column2 = gtk_tree_view_column_new_with_attributes("Function Body",
                                                     renderer, "text", 1, NULL);
  gtk_tree_view_append_column(GTK_TREE_VIEW (list), column2);

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

  OBJECT_PTR rest = debug_window_dbg_stack;

  while(rest != NIL)
  {
    OBJECT_PTR frame = car(rest);

    char buf[MAX_STRING_LENGTH];
    memset(buf, '\0', MAX_STRING_LENGTH);

    print_object_to_string(car(frame), buf, 0);

    gtk_list_store_append(store, &iter);
    gtk_list_store_set(store, &iter, 0, buf, -1);

    memset(buf, '\0', MAX_STRING_LENGTH);
    print_object_to_string(CADR(frame), buf, 0);

    gtk_list_store_set(store, &iter, 1, convert_to_upper_case(buf), -1);

    rest = cdr(rest);
  }
}

GtkToolbar *create_debugger_toolbar()
{
  GtkWidget *toolbar;

#ifdef WIN_BUILD
  GtkWidget *resume_icon = gtk_image_new_from_file ("../share/icons/resume.png");
  GtkWidget *abort_icon = gtk_image_new_from_file ("../share/icons/abort.png");
#else
#ifdef __OSX_BUNDLE__
  GtkWidget *resume_icon = gtk_image_new_from_file (concat_strings(path_buf, exec_path, "../Resources/share/plisp/icons/resume.png"));
  GtkWidget *abort_icon = gtk_image_new_from_file (concat_strings(path_buf, exec_path, "../Resources/share/plisp/icons/abort.png"));
#else
  GtkWidget *resume_icon = gtk_image_new_from_file (PLISPDATADIR "/icons/resume.png");
  GtkWidget *abort_icon = gtk_image_new_from_file (PLISPDATADIR "/icons/abort.png");
#endif
#endif  

  toolbar = gtk_toolbar_new ();
  gtk_orientable_set_orientation (GTK_ORIENTABLE (toolbar), GTK_ORIENTATION_HORIZONTAL);
  gtk_toolbar_set_style (GTK_TOOLBAR (toolbar), GTK_TOOLBAR_ICONS);
  gtk_container_set_border_width (GTK_CONTAINER (toolbar), 5);

  /* gtk_toolbar_append_item (GTK_TOOLBAR (toolbar),                    */
  /*                          NULL,                                   /\* button label *\/ */
  /*                          "Resume (F5)",                          /\* button's tooltip *\/ */
  /*                          "Private",                              /\* tooltip private info *\/ */
  /*                          resume_icon,                            /\* icon widget *\/ */
  /*                          GTK_SIGNAL_FUNC(resume_from_debugger),  /\* a signal *\/ */
  /*                          (GtkWidget *)debugger_window); */
  GtkToolItem *resume_button = gtk_tool_button_new(resume_icon, NULL);
  gtk_tool_item_set_tooltip_text(resume_button, "Resume (F5)");
  g_signal_connect (resume_button, "clicked", G_CALLBACK (resume_from_debugger), debugger_window);
  gtk_toolbar_insert((GtkToolbar *)toolbar, resume_button, 0);

  /* gtk_toolbar_append_item (GTK_TOOLBAR (toolbar),                    */
  /*                          NULL,                                   /\* button label *\/ */
  /*                          "Abort",                                /\* button's tooltip *\/ */
  /*                          "Private",                              /\* tooltip private info *\/ */
  /*                          abort_icon,                             /\* icon widget *\/ */
  /*                          GTK_SIGNAL_FUNC(abort_debugger),        /\* a signal *\/ */
  /*                          (GtkWidget *)debugger_window); */
  GtkToolItem *abort_button = gtk_tool_button_new(abort_icon, NULL);
  gtk_tool_item_set_tooltip_text(abort_button, "Abort");
  g_signal_connect (abort_button, "clicked", G_CALLBACK (abort_debugger), debugger_window);
  gtk_toolbar_insert((GtkToolbar *)toolbar, abort_button, 1);
  return (GtkToolbar *)toolbar;
}

void create_debug_window(int posx, int posy, int width, int height)
{
  GtkWidget *win = gtk_window_new (GTK_WINDOW_TOPLEVEL);

  debugger_window = (GtkWindow *)win;

#ifdef WIN_BUILD
  gtk_window_set_icon_from_file(debugger_window, "../share/icons/evaluate.png", NULL);
#else
#ifdef __OSX_BUNDLE__
  gtk_window_set_icon_from_file(debugger_window, concat_strings(path_buf, exec_path, "../Resources/share/plisp/icons/evaluate.png"), NULL);
#else
  gtk_window_set_icon_from_file(debugger_window, PLISPDATADIR "/icons/evaluate.png", NULL);
#endif
#endif  

  GtkWidget *scrolled_win1;
  GtkWidget *vbox, *hbox1;

  gtk_window_set_title((GtkWindow *)win, "pLisp Debugger");

  /* gtk_window_set_default_size((GtkWindow *)win, 600, 400); */
  /* gtk_window_set_position(GTK_WINDOW(win), GTK_WIN_POS_CENTER); */
  gtk_window_set_default_size(debugger_window, width, height);
  gtk_window_move(debugger_window, posx, posy); 

  g_signal_connect (win, "delete-event",
                    G_CALLBACK (delete_event), NULL);

  g_signal_connect (win, "focus",
                    G_CALLBACK (set_triggering_window), NULL);

  g_signal_connect(G_OBJECT(win), 
                   "key_press_event", 
                   G_CALLBACK (handle_key_press_events), 
                   NULL);

  gtk_container_set_border_width (GTK_CONTAINER (win), 10);

  scrolled_win1 = gtk_scrolled_window_new(NULL, NULL);

  frames_list = (GtkTreeView *)gtk_tree_view_new();
  gtk_tree_view_set_headers_visible(GTK_TREE_VIEW(frames_list), TRUE);
  gtk_widget_override_font(GTK_WIDGET(frames_list), pango_font_description_from_string(FONT));

  initialize_frames_list((GtkTreeView *)frames_list);

  //frames should NOT be sorted
  //gtk_tree_view_column_set_sort_column_id(gtk_tree_view_get_column(frames_list, 0), 0); 

  populate_frames_list((GtkTreeView *)frames_list);

  gtk_container_add(GTK_CONTAINER (scrolled_win1), (GtkWidget *)frames_list);

  hbox1 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);

  gtk_box_pack_start(GTK_BOX (hbox1), scrolled_win1, TRUE, TRUE, 0);

  vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
  if(debug_mode)
    gtk_box_pack_start (GTK_BOX (vbox), (GtkWidget *)create_debugger_toolbar(), FALSE, FALSE, 0);

  gtk_box_pack_start (GTK_BOX (vbox), hbox1, TRUE, TRUE, 0);

  //uncomment this to add status bar
  //gtk_box_pack_start (GTK_BOX (vbox), gtk_statusbar_new(), FALSE, FALSE, 0);  

  gtk_container_add (GTK_CONTAINER (win), vbox);

  gtk_widget_show_all(win);

  gtk_widget_grab_focus((GtkWidget *)frames_list);
}

void initialize_operators_list(GtkTreeView *list)
{
  GtkCellRenderer    *renderer;
  GtkTreeViewColumn  *column1, *column2, *column3, *column4, *column5, *column6;
  GtkListStore       *store;

  renderer = gtk_cell_renderer_text_new();

  column1 = gtk_tree_view_column_new_with_attributes("Operator/Closure",
                                                     renderer, "text", 0, NULL);
  gtk_tree_view_append_column(GTK_TREE_VIEW (list), column1);

  column2 = gtk_tree_view_column_new_with_attributes("# of times called",
                                                     renderer, "text", 1, NULL);
  gtk_tree_view_append_column(GTK_TREE_VIEW (list), column2);

  column3 = gtk_tree_view_column_new_with_attributes("Wall time (seconds)",
                                                     renderer, "text", 2, NULL);
  gtk_tree_view_append_column(GTK_TREE_VIEW (list), column3);

  column4 = gtk_tree_view_column_new_with_attributes("CPU time (seconds)",
                                                     renderer, "text", 3, NULL);
  gtk_tree_view_append_column(GTK_TREE_VIEW (list), column4);

  column5 = gtk_tree_view_column_new_with_attributes("Allocated memory (words)",
                                                     renderer, "text", 4, NULL);
  gtk_tree_view_append_column(GTK_TREE_VIEW (list), column5);

  column6 = gtk_tree_view_column_new_with_attributes("Dellocated memory (words)",
                                                     renderer, "text", 5, NULL);
  gtk_tree_view_append_column(GTK_TREE_VIEW (list), column6);

  gtk_tree_view_column_set_sort_column_id(column1, 0);
  gtk_tree_view_column_set_sort_column_id(column2, 1);
  gtk_tree_view_column_set_sort_column_id(column3, 2);
  gtk_tree_view_column_set_sort_column_id(column4, 3);
  gtk_tree_view_column_set_sort_column_id(column5, 4); 
  gtk_tree_view_column_set_sort_column_id(column6, 5); 

  store = gtk_list_store_new (6, 
                              G_TYPE_STRING,
                              G_TYPE_INT,
                              G_TYPE_FLOAT,
                              G_TYPE_FLOAT,
                              G_TYPE_INT,
                              G_TYPE_INT);

  gtk_tree_view_set_model(GTK_TREE_VIEW (list), 
                          GTK_TREE_MODEL(store));

  g_object_unref(store);  
}

void populate_operators_list(GtkTreeView *list)
{
  /* GtkListStore *store; */
  /* GtkTreeIter  iter; */

  /* store = GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(list))); */

  /* hashtable_entry_t **entries = profiling_tab->entries; */

  /* int i, count = profiling_tab->hash_size; */

  /* for(i=0; i<count; i++) */
  /* { */
  /*   if(profiling_tab->entries[i]) */
  /*   { */
  /*     hashtable_entry_t *e = profiling_tab->entries[i]; */

  /*     while(e) */
  /*     { */
  /*       OBJECT_PTR operator = (OBJECT_PTR)e->ptr; */

  /*       profiling_datum_t *pd = (profiling_datum_t *)e->value; */

  /*       gtk_list_store_append(store, &iter); */

  /*       char buf1[MAX_STRING_LENGTH]; */
  /*       memset(buf1, '\0', MAX_STRING_LENGTH); */

  /*       OBJECT_PTR temp_obj = operator; */

  /*       print_object_to_string(temp_obj, buf1, 0); */

  /*       unsigned int count = pd->count; */
  /*       double elapsed_wall_time = pd->elapsed_wall_time; */
  /*       double elapsed_cpu_time = pd->elapsed_cpu_time; */
  /*       unsigned int mem_alloc = pd->mem_allocated; */
  /*       unsigned int mem_dealloc = pd->mem_deallocated; */
        
  /*       gtk_list_store_set(store, &iter, 0, buf1, 1, count, 2, elapsed_wall_time, 3, elapsed_cpu_time, 4, mem_alloc, 5, mem_dealloc, -1); */

  /*       e = e->next; */
  /*     } */
  /*   } */
  /* } */

  /* hashtable_entry_t *e = hashtable_entries(profiling_tab); */

  /* while(e) */
  /* { */
  /*   OBJECT_PTR operator = (OBJECT_PTR)e->ptr; */

  /*   profiling_datum_t *pd = (profiling_datum_t *)e->value; */

  /*   gtk_list_store_append(store, &iter); */

  /*   char buf1[MAX_STRING_LENGTH]; */
  /*   memset(buf1, '\0', MAX_STRING_LENGTH); */

  /*   OBJECT_PTR temp_obj = operator; */

  /*   /\* if(IS_SYMBOL_OBJECT(operator)) *\/ */
  /*   /\*    temp_obj = operator; *\/ */
  /*   /\* else *\/ */
  /*   /\*   temp_obj = cons(LAMBDA, *\/ */
  /*   /\*                   cons(get_params_object(operator), *\/ */
  /*   /\*                        cons(car(get_source_object(operator)), NIL))); *\/ */

  /*   print_object_to_string(temp_obj, buf1, 0); */

  /*   unsigned int count = pd->count; */
  /*   double elapsed_wall_time = pd->elapsed_wall_time; */
  /*   double elapsed_cpu_time = pd->elapsed_cpu_time; */
  /*   unsigned int mem_alloc = pd->mem_allocated; */
  /*   unsigned int mem_dealloc = pd->mem_deallocated; */

  /*   gtk_list_store_set(store, &iter, 0, buf1, 1, count, 2, elapsed_wall_time, 3, elapsed_cpu_time, 4, mem_alloc, 5, mem_dealloc, -1); */

  /*   hashtable_entry_t *temp = e->next; */
  /*   /\* free(e->value); *\/ */
  /*   /\* free(e); *\/ */
  /*   e = temp; */
  /* } */
}

void create_profiler_window(int posx, int posy, int width, int height)
{
  GtkWidget *win = gtk_window_new (GTK_WINDOW_TOPLEVEL);

  profiler_window = (GtkWindow *)win;

#ifdef WIN_BUILD
  gtk_window_set_icon_from_file(profiler_window, "../share/icons/evaluate.png", NULL);
#else
#ifdef __OSX_BUNDLE__
  gtk_window_set_icon_from_file(profiler_window, concat_strings(path_buf, exec_path, "../Resources/share/plisp/icons/evaluate.png"), NULL);
#else
  gtk_window_set_icon_from_file(profiler_window, PLISPDATADIR "/icons/evaluate.png", NULL);
#endif
#endif  

  GtkWidget *scrolled_win1;
  GtkWidget *vbox, *hbox1;

  gtk_window_set_title((GtkWindow *)win, "pLisp Profiler");

  /* gtk_window_set_default_size((GtkWindow *)win, 600, 400); */
  /* gtk_window_set_position(GTK_WINDOW(win), GTK_WIN_POS_CENTER); */
  gtk_window_set_default_size(profiler_window, width, height);
  gtk_window_move(profiler_window, posx, posy); 

  g_signal_connect (win, "delete-event",
                    G_CALLBACK (delete_event), NULL);

  g_signal_connect (win, "focus",
                    G_CALLBACK (set_triggering_window), NULL);

  g_signal_connect(win, 
		  "key_press_event", 
		  G_CALLBACK (handle_key_press_events), 
		  NULL);

  gtk_container_set_border_width (GTK_CONTAINER (win), 10);

  scrolled_win1 = gtk_scrolled_window_new(NULL, NULL);

  operators_list = (GtkTreeView *)gtk_tree_view_new();
  gtk_tree_view_set_headers_visible(GTK_TREE_VIEW(operators_list), TRUE);
  gtk_widget_override_font(GTK_WIDGET(operators_list), pango_font_description_from_string(FONT));

  initialize_operators_list((GtkTreeView *)operators_list);

  populate_operators_list((GtkTreeView *)operators_list);

  gtk_container_add(GTK_CONTAINER (scrolled_win1), (GtkWidget *)operators_list);

  hbox1 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);

  gtk_box_pack_start(GTK_BOX (hbox1), scrolled_win1, TRUE, TRUE, 0);

  vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
  gtk_box_pack_start (GTK_BOX (vbox), hbox1, TRUE, TRUE, 0);

  //uncomment this to add status bar
  //gtk_box_pack_start (GTK_BOX (vbox), gtk_statusbar_new(), FALSE, FALSE, 0);  

  gtk_container_add (GTK_CONTAINER (win), vbox);

  gtk_widget_show_all(win);

  gtk_widget_grab_focus((GtkWidget *)operators_list);
}

void create_help_window()
{
  GtkWidget *win = gtk_window_new(GTK_WINDOW_TOPLEVEL);

  gtk_window_set_title((GtkWindow *)win, "pLisp Symbol Help");

  help_window = win;

#ifdef WIN_BUILD
  gtk_window_set_icon_from_file(help_window, "../share/icons/evaluate.png", NULL);
#else
#ifdef __OSX_BUNDLE__
  gtk_window_set_icon_from_file(help_window, concat_strings(path_buf, exec_path, "../Resources/share/plisp/icons/evaluate.png"), NULL);
#else
  gtk_window_set_icon_from_file(help_window, PLISPDATADIR "/icons/evaluate.png", NULL);
#endif
#endif  

  //gtk_window_set_decorated((GtkWindow *)win, FALSE);

  gtk_window_set_default_size((GtkWindow *)win, 600, 280);
  gtk_window_set_position(GTK_WINDOW(win), GTK_WIN_POS_CENTER);

  GtkWidget *scrolled_win, *vbox;
  GtkWidget *textview = gtk_text_view_new ();

  gtk_text_view_set_wrap_mode((GtkTextView *)textview, GTK_WRAP_WORD);

  gtk_text_view_set_editable((GtkTextView *)textview, FALSE);

  help_buffer = gtk_text_view_get_buffer((GtkTextView *)textview);

  gtk_text_buffer_create_tag(help_buffer, "bold", 
                             "weight", PANGO_WEIGHT_BOLD, 
                             NULL);

  gtk_text_buffer_create_tag(help_buffer, "bold_blue_foreground",
                             "weight", PANGO_WEIGHT_BOLD, "foreground", "blue", NULL);  

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
                    G_CALLBACK (delete_event), NULL);

  g_signal_connect (win, "focus",
                    G_CALLBACK (set_triggering_window), NULL);

  g_signal_connect(G_OBJECT(win), 
                   "key_press_event", 
                   G_CALLBACK (handle_key_press_events), 
                   NULL);
}

void initialize_callers_symbols_list(GtkTreeView *list)
{
  GtkCellRenderer    *renderer;
  GtkTreeViewColumn  *column1;
  GtkListStore       *store;

  renderer = gtk_cell_renderer_text_new();

  column1 = gtk_tree_view_column_new_with_attributes("Referring Objects",
                                                     renderer, "text", 0, NULL);
  gtk_tree_view_append_column(GTK_TREE_VIEW (list), column1);

#if __x86_64__  
  store = gtk_list_store_new (2, G_TYPE_STRING, G_TYPE_INT64);
#else
#ifdef __APPLE__  
  store = gtk_list_store_new (2, G_TYPE_STRING, G_TYPE_INT64);
#else
  store = gtk_list_store_new (2, G_TYPE_STRING, G_TYPE_INT);
#endif
#endif
  
  gtk_tree_view_set_model(GTK_TREE_VIEW (list), 
                          GTK_TREE_MODEL(store));

  g_object_unref(store);  
}

extern OBJECT_PTR QUOTE;

BOOLEAN symbol_exists_in_list(OBJECT_PTR sym, OBJECT_PTR lst)
{
  if(lst == NIL)
    return false;

  if(is_atom(lst))
    return equal(sym, lst);

  //to ignore symbols that occur in quotes
  if(car(lst) == QUOTE)
    return false;
  
  if(symbol_exists_in_list(sym, car(lst)))
    return true;
  else
    return symbol_exists_in_list(sym, cdr(lst));
}

void populate_callers_symbols_list(GtkTreeView *symbols_list)
{
  remove_all_from_list(symbols_list);

  GtkListStore *store;
  GtkTreeIter  iter;

  store = GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(symbols_list)));

  int i;

  for(i=0; i<nof_global_vars; i++)
  {
    if(top_level_symbols[i].delete_flag || IS_NATIVE_FN_OBJECT(top_level_symbols[i].val))
      continue;

    /* OBJECT_PTR val = top_level_symbols[i].val; */

    /* if(IS_CONS_OBJECT(val) && */
    /*   //(IS_FUNCTION2_OBJECT(car(val)) && !is_continuation_object(car(val))) || IS_MACRO2_OBJECT(car(val))) */
    /*    (IS_FUNCTION2_OBJECT(car(val)) || IS_MACRO2_OBJECT(car(val)))) */
    /* { */
    /*   if(symbol_exists_in_list(callers_sym, car(get_source_object(car(val))))) */
    /*   { */
    /*     char buf[100]; */
    /*     memset(buf, '\0', 100); */

    /*     print_qualified_symbol(top_level_symbols[i].sym, buf); */

    /*     gtk_list_store_append(store, &iter); */
    /*     gtk_list_store_set(store, &iter, 0, buf, -1);   */
    /*     gtk_list_store_set(store, &iter, 1, top_level_symbols[i].sym, -1); */
    /*   } */
    /* } */

    if(top_level_symbols[i].sym == callers_sym)
    {
      int j;

      for(j=0; j<top_level_symbols[i].ref_count; j++)
      {
        OBJECT_PTR sym = reverse_sym_lookup(top_level_symbols[i].references[j].referrer);

        char buf[100];
        memset(buf, '\0', 100);

        print_qualified_symbol(sym, buf);

        gtk_list_store_append(store, &iter);
        gtk_list_store_set(store, &iter, 0, buf, -1);
        gtk_list_store_set(store, &iter, 1, sym, -1);        
        
      }
    }
  }
}

void create_callers_window(int posx, int posy, int width, int height)
{
  GtkWidget *win = gtk_window_new (GTK_WINDOW_TOPLEVEL);

  GtkWidget *scrolled_win1, *scrolled_win2;
  GtkWidget *vbox, *hbox;

#ifdef WIN_BUILD
  gtk_window_set_icon_from_file(win, "../share/icons/evaluate.png", NULL);
#else
#ifdef __OSX_BUNDLE__
  gtk_window_set_icon_from_file(win, concat_strings(path_buf, exec_path, "../Resources/share/plisp/icons/evaluate.png"), NULL);
#else
  gtk_window_set_icon_from_file(win, PLISPDATADIR "/icons/evaluate.png", NULL);
#endif
#endif  

  char buf1[SYMBOL_STRING_SIZE];
  print_qualified_symbol(callers_sym, buf1);
  
  char buf2[150];
  memset(buf2, '\0', 150);
  sprintf(buf2, "pLisp - Referrers for %s", buf1);
  
  gtk_window_set_title((GtkWindow *)win, buf2);

  gtk_window_set_default_size(win, width, height);
  gtk_window_move(win, posx, posy); 

  g_signal_connect (win, "delete-event",
                    G_CALLBACK (delete_event), NULL);

  g_signal_connect (win, "focus",
                    G_CALLBACK (set_triggering_window), NULL);

  gtk_container_set_border_width (GTK_CONTAINER (win), 10);

  scrolled_win1 = gtk_scrolled_window_new(NULL, NULL);
  scrolled_win2 = gtk_scrolled_window_new(NULL, NULL);

  callers_symbols_list = (GtkTreeView *)gtk_tree_view_new();
  gtk_tree_view_set_headers_visible(callers_symbols_list, TRUE);
  gtk_widget_override_font(GTK_WIDGET(callers_symbols_list), pango_font_description_from_string(FONT));

  initialize_callers_symbols_list(callers_symbols_list);

  gtk_tree_view_column_set_sort_column_id(gtk_tree_view_get_column(callers_symbols_list, 0), 0); 

  populate_callers_symbols_list(callers_symbols_list);

  g_signal_connect(G_OBJECT(callers_symbols_list), "cursor-changed",
                   G_CALLBACK(fetch_symbol_value_for_caller), NULL);

  g_signal_connect(G_OBJECT(win), 
                   "key_press_event", 
                   G_CALLBACK (handle_key_press_events), 
                   NULL);

  /* g_signal_connect(G_OBJECT(workspace_buffer),  */
  /*                  "notify::cursor-position",  */
  /*                  G_CALLBACK (handle_cursor_move),  */
  /*                  NULL); */

  gtk_container_add(GTK_CONTAINER (scrolled_win1), (GtkWidget *)callers_symbols_list);

  hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
  gtk_box_pack_start(GTK_BOX (hbox), scrolled_win1, TRUE, TRUE, 0);

  GtkWidget *scrolled_win;

  callers_source_buffer = gtk_source_buffer_new_with_language(source_language);
  callers_source_view = gtk_source_view_new_with_buffer(callers_source_buffer);

  gtk_text_buffer_create_tag(callers_source_buffer, "gray_bg", 
                             "background", "lightgray", NULL); 

  gtk_widget_override_font(GTK_WIDGET(callers_source_view), pango_font_description_from_string(FONT));
  //gtk_text_view_set_editable((GtkTextView *)callers_source_view, FALSE);

  g_signal_connect(G_OBJECT(callers_source_buffer), 
                   "notify::cursor-position", 
                   G_CALLBACK (handle_cursor_move), 
                   NULL);

  scrolled_win = gtk_scrolled_window_new (NULL, NULL);
  gtk_container_add (GTK_CONTAINER (scrolled_win), callers_source_view);

  vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
  gtk_box_pack_start (GTK_BOX (vbox), hbox, TRUE, TRUE, 0);
  gtk_box_pack_start (GTK_BOX (vbox), scrolled_win, TRUE, TRUE, 0);

  callers_statusbar = (GtkStatusbar *)gtk_statusbar_new();
  gtk_box_pack_start (GTK_BOX (vbox), (GtkWidget *)callers_statusbar, FALSE, FALSE, 0);  

  gtk_container_add (GTK_CONTAINER (win), vbox);

  callers_window = win;

  gtk_widget_show_all(win);
}

void highlight_text(GtkTextBuffer *buffer, char *text)
{
  GtkTextIter start_sel, end_sel;
  GtkTextIter start_find, end_find;
  GtkTextIter start_match, end_match;

  gtk_text_buffer_get_start_iter(buffer, &start_find);
  gtk_text_buffer_get_end_iter(buffer, &end_find);

  gtk_text_buffer_remove_tag_by_name(buffer, "gray_bg", 
                                     &start_find, &end_find);  

  //to skip the first line of the definition
  //(to avoid recursive calls)
  gtk_text_iter_forward_search(&start_find, "\n",
                               GTK_TEXT_SEARCH_TEXT_ONLY |
                               GTK_TEXT_SEARCH_VISIBLE_ONLY,
                               &start_match, &end_match, NULL);

  start_find = end_match;

  while (gtk_text_iter_forward_search(&start_find, text, 
                                      GTK_TEXT_SEARCH_TEXT_ONLY | 
                                      GTK_TEXT_SEARCH_VISIBLE_ONLY, 
                                      &start_match, &end_match, NULL))
  {
    gint offset1 = gtk_text_iter_get_offset(&start_match);

    BOOLEAN text_start_is_word_start, text_end_is_word_end;

    /* if(offset1 > 0) */
    /* { */
    /*   GtkTextIter st; */
    /*   gtk_text_buffer_get_iter_at_offset(buffer, &st, offset1-1); */

    /*   if(gtk_text_iter_get_char(&st) == '(') */
    /*     text_start_is_word_start = true; */
    /*   else */
    /*     text_start_is_word_start = false; */
    /* } */
    /* else */
      text_start_is_word_start = true;
    
    if(gtk_text_iter_get_char(&end_match) == ' ' || gtk_text_iter_get_char(&end_match) == ')' || gtk_text_iter_get_char(&end_match) == '\n')
      text_end_is_word_end = true;
    else
      text_end_is_word_end = false;

    //to avoid highlighting partial matches
    if(text_start_is_word_start && text_end_is_word_end)
      gtk_text_buffer_apply_tag_by_name(buffer, "gray_bg", 
                                        &start_match, &end_match);

    gint offset = gtk_text_iter_get_offset(&end_match);
    gtk_text_buffer_get_iter_at_offset(buffer, 
                                       &start_find, offset);  
  }
}
