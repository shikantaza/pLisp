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
#include <assert.h>
#include <stdint.h>

#include "../plisp.h"

#include "../util.h"

#include "../hashtable.h"

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

#define DEFAULT_SYSTEM_BROWSER_POSX 650
#define DEFAULT_SYSTEM_BROWSER_POSY 200

#define DEFAULT_SYSTEM_BROWSER_WIDTH 600
#define DEFAULT_SYSTEM_BROWSER_HEIGHT 400

#define DEFAULT_WORKSPACE_POSX 650
#define DEFAULT_WORKSPACE_POSY 200

#define DEFAULT_WORKSPACE_WIDTH 600
#define DEFAULT_WORKSPACE_HEIGHT 400

char *default_workspace_text =  "; This is the workspace; type pLisp expressions here.\n"
                                "; To evaluate an expression, enter the expression\n"
                                "; and press Ctrl+Enter when the expression is complete\n"
                                "; (indicated by the highlighted matching parens).\n"
                                "; To display help information about a core symbol or\n" 
                                "; operator, press F1 when the focus is on that symbol.\n"
                                "; If you are new to pLisp (or Lisp), please visit the file\n"
                                "; tutorial.lisp from the File Browser for a quick overview\n"
                                "; of the language and its features. This file is present in\n"
#ifdef WIN32
                                "; in the 'doc' directory of your pLisp installation.\n";
#else
#ifdef __OSX_BUNDLE__
                                "; in the 'doc' directory of your pLisp installation.\n";
#else
                                "; in the 'doc' directory of your pLisp installation\n"
                                "; (typically /usr/local/share/doc/plisp).\n";
#endif
#endif


extern OBJECT_PTR CAAR(OBJECT_PTR);

extern GtkTextBuffer *transcript_buffer;
extern GtkTextBuffer *workspace_buffer;

extern GtkWindow *transcript_window;
extern GtkWindow *workspace_window;
extern GtkWindow *system_browser_window;
extern GtkWindow *debugger_window;
extern GtkWindow *profiler_window;

extern GtkWindow *callers_window;

extern GtkWindow *file_browser_window;
extern GtkTextBuffer *curr_file_browser_buffer;
extern GtkTextView *curr_file_browser_text_view;

extern GtkTextBuffer *help_buffer;
extern GtkWindow *help_window;

extern GtkTreeView *packages_list;
extern GtkTreeView *symbols_list;

extern GtkTextBuffer *system_browser_buffer;
extern GtkTextView *system_browser_textview;

extern GtkSourceView *workspace_source_view;
extern GtkSourceBuffer *workspace_source_buffer;

extern GtkTreeView *callers_symbols_list;

extern GtkSourceBuffer *callers_source_buffer;
extern GtkSourceView *callers_source_view;

extern char *loaded_image_file_name;

extern unsigned int nof_packages;
extern package_t *packages;

extern OBJECT_PTR NIL;
extern OBJECT_PTR top_level_env;
extern OBJECT_PTR LAMBDA;
extern OBJECT_PTR MACRO;
extern OBJECT_PTR DEFINE;

extern BOOLEAN new_symbol_being_created;

extern GtkWindow *action_triggering_window;

extern GtkTreeView *frames_list;
extern GtkTreeView *variables_list;

extern GtkStatusbar *system_browser_statusbar;
extern GtkStatusbar *workspace_statusbar;
extern GtkStatusbar *callers_statusbar;
extern GtkStatusbar *file_browser_statusbar;

extern GtkNotebook *fb_notebook;

extern BOOLEAN debug_mode;

extern OBJECT_PTR build_list(int, ...);

extern void refresh_system_browser();

extern BOOLEAN system_changed;

//extern hashtable_t *profiling_tab;

void evaluate();
void close_application_window(GtkWidget **);
void show_workspace_window();
void load_image();
void save_image();
void set_focus_to_last_row(GtkTreeView *);

void show_system_browser_window();

void build_form_for_eval(GtkTextBuffer *);

extern BOOLEAN in_error;

extern OBJECT_PTR DEFUN;
extern OBJECT_PTR DEFMACRO;

extern unsigned nof_global_vars;
extern global_var_mapping_t *top_level_symbols;

char *form_for_eval;

extern int call_repl(char *);

char *get_current_word(GtkTextBuffer *);
void do_auto_complete(GtkTextBuffer *);

char **autocomplete_words = NULL;
unsigned int nof_autocomplete_words = 0;

extern unsigned int current_package;

extern help_entry_t *find_help_entry(char *);

char *get_common_prefix(unsigned int, char **);

extern OBJECT_PTR get_signature(char *);

extern char *get_signature_for_core_symbol(char *);

extern OBJECT_PTR callers_sym;

extern void highlight_text(GtkTextBuffer *, char *);

extern BOOLEAN quit_file_browser();

extern void find_text();

extern OBJECT_PTR replace_symbol(OBJECT_PTR, OBJECT_PTR, OBJECT_PTR);

extern OBJECT_PTR cons_equivalent(OBJECT_PTR);
extern OBJECT_PTR get_source_object(OBJECT_PTR);
extern uintptr_t extract_ptr(OBJECT_PTR);
extern OBJECT_PTR butlast(OBJECT_PTR);

extern OBJECT_PTR first(OBJECT_PTR);
extern OBJECT_PTR second(OBJECT_PTR);
extern OBJECT_PTR third(OBJECT_PTR);

extern int add_symbol_to_package(char *, int);

extern char *get_doc_str(OBJECT_PTR);

unsigned int print_context_pkg_index;

void quit_application();
void show_file_browser_window();
void callers_window_accept();

void build_autocomplete_words();

void fetch_symbol_value(GtkWidget *, gpointer);
  
int get_indents_for_form(char *form)
{
  char *up = convert_to_upper_case(form);

  if(!strcmp(up, "DEFUN") || !strcmp(up, "DEFMACRO") || !strcmp(up, "LAMBDA"))
    return 2;
  else if(!strcmp(up, "PROGN"))
    return 7;
  else if(!strcmp(up, "COND"))
    return 6;
  else if(!strcmp(up, "IF"))
    return 4;
  else if(!strcmp(up, "LET"))
    return 6;
  else if(!strcmp(up, "LET*"))
    return 7;

  return 0;
}

void get_form(char *str, char *ret)
{
  if(!str)
  {
    ret = NULL;
    return;
  }

  int i, len = strlen(str);

  int start, size;

  for(i=0; i<len; i++)
  {
    if(str[i] != '(' && str[i] != ' ')
    {
      start = i;
      break;
    }
  }

  while(str[i] != ' ' && i < len)
  {
    ret[i-start] = str[i];
    i++;
  }
}

int get_carried_over_indents(char *str)
{
  int ret = 0;

  int i = 0, len = strlen(str);

  while(i<len)
  {
    if(str[i] == ' ')ret++;
    else break;
    i++;
  }

  return ret;
}

//position of the first non-space character
//in the 
int get_position_of_first_arg(char *str)
{
  int i, len = strlen(str);

  for(i=1; i<len-1; i++)
    if(str[i] == ' ' && str[i+1] != ' ')
      return i+2;

  return 0;
}

void resume()
{
  close_application_window((GtkWidget **)&debugger_window);
  call_repl("(RESUME)");
}

void update_transcript_title()
{
  if(transcript_window)
  {
    if(loaded_image_file_name == NULL)
      gtk_window_set_title(transcript_window, "pLisp Transcript");
    else
    {
      char buf[MAX_STRING_LENGTH];
      memset(buf, '\0', MAX_STRING_LENGTH);
      sprintf(buf,"pLisp Transcript - %s", loaded_image_file_name);
      gtk_window_set_title(transcript_window, buf);
    }
  }
}

void update_workspace_title()
{
  if(workspace_window)
    prompt();
}

gboolean delete_event( GtkWidget *widget,
                       GdkEvent  *event,
                       gpointer   data )
{
  if(widget == (GtkWidget *)transcript_window)
  {
    quit_application();

    //if control comes here, it means
    //the user cancelled the quit operation
    return TRUE;
  }
  else if(widget == (GtkWidget *)workspace_window)
    close_application_window((GtkWidget **)&workspace_window);
  else if(widget == (GtkWidget *)system_browser_window)
    close_application_window((GtkWidget **)&system_browser_window);
  else if(widget == (GtkWidget *)debugger_window)
  {
    close_application_window((GtkWidget **)&debugger_window);
  }
  /* else if(widget == (GtkWidget *)profiler_window) */
  /* { */
  /*   close_application_window((GtkWidget **)&profiler_window); */
  /*   hashtable_delete(profiling_tab); */
  /*   profiling_tab = NULL; */
  /* } */
  else if(widget == (GtkWidget *)help_window)
    close_application_window(&help_window);
  else if(widget == (GtkWidget *)callers_window)
    close_application_window(&callers_window);

  return FALSE;
}

void quit_application()
{
  GtkWidget *dialog = gtk_message_dialog_new ((GtkWindow *)transcript_window,
                                              GTK_DIALOG_DESTROY_WITH_PARENT,
                                              GTK_MESSAGE_QUESTION,
                                              GTK_BUTTONS_YES_NO,
                                              "Do you really want to quit?");

  gtk_widget_grab_focus(gtk_dialog_get_widget_for_response(GTK_DIALOG(dialog), GTK_RESPONSE_NO));

  if(gtk_dialog_run(GTK_DIALOG (dialog)) == GTK_RESPONSE_YES)
  {
    gtk_widget_destroy((GtkWidget *)dialog);

    //uncomment this if we want to
    //auto save image on quit
    //if(loaded_image_file_name != NULL)
    //  save_image();
    if(system_changed && loaded_image_file_name)
    {
      GtkWidget *dialog1 = gtk_message_dialog_new ((GtkWindow *)transcript_window,
                                                   GTK_DIALOG_DESTROY_WITH_PARENT,
                                                   GTK_MESSAGE_QUESTION,
                                                   GTK_BUTTONS_YES_NO,
                                                   "System has changed; save image?");

      gtk_widget_grab_focus(gtk_dialog_get_widget_for_response(GTK_DIALOG(dialog1), GTK_RESPONSE_YES));

      if(gtk_dialog_run(GTK_DIALOG (dialog1)) == GTK_RESPONSE_YES)
      {
        gtk_widget_destroy((GtkWidget *)dialog1);
        save_image();
      }
    }

    g_free(loaded_image_file_name);

    cleanup();

    gtk_main_quit();
    exit(0);
  }
  else
    gtk_widget_destroy((GtkWidget *)dialog);
}

void set_triggering_window(GtkWidget *widget,
                           gpointer   data)
{
  action_triggering_window = (GtkWindow *)widget;
}

/* Another callback */
void quit(GtkWidget *widget,
          gpointer   data )
{
  quit_application();
}

void create_new_symbol()
{
  gchar *package_name = NULL;

  GtkListStore *store1 = GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(packages_list)));
  GtkTreeModel *model1 = gtk_tree_view_get_model (GTK_TREE_VIEW (packages_list));
  GtkTreeIter  iter1;

  if(gtk_tree_selection_get_selected(gtk_tree_view_get_selection(GTK_TREE_VIEW(packages_list)), &model1, &iter1))
  {
    gtk_tree_model_get(model1, &iter1,
                       0, &package_name,
                       -1);
  }

  if(!package_name)
  {
    show_error_dialog("Please select package in which to create the symbol");
    return;
  }

  if(!strcmp(package_name, "CORE"))
  {
    show_error_dialog("CORE package cannot be updated");
    return;
  }

  new_symbol_being_created = true;

  gtk_text_buffer_set_text(system_browser_buffer, "", -1);
  gtk_statusbar_remove_all(system_browser_statusbar, 0);

  gtk_widget_grab_focus((GtkWidget *)system_browser_textview);
}

void delete_package_or_symbol()
{
  gchar *symbol_name;

  GtkListStore *store2 = GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(symbols_list)));
  GtkTreeModel *model2 = gtk_tree_view_get_model (GTK_TREE_VIEW (packages_list));
  GtkTreeIter  iter2;

  if(!gtk_tree_selection_get_selected(gtk_tree_view_get_selection(GTK_TREE_VIEW(symbols_list)), &model2, &iter2))
  {
    show_error_dialog("Please select a symbol to delete\n");
    return;
  }

  GtkWidget *dialog = gtk_message_dialog_new ((GtkWindow *)system_browser_window,
                                              GTK_DIALOG_DESTROY_WITH_PARENT,
                                              GTK_MESSAGE_QUESTION,
                                              GTK_BUTTONS_YES_NO,
                                              "Confirm delete");

  gtk_widget_grab_focus(gtk_dialog_get_widget_for_response(GTK_DIALOG(dialog), GTK_RESPONSE_NO));

  if(gtk_dialog_run(GTK_DIALOG (dialog)) == GTK_RESPONSE_YES)
  {
    gtk_widget_destroy((GtkWidget *)dialog);

    gchar *package_name;

    GtkListStore *store1 = GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(packages_list)));
    GtkTreeModel *model1 = gtk_tree_view_get_model (GTK_TREE_VIEW (packages_list));
    GtkTreeIter  iter1;

    if(gtk_tree_selection_get_selected(gtk_tree_view_get_selection(GTK_TREE_VIEW(packages_list)), &model1, &iter1))
    {
      gtk_tree_model_get(model1, &iter1,
                         0, &package_name,
                         -1);
    }

    gtk_tree_model_get(model2, &iter2,
                       0, &symbol_name,
                       -1);

    char buf[MAX_STRING_LENGTH];
    memset(buf, '\0', MAX_STRING_LENGTH);

    sprintf(buf, "(progn (in-package \"%s\") (unbind '%s:%s))", package_name, package_name, symbol_name);
    //sprintf(buf, "(unbind '%s)", symbol_name);

    if(!call_repl(buf))
    {
      refresh_system_browser();
      gtk_statusbar_push(system_browser_statusbar, 0, "Evaluation successful");
    }
  
  }
  else
    gtk_widget_destroy((GtkWidget *)dialog);  
}

void delete_pkg_or_sym(GtkWidget *widget,
                gpointer data)
{
  delete_package_or_symbol();
}

int get_new_package_name(char *buf)
{
  GtkWidget *dialog;
  GtkWidget *entry;
  GtkWidget *content_area;

  dialog = gtk_dialog_new_with_buttons("Create Package",
                                       action_triggering_window,
                                       GTK_DIALOG_DESTROY_WITH_PARENT, 
                                       //GTK_STOCK_OK,
                                       "OK",
                                       GTK_RESPONSE_ACCEPT,
                                       //GTK_STOCK_CANCEL,
                                       "Cancel",
                                       GTK_RESPONSE_REJECT,
                                       NULL);

  gtk_window_set_resizable((GtkWindow *)dialog, FALSE);

  gtk_window_set_transient_for(GTK_WINDOW(dialog), action_triggering_window);
  gtk_window_set_position(GTK_WINDOW(dialog), GTK_WIN_POS_CENTER_ON_PARENT);

  content_area = gtk_dialog_get_content_area(GTK_DIALOG(dialog));
  entry = gtk_entry_new();
  gtk_container_add(GTK_CONTAINER(content_area), entry);

  gtk_widget_show_all(dialog);
  gint result = gtk_dialog_run(GTK_DIALOG(dialog));

  if(result == GTK_RESPONSE_ACCEPT)
    strcpy(buf, gtk_entry_get_text(GTK_ENTRY(entry)));

  gtk_widget_destroy(dialog);
  
  return result;
    
}

void create_new_package()
{
  char buf[MAX_STRING_LENGTH];
  memset(buf, '\0', MAX_STRING_LENGTH);

  int result = GTK_RESPONSE_ACCEPT;

  unsigned int valid_package_name = 0;

  char trimmed_buf[100];
  
  while(result == GTK_RESPONSE_ACCEPT && (strlen(buf) == 0 || !valid_package_name))
  {
    result = get_new_package_name(buf);

    memset(trimmed_buf, '\0', 100);

    trim_whitespace(trimmed_buf, 100, buf);

    valid_package_name = is_valid_symbol_name(trimmed_buf);
    
    if((strlen(buf) == 0 || !valid_package_name) && result == GTK_RESPONSE_ACCEPT)
      show_error_dialog("Please enter a valid package name\n");
  }

  if(result == GTK_RESPONSE_ACCEPT)
  {
    char buf1[MAX_STRING_LENGTH];
    memset(buf1, '\0', MAX_STRING_LENGTH);

    sprintf(buf1, "(create-package \"%s\")", trimmed_buf);

    if(!call_repl(buf1))
    {
      refresh_system_browser();
      gtk_statusbar_push(system_browser_statusbar, 0, "Package created");
    }
  }
}

void refresh_sys_browser(GtkWidget *widget,
                         gpointer data)
{
  refresh_system_browser();
}

void new_package(GtkWidget *widget,
                gpointer data)
{
  create_new_package();
}


void new_symbol(GtkWidget *widget,
                gpointer data)
{
  action_triggering_window = (GtkWindow *)data;
  create_new_symbol();
}

int get_new_symbol_name(char *buf)
{
  GtkWidget *dialog;
  GtkWidget *entry;
  GtkWidget *content_area;

  dialog = gtk_dialog_new_with_buttons("Rename Symbol",
                                       action_triggering_window,
                                       GTK_DIALOG_DESTROY_WITH_PARENT, 
                                       //GTK_STOCK_OK,
                                       "OK",
                                       GTK_RESPONSE_ACCEPT,
                                       //GTK_STOCK_CANCEL,
                                       "Cancel",
                                       GTK_RESPONSE_REJECT,
                                       NULL);

  gtk_window_set_resizable((GtkWindow *)dialog, FALSE);

  gtk_window_set_transient_for(GTK_WINDOW(dialog), action_triggering_window);
  gtk_window_set_position(GTK_WINDOW(dialog), GTK_WIN_POS_CENTER_ON_PARENT);

  content_area = gtk_dialog_get_content_area(GTK_DIALOG(dialog));
  entry = gtk_entry_new();
  gtk_container_add(GTK_CONTAINER(content_area), entry);

  gtk_widget_show_all(dialog);
  gint result = gtk_dialog_run(GTK_DIALOG(dialog));

  if(result == GTK_RESPONSE_ACCEPT)
    strcpy(buf, gtk_entry_get_text(GTK_ENTRY(entry)));

  gtk_widget_destroy(dialog);
  
  return result;
    
}

void rename_sym()
{
  gchar *package_name = NULL;

  GtkListStore *store1 = GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(packages_list)));
  GtkTreeModel *model1 = gtk_tree_view_get_model (GTK_TREE_VIEW (packages_list));
  GtkTreeIter  iter1;

  if(gtk_tree_selection_get_selected(gtk_tree_view_get_selection(GTK_TREE_VIEW(packages_list)), &model1, &iter1))
  {
    gtk_tree_model_get(model1, &iter1,
                       0, &package_name,
                       -1);
  }

  if(!package_name)
  {
    show_error_dialog("Please select package containing symbol to be renamed");
    return;
  }

  if(!strcmp(package_name, "CORE"))
  {
    show_error_dialog("CORE package cannot be updated");
    return;
  }

  gchar *symbol_name;

  GtkListStore *store2 = GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(symbols_list)));
  GtkTreeModel *model2 = gtk_tree_view_get_model (GTK_TREE_VIEW (packages_list));
  GtkTreeIter  iter2;

  if(!gtk_tree_selection_get_selected(gtk_tree_view_get_selection(GTK_TREE_VIEW(symbols_list)), &model2, &iter2))
  {
    show_error_dialog("Please select a symbol to rename\n");
    return;
  }
  
  char buf[MAX_STRING_LENGTH];
  memset(buf, '\0', MAX_STRING_LENGTH);

  int result = GTK_RESPONSE_ACCEPT;

  unsigned int valid_symbol_name = 0;

  char trimmed_buf[100];
  
  while(result == GTK_RESPONSE_ACCEPT && (strlen(buf) == 0 || !valid_symbol_name))
  {
    result = get_new_symbol_name(buf);

    memset(trimmed_buf, '\0', 100);

    trim_whitespace(trimmed_buf, 100, buf);

    valid_symbol_name = is_valid_symbol_name(trimmed_buf);
    
    if((strlen(buf) == 0 || !valid_symbol_name) && result == GTK_RESPONSE_ACCEPT)
      show_error_dialog("Please enter a valid symbol name\n");
  }

  if(result == GTK_RESPONSE_ACCEPT)
  {
    
    GtkListStore *store3 = GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(symbols_list)));
    GtkTreeModel *model3 = gtk_tree_view_get_model (GTK_TREE_VIEW (symbols_list));
    GtkTreeIter  iter3;

    GtkTreeSelection *selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(symbols_list));

    if(!selection)
      return;

    if(gtk_tree_selection_get_selected(selection, &model3, &iter3))
    {
#if __x86_64__
      gint64 ptr;
#else
#ifdef __APPLE__
      gint64 ptr;
#else
      gint ptr;
#endif
#endif

      gtk_tree_model_get(model3, &iter3,
                         0, &symbol_name,
                         -1);
      
      gtk_tree_model_get(model3, &iter3,
                         1, &ptr,
                         -1);    


      int package_index = extract_package_index((OBJECT_PTR)ptr);

      int index = find_symbol(trimmed_buf, package_index);

      OBJECT_PTR new_sym_obj;
      
      if(index != NOT_FOUND) //symbol exists in symbol table
        new_sym_obj = build_symbol_object(package_index, index);
      else
        new_sym_obj = build_symbol_object(package_index, add_symbol_to_package(convert_to_upper_case(trimmed_buf), package_index));
    
      int i;

      for(i=0; i<nof_global_vars; i++)
      {
        if(top_level_symbols[i].delete_flag)
          continue;

        if(top_level_symbols[i].sym == (OBJECT_PTR)ptr)
        {
          int j;

          for(j=0; j<top_level_symbols[i].ref_count; j++)
          {
            OBJECT_PTR referrer = top_level_symbols[i].references[j].referrer;

            OBJECT_PTR cons_equiv = cons_equivalent(referrer);
            
            OBJECT_PTR source_obj = car(last_cell(referrer));

            OBJECT_PTR rest = cons_equiv;

            OBJECT_PTR prev_elem;
            
            while(rest != NIL)
            {
              prev_elem = rest;
              rest = cdr(rest);
            }
            
            set_heap(extract_ptr(prev_elem),
                     1,
                     cons(list(3, first(source_obj), second(source_obj), replace_symbol(third(source_obj), (OBJECT_PTR)ptr, new_sym_obj)),
                          NIL));
          }

          top_level_symbols[i].sym = new_sym_obj;
          break;
        }
      }

      //update new symbol name in the symbols list
      gtk_list_store_set(store3, &iter3, 0, trimmed_buf);
      gtk_list_store_set(store3, &iter3, 1, new_sym_obj);

      //to replace reference to the old symbol name
      build_autocomplete_words();

      //update the code panel with new symbol value
      fetch_symbol_value(symbols_list, system_browser_window);
      
      gtk_statusbar_push(system_browser_statusbar, 0, "Symbol renamed");
    }
  }
}

void rename_symbol(GtkWidget *widget,
                   gpointer data)
{
  action_triggering_window = (GtkWindow *)data;
  rename_sym();
}

void system_browser_accept()
{
  if(!gtk_text_view_get_editable(system_browser_textview))
    return;

  gchar *package_name, *symbol_name;

  GtkListStore *store1 = GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(packages_list)));
  GtkTreeModel *model1 = gtk_tree_view_get_model (GTK_TREE_VIEW (packages_list));
  GtkTreeIter  iter1;

  if(gtk_tree_selection_get_selected(gtk_tree_view_get_selection(GTK_TREE_VIEW(packages_list)), &model1, &iter1))
  {
    gtk_tree_model_get(model1, &iter1,
                       0, &package_name,
                       -1);
  }

  if(new_symbol_being_created)
  {
    char buf[MAX_STRING_LENGTH];
    memset(buf, '\0', MAX_STRING_LENGTH);

    sprintf(buf, "(in-package \"%s\")", package_name);

    if(call_repl(buf))
    {
        show_error_dialog("Evaluation failed\n");
        return;
    }

    GtkTextIter start, end;

    gtk_text_buffer_get_start_iter(system_browser_buffer, &start);
    gtk_text_buffer_get_end_iter (system_browser_buffer, &end);

    if(!call_repl(gtk_text_buffer_get_text(system_browser_buffer, &start, &end, FALSE)))
    {
      update_workspace_title();
      refresh_system_browser();

      gboolean valid;
      BOOLEAN package_found = false;
      int idx = 0;
      gchar *pkg_name;

      valid = gtk_tree_model_get_iter_first(GTK_TREE_MODEL(store1), &iter1);

      while (valid && !package_found)
      {
        gtk_tree_model_get(model1, &iter1, 0, &pkg_name, -1);

        if(!strcmp(pkg_name, package_name))
        {
          package_found = true;

          GtkTreePath *path = gtk_tree_path_new_from_indices(idx, -1);
          gtk_tree_view_set_cursor(packages_list, path, NULL, false);

          g_free(pkg_name);
          //g_free(path);
          break;
        }

        valid = gtk_tree_model_iter_next(GTK_TREE_MODEL(store1), &iter1);
        idx++;
      }

      //the newly added symbol will be the last row
      set_focus_to_last_row(symbols_list);
      gtk_statusbar_push(system_browser_statusbar, 0, "Evaluation successful");
    }

    new_symbol_being_created = false;
  }
  else
  {
    GtkListStore *store2 = GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(symbols_list)));
    GtkTreeModel *model2 = gtk_tree_view_get_model (GTK_TREE_VIEW (symbols_list));
    GtkTreeIter  iter2;

    if(gtk_tree_selection_get_selected(gtk_tree_view_get_selection(GTK_TREE_VIEW(symbols_list)), &model2, &iter2))
    {
      gtk_tree_model_get(model2, &iter2,
                         0, &symbol_name,
                         -1);
    }

    if(package_name && symbol_name)
    {
      char buf[MAX_STRING_LENGTH];
      memset(buf, '\0', MAX_STRING_LENGTH);

      sprintf(buf, "(in-package \"%s\")", package_name);

      if(call_repl(buf))
        return;

      GtkTextIter start, end;

      gtk_text_buffer_get_start_iter(system_browser_buffer, &start);
      gtk_text_buffer_get_end_iter (system_browser_buffer, &end);

      if(!call_repl(gtk_text_buffer_get_text(system_browser_buffer, &start, &end, FALSE)))
      {
        update_workspace_title();
        gtk_statusbar_push(system_browser_statusbar, 0, "Evaluation successful");
      }
    }
  }
}

void accept(GtkWidget *widget,
            gpointer data)
{
  action_triggering_window = (GtkWindow *)data;
  system_browser_accept();
}

void eval_expression(GtkWidget *widget,
                     gpointer data)
{
  action_triggering_window = (GtkWindow *)data;
  evaluate();
  //gtk_window_present(transcript_window);
}

void close_window(GtkWidget *widget,
                  gpointer data)
{
  if((GtkWidget *)data == (GtkWidget *)workspace_window)
    close_application_window((GtkWidget **)&workspace_window);
  else if((GtkWidget *)data == (GtkWidget *)system_browser_window)
    close_application_window((GtkWidget **)&system_browser_window);
  else if((GtkWidget *)data == (GtkWidget *)file_browser_window)
    //close_application_window((GtkWidget **)&file_browser_window);
    quit_file_browser();
}

void evaluate()
{
  GtkTextBuffer *buf;
  GtkTextIter start_sel, end_sel;
  gboolean selected;

  if(action_triggering_window == workspace_window)
    buf = workspace_buffer;
  else
    buf = curr_file_browser_buffer;

  selected = gtk_text_buffer_get_selection_bounds(buf, &start_sel, &end_sel);

  char *expression;

  if(selected)
  {
    expression = (char *) gtk_text_buffer_get_text(buf, &start_sel, &end_sel, FALSE);
  }
  else
  {
    /* GtkTextIter line_start, line_end, iter; */
    /* gtk_text_buffer_get_iter_at_mark(buf, &iter, gtk_text_buffer_get_insert(buf)); */
    /* gint line_number = gtk_text_iter_get_line(&iter); */

    /* gint line_count = gtk_text_buffer_get_line_count(buf); */

    /* if(line_count == (line_number+1)) */
    /* { */
    /*   gtk_text_buffer_get_iter_at_line(buf, &line_start, line_number-1); */
    /*   gtk_text_buffer_get_end_iter (buf, &line_end); */
    /* } */
    /* else */
    /* { */
    /*   gtk_text_buffer_get_iter_at_line(buf, &line_start, line_number); */
    /*   gtk_text_buffer_get_iter_at_line(buf, &line_end, line_number+1); */
    /* } */

    /* expression = (char *)gtk_text_buffer_get_text(buf, &line_start, &line_end, FALSE); */

    expression = form_for_eval;
  }

  if(expression)
    if(!call_repl(expression))
      update_workspace_title();
}

void load_source()
{
  GtkWidget *dialog;

  dialog = gtk_file_chooser_dialog_new ("Load pLisp source file",
                                        (GtkWindow *)workspace_window,
                                        GTK_FILE_CHOOSER_ACTION_OPEN,
                                        "Cancel", GTK_RESPONSE_CANCEL,
                                        "Open", GTK_RESPONSE_ACCEPT,
                                        NULL);

  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
  {

    char *loaded_source_file_name = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));

    gtk_widget_destroy (dialog);

    char buf[MAX_STRING_LENGTH];
    memset(buf, '\0', MAX_STRING_LENGTH);

    sprintf(buf, "(load-file \"%s\")", loaded_source_file_name);

    if(!call_repl(buf))
    {
      update_workspace_title();

      print_to_transcript("Source file ");
      print_to_transcript(loaded_source_file_name);
      print_to_transcript(" loaded successfully\n");
    }
    else
      print_to_transcript("Error loading source file\n");
  }
  else
    gtk_widget_destroy (dialog);
}

void close_application_window(GtkWidget **window)
{
  gtk_widget_destroy(*window);
  *window = NULL;
}

//this function is needed because
//referring to debugger_window from full_monty_compiler.c
//entails #include'ing gtk.h, which screws up the definition of TRUE
void close_debugger_window()
{
  if(debugger_window)
    close_application_window((GtkWidget **)&debugger_window);
}

void handle_cursor_move(GtkWidget *widget,
                        gpointer data)
{
  GtkTextBuffer *buffer = (GtkTextBuffer *)widget;
  GtkStatusbar *statusbar;

  if(buffer == workspace_buffer)
    statusbar = workspace_statusbar;
  else if(buffer == system_browser_buffer)
    statusbar = system_browser_statusbar;
  else if(buffer == curr_file_browser_buffer)
    statusbar = file_browser_statusbar;
  else
    statusbar = callers_statusbar;

  char *s = get_current_word(buffer);

  //TODO: figure out when to clear the status bar
  /* if(strlen(s) == 0) */
  /* { */
  /*   gtk_statusbar_remove_all(statusbar, 0); */
  /*   return; */
  /* } */

  
  if(is_core_symbol(s))
  {
    gtk_statusbar_remove_all(statusbar, 0);
    gtk_statusbar_push(statusbar, 0, get_signature_for_core_symbol(s));
    return;
  }

  OBJECT_PTR signature = get_signature(convert_to_upper_case(s));

  if(signature != NIL)
  {
    char buf[200];
    memset(buf, '\0', 200);

    if(!strcmp(s, "DEFUN") || !strcmp(s, "DEFMACRO"))
    {
      unsigned int len = 0;
      len += sprintf(buf, "(%s ", s);

      char buf_params[200];
      memset(buf_params, '\0', 200);

      print_object_to_string(cdr(signature), buf_params, 0);

      len += sprintf(buf+len, "%s", buf_params+1);
    }
    else
      print_object_to_string(signature, buf, 0);

    //replace newlines with spaces
    int i;
    for(i=0; i<strlen(buf); i++)
      if(buf[i] == '\n')
        buf[i] = ' ';

    //replace multiple spaces by a single space
    char buf1[100];
    memset(buf1, '\0', 100);

    buf1[0] = buf[0];

    i=1;
    int j=1;
    while(i<strlen(buf))
    {
      if(buf[i-1] == ' ' && buf[i] == ' ')
        i++;
      else
      {
        buf1[j] = buf[i];
        j++;
        i++;
      }
    }

    gtk_statusbar_remove_all(statusbar, 0);
    gtk_statusbar_push(statusbar, 0, convert_to_lower_case(buf1));
  }

  //free(s);
}

gboolean handle_key_press_events(GtkWidget *widget, GdkEventKey *event, gpointer user_data)
{
  if(widget == (GtkWidget *)file_browser_window && (event->state & GDK_CONTROL_MASK) && event->keyval == GDK_KEY_n)
  {
    new_source_file();
    return TRUE;
  }
  else if(widget == (GtkWidget *)file_browser_window && (event->state & GDK_CONTROL_MASK) && event->keyval == GDK_KEY_o)
  {
    fb_load_source_file();
    return TRUE;
  }
  else if(widget == (GtkWidget *)file_browser_window && (event->state & GDK_CONTROL_MASK) && event->keyval == GDK_KEY_s)
    save_file();
  else if(widget == (GtkWidget *)file_browser_window && (event->state & GDK_CONTROL_MASK) && event->keyval == GDK_KEY_w)
    close_file();
  else if(widget == (GtkWidget *)file_browser_window && event->keyval == GDK_KEY_F5)
    reload_file();
  else if(widget == (GtkWidget *)file_browser_window && (event->state & GDK_CONTROL_MASK) && event->keyval == GDK_KEY_f)
    find_text();
  else if(widget == (GtkWidget *)file_browser_window && (event->state & GDK_CONTROL_MASK) && event->keyval == GDK_KEY_q)
  {
    quit_file_browser();
    return TRUE;
  }
  else if(widget == (GtkWidget *)workspace_window && (event->state & GDK_CONTROL_MASK) && event->keyval == GDK_KEY_l)
    load_source();
  //else if(widget == (GtkWidget *)workspace_window && event->keyval == GDK_F5)
  else if(widget == (GtkWidget *)workspace_window && (event->state & GDK_CONTROL_MASK) && event->keyval == GDK_KEY_b)
  {
    show_file_browser_window();
    return TRUE;
  }
  else if(widget == (GtkWidget *)workspace_window && (event->state & GDK_CONTROL_MASK) && event->keyval == GDK_KEY_Return)
  {
    if(event->state & GDK_CONTROL_MASK)
    {
      build_form_for_eval(workspace_buffer);
      action_triggering_window = workspace_window;
      evaluate();
      //gtk_window_present(transcript_window);
      /* GtkTextIter start_sel, end_sel; */
      /* if(!gtk_text_buffer_get_selection_bounds(workspace_buffer, &start_sel, &end_sel)) */
      /* 	gtk_text_buffer_insert_at_cursor(workspace_buffer, "\n", -1); */
      //return FALSE;
      return TRUE;
    }
  }
  else if(widget == (GtkWidget *)file_browser_window && (event->state & GDK_CONTROL_MASK) && event->keyval == GDK_KEY_Return)
  {
    if(gtk_notebook_get_n_pages(fb_notebook) == 0)
      return TRUE;

    if(event->state & GDK_CONTROL_MASK)
    {
      build_form_for_eval(curr_file_browser_buffer);
      action_triggering_window = file_browser_window;
      evaluate();
      //gtk_window_present(transcript_window);
      /* GtkTextIter start_sel, end_sel; */
      /* if(!gtk_text_buffer_get_selection_bounds(workspace_buffer, &start_sel, &end_sel)) */
      /* 	gtk_text_buffer_insert_at_cursor(workspace_buffer, "\n", -1); */
      //return FALSE;
      return TRUE;
    }
  }
  else if((widget == (GtkWidget *)workspace_window || widget == (GtkWidget *)system_browser_window) && 
	  event->keyval == GDK_KEY_Tab)
  {
    indent(widget == (GtkWidget *)workspace_window ? workspace_buffer : system_browser_buffer);
    do_auto_complete(widget == (GtkWidget *)workspace_window ? workspace_buffer : system_browser_buffer);
    return TRUE;
  }
  else if((widget == (GtkWidget *)workspace_window      || 
           widget == (GtkWidget *)system_browser_window || 
           widget == (GtkWidget *)callers_window        ||
           widget == (GtkWidget *)help_window           ||
           widget == (GtkWidget *)file_browser_window)  && 
	  event->keyval == GDK_KEY_F1)
  {
    enum {FUNCTION, MACRO, SPECIAL_OPERATOR};

    if(!help_window)
      create_help_window();

    GtkTextBuffer *buffer;

    if(widget == (GtkWidget *)workspace_window)
      buffer = workspace_buffer;
    else if(widget == (GtkWidget *)system_browser_window)
      buffer = system_browser_buffer;
    else if(widget == (GtkWidget *)callers_window)
      buffer = callers_source_buffer;
    else if(widget == (GtkWidget *)file_browser_window)
      buffer = curr_file_browser_buffer;
    else
      buffer = help_buffer;

    char *s = get_current_word(buffer);

    help_entry_t *entry;

    //to remove the comma that occurs
    //in the see-also list in the help window
    if(buffer == help_buffer && s[strlen(s)-1] == ',')
    {
      char *s1 = substring(s, 0, strlen(s)-1);
      entry = find_help_entry(s1);
      //free(s1);
    }
    else
      entry = find_help_entry(s);

    if(!entry)
    {
      show_error_dialog("No help entry available for symbol");
      return TRUE;
    }

    gtk_text_buffer_set_text(help_buffer, "", -1);

    GtkTextIter start_iter, curr_iter;

    gint index;

    unsigned int i;

    if(entry->type == FUNCTION)
    {
      gtk_text_buffer_insert_at_cursor(help_buffer, "Function ", -1);
      index = 9;
    }
    else if(entry->type == MACRO)
    {
      gtk_text_buffer_insert_at_cursor(help_buffer, "Macro ", -1);
      index = 6;
    }
    else
    {
      gtk_text_buffer_insert_at_cursor(help_buffer, "Special Operator ", -1);
      index = 17;
    }

    gtk_text_buffer_get_iter_at_line(help_buffer, &start_iter, 0);
    gtk_text_buffer_get_iter_at_mark(help_buffer, &curr_iter, gtk_text_buffer_get_insert(help_buffer));
    gtk_text_buffer_apply_tag_by_name(help_buffer, "bold", &start_iter, &curr_iter);

    gtk_text_buffer_insert_at_cursor(help_buffer, entry->name, -1);

    gtk_text_buffer_get_iter_at_line_offset(help_buffer, &start_iter, 0, index);
    gtk_text_buffer_get_end_iter(help_buffer, &curr_iter);

    gtk_text_buffer_apply_tag_by_name(help_buffer, "bold_blue_foreground", &start_iter, &curr_iter);

    gtk_text_buffer_insert_at_cursor(help_buffer, "\n\n", -1);

    gtk_text_buffer_insert_at_cursor(help_buffer, "Syntax: ", -1);
    gtk_text_buffer_get_iter_at_line(help_buffer, &start_iter, 2);
    gtk_text_buffer_get_iter_at_mark(help_buffer, &curr_iter, gtk_text_buffer_get_insert(help_buffer));
    gtk_text_buffer_apply_tag_by_name(help_buffer, "bold", &start_iter, &curr_iter);

    gtk_text_buffer_insert_at_cursor(help_buffer, entry->syntax, -1);
    gtk_text_buffer_insert_at_cursor(help_buffer, "\n\n", -1);

    gtk_text_buffer_insert_at_cursor(help_buffer, "Arguments and Values: ", -1);
    gtk_text_buffer_get_iter_at_line(help_buffer, &start_iter, 4);
    gtk_text_buffer_get_iter_at_mark(help_buffer, &curr_iter, gtk_text_buffer_get_insert(help_buffer));
    gtk_text_buffer_apply_tag_by_name(help_buffer, "bold", &start_iter, &curr_iter);

    gtk_text_buffer_insert_at_cursor(help_buffer, entry->args, -1);
    gtk_text_buffer_insert_at_cursor(help_buffer, "\n\n", -1);

    gtk_text_buffer_insert_at_cursor(help_buffer, "Description: ", -1);
    gtk_text_buffer_get_iter_at_line(help_buffer, &start_iter, 6);
    gtk_text_buffer_get_iter_at_mark(help_buffer, &curr_iter, gtk_text_buffer_get_insert(help_buffer));
    gtk_text_buffer_apply_tag_by_name(help_buffer, "bold", &start_iter, &curr_iter);

    gtk_text_buffer_insert_at_cursor(help_buffer, entry->desc, -1);
    gtk_text_buffer_insert_at_cursor(help_buffer, "\n\n", -1);

    i = 0;
    while(entry->desc[i] != ' ')
      i++;

    gtk_text_buffer_get_iter_at_line_offset(help_buffer, &start_iter, 6, 13);
    gtk_text_buffer_get_iter_at_line_offset(help_buffer, &curr_iter, 6, 13+i);

    gtk_text_buffer_apply_tag_by_name(help_buffer, "bold_blue_foreground", &start_iter, &curr_iter);
    

    gtk_text_buffer_insert_at_cursor(help_buffer, "Exceptions: ", -1);
    gtk_text_buffer_get_iter_at_line(help_buffer, &start_iter, 8);
    gtk_text_buffer_get_iter_at_mark(help_buffer, &curr_iter, gtk_text_buffer_get_insert(help_buffer));
    gtk_text_buffer_apply_tag_by_name(help_buffer, "bold", &start_iter, &curr_iter);

    gtk_text_buffer_insert_at_cursor(help_buffer, entry->exceptions, -1);
    gtk_text_buffer_insert_at_cursor(help_buffer, "\n\n", -1);

    gtk_text_buffer_insert_at_cursor(help_buffer, "Examples:\n", -1);
    gtk_text_buffer_get_iter_at_line(help_buffer, &start_iter, 10);
    gtk_text_buffer_get_iter_at_mark(help_buffer, &curr_iter, gtk_text_buffer_get_insert(help_buffer));
    gtk_text_buffer_apply_tag_by_name(help_buffer, "bold", &start_iter, &curr_iter);

    char text[MAX_STRING_LENGTH];
    int ii, jj, len;

    for(i=0; i<entry->examples_count; i++)
    {
      ii=0; jj=0; len = strlen(entry->examples[i]);
      memset(text, '\0', MAX_STRING_LENGTH);

      while(ii<len)
      {
        if(entry->examples[i][ii] != '\\')
        {
          text[jj] = entry->examples[i][ii];
          jj++;
        }
        else if(ii < len-1 && entry->examples[i][ii+1] == '\\')
        {
          text[jj] = entry->examples[i][ii];
          jj++;
          ii++;
        }

        ii++;
      }

      gtk_text_buffer_insert_at_cursor(help_buffer, text, -1);
      gtk_text_buffer_insert_at_cursor(help_buffer, "\n", -1);
    }

    gtk_text_buffer_insert_at_cursor(help_buffer, "\n", -1);

    gtk_text_buffer_insert_at_cursor(help_buffer, "See Also: ", -1);
    gtk_text_buffer_get_iter_at_line(help_buffer, &start_iter, 12 + entry->examples_count);
    gtk_text_buffer_get_iter_at_mark(help_buffer, &curr_iter, gtk_text_buffer_get_insert(help_buffer));
    gtk_text_buffer_apply_tag_by_name(help_buffer, "bold", &start_iter, &curr_iter);

    BOOLEAN first_time = true;

    gint line_no = 12 + entry->examples_count;

    index = 10;

    for(i=0; i<entry->see_also_count; i++)
    {
      if(!first_time)
      {
        gtk_text_buffer_insert_at_cursor(help_buffer, ", ", -1);
        index += 2;
      }

      gtk_text_buffer_insert_at_cursor(help_buffer, entry->see_also[i], -1);

      index += strlen(entry->see_also[i]);

      gtk_text_buffer_get_iter_at_line_offset(help_buffer, &start_iter, line_no, index - strlen(entry->see_also[i]));
      gtk_text_buffer_get_end_iter(help_buffer, &curr_iter);

      gtk_text_buffer_apply_tag_by_name(help_buffer, "bold_blue_foreground", &start_iter, &curr_iter);

      first_time = false;
    }

    gtk_widget_show_all((GtkWidget *)help_window);

    //free(s);
    return TRUE;
  }
  else if(widget == (GtkWidget *)help_window && event->keyval == GDK_KEY_Escape)
  {
    gtk_widget_set_visible((GtkWidget *)help_window, FALSE);
  }
  /*
  else if((widget == (GtkWidget *)workspace_window || widget == (GtkWidget *)system_browser_window) && 
	  !(event->state & GDK_CONTROL_MASK) && event->keyval == GDK_KEY_Return)
  {
    GtkTextIter start_iter, end_iter;

    GtkTextBuffer *buffer = widget == (GtkWidget *)workspace_window ? workspace_buffer : system_browser_buffer;

    gtk_text_buffer_get_start_iter(buffer, &start_iter);
    gtk_text_buffer_get_end_iter(buffer, &end_iter);

    GtkTextIter iter, line_start;
    gtk_text_buffer_get_iter_at_mark(buffer, &iter, gtk_text_buffer_get_insert(buffer));
    gint line_number = gtk_text_iter_get_line(&iter);

    gtk_text_buffer_get_iter_at_line(buffer, &line_start, line_number);

    gchar *text = gtk_text_buffer_get_text(buffer, &line_start, &iter, FALSE);

    char ret[30];
    memset(ret, '\0', 31);

    char trimmed_text[100];
    memset(trimmed_text, '\0', 100);

    trim_whitespace(trimmed_text, 100, text);

    if(strlen(trimmed_text) == 0)
    {
      gtk_text_buffer_insert_at_cursor(buffer, "\n", -1);
      return TRUE;
    }

    get_form(text, ret);

    int carried_over_indents = get_carried_over_indents(text);

    gtk_text_buffer_insert_at_cursor(buffer, "\n", -1);

    int i;
    for(i=0; i<carried_over_indents; i++)
      gtk_text_buffer_insert_at_cursor(buffer, " ", -1);

    if(ret)
    {
      GtkTextIter it;
      gtk_text_buffer_get_iter_at_line(buffer, &it, line_number);

      int indents = get_indents_for_form(ret);

      if(indents > 0)
      {
	int i;
	for(i=0; i<indents; i++)
	  gtk_text_buffer_insert_at_cursor(buffer, " ", -1);
      }
      else
      {
	int default_indents = get_position_of_first_arg(ret);
	int i;
	for(i=0; i<default_indents; i++)
	  gtk_text_buffer_insert_at_cursor(buffer, " ", -1);
      }
    }
    return TRUE;
  }
  */
  else if(widget == (GtkWidget *)workspace_window && (event->state & GDK_CONTROL_MASK) && event->keyval == GDK_KEY_w)
    close_application_window((GtkWidget **)&workspace_window);
  else if(widget == (GtkWidget *)profiler_window && (event->state & GDK_CONTROL_MASK) && event->keyval == GDK_KEY_w)
    close_application_window((GtkWidget **)&profiler_window);
  else if(widget == (GtkWidget *)debugger_window && (event->state & GDK_CONTROL_MASK) && event->keyval == GDK_KEY_w)
    close_application_window((GtkWidget **)&debugger_window);
  else if(widget == (GtkWidget *)system_browser_window && (event->state & GDK_CONTROL_MASK) && event->keyval == GDK_KEY_k)
    create_new_package();
  else if(widget == (GtkWidget *)system_browser_window && (event->state & GDK_CONTROL_MASK) && event->keyval == GDK_KEY_n)
  {
    action_triggering_window = system_browser_window;
    create_new_symbol();
  }
  else if(widget == (GtkWidget *)system_browser_window && (event->state & GDK_CONTROL_MASK) && event->keyval == GDK_KEY_s)
  {
    action_triggering_window = system_browser_window;
    system_browser_accept();
  }
  else if(widget == (GtkWidget *)callers_window && (event->state & GDK_CONTROL_MASK) && event->keyval == GDK_KEY_s)
  {
    action_triggering_window = callers_window;
    callers_window_accept();
  }
  else if(widget == (GtkWidget *)system_browser_window && (event->state & GDK_CONTROL_MASK) && event->keyval == GDK_KEY_x)
    delete_package_or_symbol();
  else if(widget == (GtkWidget *)system_browser_window && event->keyval == GDK_KEY_F2)
    rename_sym();
  else if(widget == (GtkWidget *)system_browser_window && event->keyval == GDK_KEY_F5)
    refresh_system_browser();
  else if(widget == (GtkWidget *)system_browser_window && (event->state & GDK_CONTROL_MASK) && event->keyval == GDK_KEY_w)
    close_application_window((GtkWidget **)&system_browser_window);
  else if(widget == (GtkWidget *)transcript_window && (event->state & GDK_CONTROL_MASK) && event->keyval == GDK_KEY_l)
    load_image();
  else if(widget == (GtkWidget *)transcript_window && (event->state & GDK_CONTROL_MASK) && event->keyval == GDK_KEY_s)
    save_image();
  else if(event->keyval == GDK_KEY_F2)
    gtk_window_present(transcript_window);
  else if(/*widget == (GtkWidget *)transcript_window && */event->keyval == GDK_KEY_F7)
    show_workspace_window();
  else if(/*widget == (GtkWidget *)transcript_window && */event->keyval == GDK_KEY_F9)
    show_system_browser_window();
  else if(widget == (GtkWidget *)transcript_window && (event->state & GDK_CONTROL_MASK) && event->keyval == GDK_KEY_w)
    quit_application();
  else if(widget == (GtkWidget *)debugger_window && event->keyval == GDK_KEY_F5)
    resume();

  return FALSE;
}

void load_source_file(GtkWidget *widget,
                      gpointer data)
{
  load_source();
}

void show_workspace_window()
{
  if(workspace_window == NULL)
    create_workspace_window(DEFAULT_WORKSPACE_POSX,
                            DEFAULT_WORKSPACE_POSY,
                            DEFAULT_WORKSPACE_WIDTH,
                            DEFAULT_WORKSPACE_HEIGHT,
                            default_workspace_text);
  else
  {
    gtk_window_present(workspace_window);
  }
}

void show_file_browser_window()
{
  if(file_browser_window == NULL)
    create_file_browser_window(DEFAULT_WORKSPACE_POSX,
                               DEFAULT_WORKSPACE_POSY,
                               DEFAULT_WORKSPACE_WIDTH,
                               DEFAULT_WORKSPACE_HEIGHT);
  else
  {
    gtk_window_present(file_browser_window);
  }
}

void show_workspace_win(GtkWidget *widget,
                           gpointer  data)
{
  show_workspace_window();
}

void show_file_browser_win(GtkWidget *widget,
                           gpointer  data)
{
  show_file_browser_window();
}

void load_image()
{
  GtkWidget *dialog;

  dialog = gtk_file_chooser_dialog_new ("Load pLisp Image",
                                        (GtkWindow *)transcript_window,
                                        GTK_FILE_CHOOSER_ACTION_OPEN,
                                        "Cancel", GTK_RESPONSE_CANCEL,
                                        "Open", GTK_RESPONSE_ACCEPT,
                                        NULL);

  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
  {

    loaded_image_file_name = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));

    load_from_image(loaded_image_file_name);

    print_to_transcript("Image ");
    print_to_transcript(loaded_image_file_name);
    print_to_transcript(" loaded successfully\n");

    update_transcript_title();
  }

  gtk_widget_destroy (dialog);
}

void load_image_file(GtkWidget *widget,
                     gpointer data)
{
  load_image();
}

void save_image_file(GtkWidget *widget,
                     gpointer data)
{
  save_image();
}

void show_system_browser_window()
{
  if(system_browser_window == NULL)
    create_system_browser_window(DEFAULT_SYSTEM_BROWSER_POSX,
                                 DEFAULT_SYSTEM_BROWSER_POSY,
                                 DEFAULT_SYSTEM_BROWSER_WIDTH,
                                 DEFAULT_SYSTEM_BROWSER_HEIGHT);
  else
    gtk_window_present(system_browser_window);
}

void show_system_browser_win(GtkWidget *widget,
                                gpointer  data)
{
  show_system_browser_window();
}

void fetch_package_symbols()
{
  GtkListStore *store = GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(packages_list)));
  GtkTreeModel *model = gtk_tree_view_get_model (GTK_TREE_VIEW (packages_list));
  GtkTreeIter  iter;

  GtkTreeSelection *selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(packages_list));

  if(!selection)
    return;

  if(gtk_tree_selection_get_selected(selection, &model, &iter))
  {
    gint id;

    gtk_tree_model_get(model, &iter,
                       1, &id,
                       -1);

    print_context_pkg_index = id;

    remove_all_from_list(symbols_list);

    GtkListStore *store2;
    GtkTreeIter  iter2;

    store2 = GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(symbols_list)));

    int i;

    for(i=0; i<nof_global_vars; i++)
    {
      if(top_level_symbols[i].delete_flag || IS_NATIVE_FN_OBJECT(top_level_symbols[i].val))
        continue;

      OBJECT_PTR sym = top_level_symbols[i].sym;

      //if(((int)sym >> (SYMBOL_BITS + OBJECT_SHIFT)) == id)
      if(extract_package_index(sym) == id)
      {
        gtk_list_store_append(store2, &iter2);
        gtk_list_store_set(store2, &iter2, 0, get_symbol_name(sym), -1);  
        gtk_list_store_set(store2, &iter2, 1, sym, -1);
      }
    }
  }

  gtk_text_buffer_set_text(system_browser_buffer, "", -1);
  gtk_statusbar_remove_all(system_browser_statusbar, 0);
}

void fetch_package_members(GtkWidget *list, gpointer selection)
{
  fetch_package_symbols();
}

void fetch_symbol_value(GtkWidget *lst, gpointer data)
{
  GtkListStore *store = GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(lst)));
  GtkTreeModel *model = gtk_tree_view_get_model (GTK_TREE_VIEW (lst));
  GtkTreeIter  iter;

  GtkTreeSelection *selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(lst));

  if(!selection)
    return;

  if(gtk_tree_selection_get_selected(selection, &model, &iter))
  {
    gchar *symbol_name;
#if __x86_64__
    gint64 ptr;
#else
#ifdef __APPLE__
    gint64 ptr;
#else
    gint ptr;
#endif
#endif
    
    gtk_tree_model_get(model, &iter,
                       0, &symbol_name,
                       -1);

    gtk_tree_model_get(model, &iter,
                       1, &ptr,
                       -1);

    char buf[MAX_STRING_LENGTH];
    memset(buf, '\0', MAX_STRING_LENGTH);

    OBJECT_PTR out;
    int retval = get_top_level_sym_value((OBJECT_PTR)ptr, &out);
    assert(retval == 0);
    OBJECT_PTR obj = car(out);

    gtk_text_buffer_set_text(system_browser_buffer, buf, -1);

    gtk_text_view_set_editable(system_browser_textview, FALSE);

    if(IS_CLOSURE_OBJECT(obj) || IS_FUNCTION2_OBJECT(obj))
    {
      memset(buf, '\0', MAX_STRING_LENGTH);
      /* print_object_to_string(cons(DEFINE, */
      /*                             cons((OBJECT_PTR)ptr, */
      /*                                  cons(cons(LAMBDA, */
      /*                                            cons(get_params_object(obj), */
      /*                                                 get_source_object(obj))), */
      /*                                       NIL))), buf, 0); */
      /* print_object_to_string(list(4, DEFUN, (OBJECT_PTR)ptr, get_params_object(obj), car(get_source_object(obj))), buf, 0); */

      char *doc_str = get_doc_str((OBJECT_PTR)ptr);

      OBJECT_PTR temp = cons(DEFUN, 
                             cons((OBJECT_PTR)ptr,
                                  cons(get_params_object(obj),
                                       doc_str ? cdr(get_source_object(obj)) : get_source_object(obj))));

      print_object_to_string(temp, buf, 0);

      if(doc_str)
        insert_doc_string(buf, get_first_occur(buf, '\n')+1, doc_str);
      
      gtk_text_buffer_insert_at_cursor(system_browser_buffer, (char *)conv_to_lower_case_preserve_strings(buf), -1);

      gtk_text_view_set_editable(system_browser_textview, TRUE);
    }
    else if(IS_MACRO_OBJECT(obj) || IS_MACRO2_OBJECT(obj))
    {
      memset(buf, '\0', MAX_STRING_LENGTH);
      /* print_object_to_string(cons(DEFINE, */
      /*                             cons((OBJECT_PTR)ptr, */
      /*                                  cons(cons(MACRO, */
      /*                                            cons(get_params_object(obj), */
      /*                                                 get_source_object(obj))), */
      /*                                       NIL))), buf, 0); */
      /* print_object_to_string(list(4, DEFMACRO, (OBJECT_PTR)ptr, get_params_object(obj), car(get_source_object(obj))), buf, 0); */

      char *doc_str = get_doc_str((OBJECT_PTR)ptr);
      
      OBJECT_PTR temp = cons(DEFMACRO, 
                             cons((OBJECT_PTR)ptr,
                                  cons(get_params_object(obj),
                                       doc_str ? cdr(get_source_object(obj)) : get_source_object(obj))));

      print_object_to_string(temp, buf, 0);

      if(doc_str)
        insert_doc_string(buf, get_first_occur(buf, '\n')+1, doc_str);
      
      gtk_text_buffer_insert_at_cursor(system_browser_buffer, (char *)conv_to_lower_case_preserve_strings(buf), -1);
      gtk_text_view_set_editable(system_browser_textview, TRUE);
    }
    else if(IS_CONTINUATION_OBJECT(obj))
    {
      memset(buf, '\0', MAX_STRING_LENGTH);
      print_object_to_string(obj, buf, 0);
      gtk_text_buffer_insert_at_cursor(system_browser_buffer, (char *)conv_to_lower_case_preserve_strings(buf), -1);
    }
    else
    {
      memset(buf, '\0', MAX_STRING_LENGTH);
      /* print_object_to_string(cons(DEFINE, */
      /*                             cons((OBJECT_PTR)ptr, cons(obj, NIL))), buf, 0); */
      print_object_to_string(obj, buf, 0);
      gtk_text_buffer_insert_at_cursor(system_browser_buffer, (char *)conv_to_lower_case_preserve_strings(buf), -1);
      gtk_text_view_set_editable(system_browser_textview, FALSE);
    }

    gtk_text_buffer_insert_at_cursor(system_browser_buffer, "\n", -1);
  }

  gtk_statusbar_push(system_browser_statusbar, 0, "");
}

void save_image()
{
  char exp[MAX_STRING_LENGTH];
  memset(exp, '\0', MAX_STRING_LENGTH);

  if(loaded_image_file_name == NULL)
  {
    GtkWidget *dialog;

    dialog = gtk_file_chooser_dialog_new ("Save pLisp Image",
                                          (GtkWindow *)transcript_window,
                                          GTK_FILE_CHOOSER_ACTION_SAVE,
                                          "Cancel", GTK_RESPONSE_CANCEL,
                                          "Open", GTK_RESPONSE_ACCEPT,
                                          NULL);

    if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
    {
      loaded_image_file_name = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
    }
    else
      return;

    gtk_widget_destroy (dialog);

    if(file_exists(loaded_image_file_name))
    {
      GtkWidget *dialog1 = gtk_message_dialog_new ((GtkWindow *)transcript_window,
                                                   GTK_DIALOG_DESTROY_WITH_PARENT,
                                                   GTK_MESSAGE_QUESTION,
                                                   GTK_BUTTONS_YES_NO,
                                                   "File exists, do you want to overwite it?");

      gtk_widget_grab_focus(gtk_dialog_get_widget_for_response(GTK_DIALOG(dialog1), GTK_RESPONSE_NO));

      if(gtk_dialog_run(GTK_DIALOG (dialog1)) == GTK_RESPONSE_NO)
      {
        gtk_widget_destroy((GtkWidget *)dialog1);
        g_free(loaded_image_file_name);
        loaded_image_file_name = NULL;
        return;
      }
      else
        gtk_widget_destroy((GtkWidget *)dialog1);
    }
  }

  sprintf(exp,"(create-image \"%s\")", loaded_image_file_name);
 
  GdkWindow *win = gtk_widget_get_window((GtkWidget *)transcript_window);

  GdkCursor *cursor = gdk_cursor_new(GDK_WATCH);

  gdk_window_set_cursor(win, cursor);

  g_object_unref(cursor);
  while( gtk_events_pending() )
    gtk_main_iteration();

  if(!call_repl(exp))
  {
     update_workspace_title();
     print_to_transcript("Image saved successfully\n");

     update_transcript_title();
  }

  gdk_window_set_cursor(win, NULL);

}

void fetch_variables(GtkWidget *list, gpointer data)
{
  GtkListStore *store = GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(list)));
  GtkTreeModel *model = gtk_tree_view_get_model (GTK_TREE_VIEW (list));
  GtkTreeIter  iter;

  if(gtk_tree_selection_get_selected(gtk_tree_view_get_selection(GTK_TREE_VIEW(list)), &model, &iter))
  {
    gint env_list_ptr;

    gtk_tree_model_get(model, &iter,
                       2, &env_list_ptr,
                       -1);

    remove_all_from_list(variables_list);

    GtkListStore *store2;
    GtkTreeIter  iter2;

    store2 = GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(variables_list)));

    OBJECT_PTR rest1 = (OBJECT_PTR)env_list_ptr;

    while(rest1 != NIL)
    {
      OBJECT_PTR rest2 = car(rest1);

      while(rest2 != NIL)
      {
        OBJECT_PTR binding_cons = car(rest2);

        char buf[MAX_STRING_LENGTH];

        gtk_list_store_append(store2, &iter2);

        memset(buf, '\0', MAX_STRING_LENGTH);
        print_object_to_string(car(binding_cons), buf, 0);
        gtk_list_store_set(store2, &iter2, 0, buf, -1);  

        memset(buf, '\0', MAX_STRING_LENGTH);
        print_object_to_string(cdr(binding_cons), buf, 0);
        gtk_list_store_set(store2, &iter2, 1, buf, -1);

        rest2 = cdr(rest2);
      }

      rest1 = cdr(rest1);
    }

  }

}

void set_focus_to_last_row(GtkTreeView *list)
{
  GtkTreeModel *model = gtk_tree_view_get_model (GTK_TREE_VIEW (list));
  gint rows = gtk_tree_model_iter_n_children(GTK_TREE_MODEL(model), NULL);

  if(rows == 0)
    return;

  GtkTreePath *path = gtk_tree_path_new_from_indices(rows - 1, -1);

  gtk_tree_view_set_cursor(list, path, NULL, false);

}

BOOLEAN no_unmatched_left_parens(char *str)
{
  int imbalance = 0;

  int len = strlen(str);

  int i;

  char c;

  BOOLEAN in_string_literal = false;
  BOOLEAN in_single_line_comment = false;
  BOOLEAN in_multi_line_comment = false;

  for(i=0; i<len; i++)
  {
    c = str[i];

    if(c == '"')
    {
      if(in_string_literal)
        in_string_literal = false;
      else if(!in_single_line_comment && !in_multi_line_comment)
        in_string_literal = true;
    }
    else if(c == '\n')
    {
      if(!in_string_literal & !in_multi_line_comment)
        in_single_line_comment = false;
    }
    else if(c == ';')
    {
      if(!in_string_literal && !in_multi_line_comment)
        in_single_line_comment = true;
    }
    else if(c == '#')
    {
      if((i <= len-2)            && 
         str[i+1] == '|'         &&
         !in_string_literal      &&
         !in_single_line_comment &&
         !in_multi_line_comment)
        in_multi_line_comment = true;
      else if(i > 0                   &&
              str[i-1] == '|'         &&
              !in_string_literal      &&
              !in_single_line_comment &&
              in_multi_line_comment)
        in_multi_line_comment = false;
    }
    else if(c == '(' && !in_string_literal && !in_single_line_comment && !in_multi_line_comment)
      imbalance ++;
    else if(c == ')' && !in_string_literal && !in_single_line_comment && !in_multi_line_comment)
      imbalance--;
  }

  if(imbalance > 0)
    return false;
  else
    return true;
}

BOOLEAN no_unmatched_parens(char *str)
{
  int imbalance = 0;

  int len = strlen(str);

  int i;

  char c;

  BOOLEAN in_string_literal = false;
  BOOLEAN in_single_line_comment = false;
  BOOLEAN in_multi_line_comment = false;

  for(i=0; i<len; i++)
  {
    c = str[i];

    if(c == '"')
    {
      if(in_string_literal)
        in_string_literal = false;
      else if(!in_single_line_comment && !in_multi_line_comment)
        in_string_literal = true;
    }
    else if(c == '\n')
    {
      if(!in_string_literal & !in_multi_line_comment)
        in_single_line_comment = false;
    }
    else if(c == ';')
    {
      if(!in_string_literal && !in_multi_line_comment)
        in_single_line_comment = true;
    }
    else if(c == '#')
    {
      if((i <= len-2)            && 
         str[i+1] == '|'         &&
         !in_string_literal      &&
         !in_single_line_comment &&
         !in_multi_line_comment)
        in_multi_line_comment = true;
      else if(i > 0                   &&
              str[i-1] == '|'         &&
              !in_string_literal      &&
              !in_single_line_comment &&
              in_multi_line_comment)
        in_multi_line_comment = false;
    }
    else if(c == '(' && !in_string_literal && !in_single_line_comment && !in_multi_line_comment)
      imbalance ++;
    else if(c == ')' && !in_string_literal && !in_single_line_comment && !in_multi_line_comment)
      imbalance--;
  }

  if(imbalance == 0)
    return true;
  else
    return false;
}

void get_prev_iter(GtkTextBuffer *buffer, GtkTextIter *curr_iter, GtkTextIter *prev_iter)
{
  gint col = gtk_text_iter_get_line_offset(curr_iter);
  gint line = gtk_text_iter_get_line(curr_iter);

  if(col != 0)
  {
    gtk_text_buffer_get_iter_at_line_offset(buffer, prev_iter, line, col-1);
  }
  else
  {
    int prev_line_nof_chars;
    GtkTextIter it1, it2;
    gtk_text_buffer_get_iter_at_line(buffer, &it1, line);
    gtk_text_buffer_get_iter_at_line(buffer, &it2, line);

    prev_line_nof_chars = strlen(gtk_text_buffer_get_text(buffer, &it2, &it1, FALSE));

    gtk_text_buffer_get_iter_at_line_offset(buffer, prev_iter, line-1, prev_line_nof_chars);
  }
}

void build_form_for_eval(GtkTextBuffer *buffer)
{
  GtkTextIter curr_iter;
  GtkTextIter start_match, end_match;

  //get the current iter
  gtk_text_buffer_get_iter_at_mark(buffer, &curr_iter, gtk_text_buffer_get_insert(buffer));

  GtkTextIter saved_iter = curr_iter;

  while(1)
  {
    if(gtk_text_iter_backward_search(&curr_iter, 
                                     "(", 
                                     GTK_TEXT_SEARCH_TEXT_ONLY | GTK_TEXT_SEARCH_VISIBLE_ONLY, 
                                     &start_match,
                                     &end_match, 
                                     NULL))
    {

      gchar *str = gtk_text_buffer_get_text(buffer, &start_match, &saved_iter, FALSE);

      if(no_unmatched_parens(str) && in_code(buffer, &start_match))
      {
        /* gtk_text_buffer_apply_tag_by_name(buffer,  */
        /*                                   "cyan_bg",  */
        /*                                   &start_match,  */
        /*                                   &end_match); */

        /* GtkTextIter temp_iter = saved_iter; */
        /* gtk_text_iter_backward_char(&saved_iter); */
        /* gtk_text_buffer_apply_tag_by_name(buffer,  */
        /*                                   "cyan_bg",  */
        /*                                   &saved_iter,  */
        /*                                   &temp_iter); */

        form_for_eval = str;
        
        break;
      }
      else
      {
        int offset = gtk_text_iter_get_offset(&start_match);
        gtk_text_buffer_get_iter_at_offset(buffer,
                                           &curr_iter, 
                                           offset);
      }
    }
    else
    {
      form_for_eval = NULL;
      break;
    }
  }      
}

void resume_from_debugger(GtkWidget *widget,
                          gpointer data)
{
  resume();
}

void abort_debugger(GtkWidget *widget,
                    gpointer data)
{
  close_application_window((GtkWidget **)&debugger_window);
  call_repl("(ABORT)");
}

void clear_transcript(GtkWidget *widget,
		      gpointer data)
{
  gtk_text_buffer_set_text(transcript_buffer, "", -1);
}

void clear_workspace(GtkWidget *widget,
		     gpointer data)
{
  gtk_text_buffer_set_text(workspace_buffer, "", -1);
}

void export_package_gui()
{
  gchar *package_name;

  GtkListStore *store1 = GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(packages_list)));
  GtkTreeModel *model1 = gtk_tree_view_get_model (GTK_TREE_VIEW (packages_list));
  GtkTreeIter  iter1;

  if(gtk_tree_selection_get_selected(gtk_tree_view_get_selection(GTK_TREE_VIEW(packages_list)), &model1, &iter1))
  {
    gtk_tree_model_get(model1, &iter1,
		       0, &package_name,
		       -1);

    GtkWidget *dialog;

    dialog = gtk_file_chooser_dialog_new ("Export package",
					  (GtkWindow *)system_browser_window,
					  GTK_FILE_CHOOSER_ACTION_SAVE,
					  "Cancel", GTK_RESPONSE_CANCEL,
					  "Open", GTK_RESPONSE_ACCEPT,
					  NULL);

    if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
    {
      char *file_name = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));

      gtk_widget_destroy (dialog);

      if(file_exists(file_name))
      {
        GtkWidget *dialog1 = gtk_message_dialog_new ((GtkWindow *)system_browser_window,
                                                     GTK_DIALOG_DESTROY_WITH_PARENT,
                                                     GTK_MESSAGE_QUESTION,
                                                     GTK_BUTTONS_YES_NO,
                                                     "File exists, do you want to overwite it?");

        gtk_widget_grab_focus(gtk_dialog_get_widget_for_response(GTK_DIALOG(dialog1), GTK_RESPONSE_NO));

        if(gtk_dialog_run(GTK_DIALOG (dialog1)) == GTK_RESPONSE_NO)
        {
          gtk_widget_destroy((GtkWidget *)dialog1);
          g_free(file_name);
          return;
        }
        else
        {
          gtk_widget_destroy((GtkWidget *)dialog1);
        }
      }

      char buf[MAX_STRING_LENGTH];
      memset(buf, '\0', MAX_STRING_LENGTH);

      sprintf(buf, "(export-package \"%s\" \"%s\")", package_name, file_name);

      if(!call_repl(buf))
      {
	gtk_statusbar_push(system_browser_statusbar, 0, "Package exported successfully");
      }

      g_free(file_name);
    }
  }
}

void exp_pkg(GtkWidget *widget,
	     gpointer data)
{
  export_package_gui();
}

void callers(GtkWidget *widget,
	     gpointer data)
{
  GtkListStore *store = GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(symbols_list)));
  GtkTreeModel *model = gtk_tree_view_get_model(GTK_TREE_VIEW(symbols_list));
  GtkTreeIter  iter;

  GtkTreeSelection *selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(symbols_list));

  if(!selection)
    return;

  if(gtk_tree_selection_get_selected(selection, &model, &iter))
  {
    gchar *symbol_name;
#if __x86_64__
    gint64 ptr;
#else
#ifdef __APPLE__
    gint64 ptr;
#else
    gint ptr;
#endif
#endif

    gtk_tree_model_get(model, &iter,
                       0, &symbol_name,
                       -1);

    gtk_tree_model_get(model, &iter,
                       1, &ptr,
                       -1);

    char buf[MAX_STRING_LENGTH];
    memset(buf, '\0', MAX_STRING_LENGTH);

    OBJECT_PTR out;
    int retval = get_top_level_sym_value((OBJECT_PTR)ptr, &out);
    assert(retval == 0);
    OBJECT_PTR obj = car(out);

    callers_sym = (OBJECT_PTR)ptr;

    create_callers_window(DEFAULT_SYSTEM_BROWSER_POSX,
                          DEFAULT_SYSTEM_BROWSER_POSY,
                          DEFAULT_SYSTEM_BROWSER_WIDTH,
                          DEFAULT_SYSTEM_BROWSER_HEIGHT);
  }
}

BOOLEAN in_string_literal(GtkTextBuffer *buffer, GtkTextIter *iter)
{
  return in_code_or_string_literal_or_comment(buffer, iter, IN_STRING_LITERAL);
}

BOOLEAN in_single_line_comment(GtkTextBuffer *buffer, GtkTextIter *iter)
{
  return in_code_or_string_literal_or_comment(buffer, iter, IN_SINGLE_LINE_COMMENT);
}

BOOLEAN in_multi_line_comment(GtkTextBuffer *buffer, GtkTextIter *iter)
{
  return in_code_or_string_literal_or_comment(buffer, iter, IN_MULTI_LINE_COMMENT);
}

BOOLEAN in_code(GtkTextBuffer *buffer, GtkTextIter *iter)
{
  return in_code_or_string_literal_or_comment(buffer, iter, IN_CODE);
}

BOOLEAN in_code_or_string_literal_or_comment(GtkTextBuffer *buffer, GtkTextIter *iter, cursor_pos_t pos_type)
{
  assert(pos_type == IN_CODE                ||
         pos_type == IN_STRING_LITERAL      ||
         pos_type == IN_SINGLE_LINE_COMMENT ||
         pos_type == IN_MULTI_LINE_COMMENT);

  GtkTextIter start;

  //get the start iterator for the entire buffer
  gtk_text_buffer_get_start_iter(buffer, &start);

  //get the entire text of the buffer
  gchar *text = gtk_text_buffer_get_text(buffer, &start, iter, FALSE);

  unsigned int i=0;
  unsigned int len = strlen(text);

  BOOLEAN in_string_literal = false;
  BOOLEAN in_single_line_comment = false;
  BOOLEAN in_multi_line_comment = false;

  while(i < len)
  {
    if(text[i] == '"')
    {
      if(in_string_literal)
        in_string_literal = false;
      else if(!in_single_line_comment && !in_multi_line_comment)
        in_string_literal = true;
    }
    else if(text[i] == '\n')
    {
      if(!in_string_literal & !in_multi_line_comment)
        in_single_line_comment = false;
    }
    else if(text[i] == ';')
    {
      if(!in_string_literal && !in_multi_line_comment)
        in_single_line_comment = true;
    }
    else if(text[i] == '#')
    {
      if((i <= len-2)            && 
         text[i+1] == '|'        &&
         !in_string_literal      &&
         !in_single_line_comment &&
         !in_multi_line_comment)
        in_multi_line_comment = true;
      else if(i > 0                   &&
              text[i-1] == '|'        &&
              !in_string_literal      &&
              !in_single_line_comment &&
              in_multi_line_comment)
        in_multi_line_comment = false;
    }

    i++;

  } //end of while(i < len)

  if(pos_type == IN_STRING_LITERAL)
    return in_string_literal;
  else if(pos_type == IN_SINGLE_LINE_COMMENT)
    return in_single_line_comment;
  else if(pos_type == IN_MULTI_LINE_COMMENT)
    return in_multi_line_comment;
  else
    return (!in_string_literal && !in_single_line_comment && !in_multi_line_comment);
}

enum {FIRST, LAST};

BOOLEAN is_non_identifier_char(char c)
{
  return c != '-' && c != '+' && c != '*' && c != '/' &&
         c != '<' && c != '>' && c != '=' && c != '\'' &&
         c != ',' && c != '`' && c != '@' &&
         !(c >= 65 && c <= 90) &&
         !(c >= 97 && c <= 122) &&
         !(c >= 48 && c <= 57);
}

//returns the index of the first or last non-identifier
//character (non-alphanumeric, non-hyphen) in the string
//returns -1 if such a character doesn't exist in the string
unsigned int get_loc_of_non_identifier_character(char *s, int pos)
{
  int i;
  int len = strlen(s);

  if(pos == FIRST)
  {
    for(i=0; i<len; i++)
      if(is_non_identifier_char(s[i]))
        return i;
  }
  else if(pos == LAST)
  {
    for(i=len-1; i>=0; i--)
      if(is_non_identifier_char(s[i]))
        return i;
  }
  return -1;
}

//returns the word under the cursor (for autocomplete)
char *get_current_word(GtkTextBuffer *buffer)
{
  GtkTextIter curr_iter, line_start_iter, line_end_iter;
  gint line_number, line_count;

  //get the current iter
  gtk_text_buffer_get_iter_at_mark(buffer, &curr_iter, gtk_text_buffer_get_insert(buffer));  

  //get the current line
  line_number = gtk_text_iter_get_line(&curr_iter);

  //get the total number of lines in the buffer
  line_count = gtk_text_buffer_get_line_count(buffer);

  //get the iter at the beginning of the current line
  gtk_text_buffer_get_iter_at_line(buffer, &line_start_iter, line_number);

  //get the iter at the end of the current line
  if(line_number == (line_count-1))
    gtk_text_buffer_get_end_iter(buffer, &line_end_iter);
  else
    gtk_text_buffer_get_iter_at_line(buffer, &line_end_iter, line_number+1);

  gchar *str1, *str2;

  //get the strings before and after the cursor
  str1 = gtk_text_buffer_get_text(buffer, &line_start_iter, &curr_iter, FALSE);
  str2 = gtk_text_buffer_get_text(buffer, &curr_iter, &line_end_iter, FALSE);

  unsigned int idx1 = get_loc_of_non_identifier_character(str1, LAST);
  unsigned int idx2 = get_loc_of_non_identifier_character(str2, FIRST);

  char *left, *right;

  if(idx1 == -1)
    left = strdup(str1);
  else
    left = strndup(str1+idx1+1, strlen(str1)-idx1-1);

  if(idx2 == -1)
    right = strdup(str2);
  else
    right = strndup(str2, idx2);

  char *ret = (char *)GC_MALLOC((strlen(left) + strlen(right) + 1) * sizeof(char));
  assert(ret);

  memset(ret, '\0', strlen(left) + strlen(right) + 1);

  sprintf(ret,"%s%s", left, right);

  //free(left);
  //free(right);

  return ret;
}

void add_auto_complete_words(int package_index)
{
  int i;

  for(i=0; i<nof_global_vars; i++)
  {
    if(top_level_symbols[i].delete_flag || IS_NATIVE_FN_OBJECT(top_level_symbols[i].val))
      continue;

    //if(((int)top_level_symbols[i].sym >> (SYMBOL_BITS + OBJECT_SHIFT)) == package_index)
    if(extract_package_index(top_level_symbols[i].sym) == package_index)
    {
      nof_autocomplete_words++;

      char **temp = GC_REALLOC(autocomplete_words, nof_autocomplete_words * sizeof(char *));
      assert(temp);
      autocomplete_words = temp;

      autocomplete_words[nof_autocomplete_words-1] = convert_to_lower_case(strdup(get_symbol_name(top_level_symbols[i].sym)));
    }
  }
}

void build_autocomplete_words()
{
  int i;

  /* for(i=0; i<nof_autocomplete_words; i++) */
  /*   free(autocomplete_words[i]); */
  /* free(autocomplete_words);  */

  //skipping some special operators if there is no pay-off in autocomplete
  //e.g. operators that are one- or two characters long

  //can make things more efficient by
  //loading keywords and special ops
  //only once

  char *keywords_and_special_ops[] = {"let", "letrec", "if", "set", "lambda", "macro", "error", "call/cc", "define", "nil", "atom",
                                    "concat",  "quote",  "eq",  "list",  "car", "cdr", "print", "symbol-value", "gensym", "setcar", 
                                    "setcdr", "apply", "symbol", "symbol-name", "format", "clone", "unbind", "newline", "not", 
                                    "return-from", "throw" ,"string", "make-array", "array-get", "array-set", "sub-array", "array-length", 
                                    "print-string", "consp", "listp", "integerp", "floatp", "characterp", "symbolp", "stringp",
                                    "arrayp", "closurep", "macrop", "continuationp", "load-foreign-library", "create-package",
                                    "in-package", "export-package", "import-package", "create-image", "save-object", "load-object", "load-file", "profile",
                                    "time", "env", "expand-macro", "eval", "break", "resume", "abort", "inspect-object"};


  nof_autocomplete_words = 68;
  autocomplete_words = (char **)GC_MALLOC(nof_autocomplete_words * sizeof(char *));
  
  assert(autocomplete_words);

  for(i=0; i<nof_autocomplete_words; i++)
    autocomplete_words[i] = strdup(keywords_and_special_ops[i]);

  //top-level symbols from core package
  add_auto_complete_words(CORE_PACKAGE_INDEX);

  if(current_package != CORE_PACKAGE_INDEX)
    add_auto_complete_words(current_package);
}

void do_auto_complete(GtkTextBuffer *buffer)
{
  char *s = get_current_word(buffer);

  if(strlen(s) == 0)
    return;

  int i;
  unsigned int nof_matches = 0;
  unsigned int len = strlen(s);

  char **matches = NULL;

  for(i=0; i<nof_autocomplete_words; i++)
  {
    if(!strncmp(s, autocomplete_words[i], len))
    {
      nof_matches++;

      if(nof_matches == 1)
        matches = (char **)GC_MALLOC(nof_matches * sizeof(char *));
      else
      {
        char **temp = (char **)GC_REALLOC(matches, nof_matches * sizeof(char *));
        assert(temp);
        matches = temp;
      }

      matches[nof_matches-1] = strdup(autocomplete_words[i]);
    }
  }

  unsigned int n;
  char *buf;

  char *s1 = get_common_prefix(nof_matches, matches);

  if(s1)
  {
    n = strlen(s1) - len;

    if(n==0)
      return;

    buf = (char *)GC_MALLOC(n * sizeof(char) + 2);
    memset(buf, '\0', n+2);

    for(i=len; i<len+n; i++)
      buf[i-len] = s1[i];

    /* free(s1); */

    /* for(i=0; i<nof_matches; i++) */
    /*   free(matches[i]); */

    /* if(matches) */
    /*   free(matches); */
  }
  else
    return;

  //GtkTextView *view = (buffer == (GtkTextBuffer *)workspace_source_buffer) ? (GtkTextView *)workspace_source_view : system_browser_textview;

  GtkTextView *view;

  if(buffer == (GtkTextBuffer *)workspace_source_buffer)
    view = (GtkTextView *)workspace_source_view;
  else if(buffer == (GtkTextBuffer *)system_browser_buffer)
    view = system_browser_textview;
  else if(buffer == curr_file_browser_buffer)
    view = curr_file_browser_text_view;
  else
    assert(false);


  //going into overwrite mode
  //to handle the case when autocomplete
  //is attempted when cursor is in the
  //middle of an already fully complete symbol
  gtk_text_view_set_overwrite(view, TRUE);
  gtk_text_buffer_insert_at_cursor(buffer, (char *)buf, -1);
  gtk_text_view_set_overwrite(view, FALSE);

  //to take the cursor to the end of the word
  //(overwrite doesn't move the cursor)
  GtkTextIter curr_iter;
  gtk_text_buffer_get_iter_at_mark(buffer, &curr_iter, gtk_text_buffer_get_insert(buffer));

  for(i=0; i<n; i++)
    gtk_text_iter_forward_char(&curr_iter);

  //free(buf);

  //free(s);
}

void add_to_autocomplete_list(char *word)
{
  nof_autocomplete_words++;

  char **temp = GC_REALLOC(autocomplete_words, nof_autocomplete_words * sizeof(char *));
  assert(temp);
  autocomplete_words = temp;

  autocomplete_words[nof_autocomplete_words-1] = word;
}

char *get_common_prefix(unsigned int count, char **word_list)
{
  if(count == 0)
    return NULL;

  int idx = 0;

  int i;

  BOOLEAN done = false;

  while(!done)
  {
    for(i=0; i<count; i++)
    {
      if(!word_list[i][idx])
      {
        done = true;
        break;
      }
    }

    if(!done)
    {
      for(i=0; i<count-1; i++)
      {
        if(word_list[i][idx] != word_list[i+1][idx])
        {
          done = true;
          break;
        }
      }
      if(!done)
        idx++;
    }
  }

  if(idx)
    return strndup(word_list[0], idx);
  else
    return NULL;

}

void fetch_symbol_value_for_caller(GtkWidget *lst, gpointer data)
{
  GtkListStore *store = GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(lst)));
  GtkTreeModel *model = gtk_tree_view_get_model (GTK_TREE_VIEW (lst));
  GtkTreeIter  iter;

  GtkTreeSelection *selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(lst));

  if(!selection)
    return;

  if(gtk_tree_selection_get_selected(selection, &model, &iter))
  {
    gchar *symbol_name;
#if __x86_64__
    gint64 ptr;
#else
#ifdef __APPLE__
    gint64 ptr;
#else
    gint ptr;
#endif
#endif
    
    gtk_tree_model_get(model, &iter,
                       0, &symbol_name,
                       -1);

    gtk_tree_model_get(model, &iter,
                       1, &ptr,
                       -1);

    char buf[MAX_STRING_LENGTH];
    memset(buf, '\0', MAX_STRING_LENGTH);

    print_context_pkg_index = extract_package_index(ptr);

    OBJECT_PTR out;
    int retval = get_top_level_sym_value((OBJECT_PTR)ptr, &out);
    assert(retval == 0);
    OBJECT_PTR obj = car(out);

    gtk_text_buffer_set_text(callers_source_buffer, buf, -1);

    if(IS_CLOSURE_OBJECT(obj) || IS_FUNCTION2_OBJECT(obj))
    {
      memset(buf, '\0', MAX_STRING_LENGTH);
      OBJECT_PTR temp = cons(DEFUN, 
                             cons((OBJECT_PTR)ptr,
                                  cons(get_params_object(obj),
                                       get_source_object(obj))));

      print_object_to_string(temp, buf, 0);

      gtk_text_buffer_insert_at_cursor(callers_source_buffer, (char *)conv_to_lower_case_preserve_strings(buf), -1);
    }
    else if(IS_MACRO_OBJECT(obj) || IS_MACRO2_OBJECT(obj))
    {
      memset(buf, '\0', MAX_STRING_LENGTH);
      OBJECT_PTR temp = cons(DEFMACRO, 
                             cons((OBJECT_PTR)ptr,
                                  cons(get_params_object(obj),
                                       get_source_object(obj))));

      print_object_to_string(temp, buf, 0);


      gtk_text_buffer_insert_at_cursor(callers_source_buffer, (char *)conv_to_lower_case_preserve_strings(buf), -1);
    }
    else
      printf("Warning: invalid object type for caller window\n");

    gtk_text_buffer_insert_at_cursor(callers_source_buffer, "\n", -1);

    char *text;

    unsigned int package_index = extract_package_index(callers_sym);

    if(package_index != CORE_PACKAGE_INDEX && package_index != print_context_pkg_index)
      text = strdup(get_qualified_symbol_name(callers_sym));
    else
      text = strdup(get_symbol_name(callers_sym));

    highlight_text(callers_source_buffer, convert_to_lower_case(text));
    //free(text);
  }
}

void callers_window_accept()
{
  GtkListStore *store = GTK_LIST_STORE(gtk_tree_view_get_model(callers_symbols_list));
  GtkTreeModel *model = gtk_tree_view_get_model(callers_symbols_list);
  GtkTreeIter  iter;

  GtkTreeSelection *selection = gtk_tree_view_get_selection(callers_symbols_list);

  if(!selection)
    return;

  if(gtk_tree_selection_get_selected(selection, &model, &iter))
  {
    gchar *symbol_name;
#if __x86_64__
    gint64 ptr;
#else
#ifdef __APPLE__
    gint64 ptr;
#else
    gint ptr;
#endif
#endif    

    gtk_tree_model_get(model, &iter,
                       0, &symbol_name,
                       -1);

    gtk_tree_model_get(model, &iter,
                       1, &ptr,
                       -1);

    char buf[MAX_STRING_LENGTH];
    memset(buf, '\0', MAX_STRING_LENGTH);

    OBJECT_PTR out;
    int retval = get_top_level_sym_value((OBJECT_PTR)ptr, &out);
    assert(retval == 0);
    OBJECT_PTR obj = car(out);

    int current_package_backup = current_package;

    char buf1[MAX_STRING_LENGTH];
    memset(buf1, '\0', MAX_STRING_LENGTH);

    char *temp = strdup(symbol_name);
    char *res = strtok(temp, ":");

    sprintf(buf1, "(in-package \"%s\")", res);

    //free(temp);    

    if(call_repl(buf1))
    {
      show_error_dialog("Unable to set package\n");
      return;
    }

    GtkTextIter start, end;

    gtk_text_buffer_get_start_iter(callers_source_buffer, &start);
    gtk_text_buffer_get_end_iter(callers_source_buffer, &end);

    if(call_repl(gtk_text_buffer_get_text(callers_source_buffer, &start, &end, FALSE)))
    {
      show_error_dialog("Evaluation failed\n");
      current_package = current_package_backup;
      return;
    }

    gtk_statusbar_push(callers_statusbar, 0, "Evaluation successful");
    current_package = current_package_backup;
  } 
}
