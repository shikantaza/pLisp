#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>

#include "../plisp.h"

#include "../util.h"

#include "../hashtable.h"

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
                                "; (indicated by the highlighted matching parens).\n";

extern OBJECT_PTR CAAR(OBJECT_PTR);

extern GtkTextBuffer *transcript_buffer;
extern GtkTextBuffer *workspace_buffer;

extern GtkWindow *transcript_window;
extern GtkWindow *workspace_window;
extern GtkWindow *system_browser_window;
extern GtkWindow *debugger_window;
extern GtkWindow *profiler_window;

extern GtkTreeView *packages_list;
extern GtkTreeView *symbols_list;

extern GtkTextBuffer *system_browser_buffer;
extern GtkTextView *system_browser_textview;

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

extern BOOLEAN debug_mode;

extern OBJECT_PTR build_list(int, ...);

extern void refresh_system_browser();

extern BOOLEAN system_changed;

extern hashtable_t *profiling_tab;

void evaluate();
void close_application_window(GtkWidget **);
void show_workspace_window();
void load_image();
void save_image();
void set_focus_to_last_row(GtkTreeView *);

void show_system_browser_window();

extern BOOLEAN in_error;

extern OBJECT_PTR DEFUN;
extern OBJECT_PTR DEFMACRO;

char *form_for_eval;

extern int call_repl(char *);

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
  else if(!strcmp(up, "LET1"))
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
  if(widget == (GtkWidget *)workspace_window)
    close_application_window((GtkWidget **)&workspace_window);
  else if(widget == (GtkWidget *)system_browser_window)
    close_application_window((GtkWidget **)&system_browser_window);
  else if(widget == (GtkWidget *)debugger_window)
  {
    close_application_window((GtkWidget **)&debugger_window);
    //if(!in_error)
      call_repl("(ABORT)");
  }
  else if(widget == (GtkWidget *)profiler_window)
  {
    close_application_window((GtkWidget **)&profiler_window);
    hashtable_delete(profiling_tab);
    profiling_tab = NULL;
  }

  return FALSE;
}

void quit_application()
{
  GtkWidget *dialog = gtk_message_dialog_new ((GtkWindow *)transcript_window,
                                              GTK_DIALOG_DESTROY_WITH_PARENT,
                                              GTK_MESSAGE_QUESTION,
                                              GTK_BUTTONS_YES_NO,
                                              "Do you really want to quit?");

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

/* Another callback */
void quit(GtkWidget *widget,
          gpointer   data )
{
  quit_application();
}

void create_new_symbol()
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

  gtk_window_set_position(GTK_WINDOW(dialog), GTK_WIN_POS_CENTER);

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

  while(result == GTK_RESPONSE_ACCEPT && strlen(buf) == 0)
  {
    result = get_new_package_name(buf);
    if(strlen(buf) == 0 && result == GTK_RESPONSE_ACCEPT)
      show_error_dialog("Please enter a package name\n");
  }

  if(result == GTK_RESPONSE_ACCEPT)
  {
    char buf1[MAX_STRING_LENGTH];
    memset(buf1, '\0', MAX_STRING_LENGTH);

    sprintf(buf1, "(create-package \"%s\")", buf);

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

      //the newly added symbol will be the last row
      set_focus_to_last_row(symbols_list);

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
      }
    }
  }
  gtk_statusbar_push(system_browser_statusbar, 0, "Evaluation successful");
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
}

void close_window(GtkWidget *widget,
                  gpointer data)
{
  if((GtkWidget *)data == (GtkWidget *)workspace_window)
    close_application_window((GtkWidget **)&workspace_window);
  else if((GtkWidget *)data == (GtkWidget *)system_browser_window)
    close_application_window((GtkWidget **)&system_browser_window);
}

void evaluate()
{
  GtkTextBuffer *buf;
  GtkTextIter start_sel, end_sel;
  gboolean selected;

  buf = workspace_buffer;
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

gboolean handle_key_press_events(GtkWidget *widget, GdkEventKey *event, gpointer user_data)
{
  if(widget == (GtkWidget *)workspace_window && (event->state & GDK_CONTROL_MASK) && event->keyval == GDK_KEY_l)
    load_source();
  //else if(widget == (GtkWidget *)workspace_window && event->keyval == GDK_F5)
  else if(widget == (GtkWidget *)workspace_window && (event->state & GDK_CONTROL_MASK) && event->keyval == GDK_KEY_Return)
  {
    action_triggering_window = workspace_window;
    evaluate();
    GtkTextIter start_sel, end_sel;
    if(!gtk_text_buffer_get_selection_bounds(workspace_buffer, &start_sel, &end_sel))
      gtk_text_buffer_insert_at_cursor(workspace_buffer, "\n", -1);
    return TRUE;
  }
  else if(widget == (GtkWidget *)workspace_window && (event->state & GDK_CONTROL_MASK) && event->keyval == GDK_KEY_w)
    close_application_window((GtkWidget **)&workspace_window);
  else if(widget == (GtkWidget *)profiler_window && (event->state & GDK_CONTROL_MASK) && event->keyval == GDK_KEY_w)
    close_application_window((GtkWidget **)&profiler_window);
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
  else if(widget == (GtkWidget *)system_browser_window && (event->state & GDK_CONTROL_MASK) && event->keyval == GDK_KEY_x)
    delete_package_or_symbol();
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

void show_workspace_win(GtkWidget *widget,
                           gpointer  data)
{
  show_workspace_window();
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

  if(gtk_tree_selection_get_selected(gtk_tree_view_get_selection(GTK_TREE_VIEW(packages_list)), &model, &iter))
  {
    gint id;

    gtk_tree_model_get(model, &iter,
                       1, &id,
                       -1);

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

  if(gtk_tree_selection_get_selected(gtk_tree_view_get_selection(GTK_TREE_VIEW(lst)), &model, &iter))
  {
    gchar *symbol_name;
    gint ptr;

    gtk_tree_model_get(model, &iter,
                       0, &symbol_name,
                       -1);

    gtk_tree_model_get(model, &iter,
                       1, &ptr,
                       -1);

    char buf[MAX_STRING_LENGTH];
    memset(buf, '\0', MAX_STRING_LENGTH);

    OBJECT_PTR obj = cdr(get_symbol_value_from_env((OBJECT_PTR)ptr, top_level_env));

    gtk_text_buffer_set_text(system_browser_buffer, buf, -1);

    gtk_text_view_set_editable(system_browser_textview, FALSE);

    if(IS_CLOSURE_OBJECT(obj))
    {
      memset(buf, '\0', MAX_STRING_LENGTH);
      /* print_object_to_string(cons(DEFINE, */
      /*                             cons((OBJECT_PTR)ptr, */
      /*                                  cons(cons(LAMBDA, */
      /*                                            cons(get_params_object(obj), */
      /*                                                 get_source_object(obj))), */
      /*                                       NIL))), buf, 0); */
      /* print_object_to_string(list(4, DEFUN, (OBJECT_PTR)ptr, get_params_object(obj), car(get_source_object(obj))), buf, 0); */
      OBJECT_PTR temp = cons(DEFUN, 
                             cons((OBJECT_PTR)ptr,
                                  cons(get_params_object(obj),
                                       get_source_object(obj))));

      print_object_to_string(temp, buf, 0);

      gtk_text_buffer_insert_at_cursor(system_browser_buffer, (char *)conv_to_lower_case_preserve_strings(buf), -1);

      gtk_text_view_set_editable(system_browser_textview, TRUE);
    }
    else if(IS_MACRO_OBJECT(obj))
    {
      memset(buf, '\0', MAX_STRING_LENGTH);
      /* print_object_to_string(cons(DEFINE, */
      /*                             cons((OBJECT_PTR)ptr, */
      /*                                  cons(cons(MACRO, */
      /*                                            cons(get_params_object(obj), */
      /*                                                 get_source_object(obj))), */
      /*                                       NIL))), buf, 0); */
      /* print_object_to_string(list(4, DEFMACRO, (OBJECT_PTR)ptr, get_params_object(obj), car(get_source_object(obj))), buf, 0); */

      OBJECT_PTR temp = cons(DEFMACRO, 
                             cons((OBJECT_PTR)ptr,
                                  cons(get_params_object(obj),
                                       get_source_object(obj))));

      print_object_to_string(temp, buf, 0);


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

    gtk_widget_destroy (dialog);
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

BOOLEAN no_unmatched_parens(char *str)
{
  int imbalance = 0;

  int len = strlen(str);

  int i;

  for(i=0; i<len; i++)
  {
    if(str[i] == '(')
      imbalance ++;
    else if(str[i] == ')')
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

void display_matching_parens(GtkTextBuffer *buffer)
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

      if(no_unmatched_parens(str))
      {
        gtk_text_buffer_apply_tag_by_name(buffer, 
                                          "cyan_bg", 
                                          &start_match, 
                                          &end_match);

        GtkTextIter temp_iter = saved_iter;
        gtk_text_iter_backward_char(&saved_iter);
        gtk_text_buffer_apply_tag_by_name(buffer, 
                                          "cyan_bg", 
                                          &saved_iter, 
                                          &temp_iter);

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

void display_matching_parens_forward(GtkTextBuffer *buffer)
{
  GtkTextIter curr_iter;
  GtkTextIter start_match, end_match;

  //get the current iter
  gtk_text_buffer_get_iter_at_mark(buffer, &curr_iter, gtk_text_buffer_get_insert(buffer));

  GtkTextIter saved_iter = curr_iter;

  while(1)
  {
    if(gtk_text_iter_forward_search(&curr_iter, 
                                    ")", 
                                    GTK_TEXT_SEARCH_TEXT_ONLY | GTK_TEXT_SEARCH_VISIBLE_ONLY, 
                                    &start_match,
                                    &end_match, 
                                    NULL))
    {

      gchar *str = gtk_text_buffer_get_text(buffer, &saved_iter, &end_match, FALSE);

      if(no_unmatched_parens(str))
      {
        gtk_text_buffer_apply_tag_by_name(buffer, 
                                          "cyan_bg", 
                                          &start_match, 
                                          &end_match);

        GtkTextIter temp_iter = saved_iter;
        gtk_text_iter_forward_char(&saved_iter);
        gtk_text_buffer_apply_tag_by_name(buffer,
                                          "cyan_bg",
                                          &temp_iter,
                                          &saved_iter);
        
        break;
      }
      else
      {
        int offset = gtk_text_iter_get_offset(&end_match);
        gtk_text_buffer_get_iter_at_offset(buffer,
                                           &curr_iter, 
                                           offset);
      }
    }
    else
      break;
  }      
}

gboolean handle_code_edit_key_press(GtkWidget *widget, GdkEventKey *event, gpointer user_data)
{
  GtkTextIter start_iter, end_iter;

  GtkTextBuffer *buffer = gtk_text_view_get_buffer((GtkTextView *)widget);

  gtk_text_buffer_get_start_iter(buffer, &start_iter);
  gtk_text_buffer_get_end_iter(buffer, &end_iter);

  //remove current highlighting if any
  gtk_text_buffer_remove_all_tags(buffer,
                                  &start_iter,
                                  &end_iter);
  
  if(event->keyval ==  GDK_KEY_parenright)
  {
    if (gtk_text_view_im_context_filter_keypress (GTK_TEXT_VIEW (widget), event))
    {
      display_matching_parens(buffer);
      return TRUE;
    }
  }

  if(event->keyval ==  GDK_KEY_Return)
  {
    GtkTextIter iter, line_start;
    gtk_text_buffer_get_iter_at_mark(buffer, &iter, gtk_text_buffer_get_insert(buffer));
    gint line_number = gtk_text_iter_get_line(&iter);

    gtk_text_buffer_get_iter_at_line(buffer, &line_start, line_number);

    gchar *text = gtk_text_buffer_get_text(buffer, &line_start, &iter, FALSE);

    char trimmed_text[100];
    memset(trimmed_text, '\0', 100);

    trim_whitespace(trimmed_text, 100, text);

    if(strlen(trimmed_text) == 0)
    {
      gtk_text_buffer_insert_at_cursor(buffer, "\n", -1);
      return TRUE;
    }

    char ret[10];
    memset(ret, '\0', 11);

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

  return handle_key_press_events(widget, event, user_data);
}

void handle_code_edit_cursor_move(GtkTextBuffer *buffer,
                                  GtkTextIter   *location,
                                  GtkTextMark   *mark,
                                  gpointer       user_data) 
{
  GtkTextIter start_iter, end_iter;

  gtk_text_buffer_get_start_iter(buffer, &start_iter);
  gtk_text_buffer_get_end_iter(buffer, &end_iter);

  //remove current highlighting if any
  gtk_text_buffer_remove_all_tags(buffer,
                                  &start_iter,
                                  &end_iter);

  GtkTextIter prev;
  get_prev_iter(buffer, location, &prev);

  if(!strcmp(gtk_text_buffer_get_text(buffer, &prev, location, FALSE), ")"))
    display_matching_parens(buffer);
  else if(gtk_text_iter_get_char(location) == '(')
    display_matching_parens_forward(buffer);
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

void exp_pkg(GtkWidget *widget,
	     gpointer data)
{
  export_package_gui();
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

      char buf[MAX_STRING_LENGTH];
      memset(buf, '\0', MAX_STRING_LENGTH);

      sprintf(buf, "(export-package \"%s\" \"%s\")", package_name, file_name);

      if(!call_repl(buf))
      {
	gtk_statusbar_push(system_browser_statusbar, 0, "Package exported successfully");
      }
    }

    gtk_widget_destroy (dialog);
  }
}
