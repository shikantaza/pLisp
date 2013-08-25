#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>

#include "../plisp.h"

#define MAX_STRING_LENGTH 8096

extern GtkTextBuffer *transcript_buffer;
extern GtkTextBuffer *workspace_buffer;

extern GtkWindow *transcript_window;
extern GtkWindow *workspace_window;
extern GtkWindow *system_browser_window;
extern GtkWindow *debugger_window;

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

void evaluate();
void close_application_window(GtkWidget **);
void show_workspace_window();
void load_image();
void save_image();
void set_focus_to_last_row(GtkTreeView *);

int call_repl(char *expression)
{
  yy_scan_string(expression);
  while(yyparse() != -1)
  {
    if(repl())
      return -1;
  }

  return 0;
  /* else */
  /*   return -1; */
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
    debug_mode = false;
    call_repl("(RESUME)");
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

    gtk_main_quit();
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
                                       GTK_STOCK_OK,
                                       GTK_RESPONSE_ACCEPT,
                                       GTK_STOCK_CANCEL,
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
    GtkTextIter line_start, line_end, iter;
    gtk_text_buffer_get_iter_at_mark(buf, &iter, gtk_text_buffer_get_insert(buf));
    gint line_number = gtk_text_iter_get_line(&iter);

    gint line_count = gtk_text_buffer_get_line_count(buf);

    if(line_count == (line_number+1))
    {
      gtk_text_buffer_get_iter_at_line(buf, &line_start, line_number-1);
      gtk_text_buffer_get_end_iter (buf, &line_end);
    }
    else
    {
      gtk_text_buffer_get_iter_at_line(buf, &line_start, line_number);
      gtk_text_buffer_get_iter_at_line(buf, &line_end, line_number+1);
    }

    expression = (char *)gtk_text_buffer_get_text(buf, &line_start, &line_end, FALSE);
  }

  if(!call_repl(expression))
    update_workspace_title();
}

void load_source()
{
  GtkWidget *dialog;

  dialog = gtk_file_chooser_dialog_new ("Load pLisp source file",
                                        (GtkWindow *)workspace_window,
                                        GTK_FILE_CHOOSER_ACTION_OPEN,
                                        GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                                        GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,
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
  if(widget == (GtkWidget *)workspace_window && (event->state & GDK_CONTROL_MASK) && event->keyval == GDK_l)
    load_source();
  else if(widget == (GtkWidget *)workspace_window && event->keyval == GDK_F5)
  {
    action_triggering_window = workspace_window;
    evaluate();
  }
  else if(widget == (GtkWidget *)workspace_window && (event->state & GDK_CONTROL_MASK) && event->keyval == GDK_w)
    close_application_window((GtkWidget **)&workspace_window);
  else if(widget == (GtkWidget *)system_browser_window && (event->state & GDK_CONTROL_MASK) && event->keyval == GDK_k)
    create_new_package();
  else if(widget == (GtkWidget *)system_browser_window && (event->state & GDK_CONTROL_MASK) && event->keyval == GDK_n)
  {
    action_triggering_window = system_browser_window;
    create_new_symbol();
  }
  else if(widget == (GtkWidget *)system_browser_window && (event->state & GDK_CONTROL_MASK) && event->keyval == GDK_s)
  {
    action_triggering_window = system_browser_window;
    system_browser_accept();
  }
  else if(widget == (GtkWidget *)system_browser_window && (event->state & GDK_CONTROL_MASK) && event->keyval == GDK_x)
    delete_package_or_symbol();
  else if(widget == (GtkWidget *)system_browser_window && event->keyval == GDK_F5)
    refresh_system_browser();
  else if(widget == (GtkWidget *)system_browser_window && (event->state & GDK_CONTROL_MASK) && event->keyval == GDK_w)
    close_application_window((GtkWidget **)&system_browser_window);
  else if(widget == (GtkWidget *)transcript_window && (event->state & GDK_CONTROL_MASK) && event->keyval == GDK_l)
    load_image();
  else if(widget == (GtkWidget *)transcript_window && (event->state & GDK_CONTROL_MASK) && event->keyval == GDK_s)
    save_image();
  else if(widget == (GtkWidget *)transcript_window && event->keyval == GDK_F7)
    show_workspace_window();
  else if(widget == (GtkWidget *)transcript_window && event->keyval == GDK_F9)
    create_system_browser_window();
  else if(widget == (GtkWidget *)transcript_window && (event->state & GDK_CONTROL_MASK) && event->keyval == GDK_w)
    quit_application();

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
    create_workspace_window();
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
                                        GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                                        GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,
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
    create_system_browser_window();
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

      if((sym >> (SYMBOL_BITS + OBJECT_SHIFT)) == id)
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

void fetch_symbol_value(GtkWidget *list, gpointer data)
{
  GtkListStore *store = GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(list)));
  GtkTreeModel *model = gtk_tree_view_get_model (GTK_TREE_VIEW (list));
  GtkTreeIter  iter;

  if(gtk_tree_selection_get_selected(gtk_tree_view_get_selection(GTK_TREE_VIEW(list)), &model, &iter))
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

    OBJECT_PTR obj = cdr(get_symbol_value_from_env(ptr, top_level_env));

    gtk_text_buffer_set_text(system_browser_buffer, buf, -1);

    gtk_text_view_set_editable(system_browser_textview, FALSE);

    if(IS_CLOSURE_OBJECT(obj))
    {
      memset(buf, '\0', MAX_STRING_LENGTH);
      /* print_object_to_string(cons(DEFINE, */
      /*                             cons(ptr, */
      /*                                  cons(cons(LAMBDA, */
      /*                                            cons(get_params_object(obj), */
      /*                                                 cons(get_source_object(obj),NIL))), */
      /*                                       NIL))), buf, 0);                        */
      print_object_to_string(cons(DEFINE,
                                  cons(ptr,
                                       cons(cons(LAMBDA,
                                                 cons(get_params_object(obj),
                                                      get_source_object(obj))),
                                            NIL))), buf, 0);

      gtk_text_buffer_insert_at_cursor(system_browser_buffer, (char *)convert_to_lower_case(buf), -1);
      gtk_text_view_set_editable(system_browser_textview, TRUE);
    }
    else if(IS_MACRO_OBJECT(obj))
    {
      memset(buf, '\0', MAX_STRING_LENGTH);
      /* print_object_to_string(cons(DEFINE, */
      /*                             cons(ptr, */
      /*                                  cons(cons(MACRO, */
      /*                                            cons(get_params_object(obj), */
      /*                                                 cons(get_source_object(obj),NIL))), */
      /*                                       NIL))), buf, 0); */
      print_object_to_string(cons(DEFINE,
                                  cons(ptr,
                                       cons(cons(MACRO,
                                                 cons(get_params_object(obj),
                                                      get_source_object(obj))),
                                            NIL))), buf, 0);

      gtk_text_buffer_insert_at_cursor(system_browser_buffer, (char *)convert_to_lower_case(buf), -1);
      gtk_text_view_set_editable(system_browser_textview, TRUE);
    }
    else if(IS_CONTINUATION_OBJECT(obj))
    {
      memset(buf, '\0', MAX_STRING_LENGTH);
      print_object_to_string(obj, buf, 0);
      gtk_text_buffer_insert_at_cursor(system_browser_buffer, (char *)convert_to_lower_case(buf), -1);
    }
    else
    {
      memset(buf, '\0', MAX_STRING_LENGTH);
      print_object_to_string(cons(DEFINE,
                                  cons(ptr, cons(obj, NIL))), buf, 0);
      gtk_text_buffer_insert_at_cursor(system_browser_buffer, (char *)convert_to_lower_case(buf), -1);
      gtk_text_view_set_editable(system_browser_textview, TRUE);
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
                                          GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                                          GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,
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

  gdk_cursor_unref(cursor);
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

    OBJECT_PTR rest1 = env_list_ptr;

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