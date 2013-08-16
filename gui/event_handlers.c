#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>

#include "../plisp.h"

#define MAX_STRING_LENGTH 8096

extern GtkTextBuffer *transcript_buffer;
extern GtkTextBuffer *workspace_buffer;

extern GtkWindow *transcript_window;
extern GtkWindow *workspace_window;
extern GtkWindow *system_browser_window;

extern GtkTreeView *packages_list;
extern GtkTreeView *symbols_list;

extern GtkTextBuffer *system_browser_buffer;

extern char *loaded_image_file_name;

extern unsigned int nof_packages;
extern package_t *packages;

extern OBJECT_PTR NIL;
extern OBJECT_PTR top_level_env;

extern BOOLEAN new_symbol_being_created;

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
    workspace_window = NULL;
  else if(widget == (GtkWidget *)system_browser_window)
    system_browser_window = NULL;

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
}

void delete_package_or_symbol()
{
  printf("Delete package/symbol\n");
}

void delete_pkg_or_sym(GtkWidget *widget,
                gpointer data)
{
  delete_package_or_symbol();
}


void create_new_package()
{
  printf("New package\n");
}

void new_package(GtkWidget *widget,
                gpointer data)
{
  create_new_package();
}


void new_symbol(GtkWidget *widget,
                gpointer data)
{
  create_new_symbol();
}

void system_browser_accept()
{
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

    yy_scan_string(buf);

    if(!yyparse())
    {
      if(repl())
      {
        show_error_dialog("Evaluation failed\n");
        return;
      }
    }

    GtkTextIter start, end;

    gtk_text_buffer_get_start_iter(system_browser_buffer, &start);
    gtk_text_buffer_get_end_iter (system_browser_buffer, &end);

    yy_scan_string(gtk_text_buffer_get_text(system_browser_buffer, &start, &end, FALSE));

    if(!yyparse())
    {
      if(!repl())
        update_workspace_title();
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

      yy_scan_string(buf);

      if(!yyparse())
        if(repl())
          return;

      GtkTextIter start, end;

      gtk_text_buffer_get_start_iter(system_browser_buffer, &start);
      gtk_text_buffer_get_end_iter (system_browser_buffer, &end);

      char *text = gtk_text_buffer_get_text(system_browser_buffer, &start, &end, FALSE);

      memset(buf, '\0', MAX_STRING_LENGTH);

      sprintf(buf, "(set %s %s)", symbol_name, (text == NULL) ? "nil" : text);

      yy_scan_string(buf);

      if(!yyparse())
      {
        if(!repl())
          update_workspace_title();
      }
    }
  }
}

void accept(GtkWidget *widget,
            gpointer data)
{
  system_browser_accept();
}

void eval_expression(GtkWidget *widget,
                     gpointer data)
{
  evaluate();
}

void close_window(GtkWidget *widget,
                  gpointer data)
{
  if((GtkWidget *)data == (GtkWidget *)workspace_window)
    close_application_window(&workspace_window);
  else if((GtkWidget *)data == (GtkWidget *)system_browser_window)
    close_application_window(&system_browser_window);
}

void evaluate()
{
  GtkTextBuffer *buf;
  GtkTextIter start_sel, end_sel;
  gboolean selected;

  buf = workspace_buffer;
  selected = gtk_text_buffer_get_selection_bounds(buf, &start_sel, &end_sel);

  if(selected)
  {
    yy_scan_string((char *) gtk_text_buffer_get_text(buf, &start_sel, &end_sel, FALSE));
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

    yy_scan_string((char *)gtk_text_buffer_get_text(buf, &line_start, &line_end, FALSE));
  }

  if(!yyparse())
  {
    if(!repl())
      update_workspace_title();
  }
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

    yy_scan_string(buf);

    if(!yyparse())
    {
      if(!repl())
      {
        update_workspace_title();

        print_to_transcript("Source file ");
        print_to_transcript(loaded_source_file_name);
        print_to_transcript(" loaded successfully\n");
      }
      else
        print_to_transcript("Error loading source file\n");
    }
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
    evaluate();
  else if(widget == (GtkWidget *)workspace_window && (event->state & GDK_CONTROL_MASK) && event->keyval == GDK_w)
    close_application_window(&workspace_window);
  else if(widget == (GtkWidget *)system_browser_window && (event->state & GDK_CONTROL_MASK) && event->keyval == GDK_k)
    create_new_package();
  else if(widget == (GtkWidget *)system_browser_window && (event->state & GDK_CONTROL_MASK) && event->keyval == GDK_n)
    create_new_symbol();
  else if(widget == (GtkWidget *)system_browser_window && (event->state & GDK_CONTROL_MASK) && event->keyval == GDK_s)
    system_browser_accept();
  else if(widget == (GtkWidget *)system_browser_window && (event->state & GDK_CONTROL_MASK) && event->keyval == GDK_x)
    delete_package_or_symbol();
  else if(widget == (GtkWidget *)system_browser_window && (event->state & GDK_CONTROL_MASK) && event->keyval == GDK_w)
    close_application_window(&system_browser_window);
  else if(widget == (GtkWidget *)transcript_window && (event->state & GDK_CONTROL_MASK) && event->keyval == GDK_l)
    load_image();
  else if(widget == (GtkWidget *)transcript_window && (event->state & GDK_CONTROL_MASK) && event->keyval == GDK_s)
    save_image();
  else if(widget == (GtkWidget *)transcript_window && event->keyval == GDK_F7)
    create_workspace_window();
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

void show_workspace_window(GtkWidget *widget,
                           gpointer  data)
{
  if(workspace_window == NULL)
    create_workspace_window();
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
  if(loaded_image_file_name == NULL)
  {
    show_error_dialog_for_window("No image has been loaded", transcript_window);
    return;
  }

  save_image();

  print_to_transcript("Image saved successfully\n");
}

void show_system_browser_window(GtkWidget *widget,
                                gpointer  data)
{
  if(system_browser_window == NULL)
    create_system_browser_window();
}

void fetch_package_members(GtkWidget *list, gpointer selection)
{
  GtkListStore *store = GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(list)));
  GtkTreeModel *model = gtk_tree_view_get_model (GTK_TREE_VIEW (list));
  GtkTreeIter  iter;

  if(gtk_tree_selection_get_selected(gtk_tree_view_get_selection(GTK_TREE_VIEW(list)), &model, &iter))
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
}

void fetch_symbol_value(GtkWidget *list, gpointer data)
{
  GtkListStore *store = GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(list)));
  GtkTreeModel *model = gtk_tree_view_get_model (GTK_TREE_VIEW (list));
  GtkTreeIter  iter;

  if(gtk_tree_selection_get_selected(gtk_tree_view_get_selection(GTK_TREE_VIEW(list)), &model, &iter))
  {
    gint ptr;

    gtk_tree_model_get(store, &iter,
                       1, &ptr,
                       -1);

    char buf[MAX_STRING_LENGTH];
    memset(buf, '\0', MAX_STRING_LENGTH);

    OBJECT_PTR obj = cdr(get_symbol_value_from_env(ptr, top_level_env));

    if(IS_CLOSURE_OBJECT(obj))
    {
      gtk_text_buffer_set_text(system_browser_buffer, "(lambda ", -1);

      memset(buf, '\0', MAX_STRING_LENGTH);
      print_object_to_string(get_params_object(obj), buf);
      gtk_text_buffer_insert_at_cursor(system_browser_buffer, convert_to_lower_case(buf), -1);
      gtk_text_buffer_insert_at_cursor(system_browser_buffer, "\n  ", -1);

      memset(buf, '\0', MAX_STRING_LENGTH);
      print_object_to_string(get_source_object(obj), buf);
      gtk_text_buffer_insert_at_cursor(system_browser_buffer, convert_to_lower_case(buf), -1);
      gtk_text_buffer_insert_at_cursor(system_browser_buffer, "\n", -1);
    }
    else if(IS_MACRO_OBJECT(obj))
    {
      gtk_text_buffer_set_text(system_browser_buffer, "(macro ", -1);

      memset(buf, '\0', MAX_STRING_LENGTH);
      print_object_to_string(get_params_object(obj), buf);
      gtk_text_buffer_insert_at_cursor(system_browser_buffer, convert_to_lower_case(buf), -1);
      gtk_text_buffer_insert_at_cursor(system_browser_buffer, "\n  ", -1);

      memset(buf, '\0', MAX_STRING_LENGTH);
      print_object_to_string(get_source_object(obj), buf);
      gtk_text_buffer_insert_at_cursor(system_browser_buffer, convert_to_lower_case(buf), -1);
      gtk_text_buffer_insert_at_cursor(system_browser_buffer, "\n", -1);
    }
    else
    {
      memset(buf, '\0', MAX_STRING_LENGTH);
      print_object_to_string(obj, buf);
      gtk_text_buffer_set_text(system_browser_buffer, buf, -1);
    }
  }
}

void save_image()
{
  char exp[MAX_STRING_LENGTH];
  memset(exp, '\0', MAX_STRING_LENGTH);

  sprintf(exp,"(create-image \"%s\")", loaded_image_file_name);

  yy_scan_string(exp);

  if(!yyparse())
    repl();
  update_workspace_title();
}

