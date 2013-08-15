#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>

#include "../plisp.h"

#define FONT "Courier Bold 9"

#define MAX_STRING_LENGTH 8096

GtkTextBuffer *transcript_buffer;
GtkTextBuffer *workspace_buffer;

GtkWindow *transcript_window;
GtkWindow *workspace_window;
GtkWindow *system_browser_window;

GtkTreeView *packages_list;
GtkTreeView *symbols_list;

extern char *loaded_image_file_name;

extern unsigned int nof_packages;
extern package_t *packages;

extern OBJECT_PTR NIL;
extern OBJECT_PTR top_level_env;

GtkTextBuffer *system_browser_buffer;

typedef struct
{
  GtkWidget *entry, *textview;
} Widgets;

static gboolean delete_event( GtkWidget *widget,
                              GdkEvent  *event,
                              gpointer   data )
{
  if(widget == (GtkWidget *)workspace_window)
    workspace_window = NULL;
  else if(widget == (GtkWidget *)system_browser_window)
    system_browser_window = NULL;

  return FALSE;
}

/* Another callback */
static void destroy( GtkWidget *widget,
                     gpointer   data )
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

gboolean handle_key_press_events(GtkWidget *widget, GdkEventKey *event, gpointer user_data)
{
  if(widget == workspace_window && event->keyval == GDK_F5)
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
        prompt();
    }
  }
  else if(widget == transcript_window && event->keyval == GDK_F7)
    create_workspace_window();
  else if(widget == transcript_window && event->keyval == GDK_F9)
    create_system_browser_window();


  return FALSE;
}

//Ensure that evaluate() is called only from the workspace window;
//only then will be user_data be a 'GtkTextBuffer *' object
gboolean evaluate(GtkWidget *widget, GdkEventKey *event, gpointer user_data)
{
  GtkTextBuffer *buf;
  GtkTextIter start_sel, end_sel;
  gboolean selected;

  switch (event->keyval)
  {
    case GDK_F5:
      //buf = (GtkTextBuffer *)user_data;
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
          prompt();
      }

      break;
  default:
    return FALSE;
  }

  return FALSE;
}

void load_source_file(GtkWidget *widget,
                     gpointer data)
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
        prompt();

        print_to_transcript("Source file ");
        print_to_transcript(loaded_source_file_name);
        print_to_transcript(" loaded successfully\n");
      }
      else
        print_to_transcript("Error loading source file\n");
    }
  }
}


void create_workspace_window()
{
  GtkWidget *win = gtk_window_new (GTK_WINDOW_TOPLEVEL);

  gtk_window_set_default_size((GtkWindow *)win, 600, 400);
  gtk_window_move((GtkWindow *)win, 650, 200); 

  g_signal_connect (win, "delete-event",
                    G_CALLBACK (delete_event), NULL);

  gtk_container_set_border_width (GTK_CONTAINER (win), 10);

  Widgets *w = g_slice_new (Widgets);
  GtkWidget *scrolled_win, *vbox;

  w->textview = gtk_text_view_new ();
  w->entry = gtk_entry_new ();

  gtk_widget_modify_font(GTK_WIDGET(w->textview), pango_font_description_from_string(FONT));

  workspace_buffer = gtk_text_view_get_buffer((GtkTextView *)w->textview);

  print_to_workspace("; This is the workspace; type pLisp expressions here.\n");
  print_to_workspace("; Select an expression and press F5 to evaluate it.\n");
  print_to_workspace("; Alternately, if the expression fits in a single line,\n");
  print_to_workspace("; place the cursor anywhere on the line and press F5.\n");

  g_signal_connect(G_OBJECT(win), 
                   "key_press_event", 
                   G_CALLBACK (handle_key_press_events), 
                   NULL);

  scrolled_win = gtk_scrolled_window_new (NULL, NULL);
  gtk_container_add (GTK_CONTAINER (scrolled_win), w->textview);

  GtkWidget *toolbar;

  GtkWidget *load_icon = gtk_image_new_from_file ("icons/load_file.png");

  toolbar = gtk_toolbar_new ();
  gtk_toolbar_set_orientation (GTK_TOOLBAR (toolbar), GTK_ORIENTATION_HORIZONTAL);
  gtk_toolbar_set_style (GTK_TOOLBAR (toolbar), GTK_TOOLBAR_BOTH);
  gtk_container_set_border_width (GTK_CONTAINER (toolbar), 5);

  gtk_toolbar_append_item (GTK_TOOLBAR (toolbar),                   
                           NULL,                                   /* button label */
                           "Load file",                            /* button's tooltip */
                           "Private",                              /* tooltip private info */
                           load_icon,                              /* icon widget */
                           GTK_SIGNAL_FUNC(load_source_file),      /* a signal */
                           NULL);


  vbox = gtk_vbox_new (FALSE, 5);
  gtk_box_pack_start (GTK_BOX (vbox), toolbar, FALSE, FALSE, 0);
  gtk_box_pack_start (GTK_BOX (vbox), scrolled_win, TRUE, TRUE, 0);
  
  gtk_container_add (GTK_CONTAINER (win), vbox);

  gtk_widget_show_all(win);

  workspace_window = (GtkWindow *)win;

  prompt();
}

void show_workspace_window(GtkWidget *widget,
                           gpointer  data)
{
  if(workspace_window == NULL)
    create_workspace_window();
}

void load_image_file(GtkWidget *widget,
                     gpointer data)
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

void save_image()
{
  char exp[MAX_STRING_LENGTH];
  memset(exp, '\0', MAX_STRING_LENGTH);

  sprintf(exp,"(create-image \"%s\")", loaded_image_file_name);

  yy_scan_string(exp);

  if(!yyparse())
    repl();
  prompt();
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

void populate_packages_list(GtkTreeView *list)
{
  GtkListStore *store;
  GtkTreeIter  iter;

  store = GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(list)));

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

void fetch_package_members(GtkWidget *list, gpointer selection)
{
  GtkListStore *store = GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(list)));
  GtkTreeModel *model = gtk_tree_view_get_model (GTK_TREE_VIEW (list));
  GtkTreeIter  iter;

  if(gtk_tree_selection_get_selected(gtk_tree_view_get_selection(GTK_TREE_VIEW(list)), &model, &iter))
  {
    gint id;

    gtk_tree_model_get(store, &iter,
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

void create_system_browser_window()
{
  GtkWidget *win = gtk_window_new (GTK_WINDOW_TOPLEVEL);

  GtkWidget *scrolled_win1, *scrolled_win2;
  GtkWidget *vbox, *hbox;

  gtk_window_set_title((GtkWindow *)win, "pLisp System Browser");
  gtk_window_set_default_size((GtkWindow *)win, 600, 400);
  gtk_window_set_position(GTK_WINDOW(win), GTK_WIN_POS_CENTER);

  g_signal_connect (win, "delete-event",
                    G_CALLBACK (delete_event), NULL);

  gtk_container_set_border_width (GTK_CONTAINER (win), 10);

  scrolled_win1 = gtk_scrolled_window_new(NULL, NULL);
  scrolled_win2 = gtk_scrolled_window_new(NULL, NULL);

  packages_list = gtk_tree_view_new();
  gtk_tree_view_set_headers_visible(GTK_TREE_VIEW(packages_list), TRUE);
  gtk_widget_modify_font(GTK_WIDGET(packages_list), pango_font_description_from_string(FONT));

  initialize_packages_list((GtkTreeView *)packages_list);

  gtk_tree_view_column_set_sort_column_id(gtk_tree_view_get_column(packages_list, 0), 0); 

  symbols_list = gtk_tree_view_new();
  gtk_tree_view_set_headers_visible(GTK_TREE_VIEW(symbols_list), TRUE);
  gtk_widget_modify_font(GTK_WIDGET(symbols_list), pango_font_description_from_string(FONT));

  initialize_symbols_list((GtkTreeView *)symbols_list);
  gtk_tree_view_column_set_sort_column_id(gtk_tree_view_get_column(symbols_list, 0), 0);

  populate_packages_list((GtkTreeView *)packages_list);

  g_signal_connect(G_OBJECT(packages_list), "cursor-changed",
          G_CALLBACK(fetch_package_members), NULL);

  g_signal_connect(G_OBJECT(symbols_list), "cursor-changed",
                   G_CALLBACK(fetch_symbol_value), NULL);

  gtk_container_add(GTK_CONTAINER (scrolled_win1), packages_list);
  gtk_container_add(GTK_CONTAINER (scrolled_win2), symbols_list);

  hbox = gtk_hbox_new (FALSE, 5);
  gtk_box_pack_start_defaults (GTK_BOX (hbox), scrolled_win1);
  gtk_box_pack_start_defaults (GTK_BOX (hbox), scrolled_win2);

  Widgets *w = g_slice_new (Widgets);
  GtkWidget *scrolled_win;

  w->textview = gtk_text_view_new ();
  w->entry = gtk_entry_new ();

  gtk_widget_modify_font(GTK_WIDGET(w->textview), pango_font_description_from_string(FONT));
  gtk_text_view_set_editable((GtkTextView *)w->textview, FALSE);

  system_browser_buffer = gtk_text_view_get_buffer((GtkTextView *)w->textview);

  scrolled_win = gtk_scrolled_window_new (NULL, NULL);
  gtk_container_add (GTK_CONTAINER (scrolled_win), w->textview);

  vbox = gtk_vbox_new (FALSE, 5);
  gtk_box_pack_start (GTK_BOX (vbox), hbox, TRUE, TRUE, 0);
  gtk_box_pack_start (GTK_BOX (vbox), scrolled_win, TRUE, TRUE, 0);
  
  gtk_container_add (GTK_CONTAINER (win), vbox);

  gtk_widget_show_all(win);

  system_browser_window = (GtkWindow *)win;
}

void create_transcript_window()
{
  GtkWidget *scrolled_win, *vbox;
  Widgets *w = g_slice_new (Widgets);

  transcript_window = (GtkWindow *)gtk_window_new (GTK_WINDOW_TOPLEVEL);

  gtk_window_set_title((GtkWindow *)transcript_window, "pLisp Transcript");
  gtk_window_set_default_size((GtkWindow *)transcript_window, 600, 400);
  gtk_window_set_position(GTK_WINDOW(transcript_window), GTK_WIN_POS_CENTER);
    
  g_signal_connect (transcript_window, "delete-event",
                    G_CALLBACK (delete_event), NULL);
    
  g_signal_connect (transcript_window, "destroy",
		      G_CALLBACK (destroy), NULL);

  g_signal_connect(transcript_window, 
                   "key_press_event", 
                   G_CALLBACK (handle_key_press_events), 
                   NULL);

    
  gtk_container_set_border_width (GTK_CONTAINER (transcript_window), 10);
  
  w->textview = gtk_text_view_new ();
  w->entry = gtk_entry_new ();
  gtk_text_view_set_editable((GtkTextView *)w->textview, FALSE);
  gtk_text_view_set_cursor_visible((GtkTextView *)w->textview, FALSE);

  gtk_widget_modify_font(GTK_WIDGET(w->textview), pango_font_description_from_string(FONT));

  transcript_buffer = gtk_text_view_get_buffer((GtkTextView *)w->textview);

  print_ui_copyright_notice();

  scrolled_win = gtk_scrolled_window_new (NULL, NULL);
  gtk_container_add (GTK_CONTAINER (scrolled_win), w->textview);

  vbox = gtk_vbox_new (FALSE, 5);

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
                           "Load image",                           /* button's tooltip */
                           "Private",                              /* tooltip private info */
                           load_icon,                              /* icon widget */
                           GTK_SIGNAL_FUNC(load_image_file),       /* a signal */
                           NULL);

  gtk_toolbar_append_item (GTK_TOOLBAR (toolbar),                   
                           NULL,                                   /* button label */
                           "Save image",                           /* button's tooltip */
                           "Private",                              /* tooltip private info */
                           save_icon,                              /* icon widget */
                           GTK_SIGNAL_FUNC(save_image_file),       /* a signal */
                           NULL);


  gtk_toolbar_append_item (GTK_TOOLBAR (toolbar),                   
                           NULL,                                   /* button label */
                           "Show workspace window (F7)",           /* button's tooltip */
                           "Private",                              /* tooltip private info */
                           workspace_icon,                         /* icon widget */
                           GTK_SIGNAL_FUNC(show_workspace_window), /* a signal */
                           NULL);

  gtk_toolbar_append_item (GTK_TOOLBAR (toolbar),                   
                           NULL,                                        /* button label */
                           "System Browser (F9)",                       /* button's tooltip */
                           "Private",                                   /* tooltip private info */
                           browser_icon,                                /* icon widget */
                           GTK_SIGNAL_FUNC(show_system_browser_window), /* a signal */
                           NULL);

  gtk_toolbar_append_item (GTK_TOOLBAR (toolbar),                   
                           NULL,                                   /* button label */
                           "Exit",                                 /* button's tooltip */
                           "Private",                              /* tooltip private info */
                           exit_icon,                              /* icon widget */
                           GTK_SIGNAL_FUNC(destroy),               /* a signal */
                           NULL);

  gtk_box_pack_start (GTK_BOX (vbox), toolbar, FALSE, FALSE, 0);
  gtk_box_pack_start (GTK_BOX (vbox), scrolled_win, TRUE, TRUE, 0);
  
  gtk_container_add (GTK_CONTAINER (transcript_window), vbox);
  
  gtk_widget_show_all((GtkWidget *)transcript_window);
}

void show_error_dialog(char *msg)
{
  GtkWidget *dialog = gtk_message_dialog_new ((GtkWindow *)workspace_window,
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
  Widgets *w = g_slice_new (Widgets);

  window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title (GTK_WINDOW (window), "Error");
  gtk_container_set_border_width (GTK_CONTAINER (window), 10);
  gtk_window_set_default_size((GtkWindow *)window, 400, 50);
  gtk_window_move((GtkWindow *)window, 500, 200); 

  w->textview = gtk_text_view_new ();

  gtk_widget_modify_font(GTK_WIDGET(w->textview), pango_font_description_from_string(FONT));
  gtk_text_buffer_insert_at_cursor(gtk_text_view_get_buffer((GtkTextView *)w->textview), msg, -1);

  ok = gtk_button_new_with_label("OK");

  g_signal_connect_swapped (ok, "clicked",
                            G_CALLBACK (gtk_widget_destroy),
                            window);

  scrolled_win = gtk_scrolled_window_new (NULL, NULL);
  gtk_container_add (GTK_CONTAINER (scrolled_win), w->textview);

  hbox = gtk_hbox_new (FALSE, 5);
  gtk_box_pack_start_defaults (GTK_BOX (hbox), ok);

  vbox = gtk_vbox_new (FALSE, 5);
  gtk_box_pack_start (GTK_BOX (vbox), scrolled_win, TRUE, TRUE, 0);
  gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, TRUE, 0);

  gtk_container_add (GTK_CONTAINER (window), vbox);
  gtk_widget_show_all (window);
}

