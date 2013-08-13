#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>

#define FONT "Courier Bold 9"

GtkTextBuffer *transcript_buffer;
GtkTextBuffer *workspace_buffer;

GtkWindow *transcript_window;
GtkWindow *workspace_window;

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

  return FALSE;
}

/* Another callback */
static void destroy( GtkWidget *widget,
                     gpointer   data )
{
  gtk_main_quit();
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
      buf = (GtkTextBuffer *)user_data;
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
        repl();
      prompt();

      break;
  default:
    return FALSE;
  }

  return FALSE;
}

void create_workspace_window()
{
  GtkWidget *win = gtk_window_new (GTK_WINDOW_TOPLEVEL);

  gtk_window_set_title((GtkWindow *)win, "pLisp Workspace [Package: USER]");
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
  print_to_workspace("; place cursor anywhere on the line and press F5.\n");

  g_signal_connect(G_OBJECT(win), 
                   "key_press_event", 
                   G_CALLBACK (evaluate), 
                   gtk_text_view_get_buffer((GtkTextView *)w->textview));

  scrolled_win = gtk_scrolled_window_new (NULL, NULL);
  gtk_container_add (GTK_CONTAINER (scrolled_win), w->textview);

  vbox = gtk_vbox_new (FALSE, 5);
  gtk_box_pack_start (GTK_BOX (vbox), scrolled_win, TRUE, TRUE, 0);
  
  gtk_container_add (GTK_CONTAINER (win), vbox);

  gtk_widget_show_all(win);

  workspace_window = (GtkWindow *)win;
}

void show_workspace_window(GtkWidget *widget,
                           gpointer  data )
{
  if(workspace_window == NULL)
    create_workspace_window();
}

void create_transcript_window()
{
  GtkWidget *scrolled_win, *vbox;
  Widgets *w = g_slice_new (Widgets);

  transcript_window = (GtkWindow *)gtk_window_new (GTK_WINDOW_TOPLEVEL);

  gtk_window_set_title((GtkWindow *)transcript_window, "pLisp Transcript");
  gtk_window_set_default_size((GtkWindow *)transcript_window, 600, 400);
  gtk_window_move((GtkWindow *)transcript_window, 25, 200); 
    
  g_signal_connect (transcript_window, "delete-event",
                    G_CALLBACK (delete_event), NULL);
    
  g_signal_connect (transcript_window, "destroy",
		      G_CALLBACK (destroy), NULL);
    
  gtk_container_set_border_width (GTK_CONTAINER (transcript_window), 10);
  
  w->textview = gtk_text_view_new ();
  w->entry = gtk_entry_new ();
  gtk_text_view_set_editable((GtkTextView *)w->textview, FALSE);

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
                           NULL, /* a signal */
                           NULL);

  gtk_toolbar_append_item (GTK_TOOLBAR (toolbar),                   
                           NULL,                                   /* button label */
                           "Save image",                           /* button's tooltip */
                           "Private",                              /* tooltip private info */
                           save_icon,                              /* icon widget */
                           NULL, /* a signal */
                           NULL);


  gtk_toolbar_append_item (GTK_TOOLBAR (toolbar),                   
                           NULL,                                   /* button label */
                           "Show workspace window",                /* button's tooltip */
                           "Private",                              /* tooltip private info */
                           workspace_icon,                         /* icon widget */
                           GTK_SIGNAL_FUNC(show_workspace_window), /* a signal */
                           NULL);

  gtk_toolbar_append_item (GTK_TOOLBAR (toolbar),                   
                           NULL,                                   /* button label */
                           "System Browser",                       /* button's tooltip */
                           "Private",                              /* tooltip private info */
                           browser_icon,                           /* icon widget */
                           NULL,                                   /* a signal */
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

