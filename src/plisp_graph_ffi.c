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
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <math.h>

#define ORIGIN_X 40
#define ORIGIN_Y 40

static int x_axis_len;
static int y_axis_len;

static int nof_divs_x_axis;
static int nof_divs_y_axis;

enum {LINE_GRAPH, SCATTERPLOT} graph_type;

static GdkPixmap *pixmap = NULL;

float *float_x, *float_y;
int g_size;

static int *counts;
static int nof_hist_bins;
static float hist_max;

static gboolean delete_event( GtkWidget *widget,
                              GdkEvent  *event,
                              gpointer   data )
{
  return FALSE;
}

void draw_axes(GtkWidget *darea)
{
  int i;

  x_axis_len = darea->allocation.width - 2 * ORIGIN_X;
  y_axis_len = darea->allocation.height - 2 * ORIGIN_Y;

  //draw the Y axis
  gdk_draw_line(pixmap,
                darea->style->black_gc,
                ORIGIN_X, ORIGIN_Y,
                ORIGIN_X, ORIGIN_Y + y_axis_len );

  //draw the X axis
  gdk_draw_line(pixmap,
                darea->style->black_gc,
                ORIGIN_X,y_axis_len + ORIGIN_Y,
                ORIGIN_X + x_axis_len, ORIGIN_Y + y_axis_len);

  //draw the division lines in the X axis
  for(i=1; i<=nof_divs_x_axis; i++)
    gdk_draw_line(pixmap,
                  darea->style->black_gc,
                  ORIGIN_X + (i*1.0/nof_divs_x_axis) * x_axis_len, ORIGIN_Y + y_axis_len,
                  ORIGIN_X + (i*1.0/nof_divs_x_axis) * x_axis_len, ORIGIN_Y + y_axis_len + 5);

  //draw the division lines in the Y axis
  for(i=0; i<nof_divs_y_axis; i++)
    gdk_draw_line(pixmap,
                  darea->style->black_gc,
                  ORIGIN_X, ORIGIN_Y + (i*1.0/nof_divs_y_axis) * y_axis_len,
                  ORIGIN_X - 5, ORIGIN_Y + (i*1.0/nof_divs_y_axis) * y_axis_len);
  
}

void draw_line_or_scatterplot(GtkWidget *darea)
{
  int i;

  draw_axes(darea);

  float x_max = float_x[g_size - 1];

  float y_max = float_y[0];

  for(i=1; i<g_size; i++)
    if(float_y[i] > y_max) y_max = float_y[i];

  PangoLayout *layout = pango_layout_new(gtk_widget_get_pango_context(darea));

  //alignment not working
  pango_layout_set_alignment(layout, PANGO_ALIGN_CENTER);

  for(i=1; i<=nof_divs_x_axis; i++)
  {
    char s[20];
    sprintf(s,"%.2f",x_max * (i*1.0/nof_divs_x_axis));
    pango_layout_set_text(layout, s, strlen(s));
    gdk_draw_layout(pixmap,
                    darea->style->black_gc,
                    ORIGIN_X + (i*1.0/nof_divs_x_axis) * x_axis_len - 5, 
                    ORIGIN_Y + y_axis_len + 10,
                    layout);
  }
  
  for(i=1; i<=nof_divs_y_axis; i++)
  {
    char s[20];
    sprintf(s,"%.2f",y_max * (i*1.0/nof_divs_y_axis));
    pango_layout_set_text(layout, s, strlen(s));
    gdk_draw_layout(pixmap,
                    darea->style->black_gc,
                    ORIGIN_X - 40, 
                    ORIGIN_Y + ((nof_divs_y_axis - i)*1.0/nof_divs_y_axis) * y_axis_len - 5,
                    layout);
  }

  float x_scale_factor = x_axis_len / x_max;
  float y_scale_factor = y_axis_len / y_max;

  float *x_pixels = (float *)malloc(g_size * sizeof(float));
  float *y_pixels = (float *)malloc(g_size * sizeof(float));

  for(i=0; i<g_size; i++)
  {
    x_pixels[i] = float_x[i] * x_scale_factor;
    y_pixels[i] = float_y[i] * y_scale_factor;
  }

  if(graph_type == LINE_GRAPH)
  {
    for(i=0; i<(g_size-1); i++)
    {
      gdk_draw_line(pixmap,
                    darea->style->black_gc,
                    convert_x((int)x_pixels[i]), convert_y((int)y_pixels[i]),
                    convert_x((int)x_pixels[i+1]), convert_y((int)y_pixels[i+1]));
    }
  }
  else if(graph_type == SCATTERPLOT)
  {
    for(i=0; i<(g_size); i++)
    {
      int x = convert_x((int)x_pixels[i]);
      int y = convert_y((int)y_pixels[i]);
      gdk_draw_point(pixmap, darea->style->black_gc, x-2,y);
      gdk_draw_point(pixmap, darea->style->black_gc, x-1,y);
      gdk_draw_point(pixmap, darea->style->black_gc, x+1,y);
      gdk_draw_point(pixmap, darea->style->black_gc, x+2,y);
      gdk_draw_point(pixmap, darea->style->black_gc, x,y-2);
      gdk_draw_point(pixmap, darea->style->black_gc, x,y-1);
      gdk_draw_point(pixmap, darea->style->black_gc, x,y+1);
      gdk_draw_point(pixmap, darea->style->black_gc, x,y+2);
      gdk_draw_point(pixmap, darea->style->black_gc, x,y);
    }      
  }

  free(x_pixels);
  free(y_pixels);
}

void draw_hist(GtkWidget *darea)
{
  int i;

  draw_axes(darea);
  
  int y_max = counts[0];

  for(i=1; i<nof_hist_bins; i++)
    if(counts[i] > y_max) y_max = counts[i];

  PangoLayout *layout = pango_layout_new(gtk_widget_get_pango_context(darea));

  //alignment not working
  pango_layout_set_alignment(layout, PANGO_ALIGN_CENTER);

  for(i=1; i<=nof_divs_x_axis; i++)
  {
    char s[20];
    sprintf(s,"%.2f",hist_max * i / nof_divs_x_axis);
    pango_layout_set_text(layout, s, strlen(s));
    gdk_draw_layout(pixmap,
                    darea->style->black_gc,
                    ORIGIN_X + i * x_axis_len / nof_divs_x_axis - 5, 
                    ORIGIN_Y + y_axis_len + 10,
                    layout);
  }
  
  for(i=1; i<=nof_divs_y_axis; i++)
  {
    char s[20];
    sprintf(s,"%.2f",y_max * (i*1.0/nof_divs_y_axis));
    pango_layout_set_text(layout, s, strlen(s));
    gdk_draw_layout(pixmap,
                    darea->style->black_gc,
                    ORIGIN_X - 40, 
                    ORIGIN_Y + ((nof_divs_y_axis - i)*1.0/nof_divs_y_axis) * y_axis_len - 5,
                    layout);
  }

  float x_scale_factor = x_axis_len / hist_max;
  float y_scale_factor = y_axis_len * 1.0 / (y_max * 1.0);

  float *x_pixels = (float *)malloc(nof_hist_bins * sizeof(float));;
  float *y_pixels = (float *)malloc(nof_hist_bins * sizeof(float));;

  for(i=0; i<nof_hist_bins; i++)
  {
    x_pixels[i] = hist_max * ((i+1)*1.0 / nof_hist_bins) * x_scale_factor;
    y_pixels[i] = counts[i] * y_scale_factor;
  }

  for(i=0; i<nof_hist_bins; i++)
  {
    if(counts[i] == 0)
      continue;

    int x1 = convert_x((int)x_pixels[i]);
    int y1 = convert_y((int)y_pixels[i]);
    int x2;
    if(i == 0)
      x2 = convert_x(0);
    else
      x2 = convert_x((int)x_pixels[i-1]);
    
    gdk_draw_line(pixmap, darea->style->black_gc, x1,convert_y(0),  x1,y1);
    gdk_draw_line(pixmap, darea->style->black_gc, x1,y1, x2,y1);
    gdk_draw_line(pixmap, darea->style->black_gc, x2,y1, x2,convert_y(0));
  }

  free(x_pixels);
  free(y_pixels);
}


/* Create a new backing pixmap of the appropriate size */
static gint configure_event(GtkWidget *widget, GdkEventConfigure *event)
{
  if (pixmap)
    gdk_pixmap_unref(pixmap);

  pixmap = gdk_pixmap_new(widget->window,
                          widget->allocation.width,
                          widget->allocation.height,
                          -1);

  gdk_draw_rectangle (pixmap,
                      widget->style->white_gc,
                      TRUE,
                      0, 0,
                      widget->allocation.width,
                      widget->allocation.height);

  draw_line_or_scatterplot(widget);

  return TRUE;
}

static gint configure_event_hist(GtkWidget *widget, GdkEventConfigure *event)
{
  if (pixmap)
    gdk_pixmap_unref(pixmap);

  pixmap = gdk_pixmap_new(widget->window,
                          widget->allocation.width,
                          widget->allocation.height,
                          -1);

  gdk_draw_rectangle (pixmap,
                      widget->style->white_gc,
                      TRUE,
                      0, 0,
                      widget->allocation.width,
                      widget->allocation.height);

  draw_hist(widget);

  return TRUE;
}

/* Redraw the screen from the backing pixmap */
static gint expose_event (GtkWidget *widget, GdkEventExpose *event)
{
  gdk_draw_pixmap(widget->window,
                  widget->style->fg_gc[GTK_WIDGET_STATE (widget)],
                  pixmap,
                  event->area.x, event->area.y,
                  event->area.x, event->area.y,
                  event->area.width, event->area.height);

  return FALSE;
}

int convert_x(int x)
{
  return ORIGIN_X +  x;
}

int convert_y(int y)
{
  return ORIGIN_Y + y_axis_len - y;
}

void plot(void *xval, void *yval, int size, int type)
{
  nof_divs_x_axis = 10;
  nof_divs_y_axis = 10;

  if(type == 1)
    graph_type = LINE_GRAPH;
  else if(type == 2)
    graph_type = SCATTERPLOT;
  else
    assert(0);

  float_x = (float *)xval;
  float_y = (float *)yval;
  g_size = size;

  GtkWidget *window;

  window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    
  g_signal_connect (window, "delete-event",
		      G_CALLBACK (delete_event), NULL);

  gtk_window_set_default_size((GtkWindow *)window, 600, 400);
  gtk_window_move((GtkWindow *)window, 650, 200); 
    
  gtk_container_set_border_width (GTK_CONTAINER (window), 10);

  GtkWidget *darea = gtk_drawing_area_new();

  gtk_widget_set_events (darea, GDK_EXPOSURE_MASK
			 | GDK_LEAVE_NOTIFY_MASK
			 | GDK_POINTER_MOTION_HINT_MASK);

  gtk_signal_connect(GTK_OBJECT (darea), "expose_event",
                     (GtkSignalFunc) expose_event, NULL);
  gtk_signal_connect(GTK_OBJECT(darea),"configure_event",
                     (GtkSignalFunc) configure_event, NULL);

  gtk_container_add(GTK_CONTAINER (window), darea);
  gtk_widget_show(darea);
  gtk_widget_show(window);

}

void hist(void *xval, int size)
{
  float_x = (float *)xval;
  g_size = size;

  int i,j;

  float x_max = float_x[0];

  for(i=1; i<g_size; i++)
    if(float_x[i] > x_max) x_max = float_x[i];

  nof_hist_bins = sqrt(size * 1.0);

  nof_divs_x_axis = nof_hist_bins;
  nof_divs_y_axis = 10;

  hist_max = ceilf(x_max+1);

  if(counts)
    free(counts);

  counts = (int *)malloc(nof_hist_bins * sizeof(int));

  for(i=0; i<nof_hist_bins; i++)
    counts[i] = 0;

  for(i=0; i<size; i++)
  {
    for(j=0; j<nof_hist_bins; j++)
    {
      if((float_x[i] >= (hist_max * j / nof_hist_bins)) && 
         (float_x[i] < (hist_max * (j+1) / nof_hist_bins)))
      {
        counts[j]++;
        break;
      }
    }
  }

  GtkWidget *window;

  window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    
  g_signal_connect (window, "delete-event",
		      G_CALLBACK (delete_event), NULL);

  gtk_window_set_default_size((GtkWindow *)window, 600, 400);
  gtk_window_move((GtkWindow *)window, 650, 200); 
    
  gtk_container_set_border_width (GTK_CONTAINER (window), 10);

  GtkWidget *darea = gtk_drawing_area_new();

  gtk_widget_set_events (darea, GDK_EXPOSURE_MASK
			 | GDK_LEAVE_NOTIFY_MASK
			 | GDK_POINTER_MOTION_HINT_MASK);

  gtk_signal_connect(GTK_OBJECT (darea), "expose_event",
                     (GtkSignalFunc) expose_event, NULL);
  gtk_signal_connect(GTK_OBJECT(darea),"configure_event",
                     (GtkSignalFunc) configure_event_hist, NULL);

  gtk_container_add(GTK_CONTAINER (window), darea);
  gtk_widget_show(darea);
  gtk_widget_show(window);

}
