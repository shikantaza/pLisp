/**
  Copyright 2011-2024 Rajesh Jayaprakash <rajesh.jayaprakash@pm.me>

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

typedef struct list {
  void *data;
  struct list *next;
} queue_item_t;

typedef struct {
  unsigned int count;
  struct list *first;
  struct list *last;
} queue_t;

queue_t *queue_create();
void queue_enqueue(queue_t *, void *);
queue_item_t *queue_dequeue(queue_t *);
unsigned int queue_count(queue_t *);
void queue_delete(queue_t *);
int queue_item_exists(queue_t *, void *);
int queue_is_empty(queue_t *);
