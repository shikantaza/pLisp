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

#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

#include "gc.h"

#include "queue.h"

queue_t *queue_create()
{
  queue_t *q = (queue_t *)GC_MALLOC(sizeof(queue_t));

  q->count = 0;
  q->first = NULL;
  q->last = NULL;

  return q;
}

void queue_delete(queue_t *q)
{
  queue_item_t *np = q->first;

  while(np != NULL)
  {
    queue_item_t *temp = np->next;
    //free(np);
    np = temp;
  }

  //free(q);
}

unsigned int queue_count(queue_t *q)
{
  return q->count;
}

queue_item_t *queue_dequeue(queue_t *q)
{
  queue_item_t *temp = q->first;
  q->first = q->first->next;
  q->count--;
  return temp;
}

void queue_enqueue(queue_t *q, void *value)
{
  if(!q->first)
  {
    q->first = (queue_item_t *)GC_MALLOC(sizeof(queue_item_t));
    q->first->data = value;
    q->first->next = NULL;
    q->last = q->first;
  }
  else
  {
    queue_item_t *new = (queue_item_t *)GC_MALLOC(sizeof(queue_item_t));
    new->data = value;
    new->next = NULL;
    q->last->next = new;
    q->last = new;
  }
  q->count++;
}

int queue_item_exists(queue_t *q, void *value)
{
  queue_item_t *np = q->first;

  while(np != NULL)
  {
    if(np->data == value)
      return 1;

    np = np->next;
  }

  return 0;
}

int queue_is_empty(queue_t *q)
{
  return q->count == 0;
}

#ifdef TEST_QUEUE
int main(int argc, char **argv)
{
  queue_t *q = queue_create();

  int i;

  for(i=0; i< 100; i++)
    queue_enqueue(q, (void *)i);

  assert(queue_count(q) == 100);

  for(i=0; i< 100; i++)
    assert(queue_item_exists(q, (void *)i));

  for(i=0; i<100; i++)
  {
    queue_item_t *item = queue_dequeue(q);
    assert((int)item->data == i);
    //free(item);
  }

  assert(queue_is_empty(q));

  queue_delete(q);

  return 0;
}
#endif
