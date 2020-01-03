/**
  Copyright 2011-2020 Rajesh Jayaprakash <rajesh.jayaprakash@gmail.com>

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

#include "stack.h"

stack_type *stack_create()
{
  stack_type *s = (stack_type *)GC_MALLOC(sizeof(stack_type));

  s->count = 0;
  s->data = NULL;

  return s;
}

void stack_push(stack_type *s, void *data)
{
  s->count++;
  if(s->data == NULL)
    s->data = (void **)GC_MALLOC(s->count * sizeof(void *));
  else
  {
    void **temp = (void **)GC_REALLOC(s->data, s->count * sizeof(void *));
    assert(temp);
    s->data = temp;
  }

  s->data[s->count-1] = data;
}

void *stack_top(stack_type *s)
{
  assert(s->count > 0);
  return s->data[s->count-1];
}

int stack_is_empty(stack_type *s)
{
  if(s->count == 0)
    return 1;
  else
    return 0;
}

void *stack_pop(stack_type *s)
{
  void *ret = stack_top(s);

  s->count--;

  if(s->count)
  {
    void **temp = (void **)GC_REALLOC(s->data, s->count * sizeof(void *));
    assert(temp);
    s->data = temp;
  }
  else
    s->data = NULL;

  return ret;
}

unsigned int stack_count(stack_type *s)
{
  return s->count;
}

void stack_empty(stack_type *s)
{
  s->count = 0;
  s->data = NULL;
}

//non-stack behavior (for convenience)
void **stack_data(stack_type *s)
{
  return s->data;
}
//end of non-stack behaviour

#ifdef TEST_STACK
int main(int argc, char **argv)
{
  stack_type *s = stack_create();
  assert(stack_is_empty(s));

  stack_push(s, (void *)1);
  assert(stack_count(s) == 1);

  int v = (int)stack_top(s);
  assert(v == 1);

  v = (int)stack_pop(s);
  assert(v == 1);
  assert(stack_is_empty(s));

  int i;

  for(i=0; i<100; i++)
    stack_push(s, (void *)i);

  assert(stack_count(s) == 100);

  for(i=0; i<100; i++)
    assert((int)stack_pop(s) == 99 - i);

  assert(stack_is_empty(s));

  for(i=0; i<100; i++)
    stack_push(s, (void *)i);

  stack_empty(s);

  assert(stack_is_empty(s));
}
#endif
