/**
  Copyright 2011-2013 Rajesh Jayaprakash <rajesh.jayaprakash@gmail.com>

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
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <time.h>

#include "hashtable.h"

#define HASHSIZE 1000001

static inline unsigned hash(void * val) { return (unsigned int)val % HASHSIZE; }

hashtable_entry_t *hashtable_get(hashtable_t *hashtab, void *ptr)
{
  hashtable_entry_t *np;

  for(np = hashtab[hash(ptr)]; np != NULL; np = np->next)
  {
    if(np->ptr == ptr)
      return np;
  }

  return NULL;
}

hashtable_entry_t *hashtable_put(hashtable_t *hashtab, void *ptr, void *value)
{
  hashtable_entry_t *np;
  unsigned hashval;

  if((np = hashtable_get(hashtab, ptr)) == NULL)
  {
    np = (hashtable_entry_t *) malloc(sizeof(*np));
    if(np == NULL)
      return NULL;

    hashval = hash(ptr);
    np->next = hashtab[hashval];
    np->ptr = ptr;
    np->value = value;
    hashtab[hashval] = np;
  }

  return np;
}

void hashtable_remove(hashtable_t *hashtab, void *ptr)
{
  hashtable_entry_t *np, *prev = NULL;

  for(np = hashtab[hash(ptr)]; np != NULL; np = np->next)
  {
    if(np->ptr == ptr)
    {
      if(prev)
        prev->next = np->next;
      else
        hashtab[hash(ptr)] = np->next;

      free(np);
      return;
    }
    prev = np;
  }

  printf("%d does not exist\n", ptr);
}

hashtable_t *hashtable_create()
{
  return (hashtable_t *)malloc(HASHSIZE * sizeof(hashtable_t));
}

void hashtable_delete(hashtable_t *tab)
{
  int i;
  hashtable_entry_t *np;

  for(i=0; i<HASHSIZE; i++)
  {
    np = tab[i];

    while(np != NULL)
    {
      hashtable_entry_t *temp = np->next;
      free(np);
      np = temp;
    }
  }

  free(tab);
}

#ifdef TEST_HASH

#define SIZE 1000000

int main(int argc, char **argv)
{
  int *vals = (int *)malloc(SIZE * sizeof(int));

  int i;

  for(i=0;i<SIZE; i++)
    vals[i] = i;

  hashtable_t *tab = hashtable_create();

  clock_t start, diff;
  int msec;

  start = clock();

  for(i=0; i<SIZE; i++)
    hashtable_put(tab, (void *)(vals[i]), (void *)(i*i));

  diff = clock() - start;
  msec = diff * 1000 / CLOCKS_PER_SEC;
  printf("Put took %d seconds %d milliseconds\n", msec/1000, msec%1000);

  start = clock();

  for(i=0; i<SIZE; i++)
  {
    hashtable_entry_t *b = hashtable_get(tab, (void *)(vals[i]));
    assert((int)(b->ptr) == i);
    assert((int)(b->value) == i*i);
  }

  diff = clock() - start;
  msec = diff * 1000 / CLOCKS_PER_SEC;
  printf("Get took %d seconds %d milliseconds\n", msec/1000, msec%1000);

  start = clock();

  hashtable_remove(tab, (void *)(vals[SIZE/2]));

  diff = clock() - start;
  msec = diff * 1000 / CLOCKS_PER_SEC;
  printf("Remove took %d seconds %d milliseconds\n", msec/1000, msec%1000);

  assert(hashtable_get(tab, (void *)(vals[SIZE/2])) == NULL);

  hashtable_remove(tab, (void *)(vals[0]));

  free(vals);
  hashtable_delete(tab);
}

#endif
