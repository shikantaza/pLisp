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
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <time.h>

#include "gc.h"

#include "hashtable.h"

//#define HASHSIZE 1000001

unsigned hash(void * val, unsigned int hash_size) { return (unsigned int)val % hash_size; }

unsigned int hashtable_count(hashtable_t *tab)
{
  return tab->count;
}

hashtable_entry_t *hashtable_get(hashtable_t *hashtab, void *ptr)
{
  hashtable_entry_t *np;

  for(np = hashtab->entries[hash(ptr, hashtab->hash_size)]; np != NULL; np = np->next)
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
    np = (hashtable_entry_t *) GC_MALLOC(sizeof(*np));
    if(np == NULL)
      return NULL;

    hashval = hash(ptr, hashtab->hash_size);
    np->next = hashtab->entries[hashval];
    np->ptr = ptr;
    np->value = value;
    hashtab->entries[hashval] = np;

    hashtab->count++;
  }

  hashtab->an_element = np;

  return np;
}

void hashtable_remove(hashtable_t *hashtab, void *ptr)
{
  hashtable_entry_t *np, *prev = NULL;

  for(np = hashtab->entries[hash(ptr, hashtab->hash_size)]; np != NULL; np = np->next)
  {
    if(np->ptr == ptr)
    {
      if(prev)
        prev->next = np->next;
      else
        hashtab->entries[hash(ptr, hashtab->hash_size)] = np->next;

      if(hashtab->an_element->ptr == ptr)
      {
        if(np->next)
          hashtab->an_element = np->next;
        else
        {
          int i;

          for(i=0; i<hashtab->hash_size; i++)
          {
            if(hashtab->entries[i])
            {
              hashtab->an_element = hashtab->entries[i];
              break;
            }
          }
        }
      }

      //free(np);
      hashtab->count--;

      return;
    }
    prev = np;
  }

  //printf("%d does not exist\n", ptr);
}

hashtable_t *hashtable_create(unsigned int hash_size)
{
  hashtable_t *ret = (hashtable_t *)GC_MALLOC(sizeof(hashtable_t));

  ret->count = 0;

  ret->entries = (hashtable_entry_t **)GC_MALLOC(hash_size * sizeof(hashtable_entry_t *));
  ret->hash_size = hash_size;

  int i;

  for(i=0; i<hash_size; i++)
    ret->entries[i] = NULL;

  return ret;
}

void hashtable_delete(hashtable_t *tab)
{
  int i;
  hashtable_entry_t *np;

  for(i=0; i<tab->hash_size; i++)
  {
    np = tab->entries[i];

    while(np != NULL)
    {
      hashtable_entry_t *temp = np->next;
      //free(np);
      np = temp;
    }
  }

  //free(tab->entries);

  //free(tab);
}

hashtable_entry_t *clone_entries(hashtable_entry_t *np)
{
  hashtable_entry_t *ret = NULL, *last = NULL;

  while(np)
  {
    hashtable_entry_t *temp = (hashtable_entry_t *)GC_MALLOC(sizeof(hashtable_entry_t));

    temp->ptr = np->ptr;
    temp->value = np->value;
    temp->next = NULL;

    if(!ret)
    {
      ret = temp;
      last = temp;
    }
    else
    {
      last->next = temp;
      last = temp;
    }
    
    np = np->next;
  }

  return ret;
}

hashtable_entry_t *hashtable_entries(hashtable_t *tab)
{
  hashtable_entry_t **replica = (hashtable_entry_t **)GC_MALLOC(tab->hash_size * sizeof(hashtable_entry_t *));

  int i;

  for(i=0; i<tab->hash_size; i++)
  {
    if(tab->entries[i] != NULL)
      replica[i] = clone_entries(tab->entries[i]);
    else
      replica[i] = NULL;
  }

  hashtable_entry_t *prev = NULL, *t, *ret = NULL;

  for(i=0; i< tab->hash_size; i++)
  {
    if(replica[i])
    {
      if(!ret)
        ret = replica[i];

      if(prev)
        prev->next = replica[i];

      t = replica[i];

      while(t->next)
        t = t->next;

      prev = t;
    }
  }

  return ret;
}

hashtable_entry_t *hashtable_get_any_element(hashtable_t *tab)
{
  /* int i; */

  /* for(i=0; i<tab->hash_size; i++) */
  /*   if(tab->entries[i]) return tab->entries[i]; */

  /* return NULL; */
  return tab->an_element;
}

#ifdef TEST_HASH

#define SIZE 1000000

int main(int argc, char **argv)
{
  int *vals = (int *)GC_MALLOC(SIZE * sizeof(int));

  int i;

  for(i=0;i<SIZE; i++)
    vals[i] = i;

  hashtable_t *tab = hashtable_create(1000001);

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

  assert(tab->count == SIZE);

  diff = clock() - start;
  msec = diff * 1000 / CLOCKS_PER_SEC;
  printf("Get took %d seconds %d milliseconds\n", msec/1000, msec%1000);

  start = clock();

  hashtable_remove(tab, (void *)(vals[SIZE/2]));

  assert(tab->count == SIZE-1);

  diff = clock() - start;
  msec = diff * 1000 / CLOCKS_PER_SEC;
  printf("Remove took %d seconds %d milliseconds\n", msec/1000, msec%1000);

  assert(hashtable_get(tab, (void *)(vals[SIZE/2])) == NULL);

  hashtable_remove(tab, (void *)(vals[0]));

  hashtable_delete(tab);

  tab = hashtable_create(1000001);

  for(i=0; i<SIZE; i++)
    hashtable_put(tab, (void *)vals[i], (void *)(i*i));

  hashtable_entry_t *entries = hashtable_entries(tab);

  while(entries)
  {
    assert((int)(entries->value) == (int)(entries->ptr) * (int)(entries->ptr));
    entries = entries->next;
  }

  hashtable_delete(tab);

  //free(vals);
}

#endif
