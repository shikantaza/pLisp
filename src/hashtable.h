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

typedef struct hashtable_entry
{
  struct hashtable_entry *next;
  void *ptr;
  void *value;
} hashtable_entry_t;

typedef struct hashtable
{
  unsigned int count;
  hashtable_entry_t **entries;
  unsigned int hash_size;
  hashtable_entry_t *an_element;
} hashtable_t;

//typedef hashtable_entry_t * hashtable_t;

hashtable_t *hashtable_create(unsigned int);
hashtable_entry_t *hashtable_get(hashtable_t *, void *ptr);
hashtable_entry_t *hashtable_put(hashtable_t *, void *ptr, void *value);
void hashtable_remove(hashtable_t *, void *ptr);
hashtable_entry_t *hashtable_entries(hashtable_t *);
hashtable_entry_t *hashtable_get_any_element(hashtable_t *);
