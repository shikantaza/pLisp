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

#ifndef MEMORY_H
#define MEMORY_H 

#define GC_FREQUENCY 100

int initialize_memory();

void cleanup_memory();

uintptr_t object_alloc(int);

#ifndef DEBUG_MEMORY
inline
#endif
void set_heap(uintptr_t, unsigned int, OBJECT_PTR);

#ifndef DEBUG_MEMORY
inline
#endif
OBJECT_PTR get_heap(uintptr_t, unsigned int);

unsigned int memory_allocated();

void test_memory();
void test_bst();

#endif
