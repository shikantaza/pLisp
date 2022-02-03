/**
  Copyright 2011-2022 Rajesh Jayaprakash <rajesh.jayaprakash@pm.me>

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

typedef struct {
  unsigned int count;
  void **data;
} stack_type;

stack_type *stack_create();
void *stack_pop(stack_type *);
void *stack_top(stack_type *);
void stack_push(stack_type *, void *);
unsigned int stack_count(stack_type *);
int stack_is_empty(stack_type *);
void stack_empty(stack_type *);

//non-stack behavior (for convenience)
void **stack_data(stack_type *);
