/**
  Copyright 2011-2021 Rajesh Jayaprakash <rajesh.jayaprakash@gmail.com>

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

#ifndef JIT_H
#define JIT_H

#include <stdint.h>

typedef uintptr_t OBJECT_PTR;

typedef OBJECT_PTR (*nativefn)(OBJECT_PTR, ...);

void initializeJIT();
nativefn get_function(void *, const char *);
void cleanupJIT(void *);
void *compile_functions_from_string(const char *);

#endif
