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

#include <stdarg.h>
#include <assert.h>

#include "plisp.h"

OBJECT_PTR quote(OBJECT_PTR count, OBJECT_PTR exp)
{
  return exp;
}

OBJECT_PTR primitive_add(OBJECT_PTR count1, ...)
{
  va_list ap;
  OBJECT_PTR arg;
  int i, sum=0;

  unsigned int count = get_int_value(count1);

  assert(count >= 2); //TODO: handle this via error()

  va_start(ap, count1);

  for(i=0; i<count; i++)
  {
    arg = (OBJECT_PTR)va_arg(ap, int);

    if(!IS_INTEGER_OBJECT(arg))
    {
      print_object(arg);
      assert(false);
    }

    sum += get_int_value(arg);
  }

  va_end(ap);

  return convert_int_to_object(sum);  
}
