# Copyright 2011-2013 Rajesh Jayaprakash <rajesh.jayaprakash@gmail.com>

# This file is part of pLisp.

# pLisp is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# pLisp is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with pLisp.  If not, see <http://www.gnu.org/licenses/>.

OBJS	= bison.o lex.o main.o util.o memory.o images.o ffi.o compiler.o vm.o tpl.o mmap.o red_black_tree.o stack.o misc.o

CC	= gcc
CFLAGS	= -g -I/usr/local/lib/libffi-3.0.13/include -L/usr/local/lib/

all:	plisp libplisp.so libtest.so

libplisp.so:	plisp_utils_ffi.o
		$(CC) -shared -W1,-soname,libplisp.so -o libplisp.so plisp_utils_ffi.o

plisp_utils_ffi.o:	plisp_utils_ffi.c
		$(CC) -c -fPIC plisp_utils_ffi.c -o plisp_utils_ffi.o

libtest.so:	test_so.o
		$(CC) -shared -W1,-soname,libtest.so -o libtest.so test_so.o

test_so.o:	test_so.c
		$(CC) -c -fPIC test_so.c -o test_so.o

plisp:	$(OBJS)
		$(CC) $(CFLAGS) $(OBJS) -o plisp -lffi

lex.o:	lex.yy.c
		$(CC) $(CFLAGS) -c lex.yy.c -o lex.o

lex.yy.c:	plisp.lex 
		flex plisp.lex

bison.o:	plisp.tab.c
		$(CC) $(CFLAGS) -c plisp.tab.c -o bison.o

plisp.tab.c:	plisp.y
		bison -d -v plisp.y

main.o:		main.c
		$(CC) $(CFLAGS) -c main.c -o main.o

compiler.o:	compiler.c
		$(CC) $(CFLAGS) -c compiler.c -o compiler.o

vm.o:		vm.c
		$(CC) $(CFLAGS) -c vm.c -o vm.o

util.o:		util.c
		$(CC) $(CFLAGS) -c util.c -o util.o

memory.o:	memory.c
		$(CC) $(CFLAGS) -c memory.c -o memory.o

images.o:	images.c
		$(CC) $(CFLAGS) -c images.c -o images.o

tpl.o:		./tpl/tpl.c
		$(CC) $(CFLAGS) -c ./tpl/tpl.c -o tpl.o

mmap.o:		./tpl/win/mmap.c
		$(CC) $(CFLAGS) -c ./tpl/win/mmap.c -o mmap.o

red_black_tree.o:	./rb/red_black_tree.c
		$(CC) $(CFLAGS) -c ./rb/red_black_tree.c -o red_black_tree.o

stack.o:	./rb/stack.c
		$(CC) $(CFLAGS) -c ./rb/stack.c -o stack.o

misc.o:		./rb/misc.c
		$(CC) $(CFLAGS) -c ./rb/misc.c -o misc.o

ffi.o:		ffi.c
		$(CC) $(CFLAGS) -c ffi.c -o ffi.o

bison.o		: plisp.tab.c plisp.h util.h
lex.o		: plisp.tab.h plisp.h
main.o		: plisp.h memory.h
compiler.o	: plisp.h
vm.o		: plisp.h memory.h
util.o		: util.h
images.o	: plisp.h memory.h tpl/tpl.h
ffi.o		: plisp.h
memory.o	: plisp.h memory.h

tpl.o		: tpl/tpl.h
mmap.o		: tpl/win/mman.h

clean:
	rm -f *.o *~ lex.yy.c plisp.tab.c plisp.tab.h plisp.output plisp.exe libplisp.so libtest.so *.stackdump

