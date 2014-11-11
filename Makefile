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

OBJS	= json_parser.o json_lex.o bison.o lex.o main.o util.o memory.o images.o ffi.o compiler.o vm.o red_black_tree.o stack.o misc.o ui.o event_handlers.o json.o queue.o hashtable.o native_compiler.o

#TEST_MEMORY_OBJS	= test_memory.o memory.o red_black_tree.o stack.o misc.o

CC	= gcc
CFLAGS	= -g `pkg-config --cflags libffi` `pkg-config --cflags gtk+-3.0` -I/usr/local/include -L/usr/local/lib

all:	plisp libplisp.so libtest.so

libplisp.so:	plisp_utils_ffi.o
		$(CC) $(CFLAGS) -shared -Wl,-soname,libplisp.so -o libplisp.so plisp_utils_ffi.o `pkg-config --libs libffi` `pkg-config --libs gtk+-3.0`

plisp_utils_ffi.o:	plisp_utils_ffi.c
		$(CC) $(CFLAGS) -c plisp_utils_ffi.c -o plisp_utils_ffi.o

libtest.so:	test_so.o
		$(CC) -shared -Wl,-soname,libtest.so -o libtest.so test_so.o

#test_memory:	$(TEST_MEMORY_OBJS)
#		$(CC) $(CFLAGS) $(TEST_MEMORY_OBJS) -o test_memory

plisp:	$(OBJS)
		$(CC) $(CFLAGS) $(OBJS) -ltcc -ldl -o plisp `pkg-config --libs libffi` `pkg-config --libs gtk+-3.0`

lex.o:	lex.yy.c
		$(CC) $(CFLAGS) -c lex.yy.c -o lex.o

lex.yy.c:	plisp.lex 
		flex plisp.lex

bison.o:	plisp.tab.c
		$(CC) $(CFLAGS) -c plisp.tab.c -o bison.o

plisp.tab.c:	plisp.y
		bison -d -v plisp.y

json_parser.o:	json_parser.tab.c
		$(CC) $(CFLAGS) -c json_parser.tab.c -o json_parser.o

json_parser.tab.c:	json_parser.y
		bison -d -v --name-prefix=json json_parser.y

json_lex.o:	json.lex.yy.c
		$(CC) $(CFLAGS) -c json.lex.yy.c -o json_lex.o

json.lex.yy.c:	json.lex 
		flex -P json -o json.lex.yy.c json.lex

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

red_black_tree.o:	./rb/red_black_tree.c
		$(CC) $(CFLAGS) -c ./rb/red_black_tree.c -o red_black_tree.o

stack.o:	./rb/stack.c
		$(CC) $(CFLAGS) -c ./rb/stack.c -o stack.o

misc.o:		./rb/misc.c
		$(CC) $(CFLAGS) -c ./rb/misc.c -o misc.o

ffi.o:		ffi.c
		$(CC) $(CFLAGS) -c ffi.c -o ffi.o

ui.o:		./gui/ui.c
		$(CC) $(CFLAGS) `pkg-config --cflags gtk+-3.0` -c ./gui/ui.c -o ui.o

event_handlers.o:	./gui/event_handlers.c
		$(CC) $(CFLAGS) `pkg-config --cflags gtk+-3.0` -c ./gui/event_handlers.c -o event_handlers.o

hash.o:		hash.c
		$(CC) $(CFLAGS) -c hash.c -o hash.o

json.o:		json.c
		$(CC) $(CFLAGS) -c json.c -o json.o

queue.o:	queue.c
		$(CC) $(CFLAGS) -c queue.c -o queue.o

hashtable.o:	hashtable.c
		$(CC) $(CFLAGS) -c hashtable.c -o hashtable.o

native_compiler.o:	native_compiler.c
		$(CC) $(CFLAGS) -c native_compiler.c -o native_compiler.o

test_so.o:	test_so.c
		$(CC) -c test_so.c -o test_so.o

#test_memory.o:	test_memory.c
#		$(CC) -c test_memory.c -o test_memory.o

bison.o		: plisp.tab.c plisp.h util.h
lex.o		: plisp.tab.h plisp.h
main.o		: plisp.h memory.h
compiler.o	: plisp.h
vm.o		: plisp.h memory.h
util.o		: util.h
images.o	: plisp.h memory.h queue.h json.h hashtable.h
ffi.o		: plisp.h
memory.o	: plisp.h memory.h

ui.o		: plisp.h

event_handlers.o	: plisp.h

#json_lex.o	: json_parser.tab.h json.h
json_parser.o	: json_parser.tab.c json.h
json.o		: json.h

hashtable.o	: hashtable.h

native_compiler.o	: plisp.h
clean:
	rm -f *.o *~ lex.yy.c plisp.tab.c plisp.tab.h json.lex.yy.c json_parser.tab.h json_parser.tab.c plisp.output json_parser.output plisp.exe libplisp.so libtest.so *.stackdump

