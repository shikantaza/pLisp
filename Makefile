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

CC	= gcc
CFLAGS	= -g `pkg-config --cflags libffi` `pkg-config --cflags gtk+-3.0` `pkg-config --cflags gtksourceview-3.0`-I/usr/local/include -L/usr/local/lib

all:	plisp libplisp.so libtest.so

libplisp.so:	plisp_utils_ffi.o
		$(CC) $(CFLAGS) -shared -Wl,-soname,libplisp.so -o libplisp.so plisp_utils_ffi.o `pkg-config --libs libffi` `pkg-config --libs gtk+-3.0`

plisp_utils_ffi.o:	src/plisp_utils_ffi.c
		$(CC) $(CFLAGS) -c src/plisp_utils_ffi.c -o plisp_utils_ffi.o

libtest.so:	test_so.o
		$(CC) -shared -Wl,-soname,libtest.so -o libtest.so test_so.o

plisp:	$(OBJS)
		$(CC) $(CFLAGS) $(OBJS) -ltcc -ldl -o plisp `pkg-config --libs libffi` `pkg-config --libs gtk+-3.0` `pkg-config --libs gtksourceview-3.0`

lex.o:	src/lex.yy.c
		$(CC) $(CFLAGS) -c src/lex.yy.c -o lex.o

src/lex.yy.c:	src/plisp.lex 
		flex -o src/lex.yy.c src/plisp.lex

bison.o:	src/plisp.tab.c
		$(CC) $(CFLAGS) -c src/plisp.tab.c -o bison.o

src/plisp.tab.c:	src/plisp.y
		bison -d -v src/plisp.y -o src/plisp.tab.c

json_parser.o:	src/json_parser.tab.c
		$(CC) $(CFLAGS) -c src/json_parser.tab.c -o json_parser.o

src/json_parser.tab.c:	src/json_parser.y
		bison -d -v --name-prefix=json src/json_parser.y -o src/json_parser.tab.c

json_lex.o:	src/json.lex.yy.c
		$(CC) $(CFLAGS) -c src/json.lex.yy.c -o json_lex.o

src/json.lex.yy.c:	src/json.lex 
		flex -P json -o src/json.lex.yy.c src/json.lex

main.o:		src/main.c
		$(CC) $(CFLAGS) -c src/main.c -o main.o

compiler.o:	src/compiler.c
		$(CC) $(CFLAGS) -c src/compiler.c -o compiler.o

vm.o:		src/vm.c
		$(CC) $(CFLAGS) -c src/vm.c -o vm.o

util.o:		src/util.c
		$(CC) $(CFLAGS) -c src/util.c -o util.o

memory.o:	src/memory.c
		$(CC) $(CFLAGS) -c src/memory.c -o memory.o

images.o:	src/images.c
		$(CC) $(CFLAGS) -c src/images.c -o images.o

red_black_tree.o:	src/rb/red_black_tree.c
		$(CC) $(CFLAGS) -c src/rb/red_black_tree.c -o red_black_tree.o

stack.o:	src/rb/stack.c
		$(CC) $(CFLAGS) -c src/rb/stack.c -o stack.o

misc.o:		src/rb/misc.c
		$(CC) $(CFLAGS) -c src/rb/misc.c -o misc.o

ffi.o:		src/ffi.c
		$(CC) $(CFLAGS) -c src/ffi.c -o ffi.o

ui.o:		src/gui/ui.c
		$(CC) $(CFLAGS) `pkg-config --cflags gtk+-3.0` -c src/gui/ui.c -o ui.o

event_handlers.o:	src/gui/event_handlers.c
		$(CC) $(CFLAGS) `pkg-config --cflags gtk+-3.0` -c src/gui/event_handlers.c -o event_handlers.o

hash.o:		src/hash.c
		$(CC) $(CFLAGS) -c src/hash.c -o hash.o

json.o:		src/json.c
		$(CC) $(CFLAGS) -c src/json.c -o json.o

queue.o:	src/queue.c
		$(CC) $(CFLAGS) -c src/queue.c -o queue.o

hashtable.o:	src/hashtable.c
		$(CC) $(CFLAGS) -c src/hashtable.c -o hashtable.o

native_compiler.o:	src/native_compiler.c
		$(CC) $(CFLAGS) -c src/native_compiler.c -o native_compiler.o

test_so.o:	src/test_so.c
		$(CC) -c src/test_so.c -o test_so.o

bison.o		: src/plisp.tab.c src/plisp.h src/util.h
lex.o		: src/plisp.tab.h src/plisp.h
main.o		: src/plisp.h src/memory.h
compiler.o	: src/plisp.h
vm.o		: src/plisp.h src/memory.h
util.o		: src/util.h
images.o	: src/plisp.h src/memory.h src/queue.h src/json.h src/hashtable.h
ffi.o		: src/plisp.h
memory.o	: src/plisp.h src/memory.h

ui.o		: src/plisp.h

event_handlers.o	: src/plisp.h

#json_lex.o	: json_parser.tab.h json.h
json_parser.o	: src/json_parser.tab.c src/json.h
json.o		: src/json.h

hashtable.o	: src/hashtable.h

native_compiler.o	: src/plisp.h
clean:
	rm -f *.o *~ src/lex.yy.c src/plisp.tab.c src/plisp.tab.h src/json.lex.yy.c src/json_parser.tab.h src/json_parser.tab.c src/plisp.output src/json_parser.output plisp libplisp.so libtest.so *.stackdump

