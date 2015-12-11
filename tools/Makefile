# Copyright 2011-2015 Rajesh Jayaprakash <rajesh.jayaprakash@gmail.com>

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

OBJS	= json_lex.o json_parser.o util.o docgen.o json.o

CC	= gcc -fPIC
CFLAGS	= -g 

all:	docgen

docgen:	$(OBJS)
	$(CC) $(CFLAGS) $(OBJS) -o docgen

json_parser.o:	../src/json_parser.tab.c
		$(CC) $(CFLAGS) -c ../src/json_parser.tab.c -o json_parser.o

../src/json_parser.tab.c:	../src/json_parser.y
		bison -d -v --name-prefix=json ../src/json_parser.y -o ../src/json_parser.tab.c

json_lex.o:	../src/json.lex.yy.c
		$(CC) $(CFLAGS) -c ../src/json.lex.yy.c -o json_lex.o

../src/json.lex.yy.c:	../src/json.lex 
		flex -P json -o ../src/json.lex.yy.c ../src/json.lex

util.o:		../src/util.c
		$(CC) $(CFLAGS) -c ../src/util.c -o util.o

docgen.o:	docgen.c
		$(CC) $(CFLAGS) -c docgen.c -o docgen.o

json.o:		../src/json.c
		$(CC) $(CFLAGS) -c ../src/json.c -o json.o

docgen.o	: ../src/json.h
#json_lex.o	: json_parser.tab.h json.h
json_parser.o	: ../src/json_parser.tab.c ../src/json.h
util.o		: ../src/util.h
json.o		: ../src/json.h

clean:
	rm -f *.o *~ ../src/json.lex.yy.c ../src/json_parser.tab.h ../src/json_parser.tab.c ../src/json_parser.output docgen *.stackdump

