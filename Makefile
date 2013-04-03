# Makefile

OBJS	= bison.o lex.o main.o util.o memory.o images.o ffi.o compiler.o vm.o

CC	= gcc -DDEBUG_MEMORY
CFLAGS	= -g -I/usr/local/lib/libffi-3.0.10/include -L/usr/local/lib

all:	plisp

plisp:	$(OBJS)
		$(CC) $(CFLAGS) $(OBJS) libtpl.a -o plisp -lfl -lffi

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

ffi.o:		ffi.c
		$(CC) $(CFLAGS) -c ffi.c -o ffi.o

bison.o		: plisp.tab.c plisp.h util.h
lex.o		: plisp.tab.h plisp.h
main.o		: plisp.h
compiler.o	: plisp.h
vm.o		: plisp.h
util.o		: util.h
images.o	: plisp.h
ffi.o		: plisp.h
memory.o	: plisp.h

clean:
	rm -f *.o *~ lex.yy.c plisp.tab.c plisp.tab.h plisp.output plisp.exe *.stackdump

