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

AM_YFLAGS = -d

all:	${bin_PROGRAMS} ${lib_LTLIBRARIES} help.html

bin_PROGRAMS = plisp

plisp_SOURCES = src/hashtable.h src/util.c src/images.c src/json.h src/hashtable.c src/test_so.c src/queue.c src/main.c src/memory.c src/native_compiler.c src/full_monty_compiler.c src/vm.c  src/compiler.c src/ffi.c src/gui/event_handlers.c src/gui/ui.c src/gui/indent.c src/gui/help.c src/native_primitives.c src/plisp.h src/memory.h src/rb/misc.h src/rb/misc.c src/rb/stack.h src/rb/red_black_tree.h src/rb/stack.c src/rb/red_black_tree.c src/util.h src/queue.h src/plisp.l src/plisp_parser.y src/jsonlib.c src/json.l src/json_parser.y

noinst_PROGRAMS = docgen

docgen_SOURCES = tools/docgen.c src/util.c src/util.h src/json.h src/jsonlib.c src/json.l src/json_parser.y

AM_CFLAGS = -DDATADIR=\"$(pkgdatadir)\" ${gtk_CFLAGS} ${gtksourceview_CFLAGS} ${libffi_CFLAGS} -O0
AM_LIBS = ${gtk_LIBS} ${gtksourceview_LIBS} ${libffi_LIBS} ${LEXLIB}

ACLOCAL_AMFLAGS = -I m4 --install

plisp_LDADD = ${AM_LIBS} -ltcc

lib_LTLIBRARIES = libplisp.la
libplisp_la_SOURCES = src/plisp_utils_ffi.c
libplisp_la_LDFLAGS = -version-info 0:0:0

pkgdata_DATA = data/plisp.lang data/help.json lib/plisp_full_monty_compiler.lisp

dist_doc_DATA = doc/help.html doc/pLisp_User_Manual.pdf

iconsdir = $(pkgdatadir)/icons
icons_DATA  = icons/abort.png icons/clear.png icons/export_package.png icons/refresh.png icons/accept.png  \
	      icons/delete.png icons/load_file.png icons/resume.png icons/browser.png icons/evaluate.png icons/load_image.png \
              icons/save_image.png icons/callers.png icons/exit32x32.png icons/new_package.png icons/workspace.png \
              icons/clear32x32.png icons/exit.png icons/new_symbol.png

src/json.c:	src/json.l
	$(LEX) --prefix=json -o src/json.c src/json.l

src/json_parser.c:	src/json_parser.y
	$(YACC) -d -v --name-prefix=json src/json_parser.y -o src/json_parser.c

help.html:	docgen
	./docgen
