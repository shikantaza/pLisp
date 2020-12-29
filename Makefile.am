# Copyright 2011-2020 Rajesh Jayaprakash <rajesh.jayaprakash@gmail.com>

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

all:	${bin_PROGRAMS} ${noinst_PROGRAMS} ${lib_LTLIBRARIES} help.html

bin_PROGRAMS = plisp

if !WINDOWS
if !LINUXI686
BUILT_SOURCES = libplispjitllvm.la
endif
endif

plisp_SOURCES = src/hashtable.h src/util.c src/images.c src/json.h src/hashtable.c src/queue.c src/main.c src/memory.c src/compiler.c src/ffi.c src/gui/event_handlers.c src/gui/ui.c src/gui/indent.c src/gui/help.c src/native_primitives.c src/plisp.h src/memory.h src/util.h src/queue.h src/plisp_parser.y src/plisp.l src/jsonlib.c src/json_parser.y src/json.l src/gui/file_browser.c src/plisp_config.c src/metacont.c src/jit.h src/gui/object_inspector.c src/stepper.c src/stack.c src/gui/stepper_gui.c

if WINDOWS
plisp_SOURCES += src/jit.c
endif

if LINUXI686
plisp_SOURCES += src/jit.c
endif
noinst_PROGRAMS = docgen

docgen_SOURCES = tools/docgen.c src/util.c src/util.h src/json.h src/jsonlib.c src/json.l src/json_parser.y

docgen_LDADD = -lgc $(LIBS)

AM_CFLAGS = -DPLISPDATADIR=\"$(pkgdatadir)\" ${gtk_CFLAGS} ${gtksourceview_CFLAGS} ${libffi_CFLAGS}

#set the --export-dynamic flag only for Linux (needed for LLVM)
if LINUX
AM_CFLAGS += -Wl,--export-dynamic
endif

if WINDOWS
AM_CFLAGS += -DJIT_TCC
endif

if LINUXI686
AM_CFLAGS += -DJIT_TCC
endif

AM_LIBS = ${gtk_LIBS} ${gtksourceview_LIBS} ${libffi_LIBS} ${LEXLIB}

ACLOCAL_AMFLAGS = -I m4 --install

if WINDOWS
plisp_LDADD = ${AM_LIBS} -ltcc -lgc $(LIBS)
lib_LTLIBRARIES = libplisp.la libtest.la
else
if LINUXI686
plisp_LDADD = ${AM_LIBS} -ltcc -lgc $(LIBS)
lib_LTLIBRARIES = libplisp.la libtest.la
else
plisp_LDADD = ${AM_LIBS} -lgc $(LIBS) libplispjitllvm.la
lib_LTLIBRARIES = libplisp.la libtest.la libplispjitllvm.la
endif
endif

libplisp_la_SOURCES = src/plisp_utils_ffi.c
libplisp_la_LDFLAGS = -no-undefined -avoid-version

libtest_la_SOURCES = src/test_so.c
libtest_la_LDFLAGS = -no-undefined -avoid-version

if !WINDOWS
if !LINUXI686
clang_LIBS = -lclangCodeGen  -lclangFrontend -lclangSerialization -lclangDriver -lclangParse -lclangSema -lclangAnalysis -lclangEdit -lclangAST -lclangLex -lclangBasic
libplispjitllvm_la_SOURCES = src/jit_llvm.cpp
libplispjitllvm_la_CXXFLAGS = -std=c++14 -fPIC -fno-rtti `$(LLVMDIR)/bin/llvm-config --cflags` 
libplispjitllvm_la_LDFLAGS = `$(LLVMDIR)/bin/llvm-config --ldflags` -no-undefined -avoid-version $(clang_LIBS) `$(LLVMDIR)/bin/llvm-config --libs` -lz -lncurses -pthread -ldl
endif
endif

pkgdata_DATA = data/plisp.lang data/help.json lib/plisp_full_monty_compiler.lisp

dist_pkgdata_DATA = ${pkgdata_DATA}

dist_doc_DATA = doc/help.html doc/pLisp_User_Manual.pdf doc/tutorial.lisp

iconsdir = $(pkgdatadir)/icons
icons_DATA  = icons/abort.png icons/clear.png icons/export_package.png icons/refresh.png icons/accept.png  \
	      icons/delete.png icons/load_file.png icons/resume.png icons/browser.png icons/evaluate.png icons/load_image.png \
              icons/save_image.png icons/callers.png icons/exit32x32.png icons/new_package.png icons/workspace.png \
              icons/clear32x32.png icons/exit.png icons/new_symbol.png icons/close_file.png icons/file_browser.png \
              icons/new_file.png icons/open_file.png icons/save_file.png icons/find.png icons/rename.png icons/splash.png \
              icons/step.png icons/step_over.png

dist_icons_DATA = ${icons_DATA}

src/json.c:	src/json.l
	$(LEX) --prefix=json -o src/json.c src/json.l

src/json_parser.c:	src/json_parser.y
	$(YACC) -d -v --name-prefix=json src/json_parser.y -o src/json_parser.c

help.html:	docgen
	./docgen
