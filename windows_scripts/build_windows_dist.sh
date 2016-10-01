#!/bin/sh

verno=$1

dir=plisp-$verno

rm -rf $dir

mkdir $dir
mkdir $dir/bin
mkdir $dir/doc
mkdir $dir/lib
mkdir -p $dir/share/icons
mkdir $dir/tests

cp ../plisp.exe $dir/bin
cp ../libplisp.dll $dir/bin
cp ../libtest.dll $dir/bin

cp ../doc/help.html $dir/doc
cp ../doc/pLisp_User_Manual.pdf $dir/doc
cp ../doc/tutorial.lisp $dir/doc

cp ../lib/plisp_full_monty_compiler_windows.lisp $dir/lib
cp ../lib/primitives.lisp $dir/lib
cp ../lib/libtcc1.a $dir/lib
cp ../icons/* $dir/share/icons

cp ../data/help.json $dir/share
cp ../data/plisp.lang $dir/share

cp ../tests/unit_tests_windows.lisp $dir/tests
cp ../tests/run_test_cases_windows.lisp $dir/tests
cp ../tests/run_tests_windows.bat $dir/tests

zip -r ../plisp_windows-$verno.zip $dir/*

rm -rf $dir
