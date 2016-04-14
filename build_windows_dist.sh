#!/bin/sh

rm -rf $1

mkdir $1
mkdir $1/bin
mkdir $1/doc
mkdir $1/lib
mkdir -p $1/share/icons
mkdir $1/tests

cp ./plisp.exe $1/bin
cp ./libplisp.dll $1/bin
cp ./libtest.dll $1/bin
cp ./plisp.image $1/bin

cp ./doc/help.html $1/doc
cp ./doc/pLisp_User_Manual.pdf $1/doc

cp ./lib/plisp_full_monty_compiler_windows.lisp $1/lib
cp ./lib/libtcc1.a $1/lib
cp ./icons/* $1/share/icons

cp ./data/help.json $1/share
cp ./data/plisp.lang $1/share

cp ./tests/unit_tests_windows.lisp $1/tests
cp ./tests/run_test_cases_windows.lisp $1/tests

tar czvf plisp.tar.gz $1/*

#rm -rf $1
