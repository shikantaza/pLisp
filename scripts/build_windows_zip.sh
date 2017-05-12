#!/bin/sh

tempdir=$1

verno=$2

dir=plisp-windows-$verno

rm -rf $dir

mkdir $dir
mkdir $dir/bin
mkdir $dir/doc
mkdir $dir/lib
mkdir -p $dir/share/icons

cp $tempdir/bin/* $dir/bin
cp /usr/local/bin/libgc-1.dll $dir/bin

cp $tempdir/share/doc/plisp/help.html $dir/doc
cp $tempdir/share/doc/plisp/pLisp_User_Manual.pdf $dir/doc
cp $tempdir/share/doc/plisp/tutorial.lisp $dir/doc

cp $tempdir/share/plisp/plisp_full_monty_compiler.lisp $dir/lib
cp /usr/local/lib/libtcc1.a $dir/lib
cp $tempdir/share/plisp/icons/* $dir/share/icons

cp $tempdir/share/plisp/help.json $dir/share
cp $tempdir/share/plisp/plisp.lang $dir/share

zip -r ../downloads/windows/$dir.zip $dir/*

rm -rf $dir
