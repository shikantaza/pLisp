#!/bin/sh

tempdir=$1

verno=$2

dir=plisp-windows-$verno

export CFLAGS="-I/c/sw/tcc/libtcc -DWIN_BUILD -g -O0"
export LD_FLAGS="-L/c/sw/tcc/libtcc"

cd ..

rm -rf $tempdir/bin
rm -rf $tempdir/lib
rm -rf $tempdir/share

./configure --prefix=$tempdir
make clean
make
make install

cd scripts

rm -rf $dir

mkdir $dir
mkdir $dir/bin
mkdir $dir/doc
mkdir $dir/lib
mkdir -p $dir/share/icons

cp $tempdir/bin/* $dir/bin
cp /mingw32/bin/libgc-1.dll $dir/bin

cp $tempdir/share/doc/plisp/help.html $dir/doc
cp $tempdir/share/doc/plisp/pLisp_User_Manual.pdf $dir/doc
cp $tempdir/share/doc/plisp/tutorial.lisp $dir/doc

cp $tempdir/share/plisp/plisp_full_monty_compiler.lisp $dir/lib
cp /c/sw/tcc//lib/libtcc1.a $dir/lib
cp $tempdir/share/plisp/icons/* $dir/share/icons

cp $tempdir/share/plisp/help.json $dir/share
cp $tempdir/share/plisp/plisp.lang $dir/share

zip -r ../downloads/windows/$dir.zip $dir/*

rm -rf $dir

rm -rf $tempdir/bin
rm -rf $tempdir/lib
rm -rf $tempdir/share
