#!/bin/sh
ver=$1
pkg=plisp_$ver-1_i386

mkdir $pkg

mkdir -p $pkg/usr/local/bin
mkdir -p $pkg/usr/local/lib
mkdir -p $pkg/usr/local/share/plisp/icons
mkdir -p $pkg/usr/local/share/doc/plisp

cp ../plisp $pkg/usr/local/bin

cp ../.libs/libplisp.a $pkg/usr/local/lib
cp ../libplisp.la $pkg/usr/local/lib
cp ../.libs/libplisp.so $pkg/usr/local/lib

cp ../data/help.json $pkg/usr/local/share/plisp
cp ../lib/plisp_full_monty_compiler.lisp $pkg/usr/local/share/plisp
cp ../data/plisp.lang $pkg/usr/local/share/plisp
cp ../icons/* $pkg/usr/local/share/plisp/icons

cp ../doc/help.html $pkg/usr/local/share/doc/plisp
cp ../doc/pLisp_User_Manual.pdf $pkg/usr/local/share/doc/plisp
cp ../doc/tutorial.lisp $pkg/usr/local/share/doc/plisp

mkdir -p $pkg/DEBIAN
cp control $pkg/DEBIAN

dpkg-deb --build $pkg

mv $pkg.deb ../downloads/linux

rm -rf $pkg
