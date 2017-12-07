#!/bin/sh
ver=$1
arch=amd64
pkg=plisp_$ver-1_$arch

mkdir $pkg

mkdir -p $pkg/usr/local/bin
mkdir -p $pkg/usr/local/lib
mkdir -p $pkg/usr/local/lib/tcc
mkdir -p $pkg/usr/local/share/plisp/icons
mkdir -p $pkg/usr/local/share/doc/plisp

cp /usr/local/lib/tcc/libtcc1.a $pkg/usr/local/lib/tcc

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
cp control_$arch $pkg/DEBIAN/control

dpkg-deb --build $pkg

mv $pkg.deb ../downloads/linux

rm -rf $pkg
