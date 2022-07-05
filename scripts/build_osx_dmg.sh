#!/bin/sh

# This script updates the pLisp.App directory structure
# with the latest version of the pLisp artifacts. The
# updated pLisp.App directory can then be packaged
# as a DMG file for deployment and distribution.

# Prerequisites:
# 1. Existing pLisp.App directory (extracted from the
#    most recent version of the DMG file)
# 2. Info.plist (in the osx_bundle directory) has been
#    updated with the latest version information

# Location of pLisp.App directory
# E.g., $HOME/temp/pLisp.App
appdir=$1

if [ ! -d "$appdir" ]; then
    echo "appdir does not exist"
    exit
fi

# Location where plisp artifacts are to be generated
# E.g., $HOME/temp/plisp
builddir=$2

if [ ! -d "$builddir" ]; then
    mkdir "$builddir"
fi

sudo rm -rf $builddir/*

cd ../

export PATH=$PREFIX/bin:~/.local/bin:$PATH

export CFLAGS="-I/opt/homebrew/include -L/opt/homebrew/lib -D__OSX_BUNDLE__"

sudo make distclean

./configure --prefix=$builddir LLVMDIR=/opt/homebrew/opt/llvm@11

sudo make install

cp $builddir/bin/plisp $appdir/Contents/MacOS/pLisp-bin

cp osx_bundle/Info-plisp.plist $appdir/Contents/Info.plist
cp osx_bundle/plisp.icns $appdir/Contents/Resources

cp $builddir/lib/libplisp.dylib $appdir/Contents/Resources/lib
cp $builddir/lib/libplispjitllvm.dylib $appdir/Contents/Resources/lib

cp $builddir/share/doc/plisp/help.html $appdir/Contents/Resources/share/doc/plisp
cp $builddir/share/doc/plisp/pLisp_User_Manual.pdf $appdir/Contents/Resources/share/doc/plisp
cp $builddir/share/doc/plisp/tutorial.lisp $appdir/Contents/Resources/share/doc/plisp

cp $builddir/share/plisp/help.json $appdir/Contents/Resources/share/plisp
cp $builddir/share/plisp/plisp.lang $appdir/Contents/Resources/share/plisp
cp $builddir/share/plisp/plisp_full_monty_compiler.lisp $appdir/Contents/Resources/share/plisp
cp $builddir/share/plisp/icons/* $appdir/Contents/Resources/share/plisp/icons

sudo rm -rf $builddir

exit
