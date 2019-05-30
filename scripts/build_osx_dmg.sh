#!/bin/sh

ver=$1

cd ../

export PATH=$PREFIX/bin:~/.local/bin:$PATH

CFLAGS="-I/usr/local/include -D__OSX_BUNDLE__"

make distclean

./configure LLVMDIR=/Users/a103035/clang+llvm-8.0.0-x86_64-apple-darwin/

sudo make install

cd osx_bundle

gtk-mac-bundler plisp.bundle

cp /usr/lib/libz.1.dylib ~/Desktop/pLisp.app/Contents/Resources/lib

cd ~/Desktop/pLisp.app/Contents/MacOS

sed -i -e 's/gtk-2.0/gtk-3.0/g' pLisp

exit
