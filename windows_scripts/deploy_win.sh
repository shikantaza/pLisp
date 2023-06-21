#/bin/sh

verno=$2
rm -rf $1/plisp-$verno
unzip ../plisp_windows-$verno.zip -d $1
