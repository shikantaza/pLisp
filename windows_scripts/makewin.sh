#!/bin/sh
cd ..
export PKG_CONFIG_PATH=/opt/gtk3-win32/lib/pkgconfig/:/opt/gtksourceview3/lib/pkgconfig
make -f Makefile.windows

