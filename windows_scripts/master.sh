#!/bin/sh

sh makewin.sh
sh build_windows_dist.sh 0.1
sh deploy_win.sh /cygdrive/c/sw 0.1
