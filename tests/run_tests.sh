#!/bin/sh
plisp -e '(progn (load-file "tests/unit_tests.lisp") (load-file "tests/run_test_cases.lisp"))'
