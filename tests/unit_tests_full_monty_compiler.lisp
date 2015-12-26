;;  Copyright 2011-2015 Rajesh Jayaprakash <rajesh.jayaprakash@gmail.com>

;;  This file is part of pLisp.

;;  pLisp is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.

;;  pLisp is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with pLisp.  If not, see <http://www.gnu.org/licenses/>.





;expand-macro
(test-exception 103 (expand-macro 1) 'invalid-argument)

(test-exception 104 (expand-macro '(QQQQ 1 2 3)) 'macro-undefined)

(let1 ((first (macro (lst) `(car ,lst)))
       (x (expand-macro '(first a))))
  (test-condition 105 (eq x '(car a))))
;end expand-macro

;apply
(test-exception 106 (apply 1 2) 'invalid-argument)

(test-exception 107 (apply car 2) 'invalid-argument)

(test-condition 108 (eq (apply car '((1 2 3))) 1))
;end apply

;string
(test-exception 109 (string 1) 'invalid-argument)

(let ((x (make-array 3)))
  (array-set x 0 #\a)
  (array-set x 1 #\b)
  (array-set x 2 #\c)
  (test-condition 110 (array-eq (string "abc") x)))
;end string

;make-array
(test-exception 111 (make-array 1.0) 'invalid-argument)

(let ((a (make-array 5)))
  (array-set a 0 10)
  (array-set a 4 "abc")
  (test-condition 112 (and (eq (array-get a 0) 10)
                           (null (array-get a 1))
                           (null (array-get a 2))
                           (null (array-get a 3))
                           (eq (array-get a 4) "abc"))))
;end make-array

;array-set
(test-exception 113 (array-set 1 2 3) 'invalid-argument)

(test-exception 114 (array-set (make-array 3) "abc" 100) 'invalid-argument)

(test-exception 115 (array-set (make-array 3) -1 100) 'index-out-of-bounds)

(test-exception 116 (array-set (make-array 3) 3 100) 'index-out-of-bounds)

(let ((a (make-array 3)))
  (array-set a 0 100)
  (array-set a 2 300)
  (test-condition 117 (and (eq (array-get a 0) 100)
                           (null (array-get a 1))
                           (eq (array-get a 2) 300))))
;end array-set

;array-get
(test-exception 118 (array-get 1 2 ) 'invalid-argument)

(test-exception 119 (array-get (make-array 3) "abc") 'invalid-argument)

(test-exception 120 (array-get (make-array 3) -1) 'index-out-of-bounds)

(test-exception 121 (array-get (make-array 3) 3) 'index-out-of-bounds)

(test-exception 122 (array-get "abc" 3) 'index-out-of-bounds)

(test-condition 123 (eq (array-get "abc" 0) #\a))

;second positive test case covered in array-set's test cases (last one)
;end array-get

;sub-array
(test-exception 124 (sub-array 1 2 3) 'invalid-argument)

(test-exception 125 (sub-array (make-array 3) "abc" 3) 'invalid-argument)

(test-exception 126 (sub-array (make-array 3) -1 3) 'invalid-argument)

(test-exception 127 (sub-array (make-array 3) 0 "abc") 'invalid-argument)

(test-exception 128 (sub-array (make-array 3) 2 5) 'index-out-of-bounds)

(let ((a (make-array 5)))
  (dotimes (i 5)
    (array-set a i i))
  (let ((s (sub-array a 0 3)))
    (test-condition 129 (and (eq (array-get s 0) 0)
                             (eq (array-get s 1) 1)
                             (eq (array-get s 2) 2)
                             (eq (array-length s) 3)))))

;end sub-array

;array-length
(test-exception 130 (array-length 1) 'invalid-argument)

(test-condition 131 (eq (array-length (make-array 3)) 3))
;end array-length

;print-string
(test-exception 132 (print-string 1) 'invalid-argument)

(test-happy-case 133 (print-string "Diagnostic from test case #183"))

(test-happy-case 134 (print-string (string "Diagnostic from test case #184")))
;end print-string

;create-image
(test-exception 135 (create-image 1) 'invalid-argument)

(test-happy-case 136 (progn (create-image "test1.image")
                            (utils:system "ls -l test1.image")
                            (utils:system "rm test1.image")))

(test-happy-case 137 (progn (create-image (string "test2.image"))
                            (utils:system "ls -l test2.image")
                            (utils:system "rm test2.image")))
;end create-image

;load-foreign-library
(test-exception 138 (load-foreign-library 1) 'invalid-argument)

(test-happy-case 139 (load-foreign-library "./libtest.so"))

(test-happy-case 140 (load-foreign-library (string "./libtest.so")))
;end load-foreign-library

;call-foreign-function
(test-exception 141 (call-foreign-function 1 2 3) 'invalid-argument)

(test-exception 142 (call-foreign-function "abc" 1 2) 'invalid-argument)

(test-exception 143 (call-foreign-function (string "abc") 1 2) 'invalid-argument)

(test-exception 144 (call-foreign-function "abc" integer 1) 'invalid-argument)

(test-exception 145 (call-foreign-function "abc" integer (1 2 3)) 'invalid-argument)

(test-exception 146 (call-foreign-function "abc" integer ((1) (2) (3))) 'invalid-argument)

(test-exception 147 (call-foreign-function "abc" integer ((1 2 3) (4 5 6) (7 8 9))) 'invalid-argument)

(test-exception 148 (call-foreign-function "abc" integer ((1 2) (3 4) (5 6))) 'invalid-argument)

(test-exception 149 (call-foreign-function "abc" integer ((x 2) (3 4) (5 6))) 'symbol-not-bound)

(test-exception 150 (call-foreign-function "abc" integer (("abc" integer))) 'arg-mismatch)

(test-exception 151 (call-foreign-function "abc" integer (("abc" float))) 'arg-mismatch)

(test-exception 152 (call-foreign-function "abc" integer (("abc" character))) 'arg-mismatch)

(test-exception 153 (call-foreign-function "abc" integer ((1 character-pointer))) 'arg-mismatch)

(test-exception 154 (call-foreign-function "abc" integer (("abc" integer-pointer))) 'arg-mismatch)

(test-exception 155 (call-foreign-function "abc" integer (("abc" float-pointer))) 'arg-mismatch)

(test-exception 156 (call-foreign-function "abc" integer (("abc" some-other-type))) 'invalid-argument)

(defun setup-ff-tests ()
  (load-foreign-library "./libtest.so"))

(let ((i 10)
      (x (string "abc"))
      (f 19.56))
  (test-condition 157 (eq (call-foreign-function "fn_ret_int" integer
                                                 ((i integer)
                                                  (6.5 float)
                                                  (#\a character)
                                                  (x character-pointer)))
                      100)))

(let ((i 10)
      (x (string "abc"))
      (f 19.56))
  (test-condition 158 (eq (call-foreign-function "fn_ret_float" float
                                                 ((10 integer)
                                                   (6.5 float)
                                                   (#\a character)
                                                   ("abc" character-pointer)))
                          13)))

(let ((i 10)
      (x (string "abc"))
      (f 19.56))
  (test-condition 159 (eq (call-foreign-function "fn_ret_char" character
                                                 ((10 integer)
                                                   (6.5 float)
                                                   (#\a character)
                                                   ("abc" character-pointer)))
                          #\a)))

(let ((i 10)
      (x (string "abc"))
      (f 19.56))
  (test-condition 160 (eq (call-foreign-function "fn_ret_char_ptr" character-pointer
                                                 ((10 integer)
                                                  (6.5 float)
                                                  (#\a character)
                                                  ("abc" character-pointer)))
                          "aaaaaaaaaa")))

(let ((i 10)
      (x (string "abc"))
      (f 19.56))
  (call-foreign-function "fn_arg_int_ptr" 
                         integer
                         ((i integer-pointer)))
  (test-condition 161 (eq i 100)))

(let ((i 10)
      (x (string "abc"))
      (f 19.56))
  (call-foreign-function "fn_arg_float_ptr" 
                         integer
                         ((f float-pointer)))
  (test-condition 162 (eq f 100.97)))


(let ((i 10)
      (x (string "abc"))
      (f 19.56))
  (call-foreign-function "fn_arg_char_ptr" 
                         integer
                         ((x character-pointer)))
  (test-condition 163 (eq x "ABC")))

(let ((i 10)
      (x (string "abc"))
      (f 19.56))
  (test-happy-case 164 (call-foreign-function "function_ret_void" void
                                              ((10 integer)
                                                (6.5 float)
                                                (#\a character)
                                                ("abc" character-pointer)))))
;end call-foreign-library

;load-file
(test-exception 165 (load-file 1) 'invalid-argument)

(test-exception 166 (load-file (concat-strings (symbol-name (gensym)) ".lisp")) 'file-open-error)

;happy case not tested since LOAD-FILE is tested in running this file (unit_tests.lisp) itself
;end load-file

;predicate functions that test for object type
(test-condition 167 (null (consp 1)))
(test-condition 168 (consp '(1 2)))

(test-condition 169 (null (integerp 1.0)))
(test-condition 170 (integerp 1))

(test-condition 171 (null (floatp 1)))
(test-condition 172 (floatp 1.0))

(test-condition 173 (null (characterp 1)))
(test-condition 174 (characterp #\a))

(test-condition 175 (null (stringp 1)))
(test-condition 176 (stringp "abc"))
(test-condition 177 (stringp (string "abc")))

(test-condition 178 (null (symbolp 1)))
(test-condition 179 (symbolp 'a))
(test-condition 180 (symbolp (gensym)))
(test-condition 181 (symbolp (symbol "abc")))

(test-condition 182 (null (arrayp 1)))
(test-condition 183 (arrayp (make-array 3)))
(test-condition 184 (arrayp (string "abc")))

(test-condition 185 (null (closurep 1)))
(test-condition 186 (closurep (lambda (x) x)))
(test-condition 187 (closurep cadr))

(test-condition 188 (null (macrop 1)))
(test-condition 189 (macrop (macro (x) x)))
(test-condition 190 (macrop defun))

(test-condition 191 (null (continuationp 1)))

(let ((cont))
  (call-cc (lambda (cc) (set cont cc)))
  (test-condition 192 (continuationp cont)))
;end predicate functions that test for object type

;lambda-expression
(test-exception 193 (lambda-expression) 'arg-mismatch)

(test-exception 194 (lambda-expression 1 2) 'arg-mismatch)

(test-exception 195 (lambda-expression 1) 'invalid-argument)

(let ((val (lambda-expression (lambda (x) x))))
  (test-condition 196 (and (eq (car val) '(x))
                           (eq (car (cadr val)) 'refer)
                           (eq (cadr (cadr val)) 'x))))

(let ((val (lambda-expression (macro (x) x))))
  (test-condition 197 (and (eq (car val) '(x))
                           (eq (car (cadr val)) 'refer)
                           (eq (cadr (cadr val)) 'x))))
;end lambda-expression

;format
(test-exception 198 (format 1 2) 'invalid-argument)

(test-exception 199 (format nil "%d" 'a) 'invalid-argument)

(test-exception 200 (format nil "%d" (make-array 3)) 'invalid-argument)

(test-exception 201 (format nil "%d" (lambda (x) x)) 'invalid-argument)

(test-exception 202 (format nil "%d" (macro (x) x)) 'invalid-argument)

(let ((cont))
  (call-cc (lambda (cc) (set cont cc)))
  (test-exception 203 (format nil "%d" cont) 'invalid-argument))

(test-happy-case 204 (progn (format nil "Diagnostic from test case #285") (newline nil)))

(test-happy-case 205 (progn (format nil "Diagnostic from test case #%d %d" 286 10) (newline nil)))

(test-happy-case 206 (progn (format nil "Diagnostic from test case #%d %0.2f" 287 3.1415) (newline nil)))

(test-happy-case 207 (progn (format nil "Diagnostic from test case #%d %c" 288 #\a) (newline nil)))

(test-happy-case 208 (progn (format nil "Diagnostic from test case #%d %s" 289 "abc") (newline nil)))
;end-format

;clone
(dolist (x (list 1 1.0 #\a "abc" 'a '(1 2 3)))
  (test-condition 209 (eq x (clone x))))

(let ((val (make-array 3)))
  (array-set val 0 0)
  (array-set val 1 1)
  (array-set val 2 2)
  (test-condition 210 (array-eq val (clone val))))

(let ((cont))
  (call-cc (lambda (cc) (set cont cc)))
  (test-condition 211 (eq cont (clone cont))))
;end clone

;return-from
(test-exception 212 (return-from 1 2 3) 'arg-mismatch)

(defun setup-ret-from-tests ()
  (eval '(define x 0)))

(defun f (n)
  (if (> n 0)
      (progn (set x 10) 
	     (return-from f 20)))
  (set x 30)
  40)

(defun test-ret-from-213 ()
  (let ((val))
    (set val (f 10))
    (test-condition 213 (and (eq x 10) (eq val 20))))

(defun test-ret-from-214()
  (let ((val))
    (set val (f -10))
  (test-condition 214 (and (eq x 30) (eq val 40)))))
;end return-from

;symbol
(test-exception 215 (symbol 1) 'invalid-argument)

(test-condition 216 (eq (symbol "abc") 'abc))

(test-condition 217 (eq (symbol (string "abc")) 'abc))
;end symbol

;symbol-name
(test-exception 218 (symbol-name 1) 'invalid-argument)

(test-condition 219 (eq (symbol-name 'abc) "ABC"))
;end symbol-name

;symbol not bound
(test-exception 220 an-undefined-symbol 'symbol-not-bound)
;end symbol not bound

(format nil 
        "%d of %d test cases passed (%.2f%%)" 
        passed-cases 
        (+ passed-cases failed-cases) 
        (/ (* 100.0 passed-cases) (* 1.0 (+ passed-cases failed-cases))))

(if (not (eq failed-cases 0))
    (format nil "ATTENTION! Some unit test cases have failed."))

(newline nil)
