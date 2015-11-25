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

(format nil "Running unit test cases...")
(newline nil)

(define passed-cases 0)
(define failed-cases 0)

(defmacro test-condition (id condition)
  `(try (if ,condition 
            (incf passed-cases)
          (progn (format nil "Test case #%d failed" ,id) (newline nil) (incf failed-cases)))
        (catch (e)
          (progn (format nil "Test case #%d failed" ,id) (newline nil) (incf failed-cases))) nil))

(defmacro test-exception (id body excp)
  `(try ,body
        (catch (e)
          (if (eq (car e) ,excp)
              (incf passed-cases)
            (progn (format nil "Test case #%d failed" ,id) (newline nil) (incf failed-cases)))) nil))

(defmacro test-happy-case (id body)
  `(progn (try ,body
               (catch (e)
                 (progn (format nil "Test case #%d failed" ,id) (newline nil) (incf failed-cases))) nil)
          (incf passed-cases)))

;;car
(test-exception 1 (car 1) 'not-a-cons)

(let ((x (car '(1 2 3))))
  (test-condition 2 (eq x 1)))

(let ((x (car nil)))
  (test-condition 3 (null x)))
;end car

;;cdr
(test-exception 4 (cdr 1) 'not-a-cons)

(let ((x (cdr '(1 2 3))))
  (test-condition 5 (eq x '(2 3))))

(let ((x (cdr nil)))
  (test-condition 6 (null x)))
;end cdr

;eq
(test-exception 7 (eq 1) 'compile-error)

(test-exception 8 (eq) 'compile-error)

(test-condition 9 (eq 1 1.0))

(test-condition 10 (eq 'a 'a))

(test-condition 11 (not (eq 'a 'b)))
;end eq

;cons
(test-exception 12 (cons 1) 'compile-error)

(test-exception 13 (cons) 'compile-error)

(let ((x (cons 1 2)))
  (test-condition 14 (and (eq (car x) 1)
                          (eq (cdr x) 2))))
;end cons

;atom
(test-condition 15 (atom 1))

(test-condition 16 (atom nil))

(test-condition 17 (atom 1.0))

(test-condition 18 (atom "abc"))

(test-condition 19 (atom #\a))

(test-condition 20 (atom 'a))

(test-condition 21 (not (atom '(1 2 3))))

(test-condition 22 (not (atom (lambda (x) x))))

(test-condition 23 (not (atom (make-array 10 nil))))

(test-condition 24 (not (atom (macro (x) x))))

(let ((cont))
  (call-cc (lambda (cc) (set cont cc)))
  (test-condition 25 (not (atom cont))))
;end atom

;add
(test-exception 26 (+) 'compile-error)

(test-exception 27 (+ 1) 'compile-error)

(test-exception 28 (+ 1 nil) 'invalid-argument)

(test-exception 29 (+ 1 "abc") 'invalid-argument)

(test-exception 30 (+ 1 #\a) 'invalid-argument)

(test-exception 31 (+ 1 '(1 2 3)) 'invalid-argument)

(test-exception 32 (+ 1 (lambda (x) x)) 'invalid-argument)

(test-exception 33 (+ 1 (macro (x) x)) 'invalid-argument)

(let ((cont))
  (call-cc (lambda (cc) (set cont cc)))
  (test-exception 34 (+ 1 cont) 'invalid-argument))

(test-exception 35 (+ 1 (make-array 10 nil)) 'invalid-argument)

(test-condition 36 (eq (+ 1 1) 2))

(test-condition 37 (eq (+ 1 1.0) 2))

(test-condition 38 (eq (+ 1.0 2.5) 3.5))

(test-condition 39 (eq (apply + (range 1 10 1)) 55))
;end add

;sub
(test-exception 40 (-) 'compile-error)

(test-exception 41 (- 1) 'compile-error)

(test-exception 42 (- 1 nil) 'invalid-argument)

(test-exception 43 (- 1 "abc") 'invalid-argument)

(test-exception 44 (- 1 #\a) 'invalid-argument)

(test-exception 45 (- 1 '(1 2 3)) 'invalid-argument)

(test-exception 46 (- 1 (lambda (x) x)) 'invalid-argument)

(test-exception 47 (- 1 (macro (x) x)) 'invalid-argument)

(let ((cont))
  (call-cc (lambda (cc) (set cont cc)))
  (test-exception 48 (- 1 cont) 'invalid-argument))

(test-exception 49 (- 1 (make-array 10 nil)) 'invalid-argument)

(test-condition 50 (eq (- 1 1) 0))

(test-condition 51 (eq (- 1 1.0) 0))

(test-condition 52 (eq (- 1.0 2.5) -1.5))

(test-condition 53 (eq (apply - (range 1 10 1)) -53))
;end sub

;mult
(test-exception 54 (*) 'compile-error)

(test-exception 55 (* 1) 'compile-error)

(test-exception 56 (* 1 nil) 'invalid-argument)

(test-exception 57 (* 1 "abc") 'invalid-argument)

(test-exception 58 (* 1 #\a) 'invalid-argument)

(test-exception 59 (* 1 '(1 2 3)) 'invalid-argument)

(test-exception 60 (* 1 (lambda (x) x)) 'invalid-argument)

(test-exception 61 (* 1 (macro (x) x)) 'invalid-argument)

(let ((cont))
  (call-cc (lambda (cc) (set cont cc)))
  (test-exception 62 (* 1 cont) 'invalid-argument))

(test-exception 63 (* 1 (make-array 10 nil)) 'invalid-argument)

(test-condition 64 (eq (* 1 1) 1))

(test-condition 65 (eq (* 1 1.0) 1))

(test-condition 66 (eq (* 1.0 2.5) 2.5))

(test-condition 67 (eq (apply * (range 1 10 1)) 3628800))
;end mult

;div
(test-exception 68 (/) 'compile-error)

(test-exception 69 (/ 1) 'compile-error)

(test-exception 70 (/ 1 nil) 'invalid-argument)

(test-exception 71 (/ 1 "abc") 'invalid-argument)

(test-exception 72 (/ 1 #\a) 'invalid-argument)

(test-exception 73 (/ 1 '(1 2 3)) 'invalid-argument)

(test-exception 74 (/ 1 (lambda (x) x)) 'invalid-argument)

(test-exception 75 (/ 1 (macro (x) x)) 'invalid-argument)

(let ((cont))
  (call-cc (lambda (cc) (set cont cc)))
  (test-exception 76 (/ 1 cont) 'invalid-argument))

(test-exception 77 (/ 1 (make-array 10 nil)) 'invalid-argument)

(test-condition 78 (eq (/ 1 1) 1))

(test-condition 79 (eq (/ 1 1.0) 1))

(test-condition 80 (eq (/ 1.0 2.5) 0.4))

(let ((x (apply / (range 1.0 4 1))))
  (test-condition 81 (< (math:abs (- x 0.041667)) 0.000001)))

(test-exception 82 (/ 1 0) 'div-by-zero-exception)
;end div

;list
(test-condition 83 (eq (list 1 2 3) '(1 2 3)))

(test-condition 84 (eq (list) '()))
;end list

;listp
(test-exception 85 (listp) 'compile-error)

(test-condition 86 (listp nil))

(test-condition 87 (listp '(1 2 3)))

(test-condition 88 (listp (cons 1 2)))

(test-condition 89 (not (listp 1)))

(test-condition 90 (not (listp 1.0)))

(test-condition 91 (not (listp 'a)))

(test-condition 92 (not (listp "abc")))

(test-condition 93 (not (listp #\a)))

(test-condition 94 (not (listp (make-array 10 nil))))

(test-condition 95 (not (listp (lambda (x) x))))

(test-condition 96 (not (listp (macro (x) x))))

(let ((cont))
  (call-cc (lambda (cc) (set cont cc)))
  (test-condition 97 (not (listp cont))))
;end listp

;symbol-value
(test-exception 98 (symbol-value) 'compile-error)

(dolist (x (list 1 1.0 "abc" #\a (make-array 10 nil) (lambda (x) x) (macro (x) x) '(1 2 3)))
  (test-exception 99 (symbol-value x) 'invalid-argument))

(let ((cont))
  (call-cc (lambda (cc) (set cont cc)))
  (test-exception 100 (symbol-value cont) 'invalid-argument))

(test-exception 101 (symbol-value 'abc) 'symbol-not-bound)

(let ((x 100))
  (test-condition 102 (eq (symbol-value 'x) 100)))
;end symbol-value

;gt
(test-exception 103 (>) 'compile-error)

(test-exception 104 (> 1) 'compile-error)

(test-exception 105 (> 1 '(1 2 3)) 'invalid-argument)

(test-exception 106 (> "abc" 1.0) 'invalid-argument)

(test-condition 107 (> 3 0))

(test-condition 108 (> 2.5 0.6))

(test-condition 109 (> 4.0 1))

(test-condition 110 (> 25 0.1))
;end gt

;gensym
(test-exception 111 (gensym 'a) 'arg-mismatch)
;end gensym

;setcar
(test-exception 112 (setcar) 'compile-error)

(test-exception 113 (setcar 1) 'compile-error)

(test-exception 114 (setcar 1 2) 'arg-mismatch)

(let ((x '(1 2 3)))
  (setcar x 4)
  (test-condition 115 (eq (car x) 4)))
;end setcar

;setcdr
(test-exception 116 (setcdr) 'compile-error)

(test-exception 117 (setcdr 1) 'compile-error)

(test-exception 118 (setcdr  1 2) 'arg-mismatch)

(let ((x (cons 1 2)))
  (setcdr x 4)
  (test-condition 119 (eq x (cons 1 4))))
;end setcdr

;create-package
(test-exception 120 (create-package) 'compile-error)

(test-exception 121 (create-package 1 2) 'compile-error)

(test-exception 122 (create-package 1) 'invalid-argument)

(test-exception 123 (create-package "CORE") 'package-already-exists)

(let ((x (symbol-name (gensym))))
  (create-package x)
  (test-exception 124 (create-package x) 'package-already-exists))
;end create-package

;in-package
(test-exception 125 (in-package) 'compile-error)

(test-exception 126 (in-package 1 2) 'compile-error)

(test-exception 127 (in-package 1) 'invalid-argument)

(test-exception 128 (in-package "core") 'access-violation)

(test-exception 129 (in-package (symbol-name (gensym))) 'package-not-found)

(test-happy-case 130 (let ((x (symbol-name (gensym))))
                       (create-package x)
                       (in-package x)
                       (in-package "user")))

;end in-package

;expand-macro
(test-exception 131 (expand-macro) 'compile-error)

(test-exception 132 (expand-macro 1 2) 'compile-error)

(test-exception 133 (expand-macro 1) 'invalid-argument)

(test-exception 134 (expand-macro '(QQQQ 1 2 3)) 'macro-undefined)

(let1 ((first (macro (lst) `(car ,lst)))
       (x (expand-macro '(first a))))
  (test-condition 135 (eq x '(car a))))
;end expand-macro

;apply
(test-exception 136 (apply) 'compile-error)

(test-exception 137 (apply 1) 'compile-error)

(test-exception 138 (apply 1 2 3) 'compile-error)

(test-exception 139 (apply 1 2) 'invalid-argument)

(test-exception 140 (apply car 2) 'invalid-argument)

(test-condition 141 (eq (apply car '((1 2 3))) 1))
;end apply

;string
(test-exception 142 (string) 'compile-error)

(test-exception 143 (string 1 2) 'compile-error)

(test-exception 144 (string 1) 'invalid-argument)

(let ((x (make-array 3)))
  (array-set x 0 #\a)
  (array-set x 1 #\b)
  (array-set x 2 #\c)
  (test-condition 145 (array-eq (string "abc") x)))
;end string

;make-array
(test-exception 146 (make-array) 'compile-error)

(test-exception 147 (make-array 1 1 1) 'compile-error)

(test-exception 148 (make-array 1.0) 'invalid-argument)

(let ((a (make-array 5)))
  (array-set a 0 10)
  (array-set a 4 "abc")
  (test-condition 149 (and (eq (array-get a 0) 10)
                           (null (array-get a 1))
                           (null (array-get a 2))
                           (null (array-get a 3))
                           (eq (array-get a 4) "abc"))))
;end make-array

;array-set
(test-exception 150 (array-set) 'compile-error)

(test-exception 151 (array-set 1) 'compile-error)

(test-exception 152 (array-set 1 2) 'compile-error)

(test-exception 153 (array-set 1 2 3) 'invalid-argument)

(test-exception 154 (array-set (make-array 3) "abc" 100) 'invalid-argument)

(test-exception 155 (array-set (make-array 3) -1 100) 'index-out-of-bounds)

(test-exception 156 (array-set (make-array 3) 3 100) 'index-out-of-bounds)

(let ((a (make-array 3)))
  (array-set a 0 100)
  (array-set a 2 300)
  (test-condition 157 (and (eq (array-get a 0) 100)
                           (null (array-get a 1))
                           (eq (array-get a 2) 300))))
;end array-set

;array-get
(test-exception 158 (array-get) 'compile-error)

(test-exception 159 (array-get 1) 'compile-error)

(test-exception 160 (array-get 1 2 ) 'invalid-argument)

(test-exception 161 (array-get (make-array 3) "abc") 'invalid-argument)

(test-exception 162 (array-get (make-array 3) -1) 'index-out-of-bounds)

(test-exception 163 (array-get (make-array 3) 3) 'index-out-of-bounds)

(test-exception 164 (array-get "abc" 3) 'index-out-of-bounds)

(test-condition 165 (eq (array-get "abc" 0) #\a))

;second positive test case covered in array-set's test cases (last one)
;end array-get

;sub-array
(test-exception 166 (sub-array) 'compile-error)

(test-exception 167 (sub-array 1) 'compile-error)

(test-exception 168 (sub-array 1 2) 'compile-error)

(test-exception 169 (sub-array 1 2 3 4) 'compile-error)

(test-exception 170 (sub-array 1 2 3) 'invalid-argument)

(test-exception 171 (sub-array (make-array 3) "abc" 3) 'invalid-argument)

(test-exception 172 (sub-array (make-array 3) -1 3) 'invalid-argument)

(test-exception 173 (sub-array (make-array 3) 0 "abc") 'invalid-argument)

(test-exception 174 (sub-array (make-array 3) 2 5) 'index-out-of-bounds)

(let ((a (make-array 5)))
  (dotimes (i 5)
    (array-set a i i))
  (let ((s (sub-array a 0 3)))
    (test-condition 175 (and (eq (array-get s 0) 0)
                             (eq (array-get s 1) 1)
                             (eq (array-get s 2) 2)
                             (eq (array-length s) 3)))))

;end sub-array

;array-length
(test-exception 176 (array-length) 'compile-error)

(test-exception 177 (array-length 1 2) 'compile-error)

(test-exception 178 (array-length 1) 'invalid-argument)

(test-condition 179 (eq (array-length (make-array 3)) 3))
;end array-length

;print-string
(test-exception 180 (print-string) 'compile-error)

(test-exception 181 (print-string 1 2) 'compile-error)

(test-exception 182 (print-string 1) 'invalid-argument)

(test-happy-case 183 (print-string "Diagnostic from test case #183"))

(test-happy-case 184 (print-string (string "Diagnostic from test case #184")))
;end print-string

;create-image
(test-exception 185 (create-image) 'compile-error)

(test-exception 186 (create-image 1 2) 'compile-error)

(test-exception 187 (create-image 1) 'invalid-argument)

(test-happy-case 188 (progn (create-image "test1.image")
                            (utils:system "ls -l test1.image")
                            (utils:system "rm test1.image")))

(test-happy-case 189 (progn (create-image (string "test2.image"))
                            (utils:system "ls -l test2.image")
                            (utils:system "rm test2.image")))
;end create-image

;load-foreign-library
(test-exception 190 (load-foreign-library) 'compile-error)

(test-exception 191 (load-foreign-library 1 2) 'compile-error)

(test-exception 192 (load-foreign-library 1) 'invalid-argument)

(test-happy-case 193 (load-foreign-library "./libtest.so"))

(test-happy-case 194 (load-foreign-library (string "./libtest.so")))
;end load-foreign-library

;call-foreign-function
(test-exception 195 (call-foreign-function) 'compile-error)

(test-exception 196 (call-foreign-function 1) 'compile-error)

(test-exception 197 (call-foreign-function 1 2) 'compile-error)

(test-exception 198 (call-foreign-function 1 2 3 4) 'compile-error)

(test-exception 199 (call-foreign-function 1 2 3) 'invalid-argument)

(test-exception 200 (call-foreign-function "abc" 1 2) 'invalid-argument)

(test-exception 201 (call-foreign-function (string "abc") 1 2) 'invalid-argument)

(test-exception 202 (call-foreign-function "abc" integer 1) 'invalid-argument)

(test-exception 203 (call-foreign-function "abc" integer (1 2 3)) 'invalid-argument)

(test-exception 204 (call-foreign-function "abc" integer ((1) (2) (3))) 'invalid-argument)

(test-exception 205 (call-foreign-function "abc" integer ((1 2 3) (4 5 6) (7 8 9))) 'invalid-argument)

(test-exception 206 (call-foreign-function "abc" integer ((1 2) (3 4) (5 6))) 'invalid-argument)

(test-exception 207 (call-foreign-function "abc" integer ((x 2) (3 4) (5 6))) 'symbol-not-bound)

(test-exception 208 (call-foreign-function "abc" integer (("abc" integer))) 'arg-mismatch)

(test-exception 209 (call-foreign-function "abc" integer (("abc" float))) 'arg-mismatch)

(test-exception 210 (call-foreign-function "abc" integer (("abc" character))) 'arg-mismatch)

(test-exception 211 (call-foreign-function "abc" integer ((1 character-pointer))) 'arg-mismatch)

(test-exception 212 (call-foreign-function "abc" integer (("abc" integer-pointer))) 'arg-mismatch)

(test-exception 213 (call-foreign-function "abc" integer (("abc" float-pointer))) 'arg-mismatch)

(test-exception 214 (call-foreign-function "abc" integer (("abc" some-other-type))) 'invalid-argument)

(let ((i 10)
      (x (string "abc"))
      (f 19.56))

  (load-foreign-library "./libtest.so")


  (test-condition 215 (eq (call-foreign-function "fn_ret_int" integer
                                                 ((i integer)
                                                  (6.5 float)
                                                  (#\a character)
                                                  (x character-pointer)))
                      100))


  (test-condition 216 (eq (call-foreign-function "fn_ret_float" float
                                                 ((10 integer)
                                                   (6.5 float)
                                                   (#\a character)
                                                   ("abc" character-pointer)))
                          13))

  (test-condition 217 (eq (call-foreign-function "fn_ret_char" character
                                                 ((10 integer)
                                                   (6.5 float)
                                                   (#\a character)
                                                   ("abc" character-pointer)))
                          #\a))

  (test-condition 218 (eq (call-foreign-function "fn_ret_char_ptr" character-pointer
                                                 ((10 integer)
                                                  (6.5 float)
                                                  (#\a character)
                                                  ("abc" character-pointer)))
                          "aaaaaaaaaa"))

  (call-foreign-function "fn_arg_int_ptr" 
                         integer
                         ((i integer-pointer)))

  (test-condition 219 (eq i 100))

  (call-foreign-function "fn_arg_float_ptr" 
                         integer
                         ((f float-pointer)))

  (test-condition 220 (eq f 100.97))

  (call-foreign-function "fn_arg_char_ptr" 
                         integer
                         ((x character-pointer)))

  (test-condition 221 (eq x "ABC"))

  (test-happy-case 222 (call-foreign-function "function_ret_void" void
                                              ((10 integer)
                                                (6.5 float)
                                                (#\a character)
                                                ("abc" character-pointer))))

  )
;end call-foreign-library

;load-file
(test-exception 223 (load-file) 'compile-error)

(test-exception 224 (load-file 1 2) 'compile-error)

(test-exception 225 (load-file 1) 'invalid-argument)

(test-exception 226 (load-file (concat-strings (symbol-name (gensym)) ".lisp")) 'file-open-error)

;happy case not tested since LOAD-FILE is tested in running this file (unit_tests.lisp) itself
;end load-file

;predicate functions that test for object type
(test-exception 227 (consp) 'compile-error)
(test-exception 228 (consp 1 2) 'compile-error)
(test-condition 229 (null (consp 1)))
(test-condition 230 (consp '(1 2)))

(test-exception 231 (integerp) 'compile-error)
(test-exception 232 (integerp 1 2) 'compile-error)
(test-condition 233 (null (integerp 1.0)))
(test-condition 234 (integerp 1))

(test-exception 235 (floatp) 'compile-error)
(test-exception 236 (floatp 1 2) 'compile-error)
(test-condition 237 (null (floatp 1)))
(test-condition 238 (floatp 1.0))

(test-exception 239 (characterp) 'compile-error)
(test-exception 240 (characterp 1 2) 'compile-error)
(test-condition 241 (null (characterp 1)))
(test-condition 242 (characterp #\a))

(test-exception 243 (stringp) 'compile-error)
(test-exception 244 (stringp 1 2) 'compile-error)
(test-condition 245 (null (stringp 1)))
(test-condition 246 (stringp "abc"))
(test-condition 247 (stringp (string "abc")))

(test-exception 248 (symbolp) 'compile-error)
(test-exception 249 (symbolp 1 2) 'compile-error)
(test-condition 250 (null (symbolp 1)))
(test-condition 251 (symbolp 'a))
(test-condition 252 (symbolp (gensym)))
(test-condition 253 (symbolp (symbol "abc")))

(test-exception 254 (arrayp) 'compile-error)
(test-exception 255 (arrayp 1 2) 'compile-error)
(test-condition 256 (null (arrayp 1)))
(test-condition 257 (arrayp (make-array 3)))
(test-condition 258 (arrayp (string "abc")))

(test-exception 259 (closurep) 'compile-error)
(test-exception 260 (closurep 1 2) 'compile-error)
(test-condition 261 (null (closurep 1)))
(test-condition 262 (closurep (lambda (x) x)))
(test-condition 263 (closurep cadr))

(test-exception 264 (macrop) 'compile-error)
(test-exception 265 (macrop 1 2) 'compile-error)
(test-condition 266 (null (macrop 1)))
(test-condition 267 (macrop (macro (x) x)))
(test-condition 268 (macrop defun))

(test-exception 269 (continuationp) 'compile-error)
(test-exception 270 (continuationp 1 2) 'compile-error)
(test-condition 271 (null (continuationp 1)))

(let ((cont))
  (call-cc (lambda (cc) (set cont cc)))
  (test-condition 272 (continuationp cont)))
;end predicate functions that test for object type

;lambda-expression
(test-exception 273 (lambda-expression) 'arg-mismatch)

(test-exception 274 (lambda-expression 1 2) 'arg-mismatch)

(test-exception 275 (lambda-expression 1) 'invalid-argument)

(let ((val (lambda-expression (lambda (x) x))))
  (test-condition 276 (and (eq (car val) '(x))
                           (eq (car (cadr val)) 'refer)
                           (eq (cadr (cadr val)) 'x))))

(let ((val (lambda-expression (macro (x) x))))
  (test-condition 277 (and (eq (car val) '(x))
                           (eq (car (cadr val)) 'refer)
                           (eq (cadr (cadr val)) 'x))))
;end lambda-expression

;format
(test-exception 278 (format) 'compile-error)

(test-exception 279 (format 1) 'compile-error)

(test-exception 2791 (format 1 2) 'invalid-argument)

(test-exception 280 (format nil "%d" 'a) 'invalid-argument)

(test-exception 281 (format nil "%d" (make-array 3)) 'invalid-argument)

(test-exception 282 (format nil "%d" (lambda (x) x)) 'invalid-argument)

(test-exception 283 (format nil "%d" (macro (x) x)) 'invalid-argument)

(let ((cont))
  (call-cc (lambda (cc) (set cont cc)))
  (test-exception 284 (format nil "%d" cont) 'invalid-argument))

(test-happy-case 285 (progn (format nil "Diagnostic from test case #285") (newline nil)))

(test-happy-case 286 (progn (format nil "Diagnostic from test case #%d %d" 286 10) (newline nil)))

(test-happy-case 287 (progn (format nil "Diagnostic from test case #%d %0.2f" 287 3.1415) (newline nil)))

(test-happy-case 288 (progn (format nil "Diagnostic from test case #%d %c" 288 #\a) (newline nil)))

(test-happy-case 289 (progn (format nil "Diagnostic from test case #%d %s" 289 "abc") (newline nil)))
;end-format

;clone
(test-exception 290 (clone) 'compile-error)

(test-exception 291 (clone 1 2) 'compile-error)

(dolist (x (list 1 1.0 #\a "abc" 'a '(1 2 3)))
  (test-condition 292 (eq x (clone x))))

(let ((val (make-array 3)))
  (array-set val 0 0)
  (array-set val 1 1)
  (array-set val 2 2)
  (test-condition 293 (array-eq val (clone val))))

(let ((cont))
  (call-cc (lambda (cc) (set cont cc)))
  (test-condition 294 (eq cont (clone cont))))
;end clone

;return-from
(test-exception 295 (return-from) 'compile-error)

(test-exception 296 (return-from 1) 'compile-error)

(test-exception 297 (return-from 1 2 3) 'arg-mismatch)

(define x 0)

(defun f (n)
  (if (> n 0)
      (progn (set x 10) 
	     (return-from f 20)))
  (set x 30)
  40)

(let ((val))
  (set val (f 10))
  (test-condition 298 (and (eq x 10) (eq val 20)))

  (set val (f -10))
  (test-condition 299 (and (eq x 30) (eq val 40))))

;end return-from

;compile
(test-exception 300 (compile1) 'arg-mismatch)

(test-exception 301 (compile1 1) 'arg-mismatch)

(test-exception 302 (compile1 1 2 3) 'arg-mismatch)
;end compile

;symbol
(test-exception 303 (symbol) 'compile-error)

(test-exception 304 (symbol 1 2) 'compile-error)

(test-exception 305 (symbol 1) 'invalid-argument)

(test-condition 306 (eq (symbol "abc") 'abc))

(test-condition 307 (eq (symbol (string "abc")) 'abc))
;end symbol

;symbol-name
(test-exception 308 (symbol-name) 'compile-error)

(test-exception 309 (symbol-name 1 2) 'compile-error)

(test-exception 310 (symbol-name 1) 'invalid-argument)

(test-condition 311 (eq (symbol-name 'abc) "ABC"))
;end symbol-name

;symbol not bound
(test-exception 312 an-undefined-symbol 'symbol-not-bound)
;end symbol not bound

(format nil 
        "%d of %d test cases passed (%.2f%%)" 
        passed-cases 
        (+ passed-cases failed-cases) 
        (/ (* 100.0 passed-cases) (* 1.0 (+ passed-cases failed-cases))))

(if (not (eq failed-cases 0))
    (format nil "ATTENTION! Some unit test cases have failed."))

(newline nil)
