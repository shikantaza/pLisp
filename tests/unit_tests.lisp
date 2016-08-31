;;  Copyright 2011-2016 Rajesh Jayaprakash <rajesh.jayaprakash@gmail.com>

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

(create-package "unit-tests-core")

(in-package "unit-tests-core")

(define passed-cases 0)
(define failed-cases 0)

(defun float-eq (f1 f2)
  (if (> f1 f2)
      (< (- f1 f2) 0.00001)
    (< (- f2 f1) 0.00001)))

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
(defun test-car-1 ()
  (test-exception 1 (car 1) 'not-a-cons))

(defun test-car-2 ()
  (let ((x (car '(1 2 3))))
    (test-condition 2 (eq x 1))))

(defun test-car-3 ()
  (let ((x (car nil)))
    (test-condition 3 (null x))))
;end car

;;cdr
(defun test-cdr-4 ()
  (test-exception 4 (cdr 1) 'not-a-cons))

(defun test-cdr-5 ()
  (let ((x (cdr '(1 2 3))))
    (test-condition 5 (eq x '(2 3)))))

(defun test-cdr-6 ()
  (let ((x (cdr nil)))
    (test-condition 6 (null x))))
;end cdr

;eq
(defun test-eq-7 ()
  (test-condition 7 (eq 1 1.0)))

(defun test-eq-8 ()
  (test-condition 8 (eq 'a 'a)))

(defun test-eq-9 ()
  (test-condition 9 (not (eq 'a 'b))))
;end eq

;cons
(defun test-cons-10 ()
  (let ((x (cons 1 2)))
    (test-condition 10 (and (eq (car x) 1)
                            (eq (cdr x) 2)))))
;end cons

;atom
(defun test-atom-11 ()
  (test-condition 11 (atom 1)))

(defun test-atom-12 ()
  (test-condition 12 (atom nil)))

(defun test-atom-13 ()
  (test-condition 13 (atom 1.0)))

(defun test-atom-14 ()
  (test-condition 14 (atom "abc")))

(defun test-atom-15 ()
  (test-condition 15 (atom #\a)))

(defun test-atom-16 ()
  (test-condition 16 (atom 'a)))

(defun test-atom-17 ()
  (test-condition 17 (not (atom '(1 2 3)))))

(defun test-atom-18 ()
  (test-condition 18 (not (atom (lambda (x) x)))))

(defun test-atom-19 ()
  (test-condition 19 (not (atom (make-array 10 nil)))))

;(defun test-atom-20 ()
;  (test-condition 20 (not (atom (macro (x) x)))))

(defun test-atom-21 ()
  (let ((cont))
    (call/cc (lambda (cc) (set cont cc)))
    (test-condition 21 (not (atom cont)))))
;end atom

;add
(defun test-add-22 ()
  (test-exception 22 (+ 1 nil) 'invalid-argument))

(defun test-add-23 ()
  (test-exception 23 (+ 1 "abc") 'invalid-argument))

(defun test-add-24 ()
  (test-exception 24 (+ 1 #\a) 'invalid-argument))

(defun test-add-25 ()
  (test-exception 25 (+ 1 '(1 2 3)) 'invalid-argument))

(defun test-add-26 ()
  (test-exception 26 (+ 1 (lambda (x) x)) 'invalid-argument))

;(defun test-add-27 ()
;  (test-exception 27 (+ 1 (macro (x) x)) 'invalid-argument))

(defun test-add-28 ()
  (let ((cont))
    (call/cc (lambda (cc) (set cont cc)))
    (test-exception 28 (+ 1 cont) 'invalid-argument)))

(defun test-add-29 ()
  (test-exception 29 (+ 1 (make-array 10 nil)) 'invalid-argument))

(defun test-add-30 ()
  (test-condition 30 (eq (+ 1 1) 2)))

(defun test-add-31 ()
  (test-condition 31 (eq (+ 1 1.0) 2)))

(defun test-add-32 ()
  (test-condition 32 (eq (+ 1.0 2.5) 3.5)))

;(test-condition 33 (eq (apply + (range 1 10 1)) 55))
;end add

;sub
(defun test-sub-33 ()
  (test-exception 33 (- 1 nil) 'invalid-argument))

(defun test-sub-34 ()
  (test-exception 34 (- 1 "abc") 'invalid-argument))

(defun test-sub-35 ()
  (test-exception 35 (- 1 #\a) 'invalid-argument))

(defun test-sub-36 ()
  (test-exception 36 (- 1 '(1 2 3)) 'invalid-argument))

(defun test-sub-37 ()
  (test-exception 37 (- 1 (lambda (x) x)) 'invalid-argument))

;(defun test-sub-38 ()
;  (test-exception 38 (- 1 (macro (x) x)) 'invalid-argument))

(defun test-sub-39 ()
  (let ((cont))
    (call/cc (lambda (cc) (set cont cc)))
    (test-exception 39 (- 1 cont) 'invalid-argument)))

(defun test-sub-40 ()
  (test-exception 40 (- 1 (make-array 10 nil)) 'invalid-argument))

(defun test-sub-41 ()
  (test-condition 41 (eq (- 1 1) 0)))

(defun test-sub-42 ()
  (test-condition 42 (eq (- 1 1.0) 0)))

(defun test-sub-43 ()
  (test-condition 43 (eq (- 1.0 2.5) -1.5)))

;test-condition 44 (eq (apply - (range 1 10 1)) -53))
;end sub

;mult
(defun test-mult-44 ()
  (test-exception 44 (* 1 nil) 'invalid-argument))

(defun test-mult-45 ()
  (test-exception 45 (* 1 "abc") 'invalid-argument))

(defun test-mult-46 ()
  (test-exception 46 (* 1 #\a) 'invalid-argument))

(defun test-mult-47 ()
  (test-exception 47 (* 1 '(1 2 3)) 'invalid-argument))

(defun test-mult-48 ()
  (test-exception 48 (* 1 (lambda (x) x)) 'invalid-argument))

;(defun test-mult-49 ()
;  (test-exception 49 (* 1 (macro (x) x)) 'invalid-argument))

(defun test-mult-50 ()
  (let ((cont))
    (call/cc (lambda (cc) (set cont cc)))
    (test-exception 50 (* 1 cont) 'invalid-argument)))

(defun test-mult-51 ()
  (test-exception 51 (* 1 (make-array 10 nil)) 'invalid-argument))

(defun test-mult-52 ()
  (test-condition 52 (eq (* 1 1) 1)))

(defun test-mult-53 ()
  (test-condition 53 (eq (* 1 1.0) 1)))

(defun test-mult-54 ()
  (test-condition 54 (eq (* 1.0 2.5) 2.5)))

;test-condition 55 (eq (apply * (range 1 10 1)) 3628800))
;end mult

;div
(defun test-div-55 ()
  (test-exception 55 (/ 1 nil) 'invalid-argument))

(defun test-div-56 ()
  (test-exception 56 (/ 1 "abc") 'invalid-argument))

(defun test-div-57 ()
  (test-exception 57 (/ 1 #\a) 'invalid-argument))

(defun test-div-58 ()
  (test-exception 58 (/ 1 '(1 2 3)) 'invalid-argument))

(defun test-div-59 ()
  (test-exception 59 (/ 1 (lambda (x) x)) 'invalid-argument))

;(defun test-div-60 ()
;  (test-exception 60 (/ 1 (macro (x) x)) 'invalid-argument))

(defun test-div-61 ()
  (let ((cont))
    (call/cc (lambda (cc) (set cont cc)))
    (test-exception 61 (/ 1 cont) 'invalid-argument)))

(defun test-div-62 ()
  (test-exception 62 (/ 1 (make-array 10 nil)) 'invalid-argument))

(defun test-div-63 ()
  (test-condition 63 (eq (/ 1 1) 1)))

(defun test-div-64 ()
  (test-condition 64 (eq (/ 1 1.0) 1)))

(defun test-div-65 ()
  (test-condition 65 (eq (/ 1.0 2.5) 0.4)))

(defun test-div-66 ()
  (test-exception 66 (/ 1 0) 'div-by-zero-exception))
;end div

;list
(defun test-list-67 ()
  (test-condition 67 (eq (list 1 2 3) '(1 2 3))))

(defun test-list-68 ()
  (test-condition 68 (eq (list) '())))
;end list

;listp
(defun test-listp-69 ()
  (test-condition 69 (listp nil)))

(defun test-listp-70 ()
  (test-condition 70 (listp '(1 2 3))))

(defun test-listp-71 ()
  (test-condition 71 (listp (cons 1 2))))

(defun test-listp-72 ()
  (test-condition 72 (not (listp 1))))

(defun test-listp-73 ()
  (test-condition 73 (not (listp 1.0))))

(defun test-listp-74 ()
  (test-condition 74 (not (listp 'a))))

(defun test-listp-75 ()
  (test-condition 75 (not (listp "abc"))))

(defun test-listp-76 ()
  (test-condition 76 (not (listp #\a))))

(defun test-listp-77 ()
  (test-condition 77 (not (listp (make-array 10 nil)))))

(defun test-listp-78 ()
  (test-condition 78 (not (listp (lambda (x) x)))))

;(defun test-listp-79 ()
;  (test-condition 79 (not (listp (macro (x) x)))))

(defun test-listp-80 ()
  (let ((cont))
    (call/cc (lambda (cc) (set cont cc)))
    (test-condition 80 (not (listp cont)))))
;end listp

;symbol-value
;(defun test-symbol-value-81 ()
;  (dolist (x (list 1 1.0 "abc" #\a (make-array 10 nil) (lambda (x) x) (macro (x) x) '(1 2 3)))
;    (test-exception 81 (symbol-value x) 'invalid-argument)))

(defun test-symbol-value-82 ()
  (let ((cont))
    (call/cc (lambda (cc) (set cont cc)))
    (test-exception 82 (symbol-value cont) 'invalid-argument)))

(defun test-symbol-value-83 ()
  (test-exception 83 (symbol-value 'abc) 'symbol-not-bound))

(define x84 100)

(defun test-symbol-value-84 ()
  (test-condition 84 (eq (symbol-value 'x84) 100)))
;end symbol-value

;gt
(defun test-gt-85 ()
  (test-exception 85 (> 1 '(1 2 3)) 'invalid-argument))

(defun test-gt-86 ()
  (test-exception 86 (> "abc" 1.0) 'invalid-argument))

(defun test-gt-87 ()
  (test-condition 87 (> 3 0)))

(defun test-gt-88 ()
  (test-condition 88 (> 2.5 0.6)))

(defun test-gt-89 ()
  (test-condition 89 (> 4.0 1)))

(defun test-gt-90 ()
  (test-condition 90 (> 25 0.1)))
;end gt

;setcar
(defun test-setcar-91 ()
  (test-exception 91 (setcar 1 2) 'not-a-cons))

(defun test-setcar-92 ()
  (let ((x '(1 2 3)))
    (setcar x 4)
    (test-condition 92 (eq (car x) 4))))
;end setcar

;setcdr
(defun test-setcdr-93 ()
  (test-exception 93 (setcdr  1 2) 'not-a-cons))

(defun test-setcdr-94 ()
  (let ((x (cons 1 2)))
    (setcdr x 4)
    (test-condition 94 (eq x (cons 1 4)))))
;end setcdr

;create-package
(defun test-create-package-95 ()
  (test-exception 95 (create-package 1) 'invalid-argument))

(defun test-create-package-96 ()
  (test-exception 96 (create-package "CORE") 'package-already-exists))

(defun test-create-package-97 ()
  (let ((x (symbol-name (gensym))))
    (create-package x)
    (test-exception 97 (create-package x) 'package-already-exists)))
;end create-package

;in-package
(defun test-in-package-98 ()
  (test-exception 98 (in-package 1) 'invalid-argument))

(defun test-in-package-99 ()
  (test-exception 99 (in-package "core") 'access-violation))

(defun test-in-package-100 ()
  (test-exception 100 (in-package (symbol-name (gensym))) 'package-not-found))

(defun test-in-package-101 ()
  (test-happy-case 101 (let ((x (symbol-name (gensym))))
                         (create-package x)
                         (in-package x)
                         (in-package "user"))))
;end in-package

;expand-macro
(define x-102 (expand-macro 1))

(defun test-expand-macro-102 ()
  (test-condition 102 (eq x-102 1)))

(define x-103 (expand-macro '(QQQQ 1 2 3)))

(defun test-expand-macro-103 ()
  (test-condition 103 (eq x-103 '(QQQQ 1 2 3))))

(define x-104 (expand-macro '(first a)))

(defmacro test-expand-macro-104 ()
  `(test-condition 104 (eq x-104 '(car a))))
;end expand-macro

;apply
(defun test-apply-105 ()
  (test-exception 105 (apply 1 2) 'invalid-argument))

(defun test-apply-106 ()
  (test-exception 106 (apply (lambda (x) (car x)) 2) 'invalid-argument))

(defun test-apply-107 ()
  (test-condition 107 (eq (apply (lambda (x) (car x)) '((1 2 3))) 1)))
;end apply

;string
(defun test-string-108 ()
  (test-exception 108 (string 1) 'invalid-argument))

(defun test-string-109 ()
  (let ((x (make-array 3 nil)))
    (array-set x 0 #\a)
    (array-set x 1 #\b)
    (array-set x 2 #\c)
    (test-condition 109 (array-eq (string "abc") x))))
;end string

;make-array
(defun test-array-110 ()
  (test-exception 110 (make-array 1.0 nil) 'invalid-argument))

(defun test-array-111 ()
  (let ((a (make-array 5 nil)))
    (array-set a 0 10)
    (array-set a 4 "abc")
    (test-condition 111 (and (eq (array-get a 0) 10)
                             (null (array-get a 1))
                             (null (array-get a 2))
                             (null (array-get a 3))
                             (eq (array-get a 4) "abc")))))
;end make-array

;array-set
(defun test-array-set-112 ()
  (test-exception 112 (array-set 1 2 3) 'invalid-argument))

(defun test-array-set-113 ()
  (test-exception 113 (array-set (make-array 3 nil) "abc" 100) 'invalid-argument))

(defun test-array-set-114 ()
  (test-exception 114 (array-set (make-array 3 nil) -1 100) 'index-out-of-bounds))

(defun test-array-set-115 ()
  (test-exception 115 (array-set (make-array 3 nil) 3 100) 'index-out-of-bounds))

(defun test-array-set-116 ()
  (let ((a (make-array 3 nil)))
    (array-set a 0 100)
    (array-set a 2 300)
    (test-condition 116 (and (eq (array-get a 0) 100)
                             (null (array-get a 1))
                             (eq (array-get a 2) 300)))))
;end array-set

;array-get
(defun test-array-get-117 ()
  (test-exception 117 (array-get 1 2 ) 'invalid-argument))

(defun test-array-get-118 ()
  (test-exception 118 (array-get (make-array 3 nil) "abc") 'invalid-argument))

(defun test-array-get-119 ()
  (test-exception 119 (array-get (make-array 3 nil) -1) 'index-out-of-bounds))

(defun test-array-get-120 ()
  (test-exception 120 (array-get (make-array 3 nil) 3) 'index-out-of-bounds))

(defun test-array-get-121 ()
  (test-exception 121 (array-get "abc" 3) 'index-out-of-bounds))

(defun test-array-get-122 ()
  (test-condition 122 (eq (array-get "abc" 0) #\a)))

;second positive test case covered in array-set's test cases (last one)
;end array-get

;sub-array
(defun test-sub-array-123 ()
  (test-exception 123 (sub-array 1 2 3) 'invalid-argument))

(defun test-sub-array-124 ()
  (test-exception 124 (sub-array (make-array 3 nil) "abc" 3) 'invalid-argument))

(defun test-sub-array-125 ()
  (test-exception 125 (sub-array (make-array 3 nil) -1 3) 'invalid-argument))

(defun test-sub-array-126 ()
  (test-exception 126 (sub-array (make-array 3 nil) 0 "abc") 'invalid-argument))

(defun test-sub-array-127 ()
  (test-exception 127 (sub-array (make-array 3 nil) 2 5) 'index-out-of-bounds))

(defun test-sub-array-128 ()
  (let ((a (make-array 5 nil)))
    (dotimes (i 5)
      (array-set a i i))
    (let ((s (sub-array a 0 3)))
      (test-condition 128 (and (eq (array-get s 0) 0)
                               (eq (array-get s 1) 1)
                               (eq (array-get s 2) 2)
                               (eq (array-length s) 3))))))

;end sub-array

;array-length
(defun test-array-length-129 ()
  (test-exception 129 (array-length 1) 'invalid-argument))

(defun test-array-length-130 ()
  (test-condition 130 (eq (array-length (make-array 3 nil)) 3)))
;end array-length

;print-string
(defun test-print-string-131 ()
  (test-exception 131 (print-string 1) 'invalid-argument))

(defun test-print-string-132 ()
  (test-happy-case 132 (print-string "Diagnostic from test case #132")))

(defun test-print-string-133 ()
  (test-happy-case 133 (print-string (string "Diagnostic from test case #133"))))
;end print-string

;create-image
(defun test-create-image-134 ()
  (test-exception 134 (create-image 1) 'invalid-argument))

(defun test-create-image-135 ()
  (test-happy-case 135 (progn (create-image "test1.image"))))

(defun test-create-image-136 ()
  (test-happy-case 136 (progn (create-image (string "test2.image")))))
;end create-image

;load-foreign-library
(defun test-load-foreign-library-137 ()
  (test-exception 137 (load-foreign-library 1) 'invalid-argument))

(defun test-load-foreign-library-138 ()
  (test-happy-case 138 (load-foreign-library "libtest.so")))

(defun test-load-foreign-library-139 ()
  (test-happy-case 139 (load-foreign-library (string "libtest.so"))))
;end load-foreign-library

;call-foreign-function
;(defun test-call-foreign-function-140 ()
;  (test-exception 140 (call-foreign-function 1 2 3) 'not-a-cons))

;(defun test-call-foreign-function-141 ()
;  (test-exception 141 (call-foreign-function "abc" 1 2) 'not-a-cons))

;(defun test-call-foreign-function-142 ()
;  (test-exception 142 (call-foreign-function (string "abc") 1 2) 'not-a-cons))

;(defun test-call-foreign-function-143 ()
;  (test-exception 143 (call-foreign-function "abc" integer 1) 'not-a-cons))

;(defun test-call-foreign-function-144 ()
;  (test-exception 144 (call-foreign-function "abc" integer (1 2 3)) 'not-a-cons))

(defun test-call-foreign-function-140 ()
  (test-exception 140 (call-foreign-function "abc" integer ((1) (2) (3))) 'invalid-argument))

(defun test-call-foreign-function-141 ()
  (test-exception 141 (call-foreign-function "abc" integer ((1 2 3) (4 5 6) (7 8 9))) 'invalid-argument))

(defun test-call-foreign-function-142 ()
  (test-exception 142 (call-foreign-function "abc" integer ((1 2) (3 4) (5 6))) 'invalid-argument))

(define x-ff 0)

(defun test-call-foreign-function-143 ()
  (test-exception 143 (call-foreign-function "abc" integer ((x-ff 2) (3 4) (5 6))) 'invalid-argument))

(defun test-call-foreign-function-144 ()
  (test-exception 144 (call-foreign-function "abc" integer (("abc" integer))) 'invalid-argument))

(defun test-call-foreign-function-145 ()
  (test-exception 145 (call-foreign-function "abc" integer (("abc" float))) 'invalid-argument))

(defun test-call-foreign-function-146 ()
  (test-exception 146 (call-foreign-function "abc" integer (("abc" character))) 'invalid-argument))

(defun test-call-foreign-function-147 ()
  (test-exception 147 (call-foreign-function "abc" integer ((1 character-pointer))) 'invalid-argument))

(defun test-call-foreign-function-148 ()
  (test-exception 148 (call-foreign-function "abc" integer (("abc" integer-pointer))) 'invalid-argument))

(defun test-call-foreign-function-149 ()
  (test-exception 149 (call-foreign-function "abc" integer (("abc" float-pointer))) 'invalid-argument))

(defun test-call-foreign-function-150 ()
  (test-exception 150 (call-foreign-function "abc" integer (("abc" some-other-type))) 'invalid-argument))

(defun test-call-foreign-function-151 ()
  (let ((i 10)
        (x (string "abc"))
        (f 19.56))
    (test-condition 151 (eq (call-foreign-function "fn_ret_int" integer
                                                   ((i integer)
                                                    (6.5 float)
                                                    (#\a character)
                                                    (x character-pointer)))
                            100))))

(defun test-call-foreign-function-152 ()
  (let ((i 10)
        (x (string "abc"))
        (f 19.56))
    (test-condition 152 (eq (call-foreign-function "fn_ret_float" float
                                                   ((10 integer)
                                                    (6.5 float)
                                                    (#\a character)
                                                    (x character-pointer)))
                            13))))

(defun test-call-foreign-function-153 ()
  (let ((i 10)
        (x (string "abc"))
        (f 19.56))
    (test-condition 153 (eq (call-foreign-function "fn_ret_char" character
                                                   ((10 integer)
                                                    (6.5 float)
                                                    (#\a character)
                                                    (x character-pointer)))
                            #\a))))

(defun test-call-foreign-function-154 ()
  (let ((i 10)
        (x (string "abc"))
        (f 19.56))
    (test-condition 154 (eq (call-foreign-function "fn_ret_char_ptr" character-pointer
                                                   ((10 integer)
                                                    (6.5 float)
                                                    (#\a character)
                                                    (x character-pointer)))
                            "aaaaaaaaaa"))))

(defun test-call-foreign-function-155 ()
  (let ((i 10)
        (x (string "abc"))
        (f 19.56))
    (call-foreign-function "fn_arg_int_ptr" 
                           integer
                           ((i integer-pointer)))
    (test-condition 155 (eq i 100))))

(defun test-call-foreign-function-156 ()
  (let ((i 10)
        (x (string "abc"))
        (f 19.56))
    (call-foreign-function "fn_arg_float_ptr" 
                           integer
                           ((f float-pointer)))
    (test-condition 156 (float-eq f 100.97))))

(defun test-call-foreign-function-157 ()
  (let ((i 10)
        (x (string "abc"))
        (f 19.56))
    (call-foreign-function "fn_arg_char_ptr" 
                           integer
                           ((x character-pointer)))
    (test-condition 157 (eq x "ABC"))))

(defun test-call-foreign-function-158 ()
  (let ((i 10)
        (x (string "abc"))
        (f 19.56))
    (test-happy-case 158 (call-foreign-function "function_ret_void" void
                                                ((10 integer)
                                                 (6.5 float)
                                                 (#\a character)
                                                 (x character-pointer))))))
;end call-foreign-function

;load-file
(defun test-load-file-159 ()
  (test-exception 159 (load-file 1) 'invalid-argument))

(defun test-load-file-160 ()
  (test-exception 160 (load-file (concat-strings (symbol-name (gensym)) ".lisp")) 'file-open-error))

;happy case not tested since LOAD-FILE is tested in running this file (unit_tests.lisp) itself
;end load-file

;predicate functions that test for object type
(defun test-consp-161 ()
  (test-condition 161 (null (consp 1))))

(defun test-consp-162 ()
  (test-condition 162 (consp '(1 2))))

(defun test-integerp-163 ()
  (test-condition 163 (null (integerp 1.0))))

(defun test-integerp-164 ()
  (test-condition 164 (integerp 1)))

(defun test-floatp-165 ()
  (test-condition 165 (null (floatp 1))))

(defun test-floatp-166 ()
  (test-condition 166 (floatp 1.0)))

(defun test-characterp-167 ()
  (test-condition 167 (null (characterp 1))))

(defun test-characterp-168 ()
  (test-condition 168 (characterp #\a)))

(defun test-stringp-169 ()
  (test-condition 169 (null (stringp 1))))

(defun test-stringp-170 ()
  (test-condition 170 (stringp "abc")))

(defun test-stringp-171 ()
  (test-condition 171 (stringp (string "abc"))))

(defun test-symbolp-172 ()
  (test-condition 172 (null (symbolp 1))))

(defun test-symbolp-173 ()
  (test-condition 173 (symbolp 'a)))

(defun test-symbolp-174 ()
  (test-condition 174 (symbolp (gensym))))

(defun test-symbolp-175 ()
  (test-condition 175 (symbolp (symbol "abc"))))

(defun test-arrayp-176 ()
  (test-condition 176 (null (arrayp 1))))

(defun test-arrayp-177 ()
  (test-condition 177 (arrayp (make-array 3 nil))))

(defun test-arrayp-178 ()
  (test-condition 178 (arrayp (string "abc"))))

(defun test-closurep-179 ()
  (test-condition 179 (null (closurep 1))))

(defun test-closurep-180 ()
  (test-condition 180 (closurep (lambda (x) x))))

(defun test-closurep-181 ()
  (test-condition 181 (closurep (lambda (lst) (car (cdr lst))))))

(defun test-macrop-182 ()
  (test-condition 182 (null (macrop 1))))

;(defun test-macrop-188 ()
;  (test-condition 188 (macrop (macro (x) x))))

(defmacro my-first (lst)
  `(car ,lst))

(defun test-macrop-183 ()
  (test-condition 183 (macrop my-first)))

(defun test-continuationp-184 ()
  (test-condition 184 (null (continuationp 1))))

(define cont-185 nil)

(defun test-continuationp-185 ()
  (call/cc (lambda (cc) (set cont-185 cc)))
  (test-condition 185 (continuationp cont-185)))
;end predicate functions that test for object type

;format
(defun test-format-186 ()
  (test-exception 186 (format 1 2) 'invalid-argument))

(defun test-format-187 ()
  (test-exception 187 (format nil "%d" 'a) 'invalid-argument))

(defun test-format-188 ()
  (test-exception 188 (format nil "%d" (make-array 3 nil)) 'invalid-argument))

(defun test-format-189 ()
  (test-exception 189 (format nil "%d" (lambda (x) x)) 'invalid-argument))

;(defun test-format-190 ()
;  (test-exception 190 (format nil "%d" (macro (x) x)) 'invalid-argument))

(defun test-format-191 ()
  (let ((cont))
    (call/cc (lambda (cc) (set cont cc)))
    (test-exception 191 (format nil "%d" cont) 'invalid-argument)))

(defun test-format-192 ()
  (test-happy-case 192 (progn (format nil "Diagnostic from test case #192") (newline nil))))

(defun test-format-193 ()
  (test-happy-case 193 (progn (format nil "Diagnostic from test case #%d %d" 193 10) (newline nil))))

(defun test-format-194 ()
  (test-happy-case 194 (progn (format nil "Diagnostic from test case #%d %0.2f" 194 3.1415) (newline nil))))

(defun test-format-195 ()
  (test-happy-case 195 (progn (format nil "Diagnostic from test case #%d %c" 195 #\a) (newline nil))))

(defun test-format-196 ()
  (test-happy-case 196 (progn (format nil "Diagnostic from test case #%d %s" 196 "abc") (newline nil))))
;end-format

;clone
(defun test-clone-197 ()
  (dolist (x (list 1 1.0 #\a "abc" 'a '(1 2 3)))
    (test-condition 197 (eq x (clone x)))))

(defun test-clone-198 ()
  (let ((val (make-array 3 nil)))
    (array-set val 0 0)
    (array-set val 1 1)
    (array-set val 2 2)
    (test-condition 198 (array-eq val (clone val)))))

(defun test-clone-199 ()
  (let ((cont))
    (call/cc (lambda (cc) (set cont cc)))
    (test-condition 199 (eq cont (clone cont)))))
;end clone

;return-from
(defun test-return-from-200 ()
  (test-exception 200 (return-from 1 2 3) 'exception))

(define x-ret-from 0)

(defun ret-from-fn (n)
  (if (> n 0)
      (progn (set x-ret-from 10) 
	     (return-from ret-from-fn 20)))
  (set x-ret-from 30)
  40)

(defun test-ret-from-201 ()
  (let ((val))
    (set val (ret-from-fn 10))
    (test-condition 201 (and (eq x-ret-from 10) (eq val 20)))))

(defun test-ret-from-202 ()
  (let ((val))
    (set val (ret-from-fn -10))
  (test-condition 202 (and (eq x-ret-from 30) (eq val 40)))))
;end return-from

;symbol
(defun test-symbol-203 ()
  (test-exception 203 (symbol 1) 'invalid-argument))

(defun test-symbol-204 ()
  (test-condition 204 (eq (symbol "abc") 'abc)))

(defun test-symbol-205 ()
  (test-condition 205 (eq (symbol (string "abc")) 'abc)))
;end symbol

;symbol-name
(defun test-symbol-name-206 ()
  (test-exception 206 (symbol-name 1) 'invalid-argument))

(defun test-symbol-name-207 ()
  (test-condition 207 (eq (symbol-name 'abc) "ABC")))
;end symbol-name


