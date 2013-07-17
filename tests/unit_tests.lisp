;;  Copyright 2011-2013 Rajesh Jayaprakash <rajesh.jayaprakash@gmail.com>

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

(format "Running unit test cases...")
(println)

(define passed-cases 0)
(define failed-cases 0)

(defmacro test-condition (id condition)
  `(try (if ,condition 
            (incf passed-cases)
          (progn (format "Test case #%d failed" ,id) (println) (incf failed-cases)))
        (catch (e)
          (progn (format "Test case #%d failed" ,id) (println) (incf failed-cases)))))

(defmacro test-exception (id body excp)
  `(try ,body
        (catch (e)
          (if (eq (car e) ,excp)
              (incf passed-cases)
            (progn (format "Test case #%d failed" ,id) (println) (incf failed-cases))))))

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
(test-exception 7 (eq 1) 'arg-mismatch)

(test-exception 8 (eq) 'arg-mismatch)

(test-condition 9 (eq 1 1.0))

(test-condition 10 (eq 'a 'a))

(test-condition 11 (not (eq 'a 'b)))
;end eq

;cons
(test-exception 12 (cons 1) 'arg-mismatch)

(test-exception 13 (cons) 'arg-mismatch)

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
(test-exception 26 (+) 'arg-mismatch)

(test-exception 27 (+ 1) 'arg-mismatch)

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
(test-exception 40 (-) 'arg-mismatch)

(test-exception 41 (- 1) 'arg-mismatch)

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
(test-exception 54 (*) 'arg-mismatch)

(test-exception 55 (* 1) 'arg-mismatch)

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
(test-exception 68 (/) 'arg-mismatch)

(test-exception 69 (/ 1) 'arg-mismatch)

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
(test-exception 85 (listp) 'arg-mismatch)

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
(test-exception 98 (symbol-value) 'arg-mismatch)

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
(test-exception 103 (>) 'arg-mismatch)

(test-exception 104 (> 1) 'arg-mismatch)

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
(test-exception 112 (setcar) 'arg-mismatch)

(test-exception 113 (setcar 1) 'arg-mismatch)

(test-exception 114 (setcar 1 2) 'arg-mismatch)

(let ((x '(1 2 3)))
  (setcar x 4)
  (test-condition 115 (eq (car x) 4)))
;end setcar

;setcdr
(test-exception 116 (setcdr) 'arg-mismatch)

(test-exception 117 (setcdr 1) 'arg-mismatch)

(test-exception 118 (setcdr  1 2) 'arg-mismatch)

(let ((x (cons 1 2)))
  (setcdr x 4)
  (test-condition 119 (eq x (cons 1 4))))
;end setcdr

;create-package
(test-exception 120 (create-package) 'arg-mismatch)

(test-exception 121 (create-package 1 2) 'arg-mismatch)

(test-exception 122 (create-package 1) 'invalid-argument)

(test-exception 123 (create-package "CORE") 'package-already-exists)

(let ((x (symbol-name (gensym))))
  (create-package x)
  (test-exception 124 (create-package x) 'package-already-exists))
;end create-package

;in-package
(test-exception 125 (in-package) 'arg-mismatch)

(test-exception 126 (in-package 1 2) 'arg-mismatch)

(test-exception 127 (in-package 1) 'invalid-argument)

(test-exception 128 (in-package "core") 'access-violation)

(test-exception 129 (in-package (symbol-name (gensym))) 'package-not-found)

(try (let ((x (symbol-name (gensym))))
       (create-package x)
       (in-package x)
       (in-package "user")
       (incf passed-cases))
     (catch (e)
       (progn (format "Test case #%d failed" 130) (println) (incf failed-cases))))
;end in-package

;expand-macro
(test-exception 131 (expand-macro) 'arg-mismatch)

(test-exception 132 (expand-macro 1 2) 'arg-mismatch)

(test-exception 133 (expand-macro 1) 'invalid-argument)

(test-exception 134 (expand-macro '(QQQQ 1 2 3)) 'macro-undefined)

(let1 ((first (macro (lst) `(car ,lst)))
       (x (expand-macro '(first a))))
  (test-condition 135 (eq x '(car a))))
;end expand-macro

;apply
(test-exception 136 (apply) 'arg-mismatch)

(test-exception 137 (apply 1) 'arg-mismatch)

(test-exception 138 (apply 1 2 3) 'arg-mismatch)

(test-exception 139 (apply 1 2) 'invalid-argument)

(test-exception 140 (apply car 2) 'invalid-argument)

(test-condition 141 (eq (apply car '((1 2 3))) 1))
;end apply

;string
(test-exception 142 (string) 'arg-mismatch)

(test-exception 143 (string 1 2) 'arg-mismatch)

(test-exception 144 (string 1) 'invalid-argument)

(let ((x (make-array 3)))
  (array-set x 0 #\a)
  (array-set x 1 #\b)
  (array-set x 2 #\c)
  (test-condition 145 (array-eq (string "abc") x)))
;end string

;make-array
(test-exception 146 (make-array) 'arg-mismatch)

(test-exception 147 (make-array 1 1 1) 'arg-mismatch)

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
(test-exception 150 (array-set) 'arg-mismatch)

(test-exception 151 (array-set 1) 'arg-mismatch)

(test-exception 152 (array-set 1 2) 'arg-mismatch)

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
(test-exception 158 (array-get) 'arg-mismatch)

(test-exception 159 (array-get 1) 'arg-mismatch)

(test-exception 160 (array-get 1 2 ) 'invalid-argument)

(test-exception 161 (array-get (make-array 3) "abc") 'invalid-argument)

(test-exception 162 (array-get (make-array 3) -1) 'index-out-of-bounds)

(test-exception 163 (array-get (make-array 3) 3) 'index-out-of-bounds)

(test-exception 164 (array-get "abc" 3) 'index-out-of-bounds)

(test-condition 165 (eq (array-get "abc" 0) #\a))

;second positive test case covered in array-set's test cases (last one)
;end array-get

;sub-array
(test-exception 166 (sub-array) 'arg-mismatch)

(test-exception 167 (sub-array 1) 'arg-mismatch)

(test-exception 168 (sub-array 1 2) 'arg-mismatch)

(test-exception 169 (sub-array 1 2 3 4) 'arg-mismatch)

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


(format "%d of %d test cases passed (%.2f%%)" 
        passed-cases 
        (+ passed-cases failed-cases) 
        (/ (* 100.0 passed-cases) (* 1.0 (+ passed-cases failed-cases))))

(if (not (eq failed-cases 0))
    (format "ATTENTION! Some unit test cases have failed."))

(println)