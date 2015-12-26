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

(create-package "unit-tests-core")

(in-package "unit-tests-core")

(define passed-cases 0)
(define failed-cases 0)

(define test-functions nil)

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

(defmacro add-test-function (fn)
  `(set test-functions (cons ,fn test-functions)))

;;car
(defun test-car-1 ()
  (test-exception 1 (car 1) 'not-a-cons))
(add-test-function test-car-1)

(defun test-car-2 ()
  (let ((x (car '(1 2 3))))
    (test-condition 2 (eq x 1))))
(add-test-function test-car-2)

(defun test-car-3 ()
  (let ((x (car nil)))
    (test-condition 3 (null x))))
(add-test-function test-car-3)
;end car

;;cdr
(defun test-cdr-4 ()
  (test-exception 4 (cdr 1) 'not-a-cons))
(add-test-function test-cdr-4)

(defun test-cdr-5 ()
  (let ((x (cdr '(1 2 3))))
    (test-condition 5 (eq x '(2 3)))))
(add-test-function test-cdr-5)

(defun test-cdr-6 ()
  (let ((x (cdr nil)))
    (test-condition 6 (null x))))
(add-test-function test-cdr-6)
;end cdr

;eq
(defun test-eq-7 ()
  (test-condition 7 (eq 1 1.0)))
(add-test-function test-eq-7)

(defun test-eq-8 ()
  (test-condition 8 (eq 'a 'a)))
(add-test-function test-eq-8)

(defun test-eq-9 ()
  (test-condition 9 (not (eq 'a 'b))))
(add-test-function test-eq-9)
;end eq

;cons
(defun test-cons-10 ()
  (let ((x (cons 1 2)))
    (test-condition 10 (and (eq (car x) 1)
                            (eq (cdr x) 2)))))
(add-test-function test-cons-10)
;end cons

;atom
(defun test-atom-11 ()
  (test-condition 11 (atom 1)))
(add-test-function test-atom-11)

(defun test-atom-12 ()
  (test-condition 12 (atom nil)))
(add-test-function test-atom-12)

(defun test-atom-13 ()
  (test-condition 13 (atom 1.0)))
(add-test-function test-atom-13)

(defun test-atom-14 ()
  (test-condition 14 (atom "abc")))
(add-test-function test-atom-14)

(defun test-atom-15 ()
  (test-condition 15 (atom #\a)))
(add-test-function test-atom-15)

(defun test-atom-16 ()
  (test-condition 16 (atom 'a)))
(add-test-function test-atom-16)

(defun test-atom-17 ()
  (test-condition 17 (not (atom '(1 2 3)))))
(add-test-function test-atom-17)

(defun test-atom-18 ()
  (test-condition 18 (not (atom (lambda (x) x)))))
(add-test-function test-atom-18)

(defun test-atom-19 ()
  (test-condition 19 (not (atom (make-array 10 nil)))))
(add-test-function test-atom-19)

(defun test-atom-20 ()
  (test-condition 20 (not (atom (macro (x) x)))))
(add-test-function test-atom-20)

(defun test-atom-21 ()
  (let ((cont))
    (call-cc (lambda (cc) (set cont cc)))
    (test-condition 21 (not (atom cont)))))
(add-test-function test-atom-21)
;end atom

;add
(defun test-add-22 ()
  (test-exception 22 (+ 1 nil) 'invalid-argument))
(add-test-function test-add-22)

(defun test-add-23 ()
  (test-exception 23 (+ 1 "abc") 'invalid-argument))
(add-test-function test-add-23)

(defun test-add-24 ()
  (test-exception 24 (+ 1 #\a) 'invalid-argument))
(add-test-function test-add-24)

(defun test-add-25 ()
  (test-exception 25 (+ 1 '(1 2 3)) 'invalid-argument))
(add-test-function test-add-25)

(defun test-add-26 ()
  (test-exception 26 (+ 1 (lambda (x) x)) 'invalid-argument))
(add-test-function test-add-26)

(defun test-add-27 ()
  (test-exception 27 (+ 1 (macro (x) x)) 'invalid-argument))
(add-test-function test-add-27)

(defun test-add-28 ()
  (let ((cont))
    (call-cc (lambda (cc) (set cont cc)))
    (test-exception 28 (+ 1 cont) 'invalid-argument)))
(add-test-function test-add-28)

(defun test-add-29 ()
  (test-exception 29 (+ 1 (make-array 10 nil)) 'invalid-argument))
(add-test-function test-add-29)

(defun test-add-30 ()
  (test-condition 30 (eq (+ 1 1) 2)))
(add-test-function test-add-30)

(defun test-add-31 ()
  (test-condition 31 (eq (+ 1 1.0) 2)))
(add-test-function test-add-31)

(defun test-add-32 ()
  (test-condition 32 (eq (+ 1.0 2.5) 3.5)))
(add-test-function test-add-32)

;(test-condition 33 (eq (apply + (range 1 10 1)) 55))
;end add

;sub
(defun test-sub-33 ()
  (test-exception 33 (- 1 nil) 'invalid-argument))
(add-test-function test-sub-33)

(defun test-sub-34 ()
  (test-exception 34 (- 1 "abc") 'invalid-argument))
(add-test-function test-sub-34)

(defun test-sub-35 ()
  (test-exception 35 (- 1 #\a) 'invalid-argument))
(add-test-function test-sub-35)

(defun test-sub-36 ()
  (test-exception 36 (- 1 '(1 2 3)) 'invalid-argument))
(add-test-function test-sub-36)

(defun test-sub-37 ()
  (test-exception 37 (- 1 (lambda (x) x)) 'invalid-argument))
(add-test-function test-sub-37)

(defun test-sub-38 ()
  (test-exception 38 (- 1 (macro (x) x)) 'invalid-argument))
(add-test-function test-sub-38)

(defun test-sub-39 ()
  (let ((cont))
    (call-cc (lambda (cc) (set cont cc)))
    (test-exception 39 (- 1 cont) 'invalid-argument)))
(add-test-function test-sub-39)

(defun test-sub-40 ()
  (test-exception 40 (- 1 (make-array 10 nil)) 'invalid-argument))
(add-test-function test-sub-40)

(defun test-sub-41 ()
  (test-condition 41 (eq (- 1 1) 0)))
(add-test-function test-sub-41)

(defun test-sub-42 ()
  (test-condition 42 (eq (- 1 1.0) 0)))
(add-test-function test-sub-42)

(defun test-sub-43 ()
  (test-condition 43 (eq (- 1.0 2.5) -1.5)))
(add-test-function test-sub-43)

;test-condition 44 (eq (apply - (range 1 10 1)) -53))
;end sub

;mult
(defun test-mult-44 ()
  (test-exception 44 (* 1 nil) 'invalid-argument))
(add-test-function test-mult-44)

(defun test-mult-45 ()
  (test-exception 45 (* 1 "abc") 'invalid-argument))
(add-test-function test-mult-45)

(defun test-mult-46 ()
  (test-exception 46 (* 1 #\a) 'invalid-argument))
(add-test-function test-mult-46)

(defun test-mult-47 ()
  (test-exception 47 (* 1 '(1 2 3)) 'invalid-argument))
(add-test-function test-mult-47)

(defun test-mult-48 ()
  (test-exception 48 (* 1 (lambda (x) x)) 'invalid-argument))
(add-test-function test-mult-48)

(defun test-mult-49 ()
  (test-exception 49 (* 1 (macro (x) x)) 'invalid-argument))
(add-test-function test-mult-49)

(defun test-mult-50 ()
  (let ((cont))
    (call-cc (lambda (cc) (set cont cc)))
    (test-exception 50 (* 1 cont) 'invalid-argument)))
(add-test-function test-mult-50)

(defun test-mult-51 ()
  (test-exception 51 (* 1 (make-array 10 nil)) 'invalid-argument))
(add-test-function test-mult-51)

(defun test-mult-52 ()
  (test-condition 52 (eq (* 1 1) 1)))
(add-test-function test-mult-52)

(defun test-mult-53 ()
  (test-condition 53 (eq (* 1 1.0) 1)))
(add-test-function test-mult-53)

(defun test-mult-54 ()
  (test-condition 54 (eq (* 1.0 2.5) 2.5)))
(add-test-function test-mult-54)

;test-condition 55 (eq (apply * (range 1 10 1)) 3628800))
;end mult

;div
(defun test-div-55 ()
  (test-exception 55 (/ 1 nil) 'invalid-argument))
(add-test-function test-div-55)

(defun test-div-56 ()
  (test-exception 56 (/ 1 "abc") 'invalid-argument))
(add-test-function test-div-56)

(defun test-div-57 ()
  (test-exception 57 (/ 1 #\a) 'invalid-argument))
(add-test-function test-div-57)

(defun test-div-58 ()
  (test-exception 58 (/ 1 '(1 2 3)) 'invalid-argument))
(add-test-function test-div-58)

(defun test-div-59 ()
  (test-exception 59 (/ 1 (lambda (x) x)) 'invalid-argument))
(add-test-function test-div-59)

(defun test-div-60 ()
  (test-exception 60 (/ 1 (macro (x) x)) 'invalid-argument))
(add-test-function test-div-60)

(defun test-div-61 ()
  (let ((cont))
    (call-cc (lambda (cc) (set cont cc)))
    (test-exception 61 (/ 1 cont) 'invalid-argument)))
(add-test-function test-div-61)

(defun test-div-62 ()
  (test-exception 62 (/ 1 (make-array 10 nil)) 'invalid-argument))
(add-test-function test-div-62)

(defun test-div-63 ()
  (test-condition 63 (eq (/ 1 1) 1)))
(add-test-function test-div-63)

(defun test-div-64 ()
  (test-condition 64 (eq (/ 1 1.0) 1)))
(add-test-function test-div-64)

(defun test-div-65 ()
  (test-condition 65 (eq (/ 1.0 2.5) 0.4)))
(add-test-function test-div-65)

(defun test-div-66 ()
  (test-exception 66 (/ 1 0) 'div-by-zero-exception))
(add-test-function test-div-66)
;end div

;list
(defun test-list-67 ()
  (test-condition 67 (eq (list 1 2 3) '(1 2 3))))
(add-test-function test-list-67)

(defun test-list-68 ()
  (test-condition 68 (eq (list) '())))
(add-test-function test-list-68)
;end list

;listp
(defun test-listp-69 ()
  (test-condition 69 (listp nil)))
(add-test-function test-listp-69)

(defun test-listp-70 ()
  (test-condition 70 (listp '(1 2 3))))
(add-test-function test-listp-70)

(defun test-listp-71 ()
  (test-condition 71 (listp (cons 1 2))))
(add-test-function test-listp-71)

(defun test-listp-72 ()
  (test-condition 72 (not (listp 1))))
(add-test-function test-listp-72)

(defun test-listp-73 ()
  (test-condition 73 (not (listp 1.0))))
(add-test-function test-listp-73)

(defun test-listp-74 ()
  (test-condition 74 (not (listp 'a))))
(add-test-function test-listp-74)

(defun test-listp-75 ()
  (test-condition 75 (not (listp "abc"))))
(add-test-function test-listp-75)

(defun test-listp-76 ()
  (test-condition 76 (not (listp #\a))))
(add-test-function test-listp-76)

(defun test-listp-77 ()
  (test-condition 77 (not (listp (make-array 10 nil)))))
(add-test-function test-listp-77)

(defun test-listp-78 ()
  (test-condition 78 (not (listp (lambda (x) x)))))
(add-test-function test-listp-78)

(defun test-listp-79 ()
  (test-condition 79 (not (listp (macro (x) x)))))
(add-test-function test-listp-79)

(defun test-listp-80 ()
  (let ((cont))
    (call-cc (lambda (cc) (set cont cc)))
    (test-condition 80 (not (listp cont)))))
(add-test-function test-listp-80)
;end listp

;symbol-value
(defun test-symbol-value-81 ()
  (dolist (x (list 1 1.0 "abc" #\a (make-array 10 nil) (lambda (x) x) (macro (x) x) '(1 2 3)))
    (test-exception 99 (symbol-value x) 'invalid-argument)))
(add-test-function test-symbol-value-81)

(defun test-symbol-value-82 ()
  (let ((cont))
    (call-cc (lambda (cc) (set cont cc)))
    (test-exception 82 (symbol-value cont) 'invalid-argument)))
(add-test-function test-symbol-value-82)

(defun test-symbol-value-83 ()
  (test-exception 83 (symbol-value 'abc) 'symbol-not-bound))
(add-test-function test-symbol-value-83)

(defun test-symbol-value-84 ()
  (let ((x 100))
    (test-condition 84 (eq (symbol-value 'x) 100))))
(add-test-function test-symbol-value-84)
;end symbol-value

;gt
(defun test-gt-85 ()
  (test-exception 85 (> 1 '(1 2 3)) 'invalid-argument))
(add-test-function test-gt-85)

(defun test-gt-86 ()
  (test-exception 86 (> "abc" 1.0) 'invalid-argument))
(add-test-function test-gt-86)

(defun test-gt-87 ()
  (test-condition 87 (> 3 0)))
(add-test-function test-gt-87)

(defun test-gt-88 ()
  (test-condition 88 (> 2.5 0.6)))
(add-test-function test-gt-88)

(defun test-gt-89 ()
  (test-condition 89 (> 4.0 1)))
(add-test-function test-gt-89)

(defun test-gt-90 ()
  (test-condition 90 (> 25 0.1)))
(add-test-function test-gt-90)
;end gt

;setcar
(defun test-setcar-91 ()
  (test-exception 91 (setcar 1 2) 'arg-mismatch))
(add-test-function test-setcar-91)

(defun test-setcar-92 ()
  (let ((x '(1 2 3)))
    (setcar x 4)
    (test-condition 92 (eq (car x) 4))))
(add-test-function test-setcar-92)
;end setcar

;setcdr
(defun test-setcdr-93 ()
  (test-exception 93 (setcdr  1 2) 'arg-mismatch))
(add-test-function test-setcdr-93)

(defun test-setcdr-94 ()
  (let ((x (cons 1 2)))
    (setcdr x 4)
    (test-condition 94 (eq x (cons 1 4)))))
(add-test-function test-setcdr-94)
;end setcdr

;create-package
(defun test-create-package-95 ()
  (test-exception 95 (create-package 1) 'invalid-argument))
(add-test-function test-create-package-95)

(defun test-create-package-96 ()
  (test-exception 96 (create-package "CORE") 'package-already-exists))
(add-test-function test-create-package-96)

(defun test-create-package-97 ()
  (let ((x (symbol-name (gensym))))
    (create-package x)
    (test-exception 97 (create-package x) 'package-already-exists)))
(add-test-function test-create-package-97)
;end create-package

;in-package
(defun test-in-package-98 ()
  (test-exception 98 (in-package 1) 'invalid-argument))
(add-test-function test-in-package-98)

(defun test-in-package-99 ()
  (test-exception 99 (in-package "core") 'access-violation))
(add-test-function test-in-package-99)

(defun test-in-package-100 ()
  (test-exception 100 (in-package (symbol-name (gensym))) 'package-not-found))
(add-test-function test-in-package-100)

(defun test-in-package-101 ()
  (test-happy-case 101 (let ((x (symbol-name (gensym))))
                         (create-package x)
                         (in-package x)
                         (in-package "user"))))
(add-test-function test-in-package-101)
;end in-package

(defun run-all-tests ()
  (format nil "Running unit test cases...")
  (newline nil)
  (dolist (fn (reverse test-functions))
    (fn))
  (format nil 
          "%d of %d test cases passed (%.2f%%)" 
          passed-cases 
          (+ passed-cases failed-cases) 
          (/ (* 100.0 passed-cases) (* 1.0 (+ passed-cases failed-cases))))
  (if (not (eq failed-cases 0))
      (format nil "ATTENTION! Some unit test cases have failed.")))
