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

(create-package "utils")

(in-package "utils")

(defun random ()
  (call-foreign-function "plisp_random" 'float nil))

(defun dot-product (a b)
  (if (or (null a) (null b))
      0
      (+ (* (first a) (first b))
         (dot-product (rest a) (rest b)))))

(defun random-interval (low high)
  (+ low (* (- high low) (random))))

(defun all-permutations (lst)
  (if (null lst) '(())
      (mapcan (lambda (x)
		(mapcar (lambda (y) 
                          (cons x y))
			(all-permutations (remove x lst 1)))) 
              lst)))

(defun permutations (lst n)
  (if (eq n 1)
      (mapcar (lambda (x) 
                (list x))
              lst)
    (mapcan (lambda (x)
              (mapcar (lambda (y) 
                        (cons x y))
                      (permutations (remove x lst 1) (- n 1))))
            lst)))

(defun same-list-p (lst1 lst2)
  (and (eq (length lst1) (length lst2))
       (all-elements-exist-p lst1 lst2)
       (all-elements-exist-p lst2 lst1)))

(defun my-and (lst)
  (if (null lst) 
      't
      (and (not (null (car lst))) (my-and (cdr lst)))))

(defun all-elements-exist-p (lst1 lst2)
  (my-and (mapcar (lambda (x) (find x lst2 eq)) lst1)))

(defun combinations (lst n)
  (remove-duplicates (permutations lst n) same-list-p))

(defun all-combinations (lst)
  (mapcar (lambda (x) (combinations lst x)) (range 1 (length lst) 1)))

(defun first-n (lst n)
  (let ((l (length lst)))
    (if (> l n) (butlast lst (- l n)) lst)))

(defun quicksort (lst)
  (if (or (null lst) (<= (length lst) 1))
      lst
    (let ((pivot (first lst)))
      (concat (quicksort (remove-if (lambda (x) (>= x pivot)) lst))
              (remove-if (lambda (x) (not (eq x pivot))) lst)
              (quicksort (remove-if (lambda (x) (<= x pivot)) lst))))))

(defun concat-strings (str1 str2)
  (let ((l1 (array-length str1))
        (l2 (array-length str2)))
    (let ((str (make-array (+ l1 l2) nil)))
      (dotimes (i l1)
        (array-set str i (array-get str1 i)))
      (dotimes (i l2)
        (array-set str (+ i l1) (array-get str2 i)))
      str)))

