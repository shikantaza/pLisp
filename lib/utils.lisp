;;  Copyright 2011-2022 Rajesh Jayaprakash <rajesh.jayaprakash@pm.me>

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

(defun all-elements-exist-p (lst1 lst2)
  (my-and (mapcar (lambda (x) (find x lst2 eq)) lst1)))

(defun same-list-p (lst1 lst2)
  (and (eq (length lst1) (length lst2))
       (all-elements-exist-p lst1 lst2)
       (all-elements-exist-p lst2 lst1)))

(defun my-and (lst)
  (if (null lst) 
      't
      (and (not (null (car lst))) (my-and (cdr lst)))))

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

(defun quicksort1 (lst compare-fn)
  (let1 ((lt compare-fn)
         (gt (lambda (x y) (not (or (lt x y) (eq x y))))))
    (if (null (cdr lst))
        lst
      (let ((pivot (first lst)))
        (concat (quicksort1 (select (lambda (x) (lt x pivot)) lst) compare-fn) 
                            (select (lambda (x) (eq x pivot)) lst) 
                (quicksort1 (select (lambda (x) (gt x pivot)) lst) compare-fn))))))

;helper function for sort
(defun split (lst)
  (cond ((null lst)
         (cons nil nil))
        ((eq (length lst) 1)
         (cons lst nil))
        (t (let ((res (split (cddr lst))))
             (cons (cons (car lst) (car res))
                   (cons (cadr lst) (cdr res)))))))

;helper function for sort
(defun merge (lst1 lst2 compare-fn)
  (cond ((null lst2)
         lst1)
        ((null lst1)
         lst2)
        (t (if (compare-fn (car lst1) (car lst2))
               (cons (car lst1) (merge (cdr lst1) lst2 compare-fn))
             (cons (car lst2) (merge lst1 (cdr lst2) compare-fn))))))

(defun sort (lst compare-fn)
  (if (or (null lst) (eq (length lst) 1))
      lst
    (let ((res (split lst)))
      (merge (sort (car res) compare-fn) (sort (cdr res) compare-fn) compare-fn))))

(defun convert-list-to-array (lst)
  (let ((m (make-array (length lst) nil)))
    (dotimes (i (length lst))
      (array-set m i (nth i lst)))
    m))
       
(defun convert-array-to-list (ar)
  (let ((result)
        (len (array-length ar)))
    (dotimes (i len)
      (nconc result (list (array-get ar i))))
    result))

;helper funciton for make-assoc-array
(defun expand-array (array new-length)
  (let ((len (array-length array)))
    (if (<= new-length len)
      array
      (let ((new-array (make-array new-length nil)))
        (dotimes (i len)
          (array-set new-array i  (array-get array i)))
        new-array))))

(defun make-assoc-array ()
  (let ((length 0))
    (let ((a (make-array 100 nil)))
      (let ((get (lambda (key)
                   (let ((ret))
                     (dotimes (i length)
                       (if (null ret)
                           (let ((x (array-get a i)))
                             (if (eq (car x) key)
                                 (set ret (cdr x))))))
                   ret)))
            (put (lambda (key val)
                   (dotimes (i length)
                     (let ((x (array-get a i)))
                       (if (eq (car x) key)
                           (progn (array-set a i (cons key val)) (set i length)))))
                   (progn (array-set a length (cons key val))
                          (incf length)
                          (if (> length (/ (array-length a) 2))
                              (set a (expand-array a (* array-length 2))))
                          val)))
            (map-assoc-array (lambda (fn)
                               (dotimes (i length)
                                 (let ((x (array-get a i)))
                                   (fn (car x) (cdr x)))))))
        (list get put map-assoc-array)))))

(defmacro get (a k)
  `((car ,a) ,k))

(defmacro put (a k v)
  `((cadr ,a) ,k ,v))

(defmacro map-assoc-array (a fn)
  `((caddr ,a) ,fn))

;naive implementation
(defun oddp (n)
  (if (not (integerp n))
      (throw (exception 'invalid-argument "Argument to ODDP should be an integer"))
    (let ((x 0))
      (while (< x n)
        (set x (+ x 2)))
      (eq (- x n) 1))))

(defun floor (n)
  (if (not (numberp n))
      (throw (exception 'invalid-argument "Argument to FLOOR should be a number"))
    (if (integerp n)
        n
      (call-foreign-function "plisp_floor" 'integer '((n float))))))

(defun system (str)
  (if (not (stringp str))
      (throw (exception 'invalid-argument "Argument to SYSTEM should be a string literal or a string object"))
    (call-foreign-function "plisp_system" 'void '((str character-pointer)))))

(defun sort1 (a compare-fn)
  (let ((i 0)
        (len (array-length a)))
    (labels ((min (start-index)
                    (let ((min-val (array-get a start-index))
                          (min-index start-index))
                      (dolist (i (range start-index (- (array-length a) 1) 1))
                        (if (compare-fn (array-get a i) min-val) 
                            (progn (set min-val   (array-get a i))
                                   (set min-index i))))
                      min-index)))
            (while (< i len)
              (let ((min-index)
                    (temp))
                (set min-index (min i))
                (set temp (array-get a min-index))
                (array-set a min-index (array-get a i))
                (array-set a i temp)
                (incf i))))))
                         
    
    
    
