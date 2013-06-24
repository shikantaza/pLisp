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

(define defun (macro (name vars &rest body)
                     `(define ,name (lambda ,vars ,@body))))

(define defmacro (macro (name vars &rest body)
                        `(define ,name (macro ,vars ,@body))))

(defun cadr (lst)
  (car (cdr lst)))

(defun caddr (lst)
  (car (cdr (cdr lst))))

(defun cdar (lst)
  (cdr (car lst)))

(defun caar (lst)
  (car (car lst)))

(defun cadar (lst)
  (car (cdr (car lst))))

(defun null (x)
  (eq x '()))

(defun and (&rest lst)
  (if (null lst)
      't
    (if (car lst)
        (apply and (cdr lst))
      nil)))

(defun or (&rest lst)
  (if (null lst)
      nil
    (if (car lst)
        't
      (apply or (cdr lst)))))

(defun not (x)
  (if x nil 't))

(defun append (x y)
  (if (null x)
      y
      (cons (car x) (append (cdr x) y))))

(defun pair (x y)
  (if (and (null x) (null y))
      nil
      (cons (list (car x) (car y))
	    (pair (cdr x) (cdr y)))))

(defmacro rec (var exp)
  `(let ((,var '()))
     (set ,var ,exp)))

(defun map (f lst)
  (if (null lst)
      nil
    (cons (f (car lst)) (map f (cdr lst)))))

(defmacro let (specs &rest body)
  `((lambda ,(map car specs) ,@body) ,@(map cadr specs)))

(defun assoc (x y)
  (if (eq (caar y) x)
      (cadar y)
      (assoc x (cdr y))))

(defun length (lst)
  (if (null lst)
      0
    (if (not (consp lst))
        (error "Not a list")
      (+ 1 (length (cdr lst))))))

(defun last (lst)
   (if (null (cdr lst))
      (car lst)
      (last (cdr lst))))

(defun remove-last (lst)
  (if (eq (length lst) 1)
      nil
      (cons (car lst) (remove-last (cdr lst)))))

(defun reverse (lst)
  (if (eq (length lst) 1)
      (list (car lst))
      (cons (last lst) (reverse (remove-last lst)))))

(defmacro < (x y)
  `(not (or (> ,x ,y) (eq ,x ,y))))

(defmacro <= (x y)
  `(or (< ,x ,y) (eq ,x ,y)))

(defmacro >= (x y)
  `(or (> ,x ,y) (eq ,x ,y)))

(defmacro neq (x y)
  `(not (eq ,x ,y)))

(defun range (start end incr)
  (if (> start end)
      nil
      (cons start
	    (range (+ start incr) end incr))))

(defmacro dolist (spec &rest body)
  (let ((elem (car spec))
	(lst (car (cdr spec))))
    `(let ((rest ,lst))
       (while (not (eq rest '()))
	 (let ((,elem (car rest)))
	   ,@body
	   (set rest (cdr rest)))))))

(defun nth (n lst)
  (if (> n (length lst))
      nil
      (if (eq n 0)
	  (car lst)
	  (nth (- n 1) (cdr lst)))))

(defmacro make-string (size elem)
  `(make-array ,size ,elem))

(defmacro string-set (str pos val)
  `(array-set ,str ,pos ,val))

(defmacro string-get (str pos)
  `(array-get ,str ,pos))

(defmacro incf (var)
  `(set ,var (+ ,var 1)))

(defmacro for (spec &rest body)
  (let ((var (nth 0 spec))
	(init-form (nth 1 spec))
	(condition (nth 2 spec))
	(step-form (nth 3 spec))
	(ret-form  (nth 4 spec)))
    `(let ((,var ,init-form))
       (while ,condition
	 ,@body
	 ,step-form)
       ,ret-form)))

(defun max (lst)
  (let ((curr-max (car lst)))
    (dolist (x (cdr lst))
      (if (> x curr-max)
	  (set curr-max x)))
    curr-max))

(defun min (lst)
  (let ((curr-min (car lst)))
    (dolist (x (cdr lst))
      (if (< x curr-min)
	  (set curr-min x)))
    curr-min))

(defun curry (function &rest args)
    (lambda (&rest more-args)
      (apply function (append args more-args))))

;(defmacro nconc (lst1 lst2)
; `(set ,lst1 (append ,lst1 ,lst2)))

(defun concat (lst &rest lists)
  (let ((result (copy-list lst)))
    (dolist (x lists)
      (set result (append result x)))
    result))

(defmacro nconc (lst &rest lists)
  `(set ,lst (concat ,lst ,@lists)))

(defun remove (e lst count)
  (if (null lst)
      nil
      (if (neq e (car lst))
	  (cons (car lst) (remove e (cdr lst) count))
	  (if (> count 0) 
	      (remove e (cdr lst) (- count 1))
	      lst))))

(defun find-if (predicate lst)
  (if (null lst)
      nil
      (if (predicate (car lst))
	  (car lst)
	  (find-if predicate (cdr lst)))))

(defun find (e lst predicate)
  (find-if (lambda (x) (predicate x e)) lst))

(defun remove-duplicates (lst equality-test)
  (let ((result nil))
    (dolist (x lst)
      (if (not (find x result equality-test))
	  (set result (append result (list x)))
	  ()))
    result))

(defmacro labels (decls &rest body)
  (let ((f (lambda (decl)
             (cons (nth 0 decl)
                   (list (list 'rec (nth 0 decl)
                               (list 'lambda
                                     (nth 1 decl)
                                     (nth 2 decl))))))))
    `(let ,(map f decls)
       ,@body)))

(defmacro first (lst)
  `(car ,lst))

(defmacro rest (lst)
  `(cdr ,lst))

(defmacro setq (var value)
  `(set ,var ,value))

(defmacro substring (str &rest rest)
  `(sub-array ,str ,@rest))

(defmacro cond (&rest lst)
  (if (null lst)
      nil
    `(if ,(caar lst)
         ,(cadar lst)
       (cond ,@(cdr lst)))))

(defmacro dotimes (spec &rest statements)
  `(let ((,(car spec) 0))
    (while (< ,(car spec) ,(cadr spec))
      (progn ,@statements (incf ,(car spec))))
    ,(caddr spec)))

(defmacro values (&rest body)
  `(list ,@body))

(defmacro multiple-value-bind (vars var-form &rest body)
  `(apply (lambda ,vars ,@body) ,var-form))

(defun funcall (fn &rest args)
  (apply fn args))

(defun numberp (x)
  (or (integerp x) (floatp x)))

(defmacro assert (condition text)
  `(if (not ,condition)
       (error ,text)))

(defun remove-if (pred lst)
  (if (null lst)
      nil
    (if (not (pred (car lst)))
        (cons (car lst) (remove-if pred (cdr lst)))
      (remove-if pred (cdr lst)))))

(defun sublist (lst start len)
  (let ((res))
    (for (i start (< i (+ start len)) (incf i) res)
         (set res (append res (list (nth i lst)))))))

(defun butlast (lst n)
  (sublist lst 0 (- (length lst) n)))

(defun mapcar (f &rest lists)  
  (let ((min-length (min (map length lists)))
	(result nil)
	(i 0))
    (while (< i min-length)
      (set result (append result (list (apply f (map (curry nth i) lists)))))
      (incf i))
    result))

(defun flatten (lst)
  (let ((flattened-list))
    (dolist (x lst)
      (set flattened-list (append flattened-list x)))
    flattened-list))

(defmacro mapcan (f &rest lists)  
  `(flatten (mapcar ,f ,@lists))) 

(load-foreign-library "libplisp.so")

;needed because FORMAT does not handle newlines
(defmacro println ()
  `(call-foreign-function "print_line" 'void nil))

(defun copy-list (lst)
  (if (null lst)
      nil
    (cons (car lst) (copy-list (cdr lst)))))

(load-file "utils.lisp")

(load-file "math.lisp")

(load-file "matrix.lisp")

(create-package "user")

(in-package "user")
