;;  Copyright 2011-2019 Rajesh Jayaprakash <rajesh.jayaprakash@gmail.com>

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

;;(define primitive-fns '(+ - car cdr < > <= >= error print cons setcar setcdr list * / eq concat not gensym atom
;;                        symbol-value apply symbol symbol-name format clone unbind newline consp listp integerp
;;                        floatp characterp symbolp stringp arrayp closurep macrop continuationp string make-array
;;                        array-set array-get sub-array array-length print-string load-foreign-library call-foreign-function
;;                        create-package in-package export-package create-image save-object load-object load-file
;;                        time profile env expand-macro eval))


;;(define primitive-syms '(PRIM-ADD PRIM-SUB PRIM-CAR PRIM-CDR PRIM-LT PRIM-GT PRIM-LEQ PRIM-GEQ PRIM-ERROR PRIM-PRINT PRIM-CONS
;;                         PRIM-SETCAR PRIM-SETCDR PRIM-LIST PRIM-MULT PRIM-DIV PRIM-EQ PRIM-CONCAT PRIM-NOT PRIM-GENSYM PRIM-ATOM PRIM-SYMBOL-VALUE PRIM-APPLY
;;                         PRIM-SYMBOL-VALUE PRIM-SYMBOL-NAME PRIM-FORMAT PRIM-CLONE PRIM-UNBIND PRIM-NEWLINE PRIM-CONSP PRIM-LISTP PRIM-INTEGERP PRIM-FLOATP
;;                         PRIM-CHARACTERP PRIM-SYMBOLP PRIM-STRINGP PRIM-ARRAYP PRIM-CLOSUREP PRIM-MACROP PRIM-CONTINUATIONP PRIM-STRING PRIM-MAKE-ARRAY
;;                         PRIM-ARRAY-SET PRIM-ARRAY-GET PRIM-SUB-ARRAY PRIM-ARRAY-LENGTH PRIM-PRINT-STRING PRIM-LOAD-FOREIGN-LIBRARY PRIM-CALL-FOREIGN-FUNCTION
;;                         PRIM-CREATE-PACKAGE PRIM-IN-PACKAGE PRIM-EXPORT-PACKAGE PRIM-CREATE-IMAGE PRIM-SAVE-OBJECT PRIM-LOAD-OBJECT PRIM-LOAD-FILE
;;                         PRIM-TIME PRIM-PROFILE PRIM-ENV PRIM-EXPAND-MACRO PRIM-EVAL))

;;(let ((n (length primitive-fns)))
;;  (for (i 0 (< i n) (incf i) nil)
;;       (let ((fn (nth i primitive-fns))
;;             (sym (nth i primitive-syms)))
;;         (eval `(define ,fn (lambda (&rest args) (,sym ,@args)))))))

(defun plus-internal (args)
  (if (prim-eq args nil)
      0
    (prim-add (prim-car args)
              (plus-internal (prim-cdr args)))))

(defun + (&rest args)
  (plus-internal args))

(defun - (&rest args)
  (prim-sub (prim-car args)
            (plus-internal (prim-cdr args))))

(defun car (x)
  (prim-car x))

(defun cdr (x)
  (prim-cdr x))

(defun < (a b)
  (prim-lt a b))

(defun > (a b)
  (prim-gt a b))

(defun <= (a b)
  (prim-leq a b))

(defun >= (a b)
  (prim-geq a b))

(defun error (x)
  (prim-error x))

(defun print (x)
  (prim-print x))

(defun cons (a b)
  (prim-cons a b))

(defun setcar (x y)
  (prim-setcar x y))

(defun setcdr (x y)
  (prim-setcdr x y))

(defun list-internal (args)
  (if (prim-eq args nil)
      nil
    (prim-cons (prim-car args)
               (list-internal (prim-cdr args)))))

(defun list (&rest args)
  (list-internal args))

(defun mult-internal (args)
  (if (prim-eq args nil)
      1
    (prim-mult (prim-car args)
               (mult-internal (prim-cdr args)))))

(defun * (&rest args)
  (mult-internal args))

(defun / (&rest args)
  (prim-div (prim-car args)
            (mult-internal (prim-cdr args))))

(defun eq (a b)
  (prim-eq a b))

(defun concat-two-lists (x y)
  (if (prim-eq x nil)
      y
    (prim-cons (prim-car x) (concat-two-lists (prim-cdr x) y))))

(defun concat-internal (args)
  (if (prim-eq args nil)
      nil
    (concat-two-lists (prim-car args)
                      (concat-internal (prim-cdr args)))))

(defun concat (&rest args)
  (concat-internal args))

(defun not (x)
  (prim-not x))

(defun gensym ()
  (prim-gensym))

(defun atom (x)
  (prim-atom x))

(defun symbol-value (x)
  (prim-symbol-value x))

(defun apply (x y)
  (prim-apply x y))

(defun symbol (x)
  (prim-symbol x))

(defun symbol-name (x)
  (prim-symbol-name x))

;(defun format-internal (args)
;  (prim-apply prim-format args))

;(defun format (&rest args)
;  (format-internal args))

(defun clone (x)
  (prim-clone x))

(defun unbind (x)
  (prim-unbind x))

(defun newline (x)
  (prim-newline x))

(defun consp (x)
  (prim-consp x))

(defun listp (x)
  (prim-listp x))

(defun integerp (x)
  (prim-integerp x))

(defun floatp (x)
  (prim-floatp x))

(defun characterp (x)
  (prim-characterp x))

(defun symbolp (x)
  (prim-symbolp x))

(defun stringp (x)
  (prim-stringp x))

(defun arrayp (x)
  (prim-arrayp x))

(defun closurep (x)
  (prim-closurep x))

(defun macrop (x)
  (prim-macrop x))

(defun continuationp (x)
  (prim-continuationp x))

(defun string (x)
  (prim-string x))

(defun make-array (x y)
  (prim-make-array x y))

(defun array-set (x y z)
  (prim-array-set x y z))

(defun array-get (x y)
  (prim-array-get x y))

(defun sub-array (x y z)
  (prim-sub-array x y z))

(defun array-length (x)
  (prim-array-length x))

(defun print-string (x)
  (prim-print-string x))

(defun load-foreign-library (x)
  (prim-load-foreign-library x))

(defun call-ff-internal (x y z)
  (prim-call-ff-internal x y z))

(defun create-package (x)
  (prim-create-package x))

(defun in-package (x)
  (prim-in-package x))

(defun export-package (x y)
  (prim-export-package x y))

(defun create-image (x)
  (prim-create-image x))

(defun save-object (x y)
  (prim-save-object x y))

(defun load-object (x)
  (prim-load-object x))

(defun load-file (x)
  (prim-load-file x))

(defun time (x)
  (prim-time x))

(defun profile (x)
  (prim-profile x))

(defun env ()
  (prim-env))

(defun expand-macro (x)
  (prim-expand-macro x))

(defun eval (x)
  (prim-eval x))
