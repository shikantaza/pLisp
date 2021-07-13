;;  Copyright 2011-2021 Rajesh Jayaprakash <rajesh.jayaprakash@pm.me>

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

(create-package "matrix")

(in-package "matrix")

(alias dot-product utils:dot-product)

(alias convert-list-to-array utils:convert-list-to-array)

(alias convert-array-to-list utils:convert-array-to-list)

(defun matrixp (m)
  (labels ((check (lst)
                  (if (< (length lst) 2)
                      t
                    (and (eq (nth 0 lst)
                             (nth 1 lst))
                         (check (cdr lst))))))
          (let ((l (convert-array-to-list m)))
            (and (arrayp m)
                 (apply and (map arrayp l))
                 (check (map array-length l))))))

(defun create-matrix-from-list (lst)
  (labels ((check (lst)
                  (if (< (length lst) 2)
                      t
                    (and (eq (nth 0 lst)
                             (nth 1 lst))
                         (check (cdr lst))))))
          (let ((valid-list (lambda (lst)
                              (and (listp lst)
                                   (apply and (map listp lst))
                                   (check (map length lst))))))
            (if (not (valid-list lst))
                (error "Not a valid list")
              (let ((m (make-array (length lst) nil)))
                (dotimes (i (length lst))
                  (array-set m i (convert-list-to-array (nth i lst))))
                m)))))

(defmacro matrix (lst)
  `(create-matrix-from-list ,lst))

(defun create-matrix (r c)
  (let ((result (make-array r nil)))
    (dotimes (i r)
      (array-set result i (make-array c 0.0)))
    result))

(defun set-elem (ar i j v)
  (array-set (array-get ar i) j v))

(defun get-elem (ar i j)
  (array-get (array-get ar i) j))

;create an identity matrix of the specfied dimension
(defun identity-matrix (dim)
  (let ((ident (create-matrix dim dim)))
    (dotimes (i dim) (set-elem ident i i 1.0))
    ident))

(defun swap-rows (ar r1 r2)
  (let ((temp (array-get ar r1)))
    (array-set ar r1 (array-get ar r2))
    (array-set ar r2 temp)))

;apply the specified operation on a given row of a matrix
(defun apply-op-row (ar r val fn)
  (let ((len (array-length (array-get ar r))))
    (dotimes (i len)
      (set-elem ar r i (funcall fn (get-elem ar r i) val)))))

;subtract a given list of values from a row
(defun subtract (ar r lst)
  (let ((len (array-length (array-get ar r))))
    (dotimes (i len)
      (set-elem ar r i (- (get-elem ar r i) (nth i lst))))))

;extract a given row from a matrix and multiply it by the given factor
(defun get-row (ar r factor)
  (mapcar (lambda (x) (* factor x)) (convert-array-to-list (array-get ar r))))

;get a particular column's values after multiplying them by the given factor
(defun get-col (ar c factor)
  (let ((col))
    (dotimes (i (array-length ar))
      (nconc col (list (* factor (get-elem ar i c)))))
    col))

(defun print-matrix (m)
  (let ((r (array-length m)))
    (dotimes (i r)
      (let ((c (array-length (array-get m i))))
        (dotimes (j c)
          (format "%f " (get-elem m i j)))
        (println)))))

;split a matrix into two, on the indicated column position (inclusive; zero-based)
(defun split-matrix (m pos)
  (let ((len (array-length m)))
    (let ((m1 (make-array len nil))
          (m2 (make-array len nil)))
      (dotimes (i len)
        (array-set m1 i (sub-array (array-get m i) 0 (+ pos 1)))
        (array-set m2 i (sub-array (array-get m i) (+ pos 1) (- (array-length (array-get m i)) pos 1))))
      (list m1 m2))))

;paste two matrices together
(defun paste-matrices (m1 m2)
  (let ((m (make-array (array-length m1) nil)))
    (dotimes (i (array-length m))
      (array-set m i (convert-list-to-array (append (convert-array-to-list (array-get m1 i))
                                                    (convert-array-to-list (array-get m2 i))))))
    m))

;multiply two matrices
(defun mult (m1 m2)
  (let ((r (array-length m1)) 
        (c (array-length (array-get m2 0))))
    (let ((p (create-matrix r c)))
      (dotimes (i r)
        (dotimes (j c)
          (set-elem p i j (dot-product (get-row m1 i 1) (get-col m2 j 1)))))
      p)))

;transpose of a matrix
(defun trans (m)
  (let ((len (array-length (array-get m 0))))
    (let ((tr (make-array len nil)))
      (dotimes (i len)
        (array-set tr i (convert-list-to-array (get-col m i 1))))
    tr)))

;derive the reduced row echelon form of a given matrix
;from pseudocode in http://en.wikipedia.org/wiki/Reduced_row_echelon_form#Reduced_row_echelon_form
(defun reduced-row-echelon-form (matrix)
  (let ((m (clone matrix)))
    (let ((i 0) (lead 0) (rows (array-length m)) (cols (array-length (array-get m 0))))
      (dotimes (r rows)
        (if (<= cols lead)
            (return-from reduced-row-echelon-form m))
        (set i r)
        (while (eq (get-elem m i lead) 0)        
          (incf i)
          (if (eq rows i)
              (progn (set i r)
                     (incf lead)
                     (if (eq cols lead)
                         (return-from reduced-row-echelon-form m)))))
        (if (not (eq i r))
            (swap-rows m i r))
        (apply-op-row m r (get-elem m r lead) /)
        (dotimes (j rows)
          (if (not (eq j r))
              (subtract m j (get-row m r (get-elem m j lead)))))
        (incf lead))
      m)))

;inverse of a matrix
(defun inv (matrix)
  (let ((n (array-length matrix)))
    (cadr (split-matrix (reduced-row-echelon-form (paste-matrices matrix (identity-matrix n))) (- n 1)))))
