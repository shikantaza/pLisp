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

(create-package "graph")

(in-package "graph")

(alias quicksort1 utils:quicksort1)
(alias convert-list-to-array utils:convert-list-to-array)

(defun plot-line-graph (xlist ylist)
  (plot xlist ylist 'line))

(defun scatterplot (xlist ylist)
  (plot xlist ylist 'scatterplot))

(defun plot (xlist ylist type)
  (if (and (neq type 'line) (neq type 'scatterplot))
      (throw (exception 'invalid-argument "plot: unsupported plot type")))
  (let1 ((sorted-pairs (quicksort1 (cons-pair (map (lambda (x) (* 1.0 x)) xlist) 
                                              (map (lambda (x) (* 1.0 x)) ylist)) 
                                   (lambda (x y) (< (car x) (car y)))))
         (sorted-xlist (map car sorted-pairs))
         (sorted-ylist (map cdr sorted-pairs))
         (xlist-blk (alloc-ext-mem-float (length xlist)))
         (ylist-blk (alloc-ext-mem-float (length ylist))))
        (set-ext-mem xlist-blk (convert-list-to-array sorted-xlist))
        (set-ext-mem ylist-blk (convert-list-to-array sorted-ylist))
        (plot-internal xlist-blk ylist-blk type)))
  

(defun plot-internal (xval-blk yval-blk type)
  (let ((ptrx (car xval-blk))
        (ptry (car yval-blk))
        (lenx (cadr xval-blk))
        (leny (cadr yval-blk))
        (typex (caddr xval-blk))
        (typey (caddr yval-blk)))
    (if (not (eq lenx leny))
        (throw (exception 'invalid-argument "plot: the two blocks are not of the same size")))
    (if (or (not (eq typex 2)) (not (eq typey 2)))
        (throw (exception 'invalid-argument "plot: one or both the blocks are not floats")))
    (cond ((eq type 'line)
           (call-foreign-function "plot" 
                                  'void
                                  '((ptrx integer)
                                    (ptry integer)
                                    (lenx integer)
                                    (1 integer))))
          ((eq type 'scatterplot)
           (call-foreign-function "plot" 
                                  'void
                                  '((ptrx integer)
                                    (ptry integer)
                                    (lenx integer)
                                    (2 integer)))))))

(defun hist (xlist)
  (let1 ((xlist-blk (alloc-ext-mem-float (length xlist)))
         (ptr (car xlist-blk))
         (len (cadr xlist-blk)))
    (set-ext-mem xlist-blk (convert-list-to-array (map (lambda (x) (* x 1.0)) xlist)))
    (call-foreign-function "hist"
                           'void
                           '((ptr integer)
                             (len integer)))))
