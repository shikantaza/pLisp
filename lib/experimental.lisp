;;  Copyright 2011-2018 Rajesh Jayaprakash <rajesh.jayaprakash@gmail.com>

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

(in-package "user")

(defun any-empty-list (lists)
  (if (null lists)
      nil
    (or (null (car lists))
        (any-empty-list (cdr lists)))))

(defun my-mapcar (f &rest lsts)
  (if (any-empty-list lsts)
      nil
    (cons (apply f (map (lambda (x) (car x))
                        lsts))
          (apply my-mapcar (bq-append (list f)
                                   (map (lambda (x) (cdr x))
                                        lsts))))))

(defmacro my-apply (fn args)
  `(,fn ,@args))

(defun my-mapcar2 (f &rest lsts)
  (if (any-empty-list lsts)
      nil
    (cons (apply f (map-internal (lambda (x1) (car x1)) lsts))
          (my-mapcar2 f (map-internal (lambda (x2) (cdr x2)) lsts)))))

(defun my-mapcar-2-arg (f lst1 lst2)
  (if (or (null lst1)
          (null lst2))
      nil
    (cons (f (car lst1) (car lst2))
          (my-mapcar-2-arg f (cdr lst1) (cdr lst2)))))

(defun mapcar2-internal (f lsts acc)
  (if (any-empty-list lsts)
      (reverse acc)
    (mapcar2-internal f 
                      (map (lambda (x) (cdr x)) lsts)
                      (cons (apply f (map (lambda (x) (car x)) lsts)) acc))))

(defun mapcar2 (f lsts)
  (mapcar2-internal f lsts nil))

(defun mapcar3 (f &rest lsts)
  (mapcar2-internal f lsts nil))
