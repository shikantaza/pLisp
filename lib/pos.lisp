;;  Copyright 2011-2021 Rajesh Jayaprakash <rajesh.jayaprakash@gmail.com>

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

(defmacro build-getter (sym)
  `(symbol (concat-strings "get-" (symbol-name ,sym))))

(defmacro build-setter (sym)
  `(symbol (concat-strings "set-" (symbol-name ,sym))))

(defun send-message (obj method-name &rest params)
  (dolist (m obj)
    (if (eq (car m) method-name)
        (return-from send-message (apply (cdr m) params))))
  (let ((parent (send-message obj 'get-parent)))
    (if parent
        (apply send-message (concat (list parent method-name) params))
      (throw (exception 'method-does-not-exist (symbol-name method-name))))))

(defmacro create-object (super-class vars init-form methods)
  (let ((e1 (map (lambda (x) (list x)) vars))
        (e2 (map (lambda (x) (list 'cons (list 'quote (build-getter x)) (list 'lambda nil x))) vars))
        (e3 (map (lambda (x) (list 'cons (list 'quote (build-setter x)) (list 'lambda '(v) (list 'set x 'v)))) vars))
        (e4 (map (lambda (x) (list 'cons (list 'quote (first x)) (list 'lambda (second x) (third x)))) methods))
        (e5 (gensym)))
    `(let ,e1
       (let ((,e5 ,(if (null super-class) nil (list 'send-message super-class ''create-instance))))
         ,init-form
       ,(concat (list 'list) e2 e3 e4 (list (list 'cons (list 'quote 'get-parent) (list 'lambda () e5))))))))

(defmacro create-class (super-class class-vars inst-vars class-init-form inst-init-form class-methods inst-methods)
  (let ((e1 (map (lambda (x) (list x)) class-vars))
        (e2 (map (lambda (x) (list 'cons (list 'quote (build-getter x)) (list 'lambda nil x))) class-vars))
        (e3 (map (lambda (x) (list 'cons (list 'quote (build-setter x)) (list 'lambda '(v) (list 'set x 'v)))) class-vars))
        (e4 (list (list 'cons (list 'quote 'create-instance) (list 'lambda nil (list 'create-object super-class inst-vars inst-init-form inst-methods)))))
        (e5 (map (lambda (x) (list 'cons (list 'quote (first x)) (list 'lambda (second x) (third x)))) class-methods)))
    `(let ,e1
       ,class-init-form
       ,(concat (list 'list) e2 e3 e4 e5))))

(defmacro create-instance (class)
  `(send-message ,class 'create-instance))
