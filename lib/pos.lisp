;;  Copyright 2011-2016 Rajesh Jayaprakash <rajesh.jayaprakash@gmail.com>

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

(defmacro create-instance (superclass-name
                           instance-var-list
                           instance-init-form
                           instance-methods)
  (let ((sym (gensym)))
    (let ((e1 (let ((lst nil))
                (dolist (var instance-var-list)
                  (set lst (append lst (list (list (car var) (cadr var))))))
                lst))
          (e2 (list 'parent (if (null superclass-name) 
                                nil 
                              (list 'call-method superclass-name ''make-instance))))
          (e3 (list 'get-parent (list 'lambda nil 'parent)))
          (e4 (list 'initialize instance-init-form))
          (e5 (let ((lst nil))
                (dolist (var instance-methods)
                  (set lst (append lst (list (list (car var)
                                                   (list 'lambda 
                                                         (cadr var) 
                                                         (caddr var)))))))
                lst))
          (e6 (let ((lst nil))
                (dolist (var instance-var-list)
                  (progn
                    (set lst (append lst (list (list (caddr var)
                                                     (list 'lambda nil (car var))))))
                    (set lst (append lst (list (list (cadddr var)
                                                     (list 'lambda
                                                           (list sym)
                                                           (list 'set (car var) sym))))))))
                lst)))
      (let ((e7 (append e1 (list e2)))
            (e8 (map (lambda (x) (list 'cons
                                       (list quote (car x))
                                       (car x)))
                     e5))
            (e9 (map (lambda (x) (list 'cons
                                       (list quote (car x))
                                       (car x)))
                     e6)))      
        `(let ,e7
           (let (,e3 ,e4 ,@e5 ,@e6)
             ,(if (not (null e4) '(progn (if (not (null parent)) (call-method parent 'initialize)) (initialize))))
             (list (cons 'get-parent get-parent)
                   (cons 'initialize initialize)
                   ,@e8
                   ,@e9)))))))

(defmacro define-class (class-name
                        superclass-name
                        class-var-list
                        instance-var-list
                        class-init-form
                        instance-init-form
                        class-methods
                        instance-methods)
  (let ((sym (gensym)))
    (let ((e1 (let ((lst nil))
                (dolist (var class-var-list)
                  (set lst (append lst (list (list (car var) (cadr var))))))
                lst))
          (e2 (list 'get-superclass (list 'lambda nil superclass-name)))
          (e3 (list 'initialize class-init-form))
          (e4 (let ((lst nil))
                (dolist (var class-methods)
                  (set lst (append lst (list (list (car var)
                                                   (list 'lambda 
                                                         (cadr var) 
                                                         (caddr var)))))))
                lst))
          (e5 (let ((lst nil))
                (dolist (var class-var-list)
                  (progn
                    (set lst (append lst (list (list (caddr var)
                                                     (list 'lambda nil (car var))))))
                    (set lst (append lst (list (list (cadddr var)
                                                     (list 'lambda
                                                           (list sym)
                                                           (list 'set (car var) sym))))))))
                lst)))
      (let ((e6 (map (lambda (x) (list 'cons
                                       (list quote (car x))
                                       (car x)))
                     e4))
            (e7 (map (lambda (x) (list 'cons
                                       (list quote (car x))
                                       (car x)))
                     e5)))
        `(define ,class-name (let ,e1
                               (let (,e2 ,e3 ,@e4 ,@e5 (make-instance (lambda nil (create-instance ,superclass-name 
                                                                                                   ,instance-var-list 
                                                                                                   ,instance-init-form 
                                                                                                   ,instance-methods))))
                                 ,(if (not (null e3) '(progn (if (not (null superclass-name)) (call-method superclass-name 'initialize)) (initialize))))
                                 (list (cons 'get-superclass get-superclass)
                                       (cons 'initialize initialize)
                                       ,@e6
                                       ,@e7
                                       (cons 'make-instance make-instance)))))))))

(defun get-method (obj method-name)
  (if (null obj)
      nil
    (progn (dolist (x obj)
             (if (eq (car x) method-name)
                 (return-from get-method (cdr x))))
           (get-method (call-method obj 'get-parent) method-name))))

(defun call-method (obj method-name &rest params)
  (let ((method (get-method obj method-name)))
    (if (null method)
        (throw (exception 'method-does-not-exist (symbol-name method-name)))
      (apply method params))))

(defmacro make-instance (class)
  `(call-method ,class 'make-instance))

