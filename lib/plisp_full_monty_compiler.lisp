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

(define bq-append (lambda (x y)
                    (if (prim-eq x nil)
                        y
                      (prim-cons (prim-car x) (bq-append (prim-cdr x) y)))))

;(define build-list (lambda (lst acc)
;                     (if (eq lst nil)
;                         acc
;                       (let ((x (car lst))
;                             (ret))
;                         (if (atom x)
;                             (set ret (bq-append acc (list (list 'list (list 'quote x)))))
;                           (if (eq (car x) 'comma)
;                               (set ret (bq-append acc (list (list 'list (car (cdr x))))))
;                             (if (eq (car x) 'comma-at)
;                                 (set ret (bq-append acc (list (car (cdr x)))))
;                               (set ret (bq-append acc (list (list 'list (bq1 x))))))))
;                         (build-list (cdr lst) ret)))))

;(define bq1 (lambda (exp)
;              (if (not (listp exp))
;                  (list 'quote exp)
;                (if (eq (car exp) 'comma) 
;                    (car (cdr exp))
;                  (build-list exp (list 'concat))))))

(define qq (lambda (x)
             (if (prim-consp x)
                 (if (prim-eq 'comma (prim-car x))
                     (prim-car (prim-cdr x))
                   (if (prim-eq 'backquote (prim-car x))
                       (prim-list 'quote x)
                     (if (prim-consp (prim-car x))
                         (if (prim-eq 'comma-at (prim-car (prim-car x)))
                             (prim-list 'bq-append (prim-car (prim-cdr (prim-car x))) (qq (prim-cdr x)))
                           (prim-list 'prim-cons (qq (prim-car x)) (qq (prim-cdr x))))
                       (prim-list 'prim-cons (qq (prim-car x)) (qq (prim-cdr x))))))
               (prim-list 'quote x))))

(define backquote (macro (quoted-form)
                         (qq quoted-form)))

;(define defun (macro (name vars &rest body)
;                     `(define ,name (lambda ,vars ,@body))))

;(define defmacro (macro (name vars &rest body)
;                        `(define ,name (macro ,vars ,@body))))

(define defun (macro (name vars &rest body)
                     (if (prim-stringp (prim-car body))
                         `(define ,name (lambda ,vars ,@(prim-cdr body)) ,(prim-car body))
                       `(define ,name (lambda ,vars ,@body)))))

(define defmacro (macro (name vars &rest body)
                        (if (prim-stringp (prim-car body))
                            `(define ,name (macro ,vars ,@(prim-cdr body)) ,(prim-car body))
                          `(define ,name (macro ,vars ,@body)))))

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

(defun exception (excp desc)
  (list excp desc))

(defmacro assert (condition text)
  `(if (not ,condition)
       (throw (exception 'exception ,text))))

(defun cadr (lst)
  (car (cdr lst)))

(defun cddr (lst)
  (cdr (cdr lst)))

(defun caddr (lst)
  (car (cdr (cdr lst))))

(defun cdar (lst)
  (cdr (car lst)))

(defun caar (lst)
  (car (car lst)))

(defun cadar (lst)
  (car (cdr (car lst))))

(defun cadddr (lst)
  (car (cdr (cdr (cdr lst)))))

(defun null (x)
  (eq x '()))

(defmacro and (&rest lst)
  (if (null lst)
       t
    `(if (not ,(car lst))
        nil
      (and ,@(cdr lst)))))

(defmacro or (&rest lst)
  (if (null lst)
      nil
    `(if ,(car lst)
        t
      (or ,@(cdr lst)))))

(defun append (x y)
  (assert (and (listp x) (listp y)) "Arguments to APPEND should be lists")
  (if (null x)
      y
      (cons (car x) (append (cdr x) y))))

(defun pair (x y)
  (if (and (null x) (null y))
      nil
      (cons (list (car x) (car y))
	    (pair (cdr x) (cdr y)))))

(defun cons-pair (x y)
  (if (and (null x) (null y))
      nil
      (cons (cons (car x) (car y))
	    (cons-pair (cdr x) (cdr y)))))

(defun length (lst)
  (if (null lst)
      0
    (if (not (consp lst))
        (error "Not a list")
      (+ 1 (length (cdr lst))))))

(defmacro progn (&rest body)
  (if (null body)
      nil
    (if (eq (length body) 1)
        (car body)
      `(let ((,(gensym) ,(car body)))
         (progn ,@(cdr body))))))

(defmacro while (condition &rest body)
  (let ((f (gensym)))
    `(letrec ((,f (lambda ()
                    (if ,condition
                        (progn ,@body (,f))))))
        (,f))))

;todo: include other special operators
(defun is-sp-op (sym)
  (or (eq sym 'car)
      (eq sym 'cdr)
      (eq sym 'cons)
      (eq sym 'gensym)
      (eq sym 'eval)))

(defun map-internal (f lst)
  (if (null lst)
      nil
    (cons (f (car lst)) (map-internal f (cdr lst)))))

(defmacro map (f lst)
  (if (is-sp-op f)
      (let ((sym (gensym)))
        `(let ((,sym (lambda (x) (,f x))))
           (map-internal ,sym ,lst)))
    `(map-internal ,f ,lst)))

(defun assoc (x y)
  (assert (or (null y) (listp y)) "Second argument to ASSOC should be a list of CONS objects")
  (if (null y)
      nil
    (progn 
      (assert (consp (car y)) "Second argument to ASSOC should be a list of CONS objects")
      (if (eq (caar y) x)
	  (cdar y)
	(assoc x (cdr y))))))

(defun last (lst)
   (if (null (cdr lst))
      (car lst)
      (last (cdr lst))))

(defun remove-last (lst)
  (if (null lst)
      nil
    (if (eq (length lst) 1)
        nil
      (cons (car lst) (remove-last (cdr lst))))))

(defun reverse (lst)
  (if (null lst)
      nil
    (append (reverse (cdr lst)) (list (car lst)))))

(defun range (start end incr)
  (if (> start end)
      nil
      (cons start
	    (range (+ start incr) end incr))))

(defmacro dolist (spec &rest body)
  (let ((sym (gensym))
        (elem (car spec))
	(lst (car (cdr spec))))
    `(let ((,sym ,lst))
       (while (not (eq ,sym '()))
	 (let ((,elem (car ,sym)))
	   ,@body
	   (set ,sym (cdr ,sym)))))))

;(defun nth (n lst)
;  (if (> n (length lst))
;      nil
;      (if (eq n 0)
;	  (car lst)
;	  (nth (- n 1) (cdr lst)))))

(defun nth (n lst)
  (let ((len (length lst)))
    (if (or (< n 0) (>= n len))
        (error "Invalid list index")
      (let ((i 0)
            (rest lst))
        (while (< i n)
          (set rest (cdr rest))
          (set i (+ i 1)))
        (car rest)))))

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

(defmacro generate-syms (n)
  `(map gensym (range 1 ,n 1)))

(defmacro generate-wrapper (sp-op n)
  `(let ((evaluated-n ,n))
     (let ((params (generate-syms evaluated-n)))
       (list 'lambda params (concat (list (quote ,sp-op)) params)))))

(defmacro apply-sp-op (fn arglist)
  `(let ((evaluated-arglist ,arglist))
     (let ((len (length evaluated-arglist)))
       (let ((wrapper (generate-wrapper ,fn len)))
         (apply (eval wrapper) evaluated-arglist)))))

(defmacro apply-reg-fn (fn arglist)
  `(let ((evaluated-arglist ,arglist))
     (apply ,fn evaluated-arglist)))

(defmacro apply1 (fn arglist)
  (if (is-sp-op fn)
      `(apply-sp-op ,fn ,arglist)
    `(apply-reg-fn ,fn , arglist)))

(defmacro def-curry-fn (f1 f0 &rest args)
  `(define ,f1 (lambda (&rest more-args)
                 (apply ,f0 (append (list ,@args) more-args)))))

(defmacro nconc (lst &rest lists)
  `(set ,lst (concat ,lst ,@lists)))

(defmacro neq (v1 v2)
  `(not (eq ,v1 ,v2)))

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

(defun in (sym lst)
  (if (null lst)
      nil
    (or (eq sym (car lst))
        (in sym (cdr lst)))))

(defun rem-duplicates-internal (lst equality-test)
  (let ((result nil))
    (dolist (x lst)
      (if (not (find x result equality-test))
	  (set result (append result (list x)))
	  ()))
    result))

(defmacro remove-duplicates (lst equality-test)
  (if (eq equality-test 'eq)
      `(rem-duplicates-internal ,lst (lambda (x y) (eq x y)))
    `(rem-duplicates-internal ,lst ,equality-test)))

(defmacro first (lst)
  `(car ,lst))

(defmacro second (lst)
  `(car (cdr ,lst)))

(defmacro third (lst)
  `(caddr ,lst))

(defmacro fourth (lst)
  `(cadddr ,lst))

(defmacro fifth (lst)
  `(car (cdr (cdr (cdr (cdr ,lst))))))

(defmacro rest (lst)
  `(cdr ,lst))

(defmacro setq (var value)
  `(set ,var ,value))

(defmacro substring (str start len)
  `(sub-array ,str ,start ,len))

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

(defun select (pred lst)
  (if (null lst)
      nil
    (if (pred (car lst))
        (cons (car lst) (select pred (cdr lst)))
      (select pred (cdr lst)))))

(defun remove-if (pred lst)
  (if (null lst)
      nil
    (if (not (pred (car lst)))
        (cons (car lst) (remove-if pred (cdr lst)))
      (remove-if pred (cdr lst)))))

(defmacro remove-if-not (pred lst)
  (let ((x (gensym)))
    `(remove-if (lambda (,x) (not (,pred ,x))) ,lst))) 

(defun sub-list (lst start len)
  (cond ((not (listp lst)) (throw (exception 'invalid-argument "First argument to SUB-LIST should be a list")))
	((not (integerp start)) (throw (exception 'invalid-argument "Second argument to SUB-LIST should be an integer")))
	((not (integerp len)) (throw (exception 'invalid-argument "Third argument to SUB-LIST should be an integer")))
	((or (< start 0)
	     (< (length lst) (+ start len))) (throw (exception 'INDEX-OUT-OF-BOUNDS "SUB-LIST specifies a range outside the list size")))
	(t   (let ((res))
	       (for (i start (< i (+ start len)) (incf i) res)
		    (set res (append res (list (nth i lst)))))
               res))))

(defun last-n (lst n)
  (sub-list lst (- (length lst) n) n))

(defun butlast (lst n)
  (assert (and (listp lst) (integerp n)) "Arguments to BUTLAST should be a list and an integer")
  (assert (and (>= n 1) (<= n (length lst))) "Second argument to BUTLAST should be a positive integer less than or equal to the length of the list")
  (sub-list lst 0 (- (length lst) n)))

;(defun mapcar (f &rest lists)
;  (if (eq (car lists) nil)
;      nil
;    (cons (apply1 f (map (lambda (x) (car x)) lists))
;          (apply1 mapcar (concat (list f) (map (lambda (x) (cdr x)) lists))))))

(defun any-empty-list (lists)
  (if (null lists)
      nil
    (or (null (car lists))
        (any-empty-list (cdr lists)))))

(defun mapcar-internal (f lsts acc)
  (if (any-empty-list lsts)
      (reverse acc)
    (mapcar-internal f 
                      (map cdr lsts)
                      (cons (apply f (map car lsts)) acc))))

(defun mapcar (f &rest lsts)
  (mapcar-internal f lsts nil))

;(defun flatten (lst)
;  (apply append  (map (lambda (x)
;                        (if (listp x)
;                           x
;                         (list x)))
;                     lst)))

(defun flatten (lst)
  (if (null lst)
      nil
    (progn
      (assert (consp lst) "Argument to FLATTEN must be a list")
      (append (if (listp (car lst))
                  (car lst)
                (list (car lst)))
              (flatten (cdr lst))))))

(defmacro mapcan (f &rest lists)  
  `(flatten (mapcar ,f ,@lists))) 

(load-foreign-library "libplisp")

;needed because FORMAT does not handle newlines
(defmacro println ()
  `(call-foreign-function "print_line" void nil))

(defmacro alias (sym1 sym2)
  `(define ,sym1 ,sym2))

(defun concat-strings (str1 str2)
  (let ((l1 (array-length str1))
        (l2 (array-length str2)))
    (let ((str (make-array (+ l1 l2) nil)))
      (dolist (i (range 0 (- l1 1) 1))
        (array-set str i (array-get str1 i)))
      (dolist (i (range 0 (- l2 1) 1))
        (array-set str (+ i l1) (array-get str2 i)))
      str)))

(defmacro try (body exception-clause finally-clause)
  (let ((ret (gensym))
        (cc (gensym))
        (ret1 (gensym)))
    `(call/cc (lambda (,cc)
                (let ((,ret))
                  (progn (add-exception-handler (lambda ,(cadr exception-clause)
                                                  (let ((,ret1))
                                                    (disable-exception-handlers)
                                                    ,finally-clause
                                                    (set ,ret1 ,(caddr exception-clause))
                                                    (enable-exception-handlers)
                                                    (,cc ,ret1))))
                         (set ,ret
                              ,body)
                         (disable-exception-handlers)
                         ,finally-clause
                         (enable-exception-handlers)
                         ,ret))))))

(defmacro unwind-protect (body finally-clause)
  `(try ,body (catch (e) (throw e)) ,finally-clause))

;let* named let1 as we don't allow asterisks in symbol names (yet)
(defmacro let* (specs &rest body)
  (if (or (null specs) (eq (length specs) 1))
      `(let ,specs ,@body)
    `(let (,(car specs)) (let* ,(cdr specs) ,@body))))

(defun array-eq (a1 a2)
  (if (or (not (arrayp a1)) (not (arrayp a2)))
      (throw (exception 'invalid-argument "Both arguments to ARRAY-EQ should be arrays"))
    (progn (let ((l (array-length a1)))
             (cond ((neq l (array-length a2)) nil)
                   (t (dotimes (i l)
                        (if (neq (array-get a1 i) (array-get a2 i))
                            (return-from array-eq nil))))))
           t)))

(defmacro array (dims default-value)
  (if (eq (length dims) 1)
      `(make-array ,(car dims) ,default-value)
    `(make-array ,(car dims) (array ,(cdr dims) ,default-value))))

(defmacro build-ref (a indexes)
  (if (null indexes)
      (error "Indexes cannot be nil")
    (let ((rev-indexes (reverse indexes)))
      (if (eq (length rev-indexes) 1) 
          `(array-get ,a ,(car rev-indexes))
        `(array-get (build-ref ,a ,(cdr rev-indexes)) ,(car rev-indexes))))))

(defmacro aset (ref val)
  (let ((a (second ref))
	(last-index (last ref))
	(indexes (butlast (cddr ref) 1)))
    (if (null indexes)
        `(array-set ,a ,last-index ,val)
      `(array-set (build-ref ,a ,indexes) ,last-index ,val))))

(defmacro aref (a &rest indexes)
  `(build-ref ,a ,indexes))

(defun process-ff-param-spec (spec)
  (list 'list (car spec) (list 'quote (cadr spec))))

(defmacro call-foreign-function (fn-name ret-type param-specs)
  (let ((specs (map process-ff-param-spec param-specs)))
    `(call-ff-internal ,fn-name ',ret-type (list ,@specs))))

;general-purpose read; returns integer, float or string
;depending on what is read from stdin
(defun read ()
  (let* ((i 0) 
         (f 0.0)
         (c (make-array 100 #\0))
         (ret (call-foreign-function "plisp_read" integer ((i integer-pointer)
                                                           (f float-pointer)
                                                           (c character-pointer)))))
    (cond ((eq ret -1)        (throw (exception 'read-exception "Error calling read (overflow?)")))
          ((eq ret 1)         i)
          ((eq ret 2)         f)
          (t                  c))))

(defun read-integer ()
  (let ((i (read)))
    (if (integerp i)
        i
      (throw (exception 'not-an-integer "Not an integer")))))

(defun read-float ()
  (let ((f (read)))
    (if (numberp f) 
        (* 1.0 f)
      (throw (exception 'not-a-float "Not a float")))))

(defun read-string ()
  (let ((s (read)))
    (if (stringp s) 
        s
      (throw (exception 'not-a-string "Not a string")))))

(defun read-character ()
  (array-get (read-string) 0))

#|
(defun alloc-ext-mem-int (n)
  (list (call-foreign-function "alloc_memory_int" integer ((n integer)))
        n
        1))

(defun alloc-ext-mem-float (n)
  (list (call-foreign-function "alloc_memory_float" integer ((n integer)))
        n
        2))

(defun alloc-ext-mem-char (n)
  (list (call-foreign-function "alloc_memory_char" integer ((n integer)))
        n
        3))

(defun set-ext-mem-cell (blk pos val)
  (let ((ptr (car blk))
        (len (cadr blk))
        (type (caddr blk)))
    (if (not (and (>= pos 0) (< pos len)))
        (throw (exception 'invalid-index "set-ext-mem-cell: index out of bounds")))
    (cond ((eq type 1)
           (progn (if (not (integerp val)) (exception 'invalid-argument "set-ext-mem-cell: integer expected"))
                  (call-foreign-function "set_memory_ref_int" 
                                         void
                                         ((ptr integer)
                                          (pos integer)
                                          (val integer)))))
          ((eq type 2)
           (progn (if (not (floatp val)) (exception 'invalid-argument "set-ext-mem-cell: float expected"))
                  (call-foreign-function "set_memory_ref_float" 
                                         void
                                         ((ptr integer)
                                          (pos integer)
                                          (val float)))))
          ((eq type 3)
           (progn (if (not (characterp val)) (exception 'invalid-argument "set-ext-mem-cell: character expected"))
                  (call-foreign-function "set_memory_ref_char" 
                                         void
                                         ((ptr integer)
                                          (pos integer)
                                          (val character)))))
          (t (throw (exception 'exception "Invalid type of external memory"))))))

(defun get-ext-mem-cell (blk pos)
  (let ((ptr (car blk))
        (len (cadr blk))
        (type (caddr blk)))
    (if (not (and (>= pos 0) (< pos len)))
        (throw (exception 'invalid-index "get-ext-mem-cell: index out of bounds")))
    (cond ((eq type 1)
           (call-foreign-function "get_memory_ref_int" 
                                  integer
                                  ((ptr integer)
                                   (pos integer))))
          ((eq type 2)
           (call-foreign-function "get_memory_ref_float" 
                                  float
                                  ((ptr integer)
                                   (pos integer))))
          ((eq type 3)
           (call-foreign-function "get_memory_ref_char" 
                                  character
                                  ((ptr integer)
                                   (pos integer))))
          (t (throw (exception 'exception "Invalid type of external memory"))))))

(defun set-ext-mem (blk arr)
  (let ((ptr (car blk))
        (len (cadr blk))
        (type (caddr blk)))
    (if (not (arrayp arr))
        (throw (exception 'invalid-argument "set-ext-mem expects an array as the second parameter")))
    (if (not (eq len (array-length arr)))
        (throw (exception 'invalid-argument "set-ext-mem: memory block size does not match array length")))
    (dolist (i (range 0 (- (array-length arr) 1) 1))
      (let ((val (array-get arr i)))
        (set-ext-mem-cell blk i val)))))

(defun inspect-ext-mem (memory-block)
  (let ((ptr (car memory-block))
        (len (cadr memory-block))
        (type (caddr memory-block)))
    (cond ((eq type 1) (call-foreign-function "print_memory_int" 
                                              void 
                                              ((ptr integer)
                                               (len integer))))
          ((eq type 2) (call-foreign-function "print_memory_float" 
                                              void 
                                              ((ptr integer)
                                               (len integer))))
          ((eq type 3) (call-foreign-function "print_memory_char" 
                                              void 
                                              ((ptr integer)
                                               (len integer))))
           (t (throw (exception 'exception "Invalid type of external memory"))))))

(defun free-ext-mem (memory-block)
  (let ((ptr (car memory-block)))
    (call-foreign-function "free_memory" void ((ptr integer)))))
|#

(create-package "user")

(in-package "user")
