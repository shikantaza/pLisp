;;  Copyright 2011-2015 Rajesh Jayaprakash <rajesh.jayaprakash@gmail.com>

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

(defmacro assert (condition text)
  `(if (not ,condition)
       (error ,text)))

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

(defmacro and1 (&rest lst)
  (if (null lst)
       t
    (if (eq (length lst) 1)
        (if (null (car lst))
            nil
          t)
      `(if (not ,(car lst))
           nil
         (and1 ,@(cdr lst))))))

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

(defun map (f lst)
  (if (null lst)
      nil
    (cons (f (car lst)) (map f (cdr lst)))))

(defun assoc (x y)
  (assert (listp y) "Second argument to ASSOC should be a list of CONS objects")
  (if (null y)
      nil
    (progn 
      (assert (consp (car y)) "Second argument to ASSOC should be a list of CONS objects")
      (if (eq (caar y) x)
	  (cadar y)
	(assoc x (cdr y))))))

(defun last (lst)
   (if (null (cdr lst))
      (car lst)
      (last (cdr lst))))

(defun remove-last (lst)
  (if (eq (length lst) 1)
      nil
      (cons (car lst) (remove-last (cdr lst)))))

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

(defun exception (excp desc)
  (cons excp desc))

(defun curry (function &rest args)
  (if (not (closurep function)) (throw (exception 'arg-mismatch "First parameter to CURRY should be a closure"))
    (lambda (&rest more-args)
      (apply function (append args more-args)))))

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

(defun remove-duplicates (lst equality-test)
  (let ((result nil))
    (dolist (x lst)
      (if (not (find x result equality-test))
	  (set result (append result (list x)))
	  ()))
    result))

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
		    (set res (append res (list (nth i lst)))))))))

(defun last-n (lst n)
  (sub-list lst (- (length lst) n) n))

(defun butlast (lst n)
  (assert (and (listp lst) (integerp n)) "Arguments to BUTLAST should be a list and an integer")
  (assert (and (>= n 1) (<= n (length lst))) "Second argument to BUTLAST should be a positive integer less than or equal to the length of the list")
  (sub-list lst 0 (- (length lst) n)))

(defun mapcar-old (f &rest lists)  
  (let ((min-length (min (map length lists)))
	(result nil)
	(i 0))
    (while (< i min-length)
      (print result)
      (break)
      (set result (append result (list (apply f (map (curry nth i) lists)))))
      (incf i))
    result))

(defun mapcar (f &rest lists)
  (if (eq (car lists) nil)
      nil
    (cons (apply f (map car lists))
          (apply mapcar (concat (list f) (map cdr lists))))))

(defun flatten-old (lst)
  (let ((flattened-list))
    (dolist (x lst)
      (set flattened-list (append flattened-list x)))
    flattened-list))

(defun flatten (lst)
  (apply concat (map (lambda (x)
                       (if (listp x)
                           x
                         (list x)))
                     lst)))

(defmacro mapcan (f &rest lists)  
  `(flatten (mapcar ,f ,@lists))) 

(load-foreign-library "./libplisp.so")

;needed because FORMAT does not handle newlines
(defmacro println ()
  `(call-foreign-function "print_line" 'void nil))

(defmacro alias (sym1 sym2)
  `(define ,sym1 ,sym2))

(define exception-handlers nil)

(defun concat-strings (str1 str2)
  (let ((l1 (array-length str1))
        (l2 (array-length str2)))
    (let ((str (make-array (+ l1 l2) nil)))
      (dolist (i (range 0 (- l1 1) 1))
        (array-set str i (array-get str1 i)))
      (dolist (i (range 0 (- l2 1) 1))
        (array-set str (+ i l1) (array-get str2 i)))
      str)))

(defun throw (e)
  (if (null exception-handlers)
      (let ((err-str (concat-strings "Uncaught exception: " (symbol-name (car e)))))
        (error  (if (null (cdr e))
                    err-str
                  (concat-strings err-str (concat-strings ":" (cdr e))))))
    (let ((h (car exception-handlers)))
      (progn (set exception-handlers (cdr exception-handlers))
             (h e)))))

(defmacro try (body exception-clause finally-clause)
  `(call-cc (lambda (cc)
              (let ((ret))
                (progn (set exception-handlers (cons (lambda ,(cadr exception-clause)
                                                       (cc (progn ,finally-clause
                                                                  ,(caddr exception-clause))))
                                                     exception-handlers))
                       (set ret ,body)
                       ,finally-clause
                       ret)))))

(defmacro unwind-protect (body finally-clause)
  `(try ,body (catch (e) (throw e)) ,finally-clause))

;let* named let1 as we don't allow asterisks in symbol names (yet)
(defmacro let1 (specs &rest body)
  (if (or (null specs) (eq (length specs) 1))
      `(let ,specs ,@body)
    `(let (,(car specs)) (let1 ,(cdr specs) ,@body))))

(defmacro beginold (&rest body)
  (if (null body)
      nil
    `((lambda (,(gensym)) ,(cadr body)) ,(car body))))

(defmacro begin (&rest body)
  (if (null body)
      nil
    (if (eq (length body) 1)
        (car body)
      `(let ((,(gensym) ,(car body)))
         (begin ,@(cdr body))))))

(defun array-eq (a1 a2)
  (if (or (not (arrayp a1)) (not (arrayp a2)))
      (throw (exception 'invalid-argument "Both arguments to ARRAY-EQ should be arrays"))
    (progn (let ((l (array-length a1)))
             (cond ((neq l (array-length a2)) nil)
                   (t (dotimes (i l)
                        (if (neq (array-get a1 i) (array-get a2 i))
                            (return-from array-eq nil))))))
           t)))

;general-purpose read; returns integer, float or string
;depending on what is read from stdin
(defun read ()
  (let1 ((i 0) 
         (f 0.0)
         (c (string ""))
         (ret (call-foreign-function "plisp_read" 'integer '((i integer-pointer)
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

(defun alloc-ext-mem-int (n)
  (list (call-foreign-function "alloc_memory_int" 'integer '((n integer)))
        n
        1))

(defun alloc-ext-mem-float (n)
  (list (call-foreign-function "alloc_memory_float" 'integer '((n integer)))
        n
        2))

(defun alloc-ext-mem-char (n)
  (list (call-foreign-function "alloc_memory_char" 'integer '((n integer)))
        n
        3))

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

(defun set-ext-mem-cell (blk pos val)
  (let ((ptr (car blk))
        (len (cadr blk))
        (type (caddr blk)))
    (if (not (and (>= pos 0) (< pos len)))
        (throw (exception 'invalid-index "set-ext-mem-cell: index out of bounds")))
    (cond ((eq type 1)
           (progn (if (not (integerp val)) (exception 'invalid-argument "set-ext-mem-cell: integer expected"))
                  (call-foreign-function "set_memory_ref_int" 
                                         'void
                                         '((ptr integer)
                                           (pos integer)
                                           (val integer)))))
          ((eq type 2)
           (progn (if (not (floatp val)) (exception 'invalid-argument "set-ext-mem-cell: float expected"))
                  (call-foreign-function "set_memory_ref_float" 
                                         'void
                                         '((ptr integer)
                                           (pos integer)
                                           (val float)))))
          ((eq type 3)
           (progn (if (not (characterp val)) (exception 'invalid-argument "set-ext-mem-cell: character expected"))
                  (call-foreign-function "set_memory_ref_char" 
                                         'void
                                         '((ptr integer)
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
                                  'integer
                                  '((ptr integer)
                                    (pos integer))))
          ((eq type 2)
           (call-foreign-function "get_memory_ref_float" 
                                  'float
                                  '((ptr integer)
                                    (pos integer))))
          ((eq type 3)
           (call-foreign-function "get_memory_ref_char" 
                                  'character
                                  '((ptr integer)
                                    (pos integer))))
          (t (throw (exception 'exception "Invalid type of external memory"))))))


(defun inspect-ext-mem (memory-block)
  (let ((ptr (car memory-block))
        (len (cadr memory-block))
        (type (caddr memory-block)))
    (cond ((eq type 1) (call-foreign-function "print_memory_int" 
                                              'void 
                                              '((ptr integer)
                                                (len integer))))
          ((eq type 2) (call-foreign-function "print_memory_float" 
                                              'void 
                                              '((ptr integer)
                                                (len integer))))
          ((eq type 3) (call-foreign-function "print_memory_char" 
                                              'void 
                                              '((ptr integer)
                                                (len integer))))
           (t (throw (exception 'exception "Invalid type of external memory"))))))

(defun free-ext-mem (memory-block)
  (let ((ptr (car memory-block)))
    (call-foreign-function "free_memory" 'void '((ptr integer)))))

(defun simplify (exp)
  (cond ((null exp)
	 nil)
	((atom exp)
	 exp)
	(t (let ((operator (first exp)))
	     (cond ((eq operator 'halt)
		    '(halt))
		   ((eq operator 'refer)
		    (list 'refer (second exp) (simplify (first (third exp)))))
		   ((eq operator 'constant)
		    (list 'constant (second exp) (simplify (first (third exp)))))
		   ((eq operator 'close)
		    (list 'close (second exp) (simplify (first (third exp))) (simplify (first (fifth exp)))))
		   ((eq operator 'macro)
		    (list 'macro (second exp) (simplify (first (third exp))) (simplify (first (fifth exp)))))
		   ((eq operator 'test)
		    (list 'test (simplify (first (second exp))) (simplify (first (third exp)))))
		   ((eq operator 'assign)
		    (list 'assign (second exp) (simplify (first (third exp)))))
		   ((eq operator 'define)
		    (list 'define (second exp) (simplify (first (third exp)))))
		   ((eq operator 'conti)
		    (list 'conti (simplify (first (second exp)))))
		   ((eq operator 'frame)
		    (list 'frame (simplify (first (second exp))) (simplify (first (third exp)))))
		   ((eq operator 'argument)
		    (list 'argument (simplify (first (second exp)))))
		   ((eq operator 'apply)
		    '(apply))
		   ((eq operator 'return)
		    '(return))
		   ((null operator)
		    nil)
		   (t (throw (exception operator))))))))

(defmacro compile (exp)
  `(simplify (compile1 ,exp nil)))

(defmacro array (dims default-value)
  (if (eq (length dims) 1)
      `(make-array ,(car dims) ,default-value)
    `(make-array ,(car dims) (array ,(cdr dims) ,default-value))))

(defmacro build-ref (a indexes)
  (let ((rev-indexes (reverse indexes)))
    (if (eq (length rev-indexes) 1) 
	`(array-get ,a ,(car rev-indexes))
      `(array-get (build-ref ,a ,(cdr rev-indexes)) ,(car rev-indexes)))))

(defmacro aset (ref val)
  (let ((a (second ref))
	(last-index (last ref))
	(indexes (butlast (cddr ref) 1)))
    `(array-set (build-ref ,a ,indexes) ,last-index ,val)))

(defmacro aref (a &rest indexes)
  `(build-ref ,a ,indexes))

(defun expand-macro-full (exp)
  (cond ((atom exp) exp)
        ((and (symbolp (car exp))
              (not (eq (car exp)
                       'let))
              (not (eq (car exp)
                       'letrec))
              (macrop (symbol-value (car exp)))) (expand-macro-full (expand-macro exp)))
        (t (cons (expand-macro-full (car exp))
                 (expand-macro-full (cdr exp))))))

(load-file "lib/pos.lisp")

(load-file "compiler.lisp")

(load-file "lib/utils.lisp")

(load-file "lib/math.lisp")

(load-file "lib/matrix.lisp")

(load-file "lib/statistics.lisp")

;(load-file "graph.lisp")

(load-file "lib/io.lisp")

(create-package "user")

(in-package "user")

;(load-file "tests/unit_tests.lisp")
