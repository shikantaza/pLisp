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

(defun and (x y)
  (if x (if y 't nil) nil))

(defun or (x y)
  (if x 't (if y 't nil)))

(defun not (x)
  (if x nil 't))

(defun append (x y)
  (if (null x)
      y
      (cons (car x) (append (cdr x) y))))

(defun pair (x y)
  (print x) (print y)
  (cond ((or (and (atom x) (not (atom y))) (and (not (atom x)) (atom y)))            (error "Arguments mismatch"))
	((not (eq (length x) (length y)))  (error "Length of two lists not equal"))
	('t (if (and (null x) (null y))
		nil
		(cons (list (car x) (car y))
		      (pair (cdr x) (cdr y)))))))

(defun pair (x y)
  (if (and (null x) (null y))
      nil
      (cons (list (car x) (car y))
	    (pair (cdr x) (cdr y)))))

(defun assoc (x y)
  (if (eq (caar y) x)
      (cadar y)
      (assoc x (cdr y))))

(defun mapcar (f lst)
  (if (null lst)
      nil
      (cons (f (car lst))
	    (mapcar f (cdr lst)))))

(defun length (lst)
  (if (null lst)
      0
      (+ 1 (length (cdr lst)))))

(defun last (lst)
   (if (null (cdr lst))
      (car lst)
      (last (cdr lst))))

(defun remove-last (lst)
  (if (eq (length lst) 1)
      nil
      (cons (car lst) (remove-last (cdr lst)))))

(defun sublist (lst start &optional len)
  (let ((res nil)
	(s start)
	(end (if (null len) 
		 (length lst) 
		 (+ start len))))
    (while (< s end)
      (nconc res (list (nth s lst)))
      (incf s))
    res))

(defun butlast (lst &optional (n 1))
  (sublist lst 0 (- (length lst) n)))

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

(defmacro dolistold (spec &rest body)
  (let ((elem (car spec))
	(lst (car (cdr spec))))
    (print lst)
    `(if (eq ,lst nil)
	 nil
	 (progn (let ((,elem (car ,lst)))
		  ,@body)
		(dolistold (,elem (cdr ,lst)) ,@body)))))

(defmacro dolist (spec &rest body)
  (let ((elem (car spec))
	(lst (cadr spec)))
    `(let ((rest ,lst))
       (while (not (null rest))
	 (let ((e (car rest)))
 	   (let ((,elem e))
	     ,@body
	     (set rest (cdr rest))))))))

(defun nth (n lst)
  (if (eq n 0)
      (car lst)
      (nth (- n 1) (cdr lst))))

(defun make-string (size &optional (elem nil))
  (make-array size elem))

(defun string-set (str pos val)
  (array-set str pos val))

(defun string-get (str pos)
  (array-get str pos))

(defvar substring 'sub-array)

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

(defun mapcan (f lst1 lst2)
  (if (or (null lst1) (null lst2))
      nil
      (cons (f (car lst1) (car lst2))
	    (mapcan f (cdr lst1) (cdr lst2)))))

(defmacro nconc (lst1 lst2)
  `(set ,lst1 (append ,lst1 ,lst2)))


(defvar first 'car)

(defvar rest 'cdr)

(defvar setq 'set)

#|(create-package "statistics")

(in-package "statistics")

(defun average (lst)
  (let ((sum 0))
    (dolist (x lst)
      (set sum (+ sum x)))
    (/ sum (length lst))))
|#

(create-package "user")

(in-package "user")

