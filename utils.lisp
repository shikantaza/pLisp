(create-package "utils")

(in-package "utils")

(load-foreign-library "libplisp.so")

(defun random ()
  (call-foreign-function "plisp_random" 'float nil))

(defmacro my-append (lst value)
  `(nconc ,lst (list ,value)))

(defun dot-product (a b)
  (if (or (null a) (null b))
      0
      (+ (* (first a) (first b))
         (dot-product (rest a) (rest b)))))

(defun random-interval (low high)
  (+ low (* (- high low) (random))))

(defun all-permutations (lst)
  (if (null lst) '(())
      (mapcan (lambda (x)
		(mapcar (lambda (y) (cons x y))
			(all-permutations (remove x lst)))) lst)))

(defun permutations (lst n)
  (if (eq n 1)
      (mapcar (lambda (x) (list x)) lst)
      (mapcan (lambda (x)
		(mapcar (lambda (y) (cons x y))
			(permutations (remove x lst) (- n 1)))) lst)))

(defun same-list-p (lst1 lst2)
  (and (eq (length lst1) (length lst2))
       (all-elements-exist-p lst1 lst2)
       (all-elements-exist-p lst2 lst1)))

(defun my-and (lst)
  (if (null lst) 
      't
      (and (not (null (car lst))) (my-and (cdr lst)))))

(defun all-elements-exist-p (lst1 lst2)
  (my-and (mapcar (lambda (x) (find x lst2 eq)) lst1)))

(defun combinations (lst n)
  (remove-duplicates (permutations lst n) same-list-p))

(defun all-combinations (lst)
  (mapcar (lambda (x) (combinations lst x)) (range 1 (length lst) 1)))

(defun first-n (lst n)
  (let ((l (length lst)))
    (if (> l n) (butlast lst (- l n)) lst)))

(defun quicksort (lst)
  (if (or (null lst) (<= (length lst) 1))
      lst
    (let ((pivot (first lst)))
      (concat (quicksort (remove-if (lambda (x) (>= x pivot)) lst))
              (remove-if (lambda (x) (not (eq x pivot))) lst)
              (quicksort (remove-if (lambda (x) (<= x pivot)) lst))))))
