(format "Running unit test cases...")
(println)

(define passed-cases 0)
(define failed-cases 0)

(defmacro test-condition (condition)
  `(try (if ,condition 
            (incf passed-cases)
          (incf failed-cases))
        (catch (e)
          (incf failed-cases))))

(defmacro test-exception (body excp)
  `(try ,body
        (catch (e)
          (if (eq (car e) ,excp)
              (incf passed-cases)
            (incf failed-cases)))))

;;car
(test-exception (car 1) 'not-a-cons)

(let ((x (car '(1 2 3))))
  (test-condition (eq x 1)))

(let ((x (car nil)))
  (test-condition (null x)))
;end car

;;cdr
(test-exception (cdr 1) 'not-a-cons)

(let ((x (cdr '(1 2 3))))
  (test-condition (eq x '(2 3))))

(let ((x (cdr nil)))
  (test-condition (null x)))
;end cdr

;eq
(test-exception (eq 1) 'arg-mismatch)

(test-exception (eq) 'arg-mismatch)

(test-condition (eq 1 1.0))

(test-condition (eq 'a 'a))

(test-condition (not (eq 'a 'b)))
;end eq

;cons
(test-exception (cons 1) 'arg-mismatch)

(test-exception (cons) 'arg-mismatch)

(let ((x (cons 1 2)))
  (test-condition (and (eq (car x) 1)
                     (eq (cdr x) 2))))
;end cons

;atom
(test-condition (atom 1))

(test-condition (atom nil))

(test-condition (atom 1.0))

(test-condition (atom "abc"))

(test-condition (atom #\a))

(test-condition (atom 'a))

(test-condition (not (atom '(1 2 3))))

(test-condition (not (atom (lambda (x) x))))

(test-condition (not (atom (make-array 10 nil))))

(test-condition (not (atom (macro (x) x))))

(let ((cont))
  (call-cc (lambda (cc) (set cont cc)))
  (test-condition (not (atom cont))))
;end atom

;add
(test-exception (+) 'arg-mismatch)

(test-exception (+ 1) 'arg-mismatch)

(test-exception (+ 1 nil) 'invalid-argument)

(test-exception (+ 1 "abc") 'invalid-argument)

(test-exception (+ 1 #\a) 'invalid-argument)

(test-exception (+ 1 '(1 2 3)) 'invalid-argument)

(test-exception (+ 1 (lambda (x) x)) 'invalid-argument)

(test-exception (+ 1 (macro (x) x)) 'invalid-argument)

(let ((cont))
  (call-cc (lambda (cc) (set cont cc)))
  (test-exception (+ 1 cont) 'invalid-argument))

(test-exception (+ 1 (make-array 10 nil)) 'invalid-argument)

(test-condition (eq (+ 1 1) 2))

(test-condition (eq (+ 1 1.0) 2))

(test-condition (eq (+ 1.0 2.5) 3.5))

(test-condition (eq (apply + (range 1 10 1)) 55))
;end add

;sub
(test-exception (-) 'arg-mismatch)

(test-exception (- 1) 'arg-mismatch)

(test-exception (- 1 nil) 'invalid-argument)

(test-exception (- 1 "abc") 'invalid-argument)

(test-exception (- 1 #\a) 'invalid-argument)

(test-exception (- 1 '(1 2 3)) 'invalid-argument)

(test-exception (- 1 (lambda (x) x)) 'invalid-argument)

(test-exception (- 1 (macro (x) x)) 'invalid-argument)

(let ((cont))
  (call-cc (lambda (cc) (set cont cc)))
  (test-exception (- 1 cont) 'invalid-argument))

(test-exception (- 1 (make-array 10 nil)) 'invalid-argument)

(test-condition (eq (- 1 1) 0))

(test-condition (eq (- 1 1.0) 0))

(test-condition (eq (- 1.0 2.5) -1.5))

(test-condition (eq (apply - (range 1 10 1)) -53))
;end sub

;mult
(test-exception (*) 'arg-mismatch)

(test-exception (* 1) 'arg-mismatch)

(test-exception (* 1 nil) 'invalid-argument)

(test-exception (* 1 "abc") 'invalid-argument)

(test-exception (* 1 #\a) 'invalid-argument)

(test-exception (* 1 '(1 2 3)) 'invalid-argument)

(test-exception (* 1 (lambda (x) x)) 'invalid-argument)

(test-exception (* 1 (macro (x) x)) 'invalid-argument)

(let ((cont))
  (call-cc (lambda (cc) (set cont cc)))
  (test-exception (* 1 cont) 'invalid-argument))

(test-exception (* 1 (make-array 10 nil)) 'invalid-argument)

(test-condition (eq (* 1 1) 1))

(test-condition (eq (* 1 1.0) 1))

(test-condition (eq (* 1.0 2.5) 2.5))

(test-condition (eq (apply * (range 1 10 1)) 3628800))
;end mult

;div
(test-exception (/) 'arg-mismatch)

(test-exception (/ 1) 'arg-mismatch)

(test-exception (/ 1 nil) 'invalid-argument)

(test-exception (/ 1 "abc") 'invalid-argument)

(test-exception (/ 1 #\a) 'invalid-argument)

(test-exception (/ 1 '(1 2 3)) 'invalid-argument)

(test-exception (/ 1 (lambda (x) x)) 'invalid-argument)

(test-exception (/ 1 (macro (x) x)) 'invalid-argument)

(let ((cont))
  (call-cc (lambda (cc) (set cont cc)))
  (test-exception (/ 1 cont) 'invalid-argument))

(test-exception (/ 1 (make-array 10 nil)) 'invalid-argument)

(test-condition (eq (/ 1 1) 1))

(test-condition (eq (/ 1 1.0) 1))

(test-condition (eq (/ 1.0 2.5) 0.4))

(let ((x (apply / (range 1.0 4 1))))
  (test-condition (< (math:abs (- x 0.041667)) 0.000001)))

(test-exception (/ 1 0) 'div-by-zero-exception)
;end div

;list
(test-condition (eq (list 1 2 3) '(1 2 3)))

(test-condition (eq (list) '()))
;end list

;listp
(test-exception (listp) 'arg-mismatch)

(test-condition (listp nil))

(test-condition (listp '(1 2 3)))

(test-condition (listp (cons 1 2)))

(test-condition (not (listp 1)))

(test-condition (not (listp 1.0)))

(test-condition (not (listp 'a)))

(test-condition (not (listp "abc")))

(test-condition (not (listp #\a)))

(test-condition (not (listp (make-array 10 nil))))

(test-condition (not (listp (lambda (x) x))))

(test-condition (not (listp (macro (x) x))))

(let ((cont))
  (call-cc (lambda (cc) (set cont cc)))
  (test-condition (not (listp cont))))
;end listp

;symbol-value
(test-exception (symbol-value) 'arg-mismatch)

(dolist (x '(1 1.0 "abc" #\a (make-array 10 nil) (lambda (x) x) (macro (x) x) '(1 2 3)))
  (test-exception (symbol-value x) 'invalid-argument))

(let ((cont))
  (call-cc (lambda (cc) (set cont cc)))
  (test-exception (symbol-value cont) 'invalid-argument))

(test-exception (symbol-value 'abc) 'symbol-not-bound)

(let ((x 100))
  (test-condition (eq (symbol-value 'x) 100)))
;end symbol-value

;gt
(test-exception (>) 'arg-mismatch)

(test-exception (> 1) 'arg-mismatch)

(test-exception (> 1 '(1 2 3)) 'invalid-argument)

(test-exception (> "abc" 1.0) 'invalid-argument)

(test-condition (> 3 0))

(test-condition (> 2.5 0.6))

(test-condition (> 4.0 1))

(test-condition (> 25 0.1))
;end gt

;gensym
(test-exception (gensym 'a) 'arg-mismatch)
;end gensym

;setcar
(test-exception (setcar) 'arg-mismatch)

(test-exception (setcar 1) 'arg-mismatch)

(test-exception (setcar 1 2) 'arg-mismatch)

(let ((x '(1 2 3)))
  (setcar x 4)
  (test-condition (eq (car x) 4)))
;end setcar

;setcdr
(test-exception (setcdr) 'arg-mismatch)

(test-exception (setcdr 1) 'arg-mismatch)

(test-exception (setcdr 1 2) 'arg-mismatch)

(let ((x (cons 1 2)))
  (setcdr x 4)
  (test-condition (eq x (cons 1 4))))
;end setcdr

;create-package
(test-exception (create-package) 'arg-mismatch)

(test-exception (create-package 1 2) 'arg-mismatch)

(test-exception (create-package 1) 'invalid-argument)

(test-exception (create-package "CORE") 'package-already-exists)

(let ((x (symbol-name (gensym))))
  (create-package x)
  (test-exception (create-package x) 'package-already-exists))
;end create-package

;in-package
(test-exception (in-package) 'arg-mismatch)

(test-exception (in-package 1 2) 'arg-mismatch)

(test-exception (in-package 1) 'invalid-argument)

(test-exception (in-package "core") 'access-violation)

(test-exception (in-package (symbol-name (gensym))) 'package-not-found)

(try (let ((x (symbol-name (gensym))))
       (create-package x)
       (in-package x)
       (in-package "user")
       (incf passed-cases))
     (catch (e)
       (incf failed-cases)))
;end in-package

;expand-macro
(test-exception (expand-macro) 'arg-mismatch)

(test-exception (expand-macro 1 2) 'arg-mismatch)

(test-exception (expand-macro 1) 'invalid-argument)

(test-exception (expand-macro '(QQQQ 1 2 3)) 'macro-undefined)

(test-condition (eq (expand-macro '(neq x y)) '(not (eq x y))))
;end expand-macro

(format "%d of %d test cases passed (%.2f%%)" 
        passed-cases 
        (+ passed-cases failed-cases) 
        (/ (* 100.0 passed-cases) (* 1.0 (+ passed-cases failed-cases))))

(if (not (eq failed-cases 0))
    (format "ATTENTION! Some unit test cases have failed."))

(println)