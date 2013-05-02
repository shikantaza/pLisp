(create-package "math")

(in-package "math")

(defun abs (x)
  (if (>= x 0)
      x
    (* -1 x)))

(defun power (a b)
  (if (eq b 0)
      1
    (if (< b 0)
        (/ 1.0 (power a (abs b)))
      (* a (power a (- b 1))))))
