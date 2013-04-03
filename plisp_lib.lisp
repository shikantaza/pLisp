(create-package "statistics")

(in-package "statistics")

(defun average (lst)
  (let ((sum 0))
    (dolist x lst
      (set sum (add sum x)))
  (div sum (length lst))))

(create-package "user")

(in-package "user")
