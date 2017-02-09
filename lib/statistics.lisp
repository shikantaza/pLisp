;;;; statistics.lisp

(create-package "statistics")

(in-package "statistics")

(alias make-assoc-array utils:make-assoc-array)

(alias get utils:get)

(alias put utils:put)

(alias map-assoc-array utils:map-assoc-array)

(alias mult matrix:mult)

(alias inv matrix:inv)

(alias trans matrix:trans)

(alias power math:power)

(alias sqrt math:sqrt)

(alias oddp utils:oddp)

(alias floor utils:floor)

(alias sort utils:sort)

;classify a point using the k nearest neighbours algorithm
;'point' is a list of the x,y,z,.. coordinates
;'training set' is a list of tuples. the first of each tuple is
;the point (representing the predictor), and the second value is the
;classification (representing the response)
(defun classify-knn (point training-set k)
  (let ((max-class nil) (max-class-count 0))
    (labels ((set-max-class (key val)
	       (if (> val max-class-count)
		   (progn (set max-class-count val)
			  (set max-class key)))))
            (let ((knn (first-n (sort (clone training-set) (curry compare-training-set point)) k))
                  (counts (make-assoc-array)))
              (dolist (x knn)
                (let ((key (second x)))
                  (let ((v (get counts key)))
                    (if (null v) (put counts key 1) (put counts key (+ v 1)))))
                (map-assoc-array set-max-class counts)))
            max-class)))

;similar to classify-knn, but use average of the k nearest neighbours' values
(defun classify-knn-avg (point training-set k)
    (average second (first-n (sort (clone training-set) (curry compare-training-set point)) k)))

;linear regression model coefficients: ((X^TX)^-1)X^TY
(defun glm (x y)
  (mult (mult (inv (mult (trans x) x)) 
              (trans x)) 
        y))

(defun average (f lst)
  (let ((sum 0.0))
    (dolist (x lst)
      (set sum (+ sum (f x))))
    (/ sum (length lst))))

;distance between two points (any number of dimensions)
(defun distance (p1 p2)
  (labels ((dist-internal (x1 x2)
                          (if (and (null (cdr x1)) (null (cdr x2)))
                              (power (- (car x1) (car x2)) 2)
                            (+ (power (- (car x1) (car x2)) 2) (dist-internal (cdr x1) (cdr x2))))))
          (sqrt (dist-internal p1 p2))))

(defun compare-training-set (p d1 d2)
  (< (distance p (first d1)) (distance p (first d2))))

(defun median (lst)
  (let ((sorted-list (sort (clone lst) (lambda (x y) (< x y)))))
    (let ((l (length sorted-list)))
      (let ((mid (floor (/ l 2))))
        (if (oddp l)
            (nth mid sorted-list)
          (/ (+ (nth (- mid 1) sorted-list) (nth mid sorted-list)) 2.0))))))
