;;  Copyright 2011-2020 Rajesh Jayaprakash <rajesh.jayaprakash@gmail.com>

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

(defun sqrt (x)
  (cond ((null x) 
         (throw (exception 'exception "SQRT needs a non-negative number")))
        ((not (numberp x))
         (throw (exception 'exception "SQRT needs a non-negative number")))
        ((not (>= x 0))
         (throw (exception 'exception "SQRT needs a non-negative number")))
        (t
         (if (eq x 0)
             0
           (let ((guess 1.0))
             (while (> (abs (- x (* guess guess))) 0.000001)
               (set guess (/ (+ guess (/ x guess)) 2.0)))
             guess)))))
