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

(create-package "io")

(in-package "io")

(defun open-file (name mode)
  (let ((fd))
    (set fd (call-foreign-function "open_file" 
                                   'integer
                                   '((name character-pointer)
                                     (mode character-pointer))))
    (if (eq fd -1)
        (throw (exception 'exception "Unable to open file")))
    fd))

(defun close-file (fd)
  (call-foreign-function "close_file"
                         'void
                         '((fd integer))))

(defmacro with-open-file (fd file-name mode &rest body)
  `(let ((,fd))
     (try (progn (set ,fd (open-file ,file-name ,mode))
                 ,@body)
          nil
          (close-file ,fd))))