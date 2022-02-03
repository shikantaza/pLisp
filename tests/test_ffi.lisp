;;  Copyright 2011-2022 Rajesh Jayaprakash <rajesh.jayaprakash@pm.me>

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

(load-foreign-library "libtest")

(define i 10)

(define x (string "abc"))

(call-foreign-function "fn_ret_int" 'integer
		       '((i integer)
                         (6.5 float)
                         (#\a character)
                         (x character-pointer)))

(call-foreign-function "fn_ret_float" 'float
		       '((10 integer)
                         (6.5 float)
                         (#\a character)
                         ("abc" character-pointer)))

(call-foreign-function "fn_ret_char" 'character
		       '((10 integer)
                         (6.5 float)
                         (#\a character)
                         ("abc" character-pointer)))

(call-foreign-function "fn_ret_char_ptr" 'character-pointer
		       '((10 integer)
                         (6.5 float)
                         (#\a character)
                         ("abc" character-pointer)))

(define i 10)

(call-foreign-function "fn_arg_int_ptr" 'integer
		       '((i integer-pointer)))

i

(define f1 19.56)

(call-foreign-function "fn_arg_float_ptr" 'integer
		       '((f1 float-pointer)))

f1

(call-foreign-function "fn_arg_char_ptr" 'integer
		       '((x character-pointer)))

x

(call-foreign-function "function_ret_void" 'void
		       '((10 integer)
			(6.5 float)
			(#\a character)
			("abc" character-pointer)))
