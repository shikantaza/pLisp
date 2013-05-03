(load-foreign-library "libtest.so")

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
