; 1. Introduction
; ---------------

; Welcome to the pLisp tutorial!

; If you're viewing this tutorial from within pLisp,
; you've either started pLisp for the first time
; after installation, in which case the tutorial
; would be displayed in the Workspace window, or are 
; visiting the file 'tutorial.lisp' from the 'doc' 
; directory in the pLisp File Browser.

; A couple of things before we get started with the tutorial:

; a) you can evaluate the presented code snippets by either 
; 1) placing the cursor at the end of the code snippet and 
; pressing Control + Enter, or 2) selecting the code snippet
; and clicking on the Evaluate button in the tool bar.

; For example, evaluate this expression using either
; of the above two ways:

(+ 1 1)

; This will display the results of the evaluation
; (i.e. the number 2) in the Transcript window.

; b) Pressing F1 when the focus is on a special operator
; (keyword) will bring up the help window for that
; special operator.

; 2. The Top Level
; ----------------

; The top level is the place where the global objects
; are defined in pLisp. These global objects are available
; for use anywhere in your code (subject to qualifying their
; symbols with the name of the package in which they reside;
; more on this below).

; New top-level objects are created using the DEFINE special operator:

(define x 42)

; and updated using the SET special operator:

(set x 43)

; If you are not satisfied with the definition of a global
; object, you can always UNBIND it, no questions asked:

(unbind 'user:x)

; Two things to note here: a) the symbol to be unbound is to be
; prefixed with the name of the owning package and b) the symbol should
; be quoted.

; 3. pLisp Objects
; ----------------

; pLisp objects are one of these types:

; 1. Symbols: Symbols are objects that represent identifiers. 
; They play a very important role in Lisp in general and
; pLisp in particular, because they enable us to do symbolic 
; computation (i.e. manipulate expressions composed of symbols), 
; which is the basis for macros ('Code is data'). More on this below.

; Symbols are referred to by quoting an identifier:

(list 'abc 'def)

; Here LIST is a pLisp special operator that builds a list out 
; its arguments, viz. the symbols ABC and DEF (symbols are always
; represented in upper case internally, even if you typed them in
; lower case). Go ahead and evaluate the above expression.

; 2. Integers: Integers in pLisp are equivalent to the 'int' datatype in C. 
; For example, in 32-bit architectures, the minimum and maximum 
; integers that can be represented in pLisp are -2147483647 and 
; 2147483647 respectively.

(define i 100)

; 3. Floating point numbers: Floating point numbers in pLisp are 
; equivalent to the 'double' datatype in C, with mimimum and maximum
; values (in 32-bit architectures) of -2.23e308 and 1.8e308.

(define f 3.14)

; 4. Characters: Individual characters are represented using this type.
; They are prefixed with '#\' (e.g. '#\a', '#\b'). Please note that,
; unlile symbols, characters are case-sensitive: #\a and #\A are different.
; At present characters in pLisp are limited to ASCII only.

(define p #\a)

; 5. Strings: Strings are mutable arrays of characters, represented 
; as "abc", "hello world", and so on.

(define s (string "abcde"))

(string-set s 2 #\x)

(print s)

; Non-constant strings are represented as arrays, and their
; indexing is zero-based.

; Note the use of the STRING special operator to create
; a mutable string object; if you had defined 's' as

(define s "abcde")

; the STRING-SET would have failed because you would have been
; trying to update a constant object.

; 6. CONS objects: CONS objects consist of two cells, denoted
; by CAR and CDR (head and tail), and are created using the
; 'cons' special operator:

(define some-cons-obj (cons 'a 100))

; Note that CONS objects are heterogeneous containers, i.e. their
; heads and tails can be objects of different types, as indicated
; in the above code snippet. CONS objects are used to build lists:

(cons 'a (cons 'b nil))

; NIL is the pLisp object that stands for the null object; it
; is also used to signify 'false'.

; 7. Arrays: Arrays are used to store objects sequentially, as opposed
; to list objects which are constructed by linking together CONS objects
; to form linked lists. Like lists, arrays are also heterogeneous.

(define arr (array (5) 0))

(print arr)

; The elements of an array can be accessed using square brackets:

(print arr[0])

; Arrays in pLisp can be multi-dimensional:

(define mult-dim-arr (array (2 2) 0))

(print mult-dim-arr[0 0])

; Note the absence of commas when accessing the elements of a multi-
; dimensional array.

; Arrays are updated using the ASET special operator:

(aset arr[0] "Hello")

(print arr)

(aset mult-dim-arr[0 0] "Hello")

(print mult-dim-arr)

; 8. Functions: Functions are the workhorses of pLisp. They are 
; defined using the DEFUN macro:

(defun add (a b)
  (+ a b))

; Here we define a function called 'add' that takes two
; parameters (a and b) and returns their sum when invoked:

(add 10 20)

; Functions can be recursive; here is the canonical factorial
; function defined in pLisp:

(defun fact (n)
  (if (eq n 0)
      1
    (* n (fact (- n 1)))))

(fact 10)

; pLisp functions (and macros) support the '&rest' keyword,
; used to handle variable numbers of arguments:

(defun f1 (a b &rest c)
  c)

; This function takes two or more parameters and returns
; a list comprising the remainder of the parameters after
; the first two:

(f1 1 2 3 4)

; 9. Macros: Macros are used to transform code before it
; is taken up for compilation. There are many uses for
; such transformations (e.g. simplification of code, reuse,
; and so on), and this tutorial cannot do full justice to their
; power. We will restrict ourselves to a few simple examples.

; Macros are defined using the MACRO special operator
; or, more conveniently, the DEFMACRO macro:

(define my-first (macro (lst)
                   `(car ,lst)))

(defmacro my-first (lst)
  `(car ,lst))

(my-first '(a b c))

; Here we define a macro called 'my-first' that takes as
; argument an expression and produces an expression that,
; when executed, will evaluate the CAR (i.e. the head) of the
; object referred to by the original expression. Somewhat confusing,
; I know. The services of the EXPAND-MACRO special form can
; be utilized to make things clearer:

(expand-macro '(my-first '(a b c)))

; If you evaluate the above expression, you would find the
; result (CAR (QUOTE (A B C))) displayed in the Transcript window,
; which is what is evaluated when you evaluate 
; (MY-FIRST '(A B C))

; Macros make use of the backquote (`), comma (,) and
; comma-at (,a) forms to work their magic. A backquote signifies
; that what follows has to be carried over as-is during the
; transformation, except for forms following commas and comma-at:
; such forms should be replaced by the actual values supplied
; during the execution of the macro. The difference between
; comma and comma-at is that the comma-at form's value is spliced
; into the result list. An example will make this clearer:

(define defun (macro (name vars &rest body)
                     `(define ,name (lambda ,vars ,@body))))

; The above expression is the definition of the DEFUN macro
; in pLisp. The '&rest' keyword indicates that the 'body'
; parameter would actually be a list comprising the parameters
; after the 'vars' parameter.

; If we run the expression

(defun some-fn (a)
  (print a))

; through EXPAND-MACRO:

(expand-macro '(defun some-fn (a) (print a)))

; we get

(DEFINE SOME-FN (LAMBDA (A) (PRINT A)))

; where the &rest parameter (i.e. 'body') was bound to '((print a))'
; and was spliced into the list being created by the macro execution.

; If the comma form had been (erroneously) used instead of the comma-at
; form, we would have ended up with an incorrect definition:

(DEFINE SOME-FN (LAMBDA (A) ((PRINT A))))

; [note the extra parens around '(PRINT A)']

; 10. Continuations: Continuations are functions of one argument
; that represent the rest of the computation. For example,
; in the expression

(+ 8 (- 7 2))

; the continuation after evaluating the sub-expression '(- 7 2)'
; is the function

(lambda (x)
  (+ 8 x))

; You can capture the current continuation (for later use)
; using the CALL-CC special operator. An example will make things clear:

(define x 0)

(define cont nil)

(progn (call-cc (lambda (cc)
                  (set cont cc)))
       (incf x)
       x)

(print x)

(cont 10)

(print x)

; We define a top-level variable 'x' and initialize it to zero.
; We also define another top-leve variable 'cont', initialized to NIL.
; The PROGN expression that comes next does three things: a) it
; captures the continuation in existence before it starts doing
; anything, and stores this in the top-level 'cont' variable
; b) it increments the top-level variable 'x' and finally c) it
; returns the value of 'x'

; After this, we can check that the value of 'x' has indeed been
; updated to 1 by evaluating the PRINT statement. Finally, evaluating
; '(cont 10)' and then inspecting the value of 'x' shows that 'x' has
; been incremented again via the execution of the continuation. The 
; parameter supplied to 'cont' (i.e. 10) is not strictly necessary
; since the continuation is captured at the top level and there
; is no calling expression to return any value to.

; You may not have much use for continuations directly during the
; normal course of operations, but continuations are a powerful
; mechansism and can be used to implement quite a few features
; without any other changes to the language (for example, 
; exceptions in pLisp are implemented using continuations). 
; There is a lot of material out there on the internet that show 
; the nifty things one can do using continuations, so feel free to experiment!
;