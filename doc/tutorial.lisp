;;  Copyright 2011-2023 Rajesh Jayaprakash <rajesh.jayaprakash@pm.me>

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

; 1. Introduction
; ---------------

; Welcome to the pLisp tutorial!

; A few things before we get started with the tutorial:

; a) Refer to the user manual in the 'doc' directory for
; an overview of the pLisp environment. The Language
; Reference in the same directory provides details
; of all pLisp special operators.

; b) You can evaluate the presented code snippets by either 
; i) placing the cursor at the end of the code snippet and 
; pressing Control + Enter, or ii) selecting the code snippet
; and clicking on the Evaluate button in the tool bar.

; For example, evaluate this expression using either
; of the above two ways:

(+ 1 1)

; This will display the results of the evaluation
; (i.e. the number 2) in the Transcript window.

; c) Pressing F1 when the focus is on a special operator
; (keyword) will bring up the help window for that
; special operator.

; d) pLisp is a garbage-collected language, and memory
; management is done automatically for you. As long as
; the reference to an object is retained (either because
; that object is bound to a top-level symbol or because
; a reference to the object is held in a CONS or an 
; array object), it will not be freed. The garbage 
; collection runs in the background continuously and 
; manages the memory for you.

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

; The objects defined at the top level can be inspected at any time
; by using the System Browser window. Go ahead and press F9 to do this.

; 3. pLisp Objects
; ----------------

; pLisp objects belong to one of these types:

; 3.1 Symbols
; -----------

; Symbols are objects that represent identifiers. 
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

; 3.2 Integers
; ------------

; Integers in pLisp are equivalent to the 'int' datatype in C. 
; For example, in 32-bit architectures, the minimum and maximum 
; integers that can be represented in pLisp are -2147483647 and 
; 2147483647 respectively.

(define i 100)

(+ i 10)

; 3.3 Floating point numbers
; --------------------------

; Floating point numbers in pLisp are equivalent to the 'double' 
; datatype in C, with mimimum and maximum values (in 32-bit 
; architectures) of -2.23e308 and 1.8e308.

(define f 3.14)

(/ f 2)

; 3.4 Characters
; --------------

; Individual characters are represented using this type.
; They are prefixed with '#\' (e.g. '#\a', '#\b'). Please note that,
; unlile symbols, characters are case-sensitive: #\a and #\A are different.
; At present characters in pLisp are limited to ASCII only.

(define p #\a)

; 3.5 Strings
; -----------

; Strings are mutable arrays of characters, represented 
; as "abc", "hello world", and so on.

(define s (string "abcde"))

(string-set s 2 #\x)

(print s)

; Non-constant strings are represented internally as arrays, 
; and their indexing is zero-based.

; Note the use of the STRING special operator to create
; a mutable string object; if you had defined 's' as

(define s "abcde")

; the STRING-SET would have failed because you would have been
; trying to update a string literal, which is a constant object.

; 3.6 CONS objects
; ----------------

; CONS objects consist of two cells, denoted by CAR and CDR 
; (head and tail), and are created using the CONS special operator:

(define some-cons-obj (cons 'a 100))

(car some-cons-obj)

(cdr some-cons-obj)

; Note that CONS objects are heterogeneous containers, i.e. their
; heads and tails can be objects of different types, as indicated
; in the above code snippet. CONS objects are used to build lists:

(cons 'a (cons 'b nil))

; NIL is the pLisp object that stands for the null object; it
; is also used to signify 'false'.

; 3.7 Arrays
; ----------

; Arrays are used to store objects sequentially, as opposed
; to list objects which are constructed by connecting CONS objects
; to form linked lists. Like lists, arrays are also heterogeneous.

(define arr (array (5) 0))

(print arr)

; The elements of an array can be accessed using square brackets:

(print arr[0])

; Arrays in pLisp can be multi-dimensional:

(define mult-dim-arr (array (2 2) 0))

(print mult-dim-arr)

(print mult-dim-arr[0 0])

; Note the absence of commas when accessing the elements of a multi-
; dimensional array.

; Arrays are updated using the ASET special operator:

(aset arr[0] "Hello")

(print arr)

(aset mult-dim-arr[0 0] "Hello")

(print mult-dim-arr)

; 3.8 Functions
; -------------

; Functions are the workhorses of pLisp. They are 
; defined using the LAMBDA special operator, or
; more conveniently, the DEFUN macro:

(define add (lambda (a b)
              (+ a b)))

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

; 3.9 Macros
; ----------

; Macros are used to transform code before it is taken up for 
; compilation. There are many uses for such transformations 
; (e.g. simplification of code, reuse, and so on), and this 
; tutorial cannot do full justice to their power. We will 
; restrict ourselves to a few simple examples.

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

; If you evaluate the above expression, you will find the
; result (CAR (QUOTE (A B C))) displayed in the Transcript window,
; which is what is evaluated when you evaluate 
; (MY-FIRST '(A B C))

; Macros make use of the backquote (`), comma (,) and
; comma-at (,@) forms to work their magic. A backquote signifies
; that what follows has to be carried over as-is during the
; transformation, except for forms following comma and comma-at:
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

; The example macros presented here are simple; macros
; can leverage the full power of the languge to effect their
; transformations. Have a look at the macros defined in the
; CORE package for examples.

; 3.10 Continuations
; ------------------

; Continuations are functions of one argument that represent 
; the rest of the computation. For example, in the expression

(+ 8 (- 7 2))

; the continuation after evaluating the sub-expression '(- 7 2)'
; is the function

(lambda (x)
  (+ 8 x))

; You can capture the current continuation (for later use)
; using the CALL/CC special operator. An example will make 
; things clear:

(define x 0)

(define cont nil)

(progn (call/cc (lambda (cc)
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
; the nifty things one can do using continuations, so feel free 
; to experiment!

; 4. Control Flow, Iteration, and Error-Handling
; ----------------------------------------------

; 4.1 Control Flow
; ----------------

; Control flow in pLisp is realized using the IF special
; operator and the COND and WHILE macros.

(if (< 1 2)
    100
  200)

; This expression tests the condition (< 1 2), and returns
; 100 if it's true (which it is) and 200 if it's false.

; IF expressions can, of course, be nested:

(if (< 1 2)
    (if (< 3 4)
        50
      100)
  200)

; The COND macro is for situations where multiple tests
; have to be performed, and you want to avoid
; cluttering your code with IFs:

(cond ((< 2 1) 100)
      ((> 3 4) 200)
      ((> 5 3) 300)
      (t 400))

; This expression would print 300. Note the last clause with
; the 't', which serves as the else clause that is always 
; matched.

; The WHILE macro is for executing an expression as long as a
; test returns true:

(define x 0)

(while (< x 10)
  (print x)
  (incf x))

; The above WHILE expression would print the numbers 0 through 9.

; 4.2 Iteration
; -------------

; You can iterate through the elements of the list using the
; DOLIST macro:

(dolist (x '(1 2 3 4))
  (print x))

; The DOTIMES macro can be used to execute an expression
; for a fixed number of times:

(dotimes (i 10)
  (print i))

; (note that i would range from 0 to 9)

; If you would like to have more control over the iteration
; process, the FOR macro is for you:
 
(for (i 0 (< i 3) (incf i) nil)
     (print i))

; The above expression initializes the variable i to 0,
; and prints it for each iteration. At the end of each
; iteration, i is incremented, and a check is performed
; to see if i is less than 3. If it is, the iteration
; terminates and the expression returns NIL.

; 4.3 Error-Handling
; ------------------

; Whenever an error is encountered in the evaluation
; of an expression, the debugger is brought up, showing
; the call stack at the point where the error occurs.

(defun f (n)
  (/ 1 n))

(defun g (n)
  (f n))

(g 0)

; If you execute the above code snippets, you would be
; presented with the Debugger window and a call stack 
; containing three records in reverse chronological order 
; (most recent on top), two of them referring to the functions 
; F and G and their respective arguments, and the third one 
; referring to something called the REPL-FUNCTION. This is 
; a dummy function that encapsulates the expression entered
; by the user ['(g 0)' in this case].

; An error could be a built-in error (like dividing by zero
; or attempting to invoke CAR on a non-CONS object):

(/ 1 0)

(car 10)

; or a user-created error, generated using the ERROR
; special operator:

(defun f (a)
  (if (< a 0)
      (error "Negative number!")
    (* a 2)))

(f -2)

; While the ERROR special operator provides a simple
; way to signal errors, exceptions provide more
; fine-grained control over how you would like
; to handle errors.

; Exceptions are created using the EXCEPTION special
; operator and are signalled using the THROW special
; operator:

(defun f (a)
  (if (< a 0)
      (throw (exception 'my-exception "Negative number!"))
    (* a 2)))

(f -2)

; The difference between the above code snippet and the 
; earlier one is that this version uses the EXCEPTION special
; operator to create an exception object of type MY-EXCEPTION. 
; This allows the calling code to intelligently react to 
; the error conditions; (for example, do one thing if the 
; exception object is of one type and something else if
; it's of another type). An example will make things clearer:

(try (f -2)
     (catch (e)
            (if (eq (car e) 'my-exception)
                "Failure invoking f"))
     (print "Finished invoking f"))

; We enclose the expression '(f -2)' in a TRY expression
; and catch the exception thrown by f (i.e. MY-EXCEPTION)
; and return an error message, thereby preventing the
; propagation of the exception. The form after the CATCH is 
; evaluated irrespective of whether there was any error 
; evaluating the form '(f -2)'. This is useful for executing 
; any cleanup code.

; 5. LET, LETREC, and LET*
; ------------------------

; The LET and LETREC special forms and the LET* macro enable
; us to use create and use local variables in our expressions:

(let ((a 10)
      (b 20)
      (c 30))
  (print (+ a b c)))

; This expression declares three local variable a, b, and c,
; initialized to 10, 20, and 30 respectively, and prints
; their sum.

; LET* is similar to LET, but variables defined later in
; the list can refer to those defined earlier:

(let* ((a 10)
       (b (+ a 1))
       (c (* b 2)))
  (print (+ a b c)))

; LETREC enables us to define recursive local variables:

(letrec ((odd (lambda (n) (if (eq n 0) nil (even (- n 1)))))
         (even (lambda (n) (if (eq n 0) t (odd (- n 1))))))
  (even 17))

; 6. Packages
; -----------

; pLisp objects reside in packages. Packages are useful
; for logically grouping your objects, avoiding naming 
; conflicts and enforcing information-hiding.

; There are two packages available by default in pLisp: the
; CORE and the USER packages. The CORE package consists
; of all the special operators needed for pLisp to work
; properly, and is read-only (i.e. you cannot create
; new objects in it or update existing objects). The USER
; package is for storing your own objects. It goes without
; saying that you can create your own packages:

(create-package "my-package")

; Before you can create objects in a package, you must
; first be in a package:

(in-package "my-package")

(define x 100)

; this will create an object called 'x' in the package
; MY-PACKAGE and initialize it to 100. Press F9 to head
; over to the System Browser and verify this.

; Objects in one package can be accessed from another package
; by prefixing their name with the owning package's name:

(in-package "user")

(print my-package:x)

; The only exception to this is the CORE package; you can
; access the objects belonging to the CORE package without
; prefixing their names.

; Note that there are no mechanisms to protect objects in 
; a package  beyond enforcing the prefixing requirement. 
; With great power comes great responsibility: pLisp 
; trusts that you use this power judiciously :-)

; 7. Foreign Functions Interface
; ------------------------------

; pLisp provides a mechanism to interface to code written
; in other languages as well, via the LOAD-FOREIGN-LIBRARY
; and CALL-FOREIGN-FUNCTION special forms.

; The LOAD-FOREIGN-LIBRARY loads a shared library file
; (.so, .dylib or .dll, depending on the platform) into pLisp so 
; that the functions exported by it can be invoked by 
; pLisp code.

; For example, consider this function defined in the
; shared library libtest.so (or libtest.dylib/libtest.dll if you're
; using OS X/Windows) that ships with pLisp:

; int fn_ret_int(int i, double f, char c, char *s)
; {
;   printf("entering funtion1\n");
;   printf("%d\n",i);
;   printf("%lf\n",f);
;   printf("%c\n",c);
;   printf("%s\n",s);
;   printf("exiting function1\n");
;   return i * i;
; }

; You will load this shared library as below:

(load-foreign-library "libtest")

; and invoke the above function as:

(let ((i 10)
      (x (string "abc"))
      (f 19.56))
  (call-foreign-function "fn_ret_int" integer
                         ((i integer)
                          (6.5 float)
                          (#\a character)
                          (x character-pointer))))

; this will produce the output 100 in the Transcript
; window, and the printf statements in the function will be
; output to the shell or the command window.

; Refer to the file 'unit_tests.lisp' in the 'tests'
; directory for more examples on how to use the foreign
; functions interface.

; That brings us to the end of this brief tutorial. Drop me
; an email at rajesh.jayaprakash@gmail.com if you have
; any comments or questions.

; Happy hacking!
