(create-package "compiler")

(in-package "compiler")

(defun mutating-ids (exp)
  (cond ((or (atom exp)
             (eq (car exp)
                 'error)) nil)
        ((eq (car exp)
             'set) (union (list (second exp))
                          (mutating-ids (third exp))))
        ((eq (car exp)
             'lambda) (difference (mutating-ids (third exp))
                                  (second exp)))
        ((or (eq (car exp)
                 'let)
             (eq (car exp)
                 'letrec)) (difference (union (mutating-ids (third exp))
                                              (apply union
                                                     (map mutating-ids
                                                          (map cadr
                                                               (second exp)))))
                                       (apply union
                                              (mutating-ids (map (lambda (x)
                                                                   (list (car x))))
                                                            (second exp)))))
        (t (apply union
                  (map mutating-ids
                       (subexps exp))))))

(defun union (&rest lst)
  (remove-duplicates (apply concat
                            lst)
                     eq))

(defun exists (e l)
  (if (null l)
      nil
    (if (eq e (car l))
        t
      (exists e
              (cdr l)))))

(defun difference (set1 set2)
  (remove-if (lambda (x)
               (exists x
                       set2))
             set1))

(defun assignment-conversion (exp ids)
  (cond ((symbolp exp) (if (exists exp ids)
                           (list 'car
                                 exp)
                         exp))
        ((atom exp) exp)
        ((eq (first exp)
             'set) (list 'setcar
                         (second exp)
                         (assignment-conversion (third exp)
                                                ids)))
        ((eq (first exp)
             'lambda) (let ((pids (partition (second exp)
                                             (list (third exp)))))
                        (let ((mids (car pids))
                              (uids (cdr pids)))
                          (list 'lambda
                                (second exp)
                                (wrap-cells mids
                                            (assignment-conversion (third exp)
                                                                   (difference (union ids
                                                                                      mids)
                                                                               uids)))))))
        ((eq (first exp)
             'let) (let ((pids (partition (map car
                                               (second exp))
                                          (list (third exp)))))
                     (let ((mids (car pids))
                           (uids (cdr pids)))
                       (list 'let
                             (map (lambda (x)
                                    (list (first x)
                                          (maybe-cell (first x)
                                                      mids
                                                      (assignment-conversion (second x)
                                                                             ids))))
                                  (second exp))
                             (assignment-conversion (third exp)
                                                    (difference (union ids
                                                                       mids)
                                                                uids))))))
        ((eq (first exp)
             'letrec) (let ((pids (partition (map car
                                                  (second exp))
                                             (concat (map cadr
                                                          (second exp))
                                                     (list (third exp))))))
                        (let ((mids (car pids))
                              (uids (cdr pids)))
                          (let ((ids1 (difference (union ids
                                                         mids)
                                                  uids)))
                            (list 'letrec
                                  (map (lambda (x)
                                         (list (first x)
                                               (maybe-cell (first x)
                                                           mids
                                                           (assignment-conversion (second x)
                                                                                  ids1))))
                                       (second exp))
                                  (assignment-conversion (third exp)
                                                         ids1))))))
        (t (mapsub exp
                   (lambda (e)
                     (assignment-conversion e
                                            ids))))))

(defun partition (ids exps)
  (let ((mids (apply union
                     (map mutating-ids
                          exps))))
    (cons (intersection ids
                        mids)
          (difference ids
                      mids))))

(defun wrap-cells (ids exp)
  (if (null ids)
      exp
    (list 'let
          (map (lambda (x)
                 (list x
                       (list 'cons
                             x
                             nil)))
               ids)
          exp)))

(defun maybe-cell (id ids exp)
  (if (exists id ids)
      (list 'cons
            exp
            nil)
    exp))

(defun intersection (set1 set2)
  (remove-if-not (lambda (x)
                   (exists x
                           set2))
                 set1))

(defun subst (exp1 exp2 exp)
  (cond ((null exp) nil)
        ((atom exp) (if (eq exp exp2)
                        exp1
                      exp))
        (t (cons (subst exp1
                        exp2
                        (car exp))
                 (subst exp1
                        exp2
                        (cdr exp))))))

(defun msubst (ids exp)
  (let ((res (clone exp)))
    (dolist (id ids)
      (set res
           (subst (list 'car
                        id)
                  id
                  res)))
    res))

(defun translate-to-il (exp)
  (cond ((atom exp) exp)
        ((eq (first exp)
             'letrec) (list 'let
                            (map (lambda (x)
                                   (list (first x)
                                         (list 'cons
                                               nil
                                               nil)))
                                 (second exp))
                            (desugar-il (list 'let1
                                              (map (lambda (x)
                                                     (list (gensym)
                                                           (list 'setcar
                                                                 (first x)
                                                                 (msubst (map car
                                                                              (second exp))
                                                                         (translate-to-il (second x))))))
                                                   (second exp))
                                              (msubst (map car
                                                           (second exp))
                                                      (translate-to-il (third exp)))))))
        (t (cons (translate-to-il (car exp))
                 (translate-to-il (cdr exp))))))

(defun desugar-il (exp)
  (if (null (second exp))
      (third exp)
    (list 'let
          (list (first (second exp)))
          (desugar-il (list 'let1
                            (cdr (second exp))
                            (third exp))))))

(defun rbind (iold inew re)
  (lambda (ikey)
    (if (eq ikey iold)
        inew
      (re ikey))))

(defun ren-transform (exp re)
  (cond ((null exp) nil)
        ((symbolp exp) (re exp))
        ((atom exp) exp)
        ((eq (first exp)
             'lambda) (let ((fresh-ids (gen-fresh-ids (length (second exp))))
                            (new-re re))
                        (dolist (x (mapcar cons
                                           (second exp)
                                           fresh-ids))
                          (set new-re
                               (rbind (car x)
                                      (cdr x)
                                      new-re)))
                        (list 'lambda
                              fresh-ids
                              (ren-transform (third exp)
                                             new-re))))
        ((eq (first exp)
             'let) (let ((fresh-ids (gen-fresh-ids (length (second exp))))
                         (new-re re))
                     (dolist (x (mapcar cons
                                        (map car
                                             (second exp))
                                        fresh-ids))
                       (set new-re
                            (rbind (car x)
                                   (cdr x)
                                   new-re)))
                     (list 'let
                           (map (lambda (x)
                                  (list (first x)
                                        (ren-transform (second (second x))
                                                       re)))
                                (pair fresh-ids
                                      (second exp)))
                           (ren-transform (third exp)
                                          new-re))))
        (t (cons (ren-transform (car exp)
                                re)
                 (ren-transform (cdr exp)
                                re)))))

(defun gen-fresh-ids (n)
  (map (lambda (x)
         (gensym))
       (range 1
              n
              1)))

(defun free-ids-il (exp)
  (cond ((null exp) nil)
        ((symbolp exp) (if (primop exp)
                           nil
                         (list exp)))
        ((atom exp) nil)
        ((eq (car exp)
             'if) (union (free-ids-il (second exp))
                         (free-ids-il (third exp))
                         (free-ids-il (fourth exp))))
        ((eq (car exp)
             'lambda) (difference (union (free-ids-il (third exp))
                                         (free-ids-il (fourth exp)))
                                  (second exp)))
        ((eq (car exp)
             'define) (difference (free-ids-il (third exp))
                                  (list (second exp))))
        ((eq (car exp)
             'let) (union (flatten (map free-ids-il
                                        (map cadr
                                             (second exp))))
                          (difference (free-ids-il (third exp))
                                      (map car
                                           (second exp)))))
        ((eq (car exp)
             'error) (free-ids-il (second exp)))
        ((eq (car exp)
             'call-cc) (free-ids-il (second exp)))
        (t (flatten (cons (free-ids-il (car exp))
                          (free-ids-il (cdr exp)))))))

(defun simplify-il-empty-let (exp)
  (cond ((atom exp) exp)
        ((and (eq (first exp)
                  'let)
              (null (second exp))) (third exp))
        (t (cons (simplify-il-empty-let (car exp))
                 (simplify-il-empty-let (cdr exp))))))

(defun simplify-il-implicit-let (exp)
  (if (and (consp exp) (consp (car exp)) (eq (caar exp)
                                             'lambda))
      (list 'let
            (mapcar list
                    (second (first exp))
                    (rest exp))
            (third (first exp)))
    exp))

(defun simplify-il-eta (exp)
  (cond ((atom exp) exp)
        ((and (eq (car exp)
                  'lambda)
              (consp (third exp))
              (not (primop (first (third exp))))
              (not (eq (first (third exp))
                       'let))
              (or (symbolp (first (third exp)))
                  (and (consp (first (third exp)))
                       (eq (first (first (third exp)))
                           'lambda)))
              (eq (second exp)
                  (cddr (third exp)))
              (null (intersection (free-ids-il (first (third exp)))
                                  (second exp)))) (first (third exp)))
        (t (cons (simplify-il-eta (car exp))
                 (simplify-il-eta (cdr exp))))))

(defun simplify-il-copy-prop (exp)
  (cond ((atom exp) exp)
        ((and (eq (car exp)
                  'let)
              (eq (length (second exp))
                  2)
              (symbolp (first (first (second exp))))
              (symbolp (second (first (second exp))))) (subst (second (second (first exp)))
                                                              (first (second (first exp)))
                                                              (third exp)))
        (t (cons (simplify-il-copy-prop (car exp))
                 (simplify-il-copy-prop (cdr exp))))))

(defun cps-transform-var-literal (exp)
  (let ((ik (gensym)))
    (list 'lambda
          (list ik)
          (list 'save-continuation
                ik)
          (list ik
                exp))))

(defun cps-transform-abstraction (exp)
  (let ((ik (gensym))
        (iabs (gensym))
        (ikcall (gensym)))
    (list 'lambda
          (list ik)
          (list 'save-continuation
                ik)
          (list 'let
                (list (list iabs
                            (list 'lambda
                                  (concat (second exp)
                                          (list ikcall))
                                  (list 'save-continuation
                                        ikcall)
                                  (list (cps-transform (third exp))
                                        ikcall))))
                (list ik
                      iabs)))))

(defun cps-transform (exp)
  (cond ((atom exp) (cps-transform-var-literal exp))
        ((eq (car exp)
             'lambda) (cps-transform-abstraction exp))
        ((eq (car exp)
             'let) (cps-transform-let exp))
        ((primop (car exp)) (cps-transform-primop exp))
        ((eq (car exp)
             'if) (cps-transform-if (second exp)
                                    (third exp)
                                    (fourth exp)))
        ((eq (car exp)
             'error) (cps-transform-error (second exp)))
        (t (cps-transform-application exp))))

(defun cps-transform-application (exp)
  (let ((ik (gensym)))
    (list 'lambda
          (list ik)
          (list 'save-continuation
                ik)
          (cps-trans-app-internal exp
                                  nil
                                  ik))))

(defun cps-trans-app-internal (exp syms first-sym)
  (if (null exp)
      (concat syms
              (list first-sym))
    (let ((i (gensym)))
      (list (cps-transform (car exp))
            (list 'lambda
                  (list i)
                  (cps-trans-app-internal (cdr exp)
                                          (concat syms
                                                  (list i))
                                          first-sym))))))

(defun cps-transform-let (exp)
  (let ((ik (gensym)))
    (list 'lambda
          (list ik)
          (list 'save-continuation
                ik)
          (cps-trans-let-internal (third exp)
                                  (second exp)
                                  ik))))

(defun cps-trans-let-internal (body bindings first-sym)
  (if (null bindings)
      (list (cps-transform body)
            first-sym)
    (list (cps-transform (second (first bindings)))
          (list 'lambda
                (list (first (first bindings)))
                (cps-trans-let-internal body
                                        (cdr bindings)
                                        first-sym)))))

(defun cps-transform-primop (exp)
  (let ((ik (gensym)))
    (list 'lambda
          (list ik)
          (list 'save-continuation
                ik)
          (cps-trans-primop-internal (car exp)
                                     (cdr exp)
                                     nil
                                     ik))))

(defun cps-trans-primop-internal (primop args syms first-sym)
  (if (null args)
      (let ((ians (gensym)))
        (list 'let
              (list (list ians
                          (concat (list primop)
                                  syms)))
              (list first-sym
                    ians)))
    (let ((i (gensym)))
      (list (cps-transform (car args))
            (list 'lambda
                  (list i)
                  (cps-trans-primop-internal primop
                                             (cdr args)
                                             (concat syms
                                                     (list i))
                                             first-sym))))))

(defun primop (sym)
  (or (arithop sym)
      (core-op sym)
      (string-array-op sym)
      (predicate-op sym)
      (ffi-op sym)
      (package-op sym)
      (serialization-op sym)))

(defun cps-transform-if (test then else)
  (let ((ik (gensym))
        (itest (gensym)))
    (list 'lambda
          (list ik)
          (list 'save-continuation
                ik)
          (list (cps-transform test)
                (list 'lambda
                      (list itest)
                      (list if
                            itest
                            (list (cps-transform then)
                                  ik)
                            (list (cps-transform else)
                                  ik)))))))

(defun cps-transform-error (exp)
  (let ((ik (gensym))
        (ians (gensym)))
    (list 'lambda
          (list ik)
          (list 'save-continuation
                ik)
          (list (cps-transform exp)
                (list 'lambda
                      (list ians)
                      (list 'error
                            ians))))))

(defun closure-conv-transform (exp)
  (cond ((null exp) nil)
        ((atom exp) exp)
        ((eq (car exp)
             'lambda) (if (eq (length exp) 3)
                          (closure-conv-transform-abs-no-cont exp)
                        (closure-conv-transform-abs-cont exp)))
        ((eq (car exp)
             'let) (let ((rval (second (first (second exp)))))
                     (if (and (consp rval) (eq (first rval)
                                               'lambda))
                         (closure-conv-transform-let exp)
                       (mapsub exp
                               closure-conv-transform))))
        ((or (primop (car exp))
             (eq (car exp)
                 'if)
             (eq (car exp)
                 'error)) (mapsub exp
                                  closure-conv-transform))
        (t (closure-conv-transform-app exp))))

(defun closure-conv-transform-let (exp)
  (let ((exp1 (closure-conv-transform (second (first (second exp)))))
        (icode (gensym)))
    (list 'let1
          (list (list icode
                      (second exp1))
                (list (first (first (second exp)))
                      (concat (list 'list
                                    icode)
                              (cddr exp1))))
          (closure-conv-transform (third exp)))))

(defun closure-conv-transform-app (exp)
  (let ((iclo (gensym))
        (icode (gensym)))
    (list 'let1
          (list (list iclo
                      (closure-conv-transform (first exp)))
                (list icode
                      (list 'nth
                            0
                            iclo)))
          (concat (list icode
                        iclo)
                  (map closure-conv-transform
                       (cdr exp))))))

(defun lift-transform (exp bindings)
  (cond ((atom exp) (cons exp
                          bindings))
        ((eq (first exp)
             'lambda) (let ((sym (gensym))
                            (res (lift-transform (if (eq (length exp) 3)
                                                     (third exp)
                                                   (fourth exp))
                                                 bindings)))
                        (cons sym
                              (concat (list (list sym
                                                  (if (eq (length exp) 3)
                                                      (list 'lambda
                                                            (second exp)
                                                            (car res))
                                                    (list 'lambda
                                                          (second exp)
                                                          (third exp)
                                                          (car res)))))
                                      (cdr res)))))
        (t (let ((car-res (lift-transform (car exp)
                                          bindings)))
             (let ((cdr-res (lift-transform (cdr exp)
                                            (cdr car-res))))
               (cons (cons (car car-res)
                           (car cdr-res))
                     (cdr cdr-res)))))))

(defun compile-exp (exp)
  (let ((res exp))
    (set res
         (expand-macro-full res))
    (set res
         (assignment-conversion res
                                '(call-cc1 my-cont-var)))
    (set res
         (translate-to-il res))
    (set res
         (ren-transform res
                        (lambda (x)
                          x)))
    (set res
         (simplify-il res))
    (set res
         (cps-transform res))
    (set res
         (closure-conv-transform res))
    (set res
         (lift-transform res))
    res))

(defun build-evaluatable-exp (exp)
  (let ((clos (gensym))
        (x (gensym))
        (y (gensym)))
    (list 'let1
          (concat (reverse (cdr exp))
                  (list (list clos
                              (first exp))))
          (list (list 'car
                      clos)
                clos
                (list 'list
                      (list 'lambda
                            (list y
                                  x)
                            x))))))

(defun simplify-il (exp)
  (let ((res exp))
    (set res
         (simplify-il-empty-let res))
    (set res
         (simplify-il-implicit-let res))
    (set res
         (simplify-il-eta res))
    (set res
         (simplify-il-copy-prop res))
    res))

(defun arithop (sym)
  (in sym
      '(+ -
          *
          /
          >
          <
          <=
          >=)))

(defun core-op (sym)
  (in sym
      '(atom eq
             call-cc1
             set
             error
             list
             cons
             car
             cdr
             print
             symbol-value
             backquote
             gensym
             setcar
             setcdr
             comma
             comma-at
             apply
             symbol
             symbol-name
             format
             clone
             return
             return-from
             unbind
             newline
             not
             progn)))

(defun string-array-op (sym)
  (in sym
      '(string make-array
               array-get
               array-set
               sub-array
               array-length
               print-string)))

(defun predicate-op (sym)
  (in sym
      '(consp listp
              integerp
              floatp
              characterp
              symbolp
              stringp
              arrayp
              closurep
              macrop
              continuationp)))

(defun ffi-op (sym)
  (in sym
      '(load-foreign-library call-foreign-function)))

(defun package-op (sym)
  (in sym
      '(create-package in-package
                       export-package)))

(defun serialization-op (sym)
  (in sym
      '(create-image save-object
                     load-object
                     load-file)))

(defun perf-op (sym)
  (in sym
      '(profile time)))

(defun debug-op (sym)
  (in sym
      '(break resume
              env
              expand-macro)))

(defun interpreter-specific-op (sym)
  (in sym
      '(eval)))

(defun interpret-compiled-to-il (exp)
  (eval (build-evaluatable-exp (compile-exp (expand-macro-full exp)))))

(defun mapsub (exp tf)
  (cond ((atom exp) exp)
        ((eq (car exp)
             'error) exp)
        ((eq (car exp)
             'if) (list 'if
                        (tf (second exp))
                        (tf (third exp))
                        (tf (fourth exp))))
        ((eq (car exp)
             'set) (list 'set
                         (second exp)
                         (tf (third exp))))
        ((eq (car exp)
             'lambda) (list 'lambda
                            (second exp)
                            (tf (third exp))))
        ((primop (car exp)) (cons (first exp)
                                  (map tf
                                       (rest exp))))
        ((or (eq (car exp)
                 'let)
             (eq (car exp)
                 'letrec)) (list (car exp)
                                 (map (lambda (x)
                                        (list (first x)
                                              (tf (second x))))
                                      (second exp))
                                 (tf (third exp))))
        (t (map tf
                exp))))

(defun subexps (exp)
  (cond ((or (atom exp)
             (eq (car exp)
                 error)) nil)
        ((eq (car exp)
             'if) (list (second exp)
                        (third exp)
                        (fourth exp)))
        ((or (eq (car exp)
                 'set)
             (eq (car exp)
                 'lambda)) (list (third exp)))
        ((primop (car exp)) (cdr exp))
        ((or (eq (car exp)
                 'let)
             (eq (car exp)
                 'letrec)) (concat (map cadr
                                        (second exp))
                                   (list (third exp))))
        (t exp)))

(define env0-il
        nil)

(defun get-env-il (env sym)
  (cond ((null env) nil)
        ((eq (first (first env))
             sym) (second (first env)))
        (t (get-env-il (cdr env)
                       sym))))

(defun put-env-il (sym val)
  (set env0-il
       (concat (list (list sym
                           val))
               env0-il)))

(defun set-for-il (sym exp)
  (put-env-il sym
              (build-evaluatable-exp (compile-exp (expand-macro-full exp)))))

(defun build-il-with-globals (exp flag)
  (if (eq flag 'full-monty)
      (list 'let
            env0-il
            (build-evaluatable-exp (compile-exp (expand-macro-full exp))))
    (list 'let
          env0-il
          (build-evaluatable-exp (compile-exp (expand-macro-full exp))))))

(defun closure-conv-transform-abs-cont (exp)
  (let ((free-ids (free-ids-il exp))
        (iclo (gensym)))
    (if (null free-ids)
        (concat (list 'list)
                (list (list 'lambda
                            (concat (list iclo)
                                    (second exp))
                            (third exp)
                            (closure-conv-transform (fourth exp)))))
      (concat (list 'list)
              (list (list 'lambda
                          (concat (list iclo)
                                  (second exp))
                          (third exp)
                          (list 'let1
                                (map (lambda (n)
                                       (list (nth n
                                                  free-ids)
                                             (list 'nth
                                                   (+ n
                                                      1)
                                                   iclo)))
                                     (range 0
                                            (- (length free-ids)
                                               1)
                                            1))
                                (closure-conv-transform (fourth exp)))))
              free-ids))))

(defun closure-conv-transform-abs-no-cont (exp)
  (let ((free-ids (free-ids-il exp))
        (iclo (gensym)))
    (if (null free-ids)
        (concat (list 'list)
                (list (list 'lambda
                            (concat (list iclo)
                                    (second exp))
                            (closure-conv-transform (third exp)))))
      (concat (list 'list)
              (list (list 'lambda
                          (concat (list iclo)
                                  (second exp))
                          (list 'let1
                                (map (lambda (n)
                                       (list (nth n
                                                  free-ids)
                                             (list 'nth
                                                   (+ n
                                                      1)
                                                   iclo)))
                                     (range 0
                                            (- (length free-ids)
                                               1)
                                            1))
                                (closure-conv-transform (third exp)))))
              free-ids))))

(define my-cont-var
        (cons nil nil))

(defun call-cc1 (clo)
  ((car clo) clo (cadr saved-continuations) (list (lambda (y x)
                                                    x))))

(defun save-continuation (cont)
  (set saved-continuations
       (cons cont
             saved-continuations)))

(define saved-continuations
        nil)

(defun backquote1 (exp)
  (cond ((atom exp) exp)
        ((or (eq (car exp)
                 'comma)
             (eq (car exp)
                 'comma-at)) (eval (cadr exp)))
        (t (let ((res))
             (dolist (x exp)
               (cond ((atom x) (if (null res)
                                   (set res
                                        (list x))
                                 (set res
                                      (concat res
                                              (list x)))))
                     ((eq (car x)
                          'comma) (if (null res)
                                      (set res
                                           (list (eval (cadr x))))
                                    (set res
                                         (concat res
                                                 (list (eval (cadr x)))))))
                     ((eq (car x)
                          'comma-at) (if (null res)
                                         (set res
                                              (eval (cadr x)))
                                       (setcdr res
                                               (eval (cadr x)))))))
             res))))

(defun backquote2 (exp)
  (cond ((atom exp) exp)
        ((or (eq (car exp)
                 'comma)
             (eq (car exp)
                 'comma-at)) (cadr exp))
        (t (let ((bindings)
                 (res))
             (dolist (x exp)
               (cond ((atom x) (if (null res)
                                   (set res
                                        (list x))
                                 (set res
                                      (concat res
                                              (list x)))))
                     ((eq (car x)
                          'comma) (let ((sym (gensym)))
                                    (progn (set bindings
                                                (cons (list sym
                                                            (cadr x))
                                                      bindings))
                                           (if (null res)
                                               (set res
                                                    (list sym))
                                             (set res
                                                  (concat res
                                                          (list sym)))))))
                     ((eq (car x)
                          'comma-at) (let ((sym (gensym)))
                                       (progn (set bindings
                                                   (cons (list sym
                                                               (cadr x))
                                                         bindings))
                                              (if (null res)
                                                  (set res
                                                       sym)
                                                (setcdr res
                                                        sym)))))))
             (if (null bindings)
                 res
               (list 'let
                     bindings
                     res))))))

