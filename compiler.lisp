(create-package "compiler")

(in-package "compiler")

(defun mutating-ids (exp)
  (if (listp exp)
      (let ((first-exp (first exp)))
        (cond ((eq first-exp
                   'set) (union (list (second exp))
                                (mutating-ids (third exp))))
              ((eq first-exp
                   'lambda) (difference (mutating-ids (third exp))
                                        (second exp)))
              ((or (eq first-exp
                       'let)
                   (eq first-exp
                       'letrec)) (difference (union (mutating-ids (third exp))
                                                    (apply union
                                                           (map mutating-ids
                                                                (map cadr
                                                                     (second exp)))))
                                             (apply union
                                                    (mutating-ids (map (lambda (x)
                                                                         (list (car x))))
                                                                  (second exp)))))
              ((eq first-exp
                   'error) nil)
              (t (apply union
                        (map mutating-ids
                             exp)))))))

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
        (t (cons (assignment-conversion (car exp)
                                        ids)
                 (assignment-conversion (cdr exp)
                                        ids)))))

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
          (list (car (second exp)))
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
             'lambda) (difference (free-ids-il (third exp))
                                  (second exp)))
        ((eq (car exp)
             'define) (difference (free-ids-il (third exp))
                                  (list (second exp))))
        ((eq (car exp)
             'let) (difference (free-ids-il (third exp))
                               (map car
                                    (second exp))))
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
  (if (atom exp)
      exp
    (if (consp (car exp))
        (if (eq (caar exp) 'lambda)
            (list 'let
                  (mapcar list
                          (second (first exp))
                          (rest exp))
                  (third (first exp)))
          (cons (simplify-il-implicit-let (car exp))
                (simplify-il-implicit-let (cdr exp))))
      (cons (simplify-il-implicit-let (car exp))
            (simplify-il-implicit-let (cdr exp))))))

(defun simplify-il-eta (exp)
  (cond ((atom exp) exp)
        ((and (eq (car exp)
                  'lambda)
              (or (symbolp (second (third exp)))
                  (and (consp (second (third exp)))
                       (eq (first (second (third exp)))
                           'lambda)))
              (null (intersection (free-ids-il (second (third exp)))
                                  (second exp)))) (second third))
        (t (cons (simplify-il-eta (car exp))
                 (simplify-il-eta (cdr exp))))))

(defun simplify-il-copy-prop (exp)
  (cond ((atom exp) exp)
        ((and (eq (car exp)
                  'let)
              (eq (length (second exp))
                  2)
              (symbolp (first (second exp)))
              (symbolp (second (second exp)))) (subst (second (second exp))
                                                      (first (second exp))
                                                      (third exp)))
        (t (cons (simplify-il-copy-prop (car exp))
                 (simplify-il-copy-prop (cdr exp))))))

(defun cps-transform-var-literal (exp)
  (let ((ik (gensym)))
    (list 'lambda
          (list ik)
          (list ik
                exp))))

(defun cps-transform-abstraction (exp)
  (let ((ik (gensym))
        (iabs (gensym))
        (ikcall (gensym)))
    (list 'lambda
          (list ik)
          (list 'let
                (list (list iabs
                            (list 'lambda
                                  (concat (second exp)
                                          (list ikcall))
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
             'error) (cps-transform-error (second exp)))))

(defun cps-transform-application (exp)
  (let ((ik (gensym)))
    (list 'lambda
          (list ik)
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
  (or (eq sym
          '+)
      (eq sym
          '-)
      (eq sym
          '*)
      (eq sym
          '/)))

(defun cps-transform-if (test then else)
  (let ((ik (gensym))
        (itest (gensym)))
    (list 'lambda
          (list ik)
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
          (list (cps-transform exp)
                (list 'lambda
                      (list ians)
                      (list 'error
                            ians))))))

(defun closure-conv-transform (exp)
  (cond ((null exp) nil)
        ((atom exp) exp)
        ((eq (car exp)
             'lambda) (closure-conv-transform-abs exp))
        ((eq (car exp)
             'let) (if (consp (second (first (second exp))))
                       (closure-conv-transform-let exp)
                     (cons (closure-conv-transform (car exp))
                           (closure-conv-transform (cdr exp)))))
        ((primop (car exp)) (concat (list (car exp))
                                    (map closure-conv-transform
                                         (cdr exp))))
        (t (closure-conv-transform-app exp))))

(defun closure-conv-transform-abs (exp)
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

