((
  ((lambda (eval-cond)
     (fix (lambda (eval)
            (lambda (env)
              (lambda (expr)
                (cond
                 ((number? expr)
                  expr)

                 ((= nil expr)
                  nil)

                 ((atom? expr)
                  (env expr))


                 ((= 'quote (car expr))
                  (cadr expr))

                 ;; Buildins
                 ((= 'cons (car expr))
                  (cons ((eval env) (cadr expr))
                        ((eval env) (caddr expr))))

                 ((= 'car (car expr))
                  (car ((eval env) (cadr expr))))

                 ((= 'cdr (car expr))
                  (cdr ((eval env) (cadr expr))))

                 ((= 'zero? (car expr))
                  (zero? ((eval env) (cadr expr))))

                 ((= 'sub1 (car expr))
                  (sub1 ((eval env) (cadr expr))))

                 ((= '* (car expr))
                  (* ((eval env) (cadr expr))
                     ((eval env) (caddr expr))))

                 ((= '= (car expr))
                  (= ((eval env) (cadr expr))
                     ((eval env) (caddr expr))))

                 ((= 'number? (car expr))
                  (number? ((eval env) (cadr expr))))

                 ((= 'atom? (car expr))
                  (atom? ((eval env) (cadr expr))))

                 ;; Macros

                 ((= 'fix (car expr))
                  ((eval env)
                   (cons
                    '(lambda (f) ((lambda (x) (f (lambda (n) ((x x) n))))
                                  (lambda (x) (f (lambda (n) ((x x) n))))))
                    (cons (cadr expr) nil))))

                 ((= 'cadr (car expr))
                  (cadr ((eval env) (cadr expr))))

                 ((= 'caddr (car expr))
                  (caddr ((eval env) (cadr expr))))

                 ((= 'caadr (car expr))
                  (caadr ((eval env) (cadr expr))))

                 ((= 'cond (car expr))
                  ((eval-cond
                    (lambda (expr) ((eval env) expr)))
                   (cdr expr)))

                 ((= 'lambda (car expr))
                  (lambda (arg)
                    ((eval
                      (lambda (y) (cond
                                   ((= y (caadr expr)) arg)
                                   (1 (env y)))))
                     (caddr expr))))
                 ;; application
                 (1
                  (((eval env) (car expr))
                   ((eval env) (cadr expr))))))))))
   (fix
    (lambda (eval-cond)
      (lambda (eval)
        (lambda (clauses)
          (cond
           ((= nil clauses)
            nil)

           ((eval (car (car clauses)))
            (eval (cadr (car clauses))))

           (1 ((eval-cond eval) (cdr clauses)))))))))

  ;; Environment
  (lambda (x) nil))

 ;; code to evaluate '(eval '(factorial 5))
 '((
    ((lambda (eval-cond)
       (fix (lambda (eval)
              (lambda (env)
                (lambda (expr)
                  (cond
                   ((number? expr)
                    expr)

                   ((= nil expr)
                    nil)

                   ((atom? expr)
                    (env expr))


                   ((= 'quote (car expr))
                    (cadr expr))

                   ;; Buildins
                   ((= 'cons (car expr))
                    (cons ((eval env) (cadr expr))
                          ((eval env) (caddr expr))))

                   ((= 'car (car expr))
                    (car ((eval env) (cadr expr))))

                   ((= 'cdr (car expr))
                    (cdr ((eval env) (cadr expr))))

                   ((= 'zero? (car expr))
                    (zero? ((eval env) (cadr expr))))

                   ((= 'sub1 (car expr))
                    (sub1 ((eval env) (cadr expr))))

                   ((= '* (car expr))
                    (* ((eval env) (cadr expr))
                       ((eval env) (caddr expr))))

                   ((= '= (car expr))
                    (= ((eval env) (cadr expr))
                       ((eval env) (caddr expr))))

                   ((= 'number? (car expr))
                    (number? ((eval env) (cadr expr))))

                   ((= 'atom? (car expr))
                    (atom? ((eval env) (cadr expr))))

                   ;; Macros

                   ((= 'fix (car expr))
                    ((eval env)
                     (cons
                      '(lambda (f) ((lambda (x) (f (lambda (n) ((x x) n))))
                                    (lambda (x) (f (lambda (n) ((x x) n))))))
                      (cons (cadr expr) nil))))

                   ((= 'cadr (car expr))
                    (cadr ((eval env) (cadr expr))))

                   ((= 'caddr (car expr))
                    (caddr ((eval env) (cadr expr))))

                   ((= 'caadr (car expr))
                    (caadr ((eval env) (cadr expr))))

                   ((= 'cond (car expr))
                    ((eval-cond
                      (lambda (expr) ((eval env) expr)))
                     (cdr expr)))

                   ((= 'lambda (car expr))
                    (lambda (arg)
                      ((eval
                        (lambda (y) (cond
                                     ((= y (caadr expr)) arg)
                                     (1 (env y)))))
                       (caddr expr))))
                   ;; application
                   (1
                    (((eval env) (car expr))
                     ((eval env) (cadr expr))))))))))
     (fix
      (lambda (eval-cond)
        (lambda (eval)
          (lambda (clauses)
            (cond
             ((= nil clauses)
              nil)

             ((eval (car (car clauses)))
              (eval (cadr (car clauses))))

             (1 ((eval-cond eval) (cdr clauses)))))))))


    (lambda (x) nil))

   '((fix (lambda (!)
            (lambda (n)
              (cond
               ((zero? n) 1)
               (1 (* (! (sub1 n)) n)))))) 92)))
