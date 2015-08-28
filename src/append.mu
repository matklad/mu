(((fix (lambda (append)
         (lambda (l)
           (lambda (s)
             (cond
              ((= nil l) s)
              (1 (cons (car l) ((append (cdr l)) s))))))))
  '(a b c))
 '(d e))
