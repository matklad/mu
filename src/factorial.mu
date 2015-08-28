((fix (lambda (!)
         (lambda (n)
           (cond
            ((zero? n) 1)
            (1 (* (! (sub1 n)) n)))))) 5)
