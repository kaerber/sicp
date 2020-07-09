; and/or as derived expressions
(define (and->if exp) 
  (define (iter exps)
    (if (null? exps)
        true
 		(list 'if (first-exp exps) (iter (rest-exps exps)) false)))
     
  (iter (cdr exp)))
    

(define (or->if exp)
  (define (iter exps)
    (if (null? exps)
        false
        (list 'if (first-exp exps) true (iter (rest-exps exps)))))
  
  (iter (cdr exp)))