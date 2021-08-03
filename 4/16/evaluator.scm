(load "lib/debugging.scm")
 
(define (lookup-variable-value var env)
  (let ((binding (find-env-binding var env)))
    (if (or (null? binding)
            (eq? (binding-value binding) '*unassigned*))
        (error "Unbound variable -- LOOKUP" var)
        (binding-value binding))))

(define (scan-out-defines proc-body)
  (define (collect-names exps)
    (cond ((null? exps) '())
          ((definition? (car exps)) (cons (definition-variable (car exps))
                                          (collect-names (cdr exps))))
          (else (collect-names (cdr exps)))))
  
  (define (replace-defines exps)
    (cond ((null? exps) '())
          ((definition? (car exps)) (cons (list 'set! 
                                                (definition-variable (car exps))
                                                (definition-value (car exps)))
                                          (replace-defines (cdr exps))))
          (else (cons (car exps)
                      (replace-defines (cdr exps))))))
  
  (define (make-let names body)
    (list (append 
      (list 'let
      	(map (lambda (name) (cons name '('*unassigned*))) names))
      body)))
  (let ((names (collect-names proc-body)))
    (if (null? names)
        proc-body
        (make-let names (replace-defines proc-body))))
)

(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))
