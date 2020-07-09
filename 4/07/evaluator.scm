(load "lib/debugging.scm")
(load "lib-metaeval/evaluator.scm")

; LET*
(define (let*? exp) (tagged-list? exp 'let*))
(define (let*-bindings exp) (cadr exp))
(define (let*-binding-name binding) (car binding))
(define (let*-binding-value binding) (cadr binding))
(define (let*-expressions exp) (cddr exp))
(define (make-let bindings expressions)
  (list 'let bindings expressions))

(define (let*->nested-lets exp)
  (define (iter bindings)
    (cond ((null? bindings) (sequence->exp (let*-expressions exp)))
          (else (make-let (list (car bindings)) (iter (cdr bindings))))))
  
  (iter (let*-bindings exp)))

((root 'add) let*? (lambda (exp env) (eval (log (let*->nested-lets exp)) env)))
