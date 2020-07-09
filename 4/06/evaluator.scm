(load "lib/debugging.scm")
(load "lib-metaeval/evaluator.scm")

; LET
(define (let? exp) (tagged-list? exp 'let))
(define (let-bindings exp) (cadr exp))
(define (let-binding-name binding) (car binding))
(define (let-binding-value binding) (cadr binding))
(define (let-expressions exp) (cddr exp))

(define (let->combination exp)
  (cons 
    (make-lambda (map let-binding-name (let-bindings exp))
                 (let-expressions exp))
    (map let-binding-value (let-bindings exp))))

((root 'add) let? (lambda (exp env) (eval (let->combination exp) env)))
