(load "lib/debugging.scm")
(load "lib-metaeval/evaluator.scm")

; LET
(define (let? exp) (tagged-list? exp 'let))

(define (let-binding-name binding) (car binding))
(define (let-binding-value binding) (cadr binding))

(define (common-let? exp) (list? (cadr exp)))
(define (common-let->combination exp)
  (define let-bindings (cadr exp))
  (define let-expressions (cddr exp))

  (cons 
    (make-lambda (map let-binding-name let-bindings)
                 let-expressions)
    (map let-binding-value let-bindings)))

(define (named-let? exp) (and (symbol? (cadr exp)) (list? (caddr exp))))
(define (named-let->combination exp)
  (define let-bindings (caddr exp))
  (define let-expressions (cdddr exp))
  (define let-name (cadr exp))
  
  (define proc (cons 'define 
                 (cons (cons let-name (map let-binding-name let-bindings))
                       let-expressions)))
  (define proc-call (cons let-name (map let-binding-value let-bindings)))
  
  (list (make-lambda ()
                     (list proc proc-call))))

(define (let->combination exp)
  (cond ((common-let? exp) (common-let->combination exp))
        ((named-let? exp) (named-let->combination exp))
        (else (error "Unknown LET format"))))


((root 'add) let? (lambda (exp env) (eval (let->combination exp) env)))
