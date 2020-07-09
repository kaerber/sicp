(load "lib-metaeval/evaluator.scm")

(define (application? exp) 
  (begin
    (tagged-list? exp 'call)))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))
