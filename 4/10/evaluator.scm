(load "lib/debugging.scm")
(load "lib-metaeval/evaluator.scm")

; (set! x = 10)
(define (assignment? exp)
  (logn "assignment?" (and (tagged-list? exp 'set!)
       (tagged-list? (cddr exp) '=))))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (cadddr exp))
