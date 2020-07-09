(load "lib/debugging.scm")
(load "lib-metaeval/evaluator.scm")


; FOR
(define (for? exp) (tagged-list? exp 'for))

(define (for-control exp) (cadr exp))
(define (for-init exp) (car (for-control exp)))
(define (for-var exp) (car (for-init exp)))
(define (for-init-val exp) (cadr (for-init exp)))
(define (for-test-proc exp) (cadr (for-control exp)))
(define (for-update-proc exp) (caddr (for-control exp)))

(define (for-statements exp) (cddr exp))

(define (for->combination exp)
  (list (make-lambda ()
          (list 
            (list 'define (list 'for-loop (for-var exp))
              (list 'if (for-test-proc exp)
                (cons 'begin (append (for-statements exp)
                                     (list (list 'for-loop (for-update-proc exp)))))))
            (list 'for-loop (for-init-val exp))))))

((root 'add) for? (lambda (exp env) (eval (for->combination exp) env)))
