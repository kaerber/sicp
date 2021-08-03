(load "lib/debugging.scm")
(load "lib/unit-testing.scm")

(load "lib-metaeval/evaluator.scm")
(load "4/16/evaluator.scm")

(load "lib-metaeval/tests/evaluator.scm")

(define test-env (setup-environment))
(define (run program)
  (eval program test-env))

(test-fixture "LOOKUP-VARIABLE-VALUE"
  (test "Unassigned value causes an error" (lambda ()
    (assert-error "Unbound variable -- LOOKUP" '(x) (lambda ()
      (run '(begin
              (let ((x '*unassigned*))
                x))))))
  )

  (test "No value causes an error" (lambda ()
    (assert-error "Unbound variable -- LOOKUP" '(x) (lambda ()
      (run '(begin
              x)))))
  )

  (test "Return value" (lambda ()
    (assert-equals 10
      (run '(begin
              (let ((y 10))
                y)))))
  )
)

(test-fixture "SCAN-OUT-DEFINES"
  (test "Simple" (lambda ()
    (assert-equals
      '((let ((u '*unassigned*))
          (set! u 7)
          u))
      (scan-out-defines 
        '(
          (define u 7)
          u)
    ))))
  
  (test "Lambda without defines" (lambda ()
    (assert-equals
      '(u)
      (scan-out-defines
        '(u)))))
  
  (test "SICP example" (lambda ()
    (assert-equals
      '((let ((u '*unassigned*)
             (v '*unassigned*))
         (set! u 'e1)
         (set! v 'e2)))
      (scan-out-defines
        '((define u 'e1)
          (define v 'e2))))))
)