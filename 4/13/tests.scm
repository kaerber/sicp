(load "lib/debugging.scm")
(load "lib/unit-testing.scm")

(load "lib-metaeval/tests/evaluator.scm")
(load "4/13/evaluator.scm")

(define test-env (setup-environment))
(define (run program)
  (eval program test-env))

(test-fixture "MAKE-UNBOUND"
  (test "Unbound variable is not defined" (lambda ()
    (assert-error "Unbound variable -- LOOKUP" '(x) (lambda ()
      (run '(begin
             (define x 3)
             (undefine x)
             x
             )))))
        )
  (test "Unbound variable then rebound" (lambda ()
    (assert-equals 5
      (run '(begin
             (define x 3)
             (undefine x)
             (define x 5)
             x
             )))))

)
