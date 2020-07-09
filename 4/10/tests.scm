(load "lib/debugging.scm")
(load "lib/unit-testing.scm")

(load "lib-metaeval/tests/evaluator.scm")

(load "4/10/evaluator.scm")

(define (run program)
  (eval program the-global-environment))


(test-fixture "SET! ="
  (test "Smoke" (lambda ()
    (assert-equals 55
      (run '(begin 
              (define x 0)
              (set! x = 55)
              x))))
  )
)