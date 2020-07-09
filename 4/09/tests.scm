(load "lib/debugging.scm")
(load "lib/unit-testing.scm")

(load "lib-metaeval/tests/evaluator.scm")

(load "4/09/evaluator.scm")

(test-fixture "FOR"
  (test "Smoke" (lambda ()
    (assert-equals 55
      (run '(begin 
              (define x 0)
              (for ((i 0) (< i 11) (+ i 1))
                (set! x (+ x i)))
              x))))
  )
)