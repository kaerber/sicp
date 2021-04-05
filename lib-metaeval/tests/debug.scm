(load "lib/debugging.scm")
(load "lib/unit-testing.scm")

(load "lib-metaeval/evaluator.scm")

(define (run program)
  (eval program the-global-environment))

(test-fixture "EVAL"
  (test "Smoke test" (lambda ()
    (assert-equals 8
      (run '(begin
             (define x 3)
             (define (p a)
               (set! x (+ x a)))
             (p 5)
             x)))))
  