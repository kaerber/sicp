(load "lib/debugging.scm")
(load "lib/unit-testing.scm")

(load "lib-metaeval/tests/evaluator.scm")

(load "4/0а/evaluator.scm")

(define (run program)
  (eval program the-global-environment))

(test-fixture "LET"
  (test "Zero definitions" (lambda ()
    (assert-equals 12
      (run '(let ()
              (+ 5 7))))))
  (test "One definition" (lambda ()
    (assert-equals 12
      (run '(let ((x 5))
              (+ x 7))))))
  (test "Many definitions" (lambda ()
    (assert-equals 22
      (run '(let ((x (+ 5 2))
                  (y (+ 7 8)))
              x
              y
              (+ x y))))))
  )