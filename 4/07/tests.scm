(load "lib/debugging.scm")
(load "lib/unit-testing.scm")

(load "lib-metaeval/tests/evaluator.scm")

(load "4/07/evaluator.scm")

(define (run program)
  (eval program the-global-environment))

(let*->nested-lets '(let* ((x 3)
              (y (+ x 2))
              (z (+ x y 5)))
              (* x z)))

(test-fixture "LET*"
  (test "Zero definitions" (lambda ()
    (assert-equals 12
      (run '(let* ()
              (+ 5 7))))))
  (test "One definition" (lambda ()
    (assert-equals 12
      (run '(let* ((x 5))
              (+ x 7))))))
  (test "Many definitions" (lambda ()
    (assert-equals 39
      (run '(let* ((x 3)
              (y (+ x 2))
              (z (+ x y 5)))
              (* x z))))))
   (test "Many definitions, many operations" (lambda ()
    (assert-equals 39
      (run '(let* ((x 3)
              (y (+ x 2))
              (z (+ x y 5)))
              (+ 5 10)
              (* x z))))))
 )