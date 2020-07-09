(load "lib/debugging.scm")
(load "lib/unit-testing.scm")

(load "lib-metaeval/tests/evaluator.scm")

(load "4/08/evaluator.scm")

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


(test-fixture "NAMED LET"
  (test "Zero definitions" (lambda ()
    (assert-equals 12
      (run '(let iter ()
              (+ 5 7))))))
  (test "One definition" (lambda ()
    (assert-equals 15
      (run '(let iter ((x 5))
              (if (> x 0)
                  (+ x (iter (- x 1)))
                  0))))))
  (test "Many definitions" (lambda ()
    (assert-equals 15
      (run '(let iter ((x 3) (n 5))
              (if (> n 0)
                  (+ x (iter x (- n 1)))
                  0))))))
  (test "Many definitions, many operations" (lambda ()
    (assert-equals 18
      (run '(let iter ((x 3) (n 6))
              (+ 5 7)
              (if (> n 0)
                  (+ x (iter x (- n 1)))
                  0))))))
  (test "Acceptance test" (lambda ()
    (assert-equals 21
      (run '(begin
              (define (fib n)
                (let fib-iter ((a 1)
                 (b 0)
                 (count n))
                (if (= count 0)
                  b
                  (fib-iter (+ a b) a (- count 1)))))
              (fib 8))))))
 )