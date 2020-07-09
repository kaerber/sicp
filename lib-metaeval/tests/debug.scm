(load "lib/debugging.scm")
(load "lib/unit-testing.scm")

(load "lib-metaeval/evaluator.scm")

(define (run program)
  (eval program the-global-environment))

(define env the-global-environment)

(test-fixture "NAMED LET"
  (test "parameterless lambda" (lambda ()
    (assert-equals 12 
                   (run '((lambda () (define (iter) (+ 5 7)) (iter)))))))
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
