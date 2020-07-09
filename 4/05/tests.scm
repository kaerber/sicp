(load "lib/debugging.scm")
(load "lib/unit-testing.scm")

(load "lib-metaeval/tests/evaluator.scm")

(load "4/05/evaluator.scm")

(test-fixture "COND"
  (test "Cond" (lambda ()
    (assert-equals 5
      (run '(begin
              (define x 1)
              (cond ((= x 1) 5)
                    (else 10)))))))

  (test "Cond-Else" (lambda ()
    (assert-equals 10
      (run '(begin
              (define x 2)
              (cond ((= x 1) 5)
                    (else 10)))))))
  
  (test "Cond-Test-Recipient" (lambda ()
    (assert-equals 3
      (run '(begin
              (define x 2)
              (cond ((= x 1) 5)
                    (1 => (lambda (y) (set! x (+ x y))))
                    (else 10))
              x)))))

)
