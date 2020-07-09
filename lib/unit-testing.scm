; unit-testing
(load "lib/io.scm")

(define (assert message condition)
  (if condition
      (begin
        (display "Ok.") (newline))
      (begin
        (display "FAILURE: ") (display message) (newline))))

(define (assert-equals expected actual)
  (if (eq? expected actual)
      (begin
        (display "Ok.") (newline)
        true)
      (begin
        (println "FAILURE: Expected " expected ", but got " actual " .")
        false)))

(define (test-fixture name . tests)
  (define (iter tests)
    (cond ((null? tests) true)
          (else
            (let ((result ((car tests))))
              (and result (iter (cdr tests)))))))
  
  (println name)
  (if (iter tests)
    'Ok.
    'FAILURE.))

(define (test message execute)
  (lambda ()
    (println "  " message)
    (display "  ")
    (execute)))