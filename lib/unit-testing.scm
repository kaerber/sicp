; unit-testing
(load "lib/io.scm")

; TODO rework assertions to use errors, rework test/test fixture to handle them

(define (assert message condition)
  (if (not condition)
      (eror "assert" "FAILURE: " message)))

(define (assert-equals expected actual)
  (if (not (equal? expected actual))
      (error "assert-equals" "FAILURE: Expected " expected ", but got " actual " .")))

(define (assert-error message params lambda)
  ;(lambda)
  (let ((condition (ignore-errors lambda)))
    (if (condition? condition)
        (let ((cond-message (access-condition condition 'message))
              (cond-irritants (access-condition condition 'irritants)))
          (if (not (and (equal? message cond-message)
                        (equal? params cond-irritants)))
              (error "assert-error" "Expected error with message \"" message "\" and irritants " params 
                     ", but got \"" cond-message "\" and " cond-irritants ".")))
        (error "assert-error" "Expected error with message \"" message "\" and irritants " params ", but got " condition "."))))
    
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

(define (test message test-body)
  (lambda ()
    ;(test-body)
    (println "  " message)
    (let ((result (ignore-errors test-body)))
      (if (condition? result)
          (begin 
            ;(println (condition-type/field-names (condition/type result)))
            (print "  FAILURE: " (access-condition result 'message) ": ")
            (apply println (access-condition result 'irritants))
            false)
          (begin
            (println "  Ok.")
            true)))))
