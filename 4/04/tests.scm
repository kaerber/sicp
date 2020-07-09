(load "lib/debugging.scm")
(load "lib/unit-testing.scm")

(load "4/4.04/evaluator.scm")

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
  
  (test "Quoted" (lambda ()
    (assert-equals 'a
      (run (list 'quote 'a)))))
  
  (test "Assignment" (lambda ()
    (assert-equals 5
      (run '(begin
              (define x 7)
              (set! x 5)
              x)))))

  (test "Definition" (lambda ()
    (assert-equals 7
      (run '(begin
              (define x 7)
              x)))))

  (test "Lambda" (lambda ()
    (assert-equals 7
      (run '((lambda (x) (+ x 5)) 2)))))
  
  (test "If-Positive" (lambda ()
    (assert-equals 5
      (run '(if (= 5 5) 
                (* 5 1) 
                (* 5 2))))))

  (test "If-Negative" (lambda ()
    (assert-equals 10
      (run '(if (= 5 7) 
                (* 5 1) 
                (* 5 2))))))

  (test "Begin" (lambda ()
    (assert-equals 10
      (run '(begin
              5
              10)))))

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


  (test "Cond-Else" (lambda ()
    (assert-equals 10
      (run '(begin
              (define x 2)
              (cond ((= x 1) 5)
                    (else 10)))))))
  
  (test "And-First" (lambda ()
    (assert-equals 3
      (run '(begin
              (define x 2)
              (and (= 1 1)
                   (begin 
                     (set! x 3)
                     true))
              x)))))

  (test "And-Second" (lambda ()
    (assert-equals 2
      (run '(begin
              (define x 2)
              (and (= 1 2)
                   (begin 
                     (set! x 3)
                     true))
              x)))))

  (test "Or-First" (lambda ()
    (assert-equals 2
      (run '(begin
              (define x 2)
              (or (= 1 1)
                   (begin 
                     (set! x 3)
                     true))
              x)))))

  (test "Or-Second" (lambda ()
    (assert-equals 3
      (run '(begin
              (define x 2)
              (or (= 1 2)
                   (begin 
                     (set! x 3)
                     true))
              x)))))
)