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
)

(test-fixture "AND"  
  (test "And-Empty" (lambda ()
    (assert-equals true
      (run '(and)))))

  (test "And-Single-True" (lambda ()
    (assert-equals true
      (run '(and true)))))

  (test "And-Single-False" (lambda ()
    (assert-equals false
      (run '(and false)))))

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
)

(test-fixture "Or"  
  (test "Or-Empty" (lambda ()
    (assert-equals false
      (run '(or)))))

  (test "Or-Single-True" (lambda ()
    (assert-equals true
      (run '(or true)))))

  (test "Or-Single-False" (lambda ()
    (assert-equals false
      (run '(or false)))))

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

(test-fixture "MAKE-UNBOUND"
  (test "Unbound variable is not defined" (lambda ()
    (assert-error "Unbound variable -- LOOKUP" '(x) (lambda ()
      (run '(begin
             (define x 3)
             (undefine x)
             x
             )))))
        )
  (test "Unbound variable then rebound" (lambda ()
    (assert-equals 5
      (run '(begin
             (define x 3)
             (undefine x)
             (define x 5)
             x
             )))))

)
