; derived-tests
(load "lib/unit-testing.scm")
(load "lib-metaeval/evaluator.scm")
(load "4/4.04/derived.scm")

(define root (make-root-env))
((root 'add) quoted? text-of-quotation)
((root 'add) assignment? eval-assignment)
((root 'add) definition? eval-definition)
((root 'add) lambda? (lambda (exp env) (make-procedure (lambda-parameters exp) (lambda-body exp) env)))
((root 'add) if? eval-if)
((root 'add) begin? (lambda (exp env) (eval-sequence (begin-actions exp) env)))
((root 'add) cond? (lambda (exp env) (eval (cond->if exp) env)))
((root 'add) and? (lambda (exp env) (eval (and->if exp) env)))
((root 'add) or? (lambda (exp env) (eval (or->if exp) env)))

(define (run program)
  (eval program the-global-environment))


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
