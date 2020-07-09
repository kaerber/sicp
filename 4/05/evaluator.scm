(load "lib/debugging.scm")
(load "lib-metaeval/evaluator.scm")

; COND
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))

(define (cond-test-recipient? clause)
  (and (eq? (cadr clause) '=>)
       (= (length clause) 3)))
(define (cond-test clause)
  (car clause))
(define (cond-recipient clause)
  (caddr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false      ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (cond ((cond-else-clause? first)
                (if (null? rest)
                  (sequence->exp (cond-actions first))
                  (error "ELSE clause isn't last -- COND->IF"
                         clauses)))
              ((cond-test-recipient? first)
                (list (make-lambda '(test-result)
                        (list (make-if 'test-result
                                       (list (cond-recipient first) 'test-result)
                                       (expand-clauses rest))))
                      (cond-test first)))
              (else
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest)))))))


