(define (log val)
  (display val) (newline)
  val)

(define (logn name val)
  (display name) (display " : ") (display val) (newline)
  val)


(define (logln . items)
  (define (iter items)
    (if (null? items)
        '()
        (begin
          (display (car items)) (display " ")
          (iter (cdr items)))))
  (iter items)
  (newline))
