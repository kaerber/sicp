; io

(define (println . items)
  (define (iter items)
    (if (null? items)
        '()
        (begin
          (display (car items))
          (iter (cdr items)))))
  (iter items)
  (newline))
