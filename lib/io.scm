; io

(define (print . items)
  (define (iter items)
    (if (null? items)
        '()
        (begin
          (display (car items))
          (iter (cdr items)))))
  (iter items))

(define (println . items)
  (apply print items)
  (newline))