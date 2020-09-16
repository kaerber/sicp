(load "lib/debugging.scm")
(load "lib-metaeval/evaluator.scm")

(define (add-binding-to-frame! var val frame)
  (append frame (list (make-binding var val))))

(define (lookup-variable-value var env)
  (let ((binding (find-env-binding var env)))
    (if (null? binding)
        (error "Unbound variable -- LOOKUP" var)
        (binding-value binding))))

(define (set-variable-value! var val env)
  (let ((binding (find-env-binding var env)))
    (if (null? binding)
        (error "Unbound variable -- SET!" var)
        (set-binding-value! binding val))))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (let ((binding (find-frame-binding var frame)))
      (if (null? binding)
          (add-binding-to-frame! var val frame)
          (set-binding-value! binding val)))))


(define (find-env-binding var env)
  (let ((binding (find-frame-binding var (first-frame env))))
    (cond ((not (null? binding)) binding)
          ((root-env? env) '())
          (else (find-env-binding var (enclosing-environment env))))))

(define (find-frame-binding var frame)
  (define (scan-bindings bindings)
    (cond ((null? bindings) '())
          ((eq? var (binding-var (first bindings))) (first bindings))
          (else (scan-bindings (rest-bindings bindings)))))
  (scan-bindings (frame-bindings frame)))

(define (make-binding var val)
  (cons var val))

(define (binding-var binding)
  (car binding))

(define (binding-value binding)
  (cdr binding))

(define (set-binding-value! binding val)
  (set-cdr! binding val))

(define (rest-bindings bindings)
  (cdr bindings))


