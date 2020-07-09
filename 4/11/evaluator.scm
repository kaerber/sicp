(load "lib/debugging.scm")
(load "lib-metaeval/evaluator.scm")

; ENVIRONMENTS
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame bindings)
  bindings)
(define (add-binding-to-frame! var val frame)
  (cons (cons var val) frame))

(define (extend-environment bindings base-env)
  (cons (make-frame bindings) base-env))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan bindings)
      (cond ((null? bindings)
             (env-loop (enclosing-environment env)))
            ((eq? var (caar bindings))
             (cdar bindings))
            (else (scan (cdr bindings)))))
    
    (if (root-env? env)
        (error "Unbound variable -- LOOKUP" var)
        (let ((frame (first-frame env)))
          (scan frame))))
  
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan bindings)
      (cond ((null? bindings)
             (env-loop (enclosing-environment env)))
            ((eq? var (caar bindings))
             (set-cdr! (car bindings) val))
            (else (scan (cdr bindings)))))
    
    (if (root-env? env)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan frame))))
  
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan bindings)
      (cond ((null? bindings) (add-binding-to-frame! var val frame))
            ((eq? var (caar bindings)) (set-cdr! (car bindings) val))
            (else (scan (cdr bindings)))))
    
    (scan (frame-variables frame)
          (frame-values frame))))

(define (setup-environment)
  (let ((initial-env
          (extend-environment primitive-procedures
                              the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))
