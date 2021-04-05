(load "lib/debugging.scm")
 
; UNDEFINE
(define (make-unbound! var env)
  (let ((frame (first-frame env)))
		(define (filter-bindings bindings)
			(cond ((null? bindings) '())
						((eq? var (binding-var (first bindings))) (filter-bindings (rest-bindings bindings)))
						(else (cons (first bindings) (filter-bindings (rest-bindings bindings))))))
  
		(if (null? (find-env-binding var env))
				(error "Unbound variable -- MAKE-UNBOUND" var))
	  		(set-cdr! frame
	      		      (filter-bindings (frame-bindings frame)))))

(define (eval-undefine exp env)
  (make-unbound! (undefine-variable exp)  env)
  'ok)

(define (undefine? exp)
  (tagged-list? exp 'undefine))
(define (undefine-variable exp)
  (cadr exp))

((root 'add) undefine? (lambda (exp env) (make-unbound! (undefine-variable exp) env)))
