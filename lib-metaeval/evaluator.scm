(load "lib/debugging.scm")

(define scheme-apply (environment-lookup system-global-environment 'apply))

; EVAL
(define (eval exp env)
  ((evaluator 'eval) exp env))

(define (make-evaluator)
  (let ((handlers (list)))
    (define (first-handler handlers) (car handlers))
    (define (rest-handlers handlers) (cdr handlers))
    
    (define (handler-condition handler) (car handler))
    (define (handler-procedure handler) (cadr handler))
      
    (define (handler-match? handler exp)
      ((handler-condition handler) exp))

    (define (can-handle? exp)
      (define (can-handle-iter handlers)
        (cond ((null? handlers) false)
              ((handler-match? (first-handler handlers) exp) true)
              (else (can-handle-iter (rest-handlers handlers)))))
      (can-handle-iter handlers))
    
    
    (define (handle exp env)
      (define (handler-execute handler)
        ((handler-procedure handler) exp env))
      
      (define (handle-iter handlers)
        (cond ((null? handlers) (error "Should not be here!"))
              ((handler-match? (first-handler handlers) exp)
                (handler-execute (first-handler handlers)))
              (else (handle-iter (rest-handlers handlers)))))
      
      (handle-iter handlers))
    
    (define (eval exp env)
      (cond ((self-evaluating? exp) exp)
            ((variable? exp) (lookup-variable-value exp env))
            ((can-handle? exp) (handle exp env))
            ((application? exp)
             (apply (eval (operator exp) env)
                    (list-of-values (operands exp) env)))
            (else 
              (error "Unknown expression type -- EVAL" exp))))

    (define (add predicate procedure)
      (set! handlers (cons (list predicate procedure) handlers)))
    
    (define (dispatch cmd)
      (cond ((eq? cmd 'eval) eval)
            ((eq? cmd 'add) add)))
    
    dispatch))

; APPLY
(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
          (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
          (eval-sequence
            (procedure-body procedure)
            (extend-environment 
              (map cons (procedure-parameters procedure) arguments)
              (procedure-environment procedure))))
        (else
           (error "Unknown procedure type -- APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

; SELF-EVALUATING
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        ((boolean? exp) true)
        (else false)))

; VARIABLE
(define (variable? exp) (symbol? exp))

; QUOTED
(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp env) (cadr exp))

; ASSIGNMENT
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

; DEFINE
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; formal parameters
                   (cddr exp)))) ; body

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


; LAMBDA
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

; IF
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

; BEGIN
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq)
  (cons 'begin seq))

; APPLICATION
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

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


; AND
(define (and? exp)
  (tagged-list? exp 'and))
(define (eval-and exp env)
  (define (and-expressions exp) (cdr exp))
  
  (define (iter expressions)
    (cond ((null? expressions) true)
          ((not (eval (first-exp expressions) env)) false)
          (else (iter (rest-exps expressions)))))
  
  (iter (and-expressions exp)))

; OR
(define (or? exp)
  (tagged-list? exp 'or))
(define (eval-or exp env)
  (define (or-expressions exp) (cdr exp))
  
  (define (iter expressions)
    (cond ((null? expressions) false)
          ((eval (first-exp expressions) env) true)
          (else (iter (rest-exps expressions)))))
  
  (iter (or-expressions exp)))

; LET
(define (let? exp) (tagged-list? exp 'let))

(define (let-binding-name binding) (car binding))
(define (let-binding-value binding) (cadr binding))

(define (common-let? exp) (list? (cadr exp)))
(define (common-let->combination exp)
  (define let-bindings (cadr exp))
  (define let-expressions (cddr exp))

  (cons 
    (make-lambda (map let-binding-name let-bindings)
                 let-expressions)
    (map let-binding-value let-bindings)))

(define (named-let? exp) (and (symbol? (cadr exp)) (list? (caddr exp))))
(define (named-let->combination exp)
  (define let-bindings (caddr exp))
  (define let-expressions (cdddr exp))
  (define let-name (cadr exp))
  
  (define proc (cons 'define 
                 (cons (cons let-name (map let-binding-name let-bindings))
                       let-expressions)))
  (define proc-call (cons let-name (map let-binding-value let-bindings)))
  
  (list (make-lambda ()
                     (list proc proc-call))))

(define (let->combination exp)
  (cond ((common-let? exp) (common-let->combination exp))
        ((named-let? exp) (named-let->combination exp))
        (else (error "Unknown LET format"))))

; LET*
(define (let*? exp) (tagged-list? exp 'let*))
(define (let*-bindings exp) (cadr exp))
(define (let*-binding-name binding) (car binding))
(define (let*-binding-value binding) (cadr binding))
(define (let*-expressions exp) (cddr exp))
(define (make-let bindings expressions)
  (list 'let bindings expressions))

(define (let*->nested-lets exp)
  (define (iter bindings)
    (cond ((null? bindings) (sequence->exp (let*-expressions exp)))
          (else (make-let (list (car bindings)) (iter (cdr bindings))))))
  
  (iter (let*-bindings exp)))


; PRIMITIVES
(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))

(define (apply-primitive-procedure proc args)
  (scheme-apply proc args))

(define (primitive-procedure? proc)
  (procedure? proc))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? exp)
  (tagged-list? exp 'procedure))
(define (procedure-parameters exp) (cadr exp))
(define (procedure-body exp) (caddr exp))
(define (procedure-environment exp) (cadddr exp))


; ENVIRONMENTS
(define the-empty-environment '())
(define (root-env? env)
  (eq? (enclosing-environment env) the-empty-environment))

(define (extend-environment bindings base-env)
  (cons (make-frame bindings) base-env))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))

(define (make-frame bindings)
  (cons 'frame bindings))
(define (frame-bindings frame)
  (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-cdr! frame
            (append (frame-bindings frame) (list (make-binding var val)))))

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


(define (make-binding var val)
  (cons var val))

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

(define (binding-var binding)
  (car binding))

(define (binding-value binding)
  (cdr binding))

(define (set-binding-value! binding val)
  (set-cdr! binding val))

(define (rest-bindings bindings)
  (cdr bindings))

; INIT ENVIRONMENT
(define (setup-environment)
  (let ((initial-env
          (extend-environment primitive-procedures
                              the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define evaluator (make-evaluator))
((evaluator 'add) quoted? text-of-quotation)
((evaluator 'add) assignment? eval-assignment)
((evaluator 'add) definition? eval-definition)
((evaluator 'add) undefine? (lambda (exp env) (make-unbound! (undefine-variable exp) env)))
((evaluator 'add) lambda? (lambda (exp env) (make-procedure (lambda-parameters exp) (lambda-body exp) env)))
((evaluator 'add) if? eval-if)
((evaluator 'add) begin? (lambda (exp env) (eval-sequence (begin-actions exp) env)))
((evaluator 'add) cond? (lambda (exp env) (eval (cond->if exp) env)))
((evaluator 'add) and? eval-and)
((evaluator 'add) or? eval-or)
((evaluator 'add) let? (lambda (exp env) (eval (let->combination exp) env)))
((evaluator 'add) let*? (lambda (exp env) (eval (let*->nested-lets exp) env)))


(define primitive-procedures
       (list (cons 'car car)
             (cons 'cdr cdr)
             (cons 'cons cons)
             (cons 'null? null?)
             (cons '+ +)
             (cons '- -)
             (cons '* *)
             (cons '/ /)
             (cons '= =)
             (cons '> >)
             (cons '< <)
             (cons 'display display)
             ))



(define the-global-environment (setup-environment))

; RUN AS A PROGRAM
(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))
(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))
