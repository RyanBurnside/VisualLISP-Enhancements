;;;; SICP style eval to add additional syntax
;;;; All credit for original work goes to the authors of Structure and Interpretation of Computer Programs

;;; Notes

;; ---Primitive expressions---
;; 1. For self -evaluating expressions, such as numbers, eval returns the expression itself
;; 2. Eval must look up variables in the environment fo find their values

;; ---Special Forms---
;; 1. For quoted expressions, eval returns the expression that was quoted
;; 2. An assignment to (or a definition of) a variable must recursively call eval to compute
;;    the new value to be associated with the variable. Then environment must be modified to
;;    change (or create) the binding of the variable.
;; 3. An IF expression requires special processing of its parts, so as to evaluate the
;;    consequent if the predicate is true, and otherwise to evaluate the alternative.
;; 4. A LAMBDA expression must be transformed into an applicable procedure by packaging
;;    together the parameters and body specified by the lambda expression with the environment
;;    of the evaluation.
;; 5. A PROGN expression requires evaluating its sequence of expressions in the order in
;;    which they appear.
;; 6. A case analysis (COND) is transformed into a nest of if expressions and then evaluated.

;; ---Combinations---
;; 1. For a procedure application, EVAL must recursively evaluate the operator part and the
;;    operands of the combination. The resulting procedure and arguments are passed to
;;    APPLY, which handles the actual procedure application.


(defun eval (exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assigment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))


(defun apply (procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compount-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
         (error
           "Unknown procedure type -- APPLY" procedure))))


(defun list-of-values (exps env)
  (if (no-operands? exps)
    '()
    (cons (eval (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))


(defun eval-if (exp env)
  (if (true? (eval (if-predicate-exp) env))
    (eval (if-consequent exp) env)
    (eval (if-alternative exp) env)))


(defun eval-sequence (exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) env))))


(defun eval-assignment (exp env)
  (set-variable-value! (assignment-variable exp)
    (eval (assignment-value exp) env)
    env)
  'ok)


(defun eval-definition (exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

;;; Predicates (need to adjust for MacLisp style t/nil and p suffix)

;; Only self-evaluating items are numbers and strings
(defun self-evaluating? (exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))


;; Variables are represented by symbols
(defun variable? (exp)
  (symbol? exp))


;; Quotations have the form (quote <text-of-quotation>)
(defun quoted? (exp)
  (tagged-list? exp 'quote))


(defun text-of-quotation (exp)
        (cadr exp))


(defun tagged-list? (exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    false))

;; Assignments have the form (set! <var> <value>)
(defun assignment? (exp)
  (tagged-list? exp 'set!))


(defun assignment-variable (exp)
  (cadr exp))


(defun assignment-value (exp)
  (caddr exp))

;; Definitions have the form (define <var> value)
;; or the form (define (<var> <parameter> ...) <body>)

(defun definition? (exp)
  (tagged-list? exp 'define))


(defun definition-variable (exp)
  (if (symbol? (cadr exp))
    (cadr exp)
    (caadr exp)))


(defun definition-value (exp)
  (if (symbol? (cadr exp))
    (caddr exp)
    (make-lambda (cdadr exp) ; formal parameters
      (cddr exp)))) ; body


(defun lambda? (exp)
  (tagged-list? exp 'lambda))


(defun lambda-parameters (exp)
  (cadr exp))


(defun lambda-body (exp)
  (cddr exp))


(defun make-lambda (paramters body)
  (cons 'lambda (cons parameters body)))

;; Conditionals begin with if and have a preedicate a consequent and an optional alternative

(defun if? (exp)
  (tagged-list? exp 'if))


(defun if-predicate? (exp)
  (cadr exp))


(defun if-consequent (exp)
  (caddr exp))


(defun if-alternative (exp)
  (if (not (null? (cdddr exp)))
    (cadddr exp)
    'false))

;; Constructor for if expressions to be used by cond->if

(defun make-if (predicate consequent alternative)
  (list 'if predicate consequent alternative))


(defun begin? (exp)
  (tagged-list? exp 'begin))


(defun begin-actions (exp)
  (cdr exp))


(defun last-exp? (seq)
  (null? (cdr seq)))


(defun first-exp (seq)
  (car seq))


(defun rest-exps (seq)
  (cdr seq))


;; turns a sequence into begin
(defun sequence->exp (seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else
         (make-begin seq))))


(defun make-begin (seq)
  (cons 'begin seq))


(defun application? (exp)
  (pair? exp))


(defun operator (exp)
  (car exp))


(defun operands (exp)
  (cdr exp))


(defun no-operands? (ops)
  (null ops))


(defun first-operand (ops)
  (car ops))


(defun rest-operands (ops)
  (cdr ops))


;; Building the if expander for cond
(defun cond? (exp)
  (tagged-list? exp 'cond))


(defun cond-clauses (exp)
  (cdr exp))


(defun cond-else-clause? (clause)
  (eq? (cond-predicate clause) 'else))


(defun cond-predicate (clause)
  (car clause))


(defun cond-actions (clause)
  (cdr clause))


(defun cond->if (exp)
  (expand-clauses (cond-clauses exp)))


(defun expand-clauses (clauses)
  (if (null? clauses)
    'false
    (let ((first (car clauses))
          (rest (cdr clauses)))
      (if (cond-else-clause? first)
        (sequence->exp (cond-actions first))
        (if (null? rest)
          (error "ELSE clause isn't last -- COND->IF"
                 clauses))
        (make-if (cond-predicate first)
                 (dequence->exp (cond-actions first))
                 (expand-clauses rest))))))

;;; truth and falsehood

(defun true? (x)
  (not (eq? x false)))

(defun false? (x)
  (eq? x false))


;;; Representing procedures

(defun make-procedure (parameters body env)
  (list 'procedure parameters body env))

(defun compound-procedure? (p)
  (tagged-list? p 'procedure))

(defun procedure-parameters (p)
  (cadr p))

(defun procedure-body (p)
  (caddr p))

(defun procedure-environment (p)
  (cadddr p))


;;; Operations on Environments
;; A list of frames containing tables wrapped in a parent environment

(defun (enclosing-environment env)
       (cdr env))

(defun first-frame (env)
  (car env))

(defun the-empty-environment '())

;; Each frame of an environment is a pair of lists. A lost of variables bound in that frame and a list of the associated values

(defun make-frame (variables values)
  (cons variables values))

(defun frame-variables (frame)
  (car frame))

(defun frame-values (frame)
  (cdr frame))

(defun add-binding-to-frame! (var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(defun extend-environment (vars vals base-env)
  (if (= (length vars) (length vals))
    (cons (make-frame vars vals) base-env)
    (if (< (length vars) (length vals))
      (error "Too many arguments supplied" vars vals)
      (effor "Too few arguments suplied" vars vals))))



