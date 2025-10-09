Our interface will have constructors and two kinds of observers: predicates and extractors.
The constructors are:
```
var-exp : Var → Lc-exp
lambda-exp : Var × Lc-exp → Lc-exp
app-exp : Lc-exp × Lc-exp → Lc-exp
```
The predicates are:
```
var-exp? : Lc-exp → Bool
lambda-exp? : Lc-exp → Bool
app-exp? : Lc-exp → Bool
```
Finally, the extractors are
```
var-exp->var : Lc-exp → Var
lambda-exp->bound-var : Lc-exp → Var
lambda-exp->body : Lc-exp → Lc-exp
app-exp->rator : Lc-exp → Lc-exp
app-exp->rand : Lc-exp → Lc-exp
```
Each of these extracts the corresponding portion of the lambda-calculus expression. We can now write a version of `occurs-free?` that depends only on the interface.
```
occurs-free? : Sym × LcExp → Bool
```
```
(define occurs-free?
    (lambda (search-var exp)
      (cond
        ((var-exp? exp) (eqv? search-var (var-exp->var exp)))
        ((lambda-exp? exp)
         (and (not (eqv? search-var (lambda-exp->bound-var exp)))
              (occurs-free? search-var (lambda-exp->body exp))))
        (else (or (occurs-free? search-var (app-exp->rator exp))
                  (occurs-free? search-var (app-exp->rand exp)))))))

;(occurs-free? 'y (var-exp 'x)) => #f (N/A)
;(occurs-free? 'y (var-exp 'y)) => #t
;(occurs-free? 'y (lambda-exp 'y (var-exp 'x))) => #f
;(occurs-free? 'y (lambda-exp 'x (var-exp 'y))) => #t
;(occurs-free? 'y (lambda-exp 'y (app-exp (lambda-exp 'x (var-exp 'x)) (var-exp 'y)))) => #f
;(occurs-free? 'y (app-exp (var-exp 'x) (var-exp 'y))) => #t
;(occurs-free? 'y (app-exp (lambda-exp 'x (var-exp 'x)) (var-exp 'y))) => #t
```
This works on any representation of lambda-calculus expressions, so long as they are built using these constructors.

Exercise 2.15 [*] Implement the lambda-calculus expression interface for the representation specified by the grammar above.

```scheme
#lang eopl

(define var-exp
  (lambda (var)
    var))

(define lambda-exp
  (lambda (bound-var body)
    (list 'lambda (list bound-var) body)))

(define app-exp
  (lambda (exp1 exp2)
    (list exp1 exp2)))

(define var-exp?
  (lambda (exp)
    (symbol? exp)))

(define lambda-exp?
  (lambda (exp)
    (and (pair? exp)
         (eqv? 'lambda (car exp)))))

(define app-exp?
  (lambda (exp)
    (and (pair? exp)
         (pair? (cdr exp))
         (null? (cddr exp)))))

(define var-exp->var
  (lambda (exp)
    exp))

(define lambda-exp->bound-var
  (lambda (exp)
    (caadr exp)))

(define lambda-exp->body
  (lambda (exp)
    (caddr exp)))

(define app-exp->rator
  (lambda (exp)
    (car exp)))

(define app-exp->rand
  (lambda (exp)
    (cadr exp)))

;(var-exp? (var-exp 'a)) => #t
;(lambda-exp? (lambda-exp 'x 'x)) => #t
;(app-exp? (app-exp (lambda-exp 'x 'x) (var-exp 'a))) => #t
;(app-exp? (app-exp (var-exp 'x) (var-exp 'a))) => #t
;(var-exp->var (var-exp 'a)) => a
;(lambda-exp->body (lambda-exp 'x 'y)) => y
;(app-exp->rator (app-exp (lambda-exp 'x 'x) (var-exp 'a))) => (lambda (x) x)
;(app-exp->rand (app-exp (lambda-exp 'x 'x) (var-exp 'a))) => a
```

Consider again the data type of lambda-calculus expressions, as discussed in the preceding section. We can implement an interface for lambda-calculus expressions by writing
```
(define-datatype lc-exp lc-exp? (var-exp
      (var identifier?))
    (lambda-exp
      (bound-var identifier?)
      (body lc-exp?))
    (app-exp
      (rator lc-exp?)
      (rand lc-exp?)))
```
Here the names `var-exp`, `var`, `bound-var`, `app-exp`, `rator`, and `rand` abbreviate `variable expression`, `variable`, `bound variable`, `application expression`, `operator`, and `operand`, respectively.

This expression declares three constructors, `var-exp`, `lambda-exp`, and `app-exp`, and a single predicate `lc-exp?`. The three constructors check their arguments with the predicates `identifier?` and `lc-exp?` to make sure that the arguments are ***valid***, so if an `lc-exp` is constructed using only these constructors, we can be certain that it and all its subexpressions are legal `lc-exps`. This allows us to ignore many checks while processing lambda expressions.

In place of the various predicates and extractors, we use the form cases to determine the variant to which an object of a data type belongs, and to extract its components. To illustrate this form, we can rewrite `occurs-free?` (page 43) using the data type `lc-exp`:
```
occurs-free? : Sym × LcExp → Bool
```
```scheme
#lang eopl

(define-datatype lc-exp lc-exp?
  (var-exp
   (var identifier?))
  (lambda-exp
   (bound-var identifier?)
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))

(define occurs-free?
  (lambda (search-var exp)
    (cases lc-exp exp
      (var-exp (var) (eqv? var search-var))
      (lambda-exp (bound-var body)
                  (and (not (eqv? search-var bound-var))
                       (occurs-free? search-var body)))
      (app-exp (rator rand)
          (or (occurs-free? search-var rator)
              (occurs-free? search-var rand))))))

;(occurs-free? 'y (var-exp 'x)) => #f (N/A)
;(occurs-free? 'y (var-exp 'y)) => #t
;(occurs-free? 'y (lambda-exp 'y (var-exp 'x))) => #f
;(occurs-free? 'y (lambda-exp 'x (var-exp 'y))) => #t
;(occurs-free? 'y (lambda-exp 'y (app-exp (lambda-exp 'x (var-exp 'x)) (var-exp 'y)))) => #f
;(occurs-free? 'y (app-exp (var-exp 'x) (var-exp 'y))) => #t
;(occurs-free? 'y (app-exp (lambda-exp 'x (var-exp 'x)) (var-exp 'y))) => #t
```

## 2.5 Abstract Syntax and Its Representation
Lambda calculus syntax:
```
Lc-exp ::= Identifier
           var-exp (var)
       ::= (lambda (Identifier) Lc-exp)
           lambda-exp (bound-var body)
       ::= (Lc-exp Lc-exp)
           app-exp (rator rand)
```

### parse and unparse lambda calculus expressions
```scheme
#lang eopl

(define-datatype lc-exp lc-exp?
  (var-exp
   (var identifier?))
  (lambda-exp
   (bound-var identifier?)
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))

(define identifier? symbol?)

(define parse-expression
  (lambda (datum)
    (cond
      ((symbol? datum) (var-exp datum))
      ((pair? datum)
       (if (eqv? (car datum) 'lambda)
           (lambda-exp
            (car (cadr datum))
            (parse-expression (caddr datum)))
           (app-exp
            (parse-expression (car datum))
            (parse-expression (cadr datum)))))
      (else (report-invalid-concrete-syntax datum)))))

(define report-invalid-concrete-syntax
  (lambda (datum)
    (eopl:error 'parse-expression "syntax error ~e" datum)))

(define unparse-lc-exp
  (lambda (exp)
      (cases lc-exp exp
        (var-exp (var) var)
        (lambda-exp (bound-var body)
                    (list 'lambda (list bound-var)
                          (unparse-lc-exp body)))
        (app-exp (rator rand)
                 (list (unparse-lc-exp rator) (unparse-lc-exp rand))))))

; (define p (parse-expression '(lambda (x) (f (f x)))))
; (unparse-lc-exp p) => (lambda (x) (f (f x))
```

Exercise 2.31 [⋆⋆] Sometimes it is useful to specify a concrete syntax as a sequence of symbols and integers, surrounded by parentheses. For example, one might define the set of prefix lists by
```
Prefix-list ::=(Prefix-exp)
Prefix-exp  ::= Int
            ::=- Prefix-exp Prefix-exp
```
so that `(- - 3 2 - 4 - 12 7)` is a legal prefix list. This is sometimes called Polish prefix notation, after its inventor, Jan Łukasiewicz. Write a parser to convert a prefix-list to the abstract syntax
```
(define-datatype prefix-exp prefix-exp?
  (const-exp
   (num integer?))
  (diff-exp
   (operand1 prefix-exp?)
   (operand2 prefix-exp?)))
```
so that the example above produces the same abstract syntax tree as the sequence of constructors
```
(diff-exp
 (diff-exp
  (const-exp 3)
  (const-exp 2))
 (diff-exp
  (const-exp 4)
  (diff-exp
   (const-exp 12)
   (const-exp 7))))
```
As a hint, consider writing a procedure that takes a list and produces a prefix-exp and the list of leftover list elements.

solution
```scheme
#lang eopl

(define-datatype prefix-exp prefix-exp?
  (const-exp
   (num integer?))
  (diff-exp
   (operand1 prefix-exp?)
   (operand2 prefix-exp?)))

(define-datatype parse-result parse-result?
  (a-parse-result
   (exp prefix-exp?)
   (rest list?)))

(define parse-expression
  (lambda (prefix-list)
    (cases parse-result (parse-prefix-exps prefix-list)
      (a-parse-result (exp rest)
                      (if (null? rest)
                          exp
                          (eopl:error 'parse-expression "Unused rest: ~s" rest))))))

(define parse-prefix-exps
  (lambda (prefix-exps)
    (if (pair? prefix-exps)
        (let ((prefix-exp (car prefix-exps))
              (rest (cdr prefix-exps)))
          (cond ((integer? prefix-exp)
                 (a-parse-result (const-exp prefix-exp) rest))
                ((eqv? prefix-exp '-)
                 (cases parse-result (parse-prefix-exps rest)
                   (a-parse-result (exp1 rest1)
                     (cases parse-result (parse-prefix-exps rest1)
                       (a-parse-result (exp2 rest2)
                                       (a-parse-result (diff-exp exp1 exp2) rest2))))))
                (else
                 (eopl:error 'parse-expression
                             "Expected integer or -, got ~s" prefix-exp))))
        (eopl:error 'parse-expression "Not a prefix list: ~s" prefix-exps))))

(trace parse-expression)
#(parse-expression '(- - 3 2 - 4 - 12 7))
```