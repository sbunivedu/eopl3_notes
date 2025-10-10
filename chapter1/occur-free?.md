# Lambda Calculus Grammar

(section 1.1.2) The _lambda caculus_ is a simple language that is often used to study the theory of programming languages. This language consists of only of variable references, procedures that take a single argument, and procedure calls. We can define it with the grammar:
```
LcExp   ::= Identifier
        ::= (lambda (Identifier) LcExp)
        ::= (LcExp LcExp)
where an identifier is any symbol other than lambda.
```
The identifier in the second production is the name of a variable in the body of the `lambda` expression. This variable is called the _bound variable_ of the expression, because it binds or captures any occurrences of the variable in the body. Any occurrence of that variable in the body refers to this one.

Consider the lambda calculus extended with arithmetic operations. In that language,
```
(lambda (x) (+ x 5))
```
is an expression in which `x` is the bound variable. This expression describes a procedure that adds 5 to its argument. Therefore, in
```
((lambda (x) (+ x 5)) (- x 7))
```
the last occurrence of `x` does not refer to the `x` that is bound in the lambda expression.

(section 1.2.4) The procedure `occurs-free?` should take a variable `var`, represented as a Scheme symbol, and a lambda-calculus express `exp` and determine whether or not `var` occurs free in `exp`. We say that a variable _occurs free_ in an expression `exp` if it has ***some*** occurrence in `exp` that is not inside some _lambda binding_ of the same variable. For example,
```
> (occurs-free? 'x 'x)
#t
> (occurs-free? 'x 'y)
#f
> (occurs-free? 'x '(lambda (x) (x y)))
#f
> (occurs-free? 'x '(lambda (y) (x y)))
#t
> (occurs-free? 'x '((lambda (x) x) (x y)))
#t
> (occurs-free? 'x '(lambda (y) (lambda (z) (x (y z)))))
#t
```

We can solve this problem by following the grammar for lambda-calculus expressions.
```
LcExp   ::= Identifier
        ::= (lambda (Identifier) LcExp)
        ::= (LcExp LcExp)
```

We can summarize these cases in the rules:
* If the expression `e` is a variable, then the variable `x` occurs free in `e` if and only if `x` is the same as `e`.
* If the expression `e` is of the form `(lambda (y) e')`, then the variable `x` occurs free in `e` if and only if `y` is different from `x` __and__ `x` occurs free in `e'`.
* If the expression `e` is of the form `(e1 e2)`, then `x` occurs free in `e` if and only if it occurs free in `e1` or `e2`.

```scheme
(define occurs-free?
  (lambda (var exp)
    (cond
      ((symbol? exp) (eqv? var exp))
      ((eqv? 'lambda (car exp))
       (and
        (not (eqv? var (caadr exp)))
        (occurs-free? var (caddr exp))))
      (else
       (or
        (occurs-free? var (car exp))
        (occurs-free? var (cadr exp)))))))
```

(section 2.3) We can improve `occurs-free?`  by introducing an interface for lambda-calculus expressions. Our interface will have constructors and two kinds of observers: predicates and extractors.

The constructors are:
```
var-exp     :Var → Lc-exp
lambda-exp  :Var × Lc-exp → Lc-exp
app-exp     :Lc-exp × Lc-exp → Lc-exp
```

The predicates are:
```
var-exp?    :Lc-exp → Bool
lambda-exp? :Lc-exp → Bool
app-exp?    :Lc-exp → Bool
```

Finally, the extractors are:
```
var-exp->var          :Lc-exp → Var
lambda-exp->bound-var :Lc-exp → Var
lambda-exp->body      :Lc-exp → Lc-exp
app-exp->rator        :Lc-exp → Lc-exp
app-exp->rand         :Lc-exp → Lc-exp
```

Each of these extracts the corresponding portion of the lambda-calculus expression. We can write a version of `occurs-free?` that depends only on the interface.

```scheme
; occurs-free? : Sym × LcExp → Bool
(define occurs-free?
  (lambda (search-var exp)
    (cond
      ((var-exp? exp) (eqv? search-var (var-exp->var exp)))
      ((lambda-exp? exp)
       (and
        (not (eqv? search-var (lambda-exp->bound-var exp)))
        (occurs-free? search-var (lambda-exp->body exp))))
      (else
       (or
        (occurs-free? search-var (app-exp->rator exp))
        (occurs-free? search-var (app-exp->rand exp)))))))
```
This works on any representation of lambda-calculus expressions, so long as they are built using these constructors.

We can write down a general recipe for designing an interface for a recursive data type:

## Designing an interface for a recursive data type
1. Include one constructor for each kind of data in the data type.
2. Include one predicate for each kind of data in the data type.
3. Include one extractor for each piece of data passed to a constructor of the data type.

We could implement the constructors and the observers as follows:
```scheme
;constructors:
;var-exp     :Var → Lc-exp
;lambda-exp  :Var × Lc-exp → Lc-exp
;app-exp     :Lc-exp × Lc-exp → Lc-exp

(define var-exp
  (lambda (var) var))

(define lambda-exp
  (lambda (bound-var body)
    (list 'lambda bound-var body)))

(define app-exp
  (lambda (exp1 exp2)
    (list exp1 exp2)))

;predicates:
;var-exp?    :Lc-exp → Bool
;lambda-exp? :Lc-exp → Bool
;app-exp?    :Lc-exp → Bool

(define var-exp?
  (lambda (exp)
    (symbol? exp)))

(define lambda-exp?
  (lambda (exp)
    (and
     (pair? exp)
     (eqv? (car exp) 'lambda))))

(define app-exp?
  (lambda (exp)
    (and (pair? exp)
         (pair? (cdr exp))
         (null? (cddr exp)))))


;extractors:
;var-exp->var          :Lc-exp → Var
;lambda-exp->bound-var :Lc-exp → Var
;lambda-exp->body      :Lc-exp → Lc-exp
;app-exp->rator        :Lc-exp → Lc-exp
;app-exp->rand         :Lc-exp → Lc-exp

(define var-exp->var
  (lambda (exp) exp))

(define lambda-exp->bound-var
  (lambda (exp)
    (cadr exp)))

(define lambda-exp->body
  (lambda (exp)
    (caddr exp)))

(define app-exp->rator
  (lambda (exp)
    (car exp)))

(define app-exp->rand
  (lambda (exp)
    (cadr exp)))
```
The improved implementation of `occurs-free?` should pass all the tests:
```
> (occurs-free? (var-exp 'x) (var-exp 'x))
#t
> (occurs-free? (var-exp 'x) (var-exp 'y))
#f
> (occurs-free? (var-exp 'x) (lambda-exp (var-exp 'x) (app-exp 'x 'y)))
#f
> (occurs-free? (var-exp 'x) (lambda-exp (var-exp 'y) (app-exp 'x 'y)))
#t
> (occurs-free? (var-exp 'x) (app-exp (lambda-exp (var-exp 'x) (var-exp 'x)) (app-exp 'x 'y)))
#t
>
(occurs-free? (var-exp 'x) (lambda-exp (var-exp 'y) (lambda-exp 'z (app-exp 'x (app-exp 'y 'z)))))
#t
```
