Here are some recursive programs that process lists, which are
recursively defined as follows:
```
List-of-Symbol ::= ()
               ::= (Symbol . List-of-Symbol)
```

Recursive procedures rely on an important principle:
| The Smaller-Subproblem Principle |
|:--------------------------------:|
|If we reduce a problem to a smaller subproblem, we can call the procedure that solves the problem to solve the subproblem.|

## 1.2.1 list-length
```scheme
#lang eopl

(define list-length
  (lambda (lst)
    (if (null? lst)
        0
        (+ 1 (list-length (cdr lst))))))

; (trace list-length)

; (list-length '(a b c)) => 3
; (list-length '((x) ())) => 2
; (list-length '(a (b c) (d))) => 3
```
## 1.2.2 nth-element
```scheme
#lang eopl

(define nth-element
  (lambda (lst n)
    (if (null? lst)
        (report-list-too-short n)
        (if (zero? n)
            (car lst)
            (nth-element (cdr lst) (- n 1))))))

(define report-list-too-short
  (lambda (n)
    (eopl:error 'nth-element
                "List too short by ~s elements.~%" (+ n 1))))

(trace nth-element)

; (nth-element '(a b c d e) 3) => d
; (nth-element '(a b c d e) 10) => nth-element: List too short by 6 elements.
; (list-ref '(a b c d e) 3) => d
; (list-ref '(a b c d e) 10) => list-ref: index too large for list
```

## 1.2.3 remove-first
```scheme
#lang eopl

(define remove-first
  (lambda (s los)
    (if (null? los)
        '()
        (if (eqv? (car los) s)
            (cdr los)
            (cons (car los) (remove-first s (cdr los)))))))

; (trace remove-first)
; (remove-first 'a '(a b c)) => (b c)
; (remove-first 'b '(e f g)) => (e f g)
; (remove-first 'a4 '(c1 a4 c1 a4)) => (c1 c1 a4)
; (remove-first 'x '()) => ()
```