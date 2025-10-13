The procedure `subst` should take three parameters: two symbols, `new` and `old`, and an s-list, `slist`.
All elements of slist are examined, and a new list is returned that is similar to slist but with all occurances of `old` replaced by instances of `new`.
```
> (subst 'a 'b '((b c) (b () d)))
((a c) (a () d))
```

Since `subst` is defined over s-lists, its organization should reflect the definition of s-lists
```
S-list ::= ()
       ::= (S-exp . S-list)
S-exp  ::= Symbol | S-list
```

The grammar has two nonterminals, S-list and S-exp, so we will have two procedures, one for dealing with S-list and one for dealing with S-exp.

```scheme
#lang eopl

(define subst
  (lambda (new old slist)
    (if (null? slist)
        '()
        (cons
         (subst-in-s-exp new old (car slist))
         (subst new old (cdr slist))))))

(define subst-in-s-exp
  (lambda (new old sexp)
    (if (symbol? sexp)
        (if (eqv? sexp old) new sexp)
        (subst new old sexp))))

; (trace subst)
; (trace subst-in-s-exp)
; (subst 'a 'b '((b c) (b () d))) => ((a c) (a () d))
```

Since we have strictly followed the definition of S-list and S-exp, this recursion is guaranteed to halt. Since `subst` and `subst-in-s-exp` call each other recursively, we say they are mutually recursive.

The decomposition of `subst` into two procedures, one for each syntactic category, is an important technique. It allows us to think about one syntactic category at a time, which greatly simplifies our thinking about more complicated programs.