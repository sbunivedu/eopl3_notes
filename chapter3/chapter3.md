Exercise 3.1 [⋆] In figure 3.3, list all the places where we used the fact that `⌊⌈n⌉⌋ = n`.

* 2nd step when evaluating the inner diff-exp
* 4th step when evaluating a const-exp
* 5th step when evaluating the inner diff-exp
* 8th step when evaluating a const-exp

Exercise 3.2 [⋆⋆] Give an expressed value `val ∈ ExpVal` for which `⌈⌊val⌋⌉ != val`.

For `val = (bool-val #f)`, `(num-val (expval->num val)) != val` because
`expval->num` is undefined for expressed boolean values.

Exercise 3.3 [⋆] Why is subtraction a better choice than addition for our single arithmetic operation?

One can emulate addition via subtraction (but not the other way round): `a + b = a - (-b) = a - (0 - b) = a - ((c - c) - b)`

Exercise 3.4 [⋆] Write out the derivation of figure 3.4 as a derivation tree in the style of the one on page 5.

; (value-of <<x>> rho) = (num-val 33)   (value-of <<11>> rho) = (num-val 11)
; --------------------------------------------------------------------------
;                 (value-of <<-(x,11)>> rho) = (num-val 22)       (value-of <<y>> rho) = (num-val 22)   (value-of <<4>> rho) = (num-val 4)
;             -------------------------------------------------   ------------------------------------------------------------------------
;             (value-of <<zero?(-(x,11))>> rho) = (bool-val #f)                   (value-of <<-(y,4)>> rho) = (num-val 18)
;             ------------------------------------------------------------------------------------------------------------
;                             (value-of <<if zero?(-(x,11)) then -(y,2) else -(y,4)>> rho) = (num-val 18)

Exercise 3.5 [⋆] Write out the derivation of figure 3.5 as a derivation tree in the style of the one on page 5.

Exercise 3.6 [⋆] Extend the language by adding a new operator `minus` that takes one argument, `n`, and returns `−n`. For example, the value of `minus(-(minus(5),9))` should be `14`.



Exercise 3.7 [⋆] Extend the language by adding operators for addition, multiplica- tion, and integer quotient.
