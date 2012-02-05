Atomish
=======

Levels
------

- Mini
- Core
- Sugar
- Libraries

Syntax
------

. closes a level of indentation
: starts a dependant subclause - a closure?
; starts a subclause
, is a comma splice
! ¡
… current line continues on next line.
§ + a name closes that section
× ÷ + - ^ for arithmentic
∘ for function application
\# for generalised selectors
' as quote, \` as quasiquote, ~ as unquote
Datalang, query lang, qq lang ...
_ as lots of stuff
\* as splat, bezirt transform, blask, or footnote
[] for set builder notation, quasiquoters, lists, arrays, associative array indexing/places?
/ for options, disjunction types, disjunction points?
→
⇒
←
⇐
≜
λ as lambda
((expr | variable) ‖ Type) to constrain an expression or variable to a type?

All words beginning with initial caps are Types or Constructors (of either Sum or Record types). Sum types put their constituent constructors and extractors into the current scope. 

Example:
--------

Code = Type(name≜String) | Constructor(name≜String, ...) |

LinePart = (Expression | Declaration | Closure)

Expression = LBr Expression RBr | 
Line = (Expression | Declaration | Closure) (Semicolon Line?)?

a≜(A→B) ∘ b≜(B→C) = (a b)≜(A→C)

foo = λx≜Int, w≜String, e≜Foo → 

for i = 1 to 10 do:
  print it;
  let foo = 7 × i in
  for j = it .. foo do:
    print j;
§i

