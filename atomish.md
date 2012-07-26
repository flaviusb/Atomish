Atomish
=======

Levels
------

- Mini
- Core
- Sugar
- Libraries

Types of things
---------------

expressions - anything that produces a value
lines - a line
regions - something introduced by a colon, semicolon, quote or quasiquote.
digressions - a digression is something that introduces an indent. A digression finishes when the corresponding dedent occurs.
turns - a turn is something tha goes from zero indents back to zero indents.

- Patterns
- Destructuring/Generalised Places
- Extractors
- Functions
- Modules - via Structure/Signature/Functor route, with exports a la CommonJS.
- Morphisms - Cata, Ana, Hylo, Apo, Zygo, Histo, Prepro
- Monads, MonadTransformers
- Arrows
- Applicative
- Atoms
- Sentences/Regions
- Expressions
- Closures

Syntax
------

String, Regex, symbol, number (decimal, rational, integer), associative array, set, list/array/seq, Boolean, kv (ie for Dicts and named args), cell

Read Eval 'Print' loop

Reader is a function in the language
Mirrors
Eval is a function in the language

Layout heralds
Equational reasoning/particles/cases/extractors
Sections

\#
--

- Generic reader macro start character


؟, #;
--
- Comment, irony


'
--
- Quoter


''
--

- Quasiquoter


\`
--

- Unquoter


\#'
--

- Funquoter


\`\_
--

- Hole


*
--

- Splat





:
---

- forms a list on the right and applies it to the left - make explicit the elements of a set (done via a list apply monad) - monads in do-notation - Syntactical-descriptive
- describes the left with the right ie types and type like things - Appositive
- forming (or recieving an implicitly formed) a closure/continuation/(ruby style)-block - Segmental

;
---


" ", ' ', “ ”, ‘  ’, « », / /, ( ), { }, [ ], \` \`, etc
---

- Use vs mention distinction



- 
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

--- by itself on a line splits one file up into modules

Example:
--------

Code = Type(name≜String) | Constructor(name≜String, ...) |

LinePart = (Expression | Declaration | Closure)

Expression = LBr Expression RBr | 
Line = (Expression | Declaration | Closure) (Semicolon Line?)?

a≜(A→B) ∘ b≜(B→C) = (a b)≜(A→C)

foo = λx≜Int, w≜String, e≜Foo → 
  
foo = λx∈Int, attr∈String, z∈Xml →
  return x

for i = 1 to 10 do:
  print it;
  let foo = 7 × i in
  for j = it .. foo do:
    print j;
§i

dotimes : {a∈Int|a→(→b)}
dotimes a b = let i = 0
              while i <= a:
                b ()
                i = i + 1
              §dotimes

jumble : {a∈Int, b∈Set Char, c∈String|a→b→c}
jumble len chars = strconcat $ dotimes len $ → pick chars.
