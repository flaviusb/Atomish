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
turns - a turn is something that goes from zero indents back to zero indents and the end of a line. Readers read in a turn at a time.

- Patterns
- Destructuring/Generalised Places
- Extractors
- Functions
- Modules - via Structure/Signature/Functor route, with exports a la CommonJS.
- Morphisms - Cata, Ana, Hylo, Apo, Zygo, Histo, Prepro
- Monads, MonadTransformers
- Arrows
- Applicative
- Sentences/Regions
- Expressions
- Closures
- MOP, MMOP

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

\#!
--
- Bangline


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
- Splat, bezirt transform, blask


λ 
--
- Alias to fn


× ÷ + - ^
--
- Arithmetic operators


:
---
- Forms a list on the right and applies it to the left - make explicit the elements of a set (done via a list apply monad) - monads in do-notation - Syntactical-descriptive
- Describes the left with the right ie types and type like things - Appositive
- Forming (or recieving an implicitly formed) a closure/continuation/(ruby style)-block - Segmental


;
---
- Starts a subclause (whatever that means...)


.
---
- Finish a region (or a line?)


" ", “ ”, ‘  ’, « », / /, ( ), { }, [ ], \` \`, &lt; &gt; etc
---

- MMOPery - ie for something like
  qw&lt;foo bar baz&gt; each(import)
- Use vs mention distinction
- The left op is the 'indent', the right op is the 'dedent'. In a whitespace sensitive reader, actual indents and dedents would also count on this list. Relevant to code scoping, but with MMOPery also relevant to (for example) a YAML literal reader.



- 
, is a comma splice
… current line continues on next line.
§ + a name closes that section
∘ for function application
Datalang, query lang, qq lang ...
\_ as lots of stuff


Example:
--------

let page = #'(html
               (head title \`\_)
               (body
                 div "a b c d e"
                 div "c d e f g"
                 (div \`\_)))

Byeloblog render(page("test", "rest")) println
