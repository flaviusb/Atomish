Atomish
=======

Levels
------

- Mini
- Core
- Sugar - via modules with readers and MMOP functions
- Libraries

Types of things
---------------

expressions - anything that produces a value
lines - a line
regions - something introduced by a colon, semicolon, quote or quasiquote.
digressions - a digression is something that introduces an indent. A digression finishes when the corresponding dedent occurs.
turns - a turn is something that goes from zero indents back to zero indents and the end of a line. Readers read in a turn at a time.

- Cells
- Message sends
- MOP, MMOP
- Generalised Places 
- Destructuring
- Modules - via Structure/Signature/Functor route, with exports a la CommonJS.
- Macros, reader macros, lecros
- Quoting, quasiquoting (Lisp/Ioke style, Haskell style, and MetaLua style), unquoting, funquoting
- Patterns
- Extractors
- Closures
- Morphisms - Cata, Ana, Hylo, Apo, Zygo, Histo, Prepro
- Monads, MonadTransformers
- Arrows

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

- Generic reader macro start character. Not meaningful by itself.


؟, ##
--
- Comment, irony

\#!
--
- Bangline

!
--
- Call a function with no arguments

‼, !!
--
- Fixpoint call; keep calling until the result is not a nullary function
- Fixpoint call with args - keep calling until the result is not a function that takes those args

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


\*
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
- Describes the left with the right ie key-value relationships (the value describes the key), for example in Dicts or Arglists - Appositive
- Forming (or recieving implicitly formed) a closure/continuation/(ruby style)-block - Segmental


;
---
- Starts a subclause (whatever that means...)


.
---
- Finish a region (or a line?)


" ", « », / /, { }, [ ], &lt; &gt;
---

- MMOPery - ie for something like
  qw&lt;foo bar baz&gt; each(import)
- Use vs mention distinction
- The left op is the 'indent', the right op is the 'dedent'. In a whitespace sensitive reader, actual indents and dedents would also count on this list. Relevant to code scoping, but with MMOPery also relevant to (for example) a YAML literal reader.
- The types of quote are named by symbols: sw&lt;quotes guillemet slash curly square angle any&gt;
- Standard MMOPs:
    - cell :"" for quote type :quotes - string constructor with interpolation
    - cell :"" for quote type :slash - regexp constructor without interpolation
    - cell :"" for quote type :curly - dictionary constructor
    - cell :"" for quote type :square - list constructor
    - cell :"#r" for quote type :square - regexp constructor with interpolation
    - cell :"#" for quote type :square - string constructor with interpolation
    - cell :"qw" for any quote type :angle - take string, create array by splitting on spaces, no escapes or interpolation
    - cell :"sw" for any quote type :angle - take string, create array by splitting on spaces and converting to symbols, no escapes or interpolation
    - cell :"#" for quote type :curly - set constructor
    - cell :"mf" for quote type :guillemet - math style function - single letter variables, juxtaposition = multiplication. Results in a function with named args for each of the variables used.
    - cell :"me" for quote type :guillemet - math style expression - single letter variables, juxtaposition = multiplication. Results in an expression.

_
---
- Lots of stuff. Anonymous functions, empty matches... Also, other things. The evil reflection of \*.

,
---
- A comma splice. No, really.
- The separator between arguments in an argument list, the items in a set, list or dictionary, and so forth.

…
---
- Ignore an immediately following newline. In effect, the current line continues on the next line.


---

§ + a name closes that section
∘ for function application
Datalang, query lang, qq lang ...


Example:
--------

        let(page, #'(html
                      (head title `_)
                      (body
                        (section
                          h1 `_
                          span(class: `_) `_
                          div " I'm all in "
                          (div `_)))),
            stuff, qw<Hello.atomish Hello fragment World!. bits.>,
            Byeloblog render(page(*stuff)) println)
