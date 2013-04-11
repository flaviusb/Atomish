Juxtaposition is sending.
The result of a bracketed expression is a commated.
A 'named commated' is a message send with parameters.
A sent commated is a call.
A name is a message send.
A 'literal' can be a message send, but in general is a name message send - ie '(Object 3) is a message send of the name '1 to the object 'Object.

'Generalised places' (also called 'generalised variables') basically have to be implemented inside setq; in Atomish, the Universe handles setting and getting so updating will key on the message chain sent to it via a table of accesable matchers; this way, the places will be extendable without having to define a protocol as such.
