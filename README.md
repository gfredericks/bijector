# Bijector

Bijector is a Clojure library for defining and converting between datatypes
in a bijective manner (meaning each element in one type has a corresponding
element in the other type and vice versa). This is done using a technique
I call efficient enumerations, which has the following characteristics:

* I have so far been unable to rigorously define it.
* I have never successfully convinced anyone that it is not rubbish.
* At the moment I do not have the energy to say anything else about it.

At the moment the library is not __too__ user-friendly, and might never be
if nobody is interested in it.

## Examples

Just to be arbitrary, let's try converting between hex strings and triples of integers:

```clojure
user=> (use 'bijector.core)
nil
user=> (def HEX-STRINGS (strings-with-chars "0123456789abcdef"))
#'user/HEX-STRINGS
user=> (def INT-TRIPLES (tuples-of 3 INTEGERS))
#'user/INT-TRIPLES
user=> (def-converters to-triples to-hex INT-TRIPLES HEX-STRINGS)
#'user/to-hex
user=> (to-hex [77 77 77])
"c2268cf0"
user=> (to-triples *1)
(77 77 77)
user=> (to-triples "10ff9303b5478713931b6fb4221fa3007c7da728")
(-566312798866180140005020149758 421504070 -109516)
user=> (to-hex *1)
"10ff9303b5478713931b6fb4221fa3007c7da728"
```

## API

### Built-in Types

* `BOOLEANS`: finite type containing `true` and `false`
* `NATURALS`: the numbers 1,2,3,...
* `INTEGERS`: the numbers ...,-3,-2,-1,0,1,2,3,...
* `NATURAL-LISTS`: lists of `NATURALS`, e.g. `[]`, `[1,3929]`, and `[2,2,2,2,2,2]`
* `NATURAL-SETS`: sets of `NATURALS`, e.g. `#{}`, `#{55 3824}`, and `#{9 99 999 9999}`
* `NATURAL-BAGS`: bags (i.e., multisets) of `NATURALS`, e.g. `[]`, `[55 3824]`, and `[9 9 9 9999]`
* `SIMPLE-ASCII`: strings containing any of the "normal" ASCII characters, e.g. `"thomas"`, `"\n\r~!@#"`, and `""`
* `NESTED-NATURAL-LISTS`: nested lists of `NATURALS`, e.g. `[]`, `[1,2,[],3]`, and `[[4 [2] [] 5]]`

### Type-Constructors

Built in families of types.

* `integer-range-type`: creates a finite type consisting of a range of integers
* `natural-tuples-type`: like `NATURAL-LISTS`, but of a fixed length
* `strings-with-chars`: given a collection of characters, returns a type of all strings composed of those characters

### Type-Combinators

Functions that take one or more types as arguments and return a new type.

* `lists-of`: given a type `t`, creates a new type consisting of lists of elements of `t`
* `sets-of`: given a type `t`, creates a new type consisting of sets of elements of `t`
* `cartesian-product-type`: given `N` types, creates a new type consisting of lists of length `N`
  where the first item in the list is an element of the first type, etc.
* `union-type`: if you've read this far you probably know what this does
* `tuples-of`: like `lists-of` but with a given fixed length
* `pairs-of`: convenience method that calls `tuples-of` with an argument of `2`
* `without`: given a type and some of its elements, returns a new type without those elements
* `with`: given a type and some elements not in the type, returns a new type like the old type
  but including the new elements

## Open Questions

* What do I mean by all this?
* If an e.e. set A has a subset B that is also e.e., does that
  imply that A/B is e.e.?
  E.g., is the set of all ASCII strings that are _not_ valid JSON
  e.e.?
* What about type intersections?

### Ideas

* e.e. := efficiently enumerable
* e.e.e. := elegantly efficiently enumerable -- at this level we would try to distinguish
  from the trivial injection-based bijections.
* A test for e.e. that I just thought of -- we could look at the limiting density of a given
  property as the length of representation increases. E.g., for the nested-lists, examine the
  average order of the nested lists.
* Enumerating elements of a regular language: could we use [this algorithm](http://cstheory.stackexchange.com/questions/8200/counting-words-accepted-by-a-regular-grammar) to do it?