**Matzo** was my first attempt at a language designed for creating random text fragments, now reimplemented in Rust.

(The earlier versions were Haskell and JavaScript, and both have bit-rotted rather badly: I'm hoping that this version is both cleaner—on account of my writing it a decade later—and also that this version is easier to modify and will be more resilient to bit-rot.)

## The Matzo language

### Basic features

There are two basic kinds of statements: assignment and printing. Assignment is usually done with `:=`. All statements are terminated with a semicolon (although the REPL will usually tolerate you leaving it off.)

```
greeting := "hello";
```

Printing statements are written with `puts`.

```
puts greeting; (* prints "hello" *)
```

The two most common operations are catenation and choice. Catenation is indicated by simply putting expressions next to each other:

```
puts "Hello" " " "world!";
```

Choice is indicated by putting vertical bars between various choices.

```
puts "Hello!" | "Hey!" | "Yo!";
```

Catenation binds more tightly than choice, but parentheses can explicitly group operations.

```
puts ("Bonjour" | "Buenos dias") ", world!";
```

Choices can also be weighted by using a number and `:`. In the following example, there's a 1/6 chance of getting `"tails"`, and a 5/6 chance of getting `"heads"`:

```
puts 5: "heads" | "tails";
```

Since it's quite common to want to choose from some basic string literals, there's a special case for that: the `::=` operator allows you to list zero or more space-separated identifiers, and then it will treat them as strings and choose between them with equal weight.

```
suits ::= hearts diamonds clubs spades;
(* this is equivalent to writing
   suits := "hearts" | "diamonds" | "clubs" | "spades";
*)
```

Importantly, definitions in Matzo are _lazy_: if something is defined as a choice between alternatives, then every use of that thing has a chance of choosing a different alternative.

```
letter ::= a b;
(* this might print any of aa, ab, ba, bb *)
puts letter letter;
```

Matzo also allows you to _fix_ definitions, which will evaluate them once and use that version for every subsequent reference. There are two ways of doing this: one is to prefix the assignment with the `fix` keyword:

```
fix letter ::= a b;
(* this can only print aa or bb *)
puts letter letter;
```

The other is to use `fix` as a standalone statement, which modifies a definition in-scope by fixing it.

```
letter ::= a b;
puts letter letter; (* aa, ab, ba, or bb *)
fix letter;
puts letter letter; (* aa or bb *)
```

### Data types

Matzo offers a handful of other data types in addition to strings. Matzo is dynamically typed, and additionally all of these types coerce implicitly to strings when they're printed.

- _Numbers_ are written as you'd expect. Matzo only offers integer numbers: there are no floating point numbers in Matzo. Additionally, many of the usual integer operations are missing, or are only offered via named functions in the stdlib. There's also a special case for generating numbers from a range: a range literal like `2..4` represents not an actual range but a choice from inside an inclusive range: that means that an expression like `2..4` is equivalent to writing `2 | 3 | 4`.
- _Atoms_ are simple named types, and are represented with capital letters. Atoms are used for things like boolean (e.g. `True` and `False`) but can be used for any kind of tag. It's common to use fixed atoms for certain kinds of categories and then later on branch on them to come up with specific strings: for example, defining a kind of pet as `fixed pet := Cat | Dog | Rabbit;` and then later on casing on `pet` to build parts of a description.
- _Tuples_ are sequences of values, and are written within angle brackets, e.g. `<2, Foo>` is a 2-tuple containing the number `2` and the atom `Foo`. Tuples are especially useful with functions and pattern-matching.

### Functions

Applying functions is done using an explicit operator: the `.` operator. That is to say, to apply the `to-upper` function to the string `"foo"`, you'd write `to-upper."foo"`.

Functions can only be applied to one value at a time. This means that functions can sometimes take tuples of arguments (e.g. `rep.<3, "na">`) but functions can also be curried. Most stdlib functions take the tuple approach.

All functions are defined as anonymous functions and can optionally feature definition-by-cases. A simple function which just returns its argument looks like this:

```
id := { x => x };
```

In order to define a function by cases, you can separate individual cases with `;`.

```
if := { <True,  x, _> => x
      ; <False, _, y> => y
      };
```

You can also return other functions, which retain access to their environment, which means the above function be expressed isomorphically as:

```
if := { True  => { x => { _ => x }}
      ; False => { _ => { y => y }}
      };
```

While these two express the same functionality, the former would be called with `if.<condition, thenCase, elseCase>` while the latter would be called with `if.condition.then-case.else-case`.

### A sample program

The intention of Matzo is to be used for generating text, and was originally created for the task of creating fake words for constructed languages. Here is an example program which creates words in a simple language:

```
consonant ::= p t k w h n;
vowel ::= a e i o u;
nucleus := 4: vowel | vowel "'";
syll := 4: consonant nucleus | nucleus;
puts syll rep.<1..5, syll>;
```

## Implementation notes

## Todo

- [ ] Expand the stdlib
- [ ] Think about how to express GC
