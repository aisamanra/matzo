**Matzo** is my first attempt at a language designed for creating random text fragments, now reimplemented in Rust.

(The earlier versions were Haskell and JavaScript, and both have bit-rotted rather badly: I'm hoping that this version is both cleaner—on account of my writing it a decade later—and also that this version is easier to modify and will be more resilient to bit-rot.)

## Matzo basics

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

## Fancier Matzo

- Data types (atoms, tuples)
- Functions
- Pattern-matching

## Implementation notes

## Todo

- [ ] Pattern matching
- [ ] Closures
- [ ] Expand the stdlib
- [ ] Switch to packed expr representation for easier sharing
- [ ] Think about how to express GC
