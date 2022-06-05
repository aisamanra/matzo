**Matzo** is a little dynamically typed language intended for creating fragments of random text. Matzo programs nondeterministically produce strings as output.

```
consonant ::= p t k w h n;
vowel := "a" | "e" | "i" | "o" | "u";
nucleus := 4: vowel | vowel "'";
syll := 4: consonant nucleus | nucleus;
puts rep[2..5, syll];
```

Matzo is still very immature, and it's likely that bugs are lurking very prominently. Please [feel free to report them](https://github.com/aisamanra/matzo/issues) if you come across any problems!

## Building and running Matzo

Matzo is implemented in Rust, so if you've got a Rust toolchain installed, you should be able to install it with

```
cargo install --git https://github.com/aisamanra/matzo.git
```

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

The other way is to use `fix` as a standalone statement, which modifies a definition in-scope by fixing it.

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

Applying functions is done using parameter lists contained in square brackets. That is to say, to apply the `to-upper` function to the string `"foo"`, you'd write `to-upper["foo"]`.

All functions are defined as anonymous functions and can optionally feature definition-by-cases. A simple function which just returns its argument looks like this:

```
id := { [x] => x };
```

In order to define a function by cases, you can separate individual cases with `;`.

```
if := { [True,  x, _] => x
      ; [False, _, y] => y
      };
```

It is a runtime error if no case matches.

```
>>> {[A] => 1; [B] => 2}[C]
error: No pattern matched C
  1 |{[A] => 1; [B] => 2}[C]
     ^^^^^^^^^^^^^^^^^^^^
```

Matzo does not permit nullary (i.e. zero-argument) functions.

### Provided functions

Matzo has a handful of functions which help writing certain kinds of programs. The standard library is not yet mature and it's likely that new functions will be added as time goes on, or the behavior of existing functions modified:

- `rep[num, expr]` produces the string created by `num` repetitions of `expr`. This function lazily evaluates `expr` each time it's needed, so call like `rep[2, "a" | "b"]` could produce either `aa`, `ab`, `ba`, or `bb`.
- `wd[str...]` simply appends all its arguments together, e.g. `wd["a", "b"]` produces `ab`. This is almost always redundant but can make some code clearer.
- `se[str...]` is a sentence creation helper: it attemps to intelligently put spaces between fragments while respecting punctuation and will also always capitalize the first word in the sentence. e.g. `se["a", "b", "c."]` ends up producing `A b c.`
- `str/upper[str]`, `str/lower[str]`, and `str/capitalize[str]` will modify strings for their case. The two functions `str/upper` and `str/lower` convert their arguments fully to upper- and lower-case, respectively, while `str/capitalize` will attempt to turn its argument to title-case, e.g. `str/capitalize["foo bar"]` produces `Foo Bar`.
- `add[x, y]`, `sub[x, y]`, and `mul[x, y]` peform the relevant arithmetic operations on integers.
- `tuple/concat[tup]` takes a tuple of tuples and flattens it into a tuple, e.g. `tuple/concat[<<1>, <2>>]` produces `<1, 2>`.
- `tuple/flatten[val...]` takes any number of arguments and produces a flat tuple, appending and concatenating as necessary: for example, `tuple/flatten[1, 2, 3]` and `tuple/flatten[<1, <2>>, <<<3>>>]` will both produce `<1, 2, 3>`. It should be noted that in order for this to work `tuple/flatten` will necessarily force its argument fully.
- `tuple/rep[num, expr]` creates a tuple by repeating `expr` `num` times. For example, `tuple/rep[2, X]` produces `<X, X>`. Like `rep`, this treats the second argument lazily.
- `tuple/index[tup, num]` returns the nth element of `tup`, zero-indexed.
- `tuple/replace[tup, num, new]` replaced the nth element of `tup` with `new`.
- `tuple/len[tup]` returns the number of elements in the tuple `tup`.
- `tuple/join[tup]` appends the elements of `tup` into a single string. This function takes an optional second argument, as well, so `tuple/join[tup, str]` appents the elements of `tup` separated by copies of `str` into a single string.


### A more advanced example

To see how we might use these functions, imagine that we want to create random words that are written in an typical orthography but _also_ include their intended pronunciation in IPA. One such example can be found in the [`simple_ipa`](examples/simple_ipa.matzo) example, which we can walk through here. We start with the same typical definitions of consonants and vowels, but notice that some of the consonants are IPA symbols:

```
cons := "p" | "t" | "k"
      | "m" | "n" | "ŋ"
      | "s" | "ʃ" | "ɾ";
vowel ::= a i u;
```

Our next definitions create the [rime](https://en.wikipedia.org/wiki/Syllable#Rime) out of a vowel and an optional glottal stop, and then the syllable by choosing a random consonant and then a vowel. However, notice that we are producing _tuples_ instead of strings here: the result of `rime` will be either `<vowel>` or `<vowel, "ʔ">`, and `syll` will use `tuple/concat` to combine the rime with a consonant and flatten it down to either `<cons, vowel>` or `<cons, vowel, "ʔ">`.

```
rime := 4: <vowel> | <vowel, "ʔ">;
syll := tuple/concat[<<cons>, rime>];
```

We then produce the `word` by producing 2 to 4 copies of this syllable (which might look like, say, `<<cons, vowel>, <cons vowel, "ʔ">>`) and then flattening it again (to produce `<cons, vowel, cons, vowel, "ʔ">`.)

```
word := tuple/concat[tuple/rep[2..4, syll]];
```

Now, we define a function called `orthography` which takes a IPA string and replaces it with the orthographic equivalent. We also allow most of the letters to remain unchanged, but the four IPA letters are replaced with corresponding letters in the Latin alphabet.

```
orthography := {
  ["ʃ"] => "sh";
  ["ŋ"] => "ng";
  ["ɾ"] => "r";
  ["ʔ"] => "'";
  [ch]  => ch
};
```

In order to produce the same word twice, we use `fix word` to deterministically reuse it:

```
fix word;
```

Then, we produce two strings: the first one is the orthography, where we use `tuple/map` to apply `orthography` to `word`, and then `tuple/join` to condense it to a single string, and the other simply joins the existing IPA string together. Finally, we print both:

```
ortho := tuple/join[tuple/map[orthography, word]];
ipa := tuple/join[word];
puts ortho " (pronounced /ˈ" ipa "/)";
```

Some example outputs of this program look like this:

```
mu'pa (pronounced /ˈmuʔpa/)
ngusisa (pronounced /ˈŋusisa/)
tisha' (pronounced /ˈtiʃaʔ/)
```
