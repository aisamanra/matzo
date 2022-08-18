**Matzo** is a little dynamically typed language intended for creating fragments of random text. Matzo programs nondeterministically produce strings as output. [Try Matzo in your browser today!](https://gdritter.com/mtz.html)

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
- _Records_ are mappings from names to values, and are written with curly brackets, e.g. `{x: 2, y: Foo}` is a two-element record where the name `x` maps to `2` and the name `y` maps to `Foo`. Only valid identifiers can be record keys. Elements can be extracted from records with a dot syntax, e.g. `r.x` can be used on the above record to pull out `2`.

### Functions

Applying functions is done using parameter lists contained in square brackets. That is to say, to apply the `to-upper` function to the string `"foo"`, you'd write `to-upper["foo"]`.

The simplest way to define a function is using the `fn` keyword. Functions define their parameters within square brackets and their body after a `=>` symbol.

```
fn id[x] => x;
```

In order to define a function by cases, you need to surround the body with curly braces and separate individual cases with `;`. (The last `;` in the body is optional, and can be included or omitted.)

```
fn if {
  [True,  x, _] => x;
  [False, _, y] => y;
};
```

You also can create inline anonymous functions by using the `fn` syntax. Unlike the top-level variation, curly braces for inline functions are _mandatory_ and cannot be omitted, even for functions without multiple cases.

```
const := fn { [_, y] => y};
```

It is a runtime error if no case matches.

```
>>> fn {[A] => 1; [B] => 2}[C]
error: No pattern matched C
  1 |fn {[A] => 1; [B] => 2}[C]
     ^^^^^^^^^^^^^^^^^^^^^^^
```

Matzo does not permit nullary (i.e. zero-argument) functions: because Matzo is lazy-by-default, zero-argument functions are redundant.

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

### Pattern-matching

Patterns in both function definitions and `case` expressions work identically. The following patterns are allowed:
- Any expression can be matched by a variable name, so the function `fn {[var] => <Yes, var>; [_] => No}` will never return `No`: if it's called with a single argument, that argument will be bound to `var` and the function will return `<Yes, ...>` where `...` is replaced by the function's argument.
- If the argument is not used, it can be bound to the wildcard `_`, which discards it. `fn {[_] => Yes; [_] => No}` will never return `No`, because the wildcard will match any argument passed to it.
- Literals—including atoms, numbers, and strings—will attempt to match against themselves. Identical strings, atoms, or numbers will successfully match, and will fail a pattern-match against anything else.
- Pattern-matching against a tuple will recursively bind patterns within the tuple itself: for example, the pattern `<a, b, c>` will bind against tuples of length 3, binding the first element to `a`, the second one to `b`, and the third one to `c`. You can also against just the beginning of arbitrary-length tuples: the pattern `<a, b, ..>` will match tuples whose length is 2 or more and will bind the first element to `a` and the second one to `b`. If you'd like to bind the _rest_ of the tuple to a name, you can do that by including a variable name after the final `..`: the pattern `<a, b, ..c>` will match tuples whose length is 2 or more and will bind the first element to `a`, the second one to `b`, and a (possibly empty) tuple containing `len-2` elements to `c`.
- Pattern-matching against a record works similarly to pattern-matching against a tuple, except names are used instead of positions. Keep in mind that wildcard patterns work in the same way. This means that the pattern `{x: a}` will pattern-match against a record that contains _only_ a single field `x` and will bind the value of that field to `a`. If you want to match against any record that contains a field `x`, you can use the pattern `{x: a, ..}`, and you can get access to the rest of the record (not including the fields already matched) using the pattern `{x: a, ..b}`.

These can of course be used recursively, so a function like

```
fn sum-all {
  [<{x: n, y: m}, ..rest>] => add[add[n, m], sum-all[rest]];
  [<>] => 0;
}
```

will take tuples whose elements are records containing _only_ fields `x` and `y` and sum the values of those fields together for the entire tuple.

It's also worth noting how this interacts with non-determinism: what happens when we try to write something like

```
(fn {[<A, x>] => "A plus " x x; [<B, x>] => "B plus " x x})[<A | B, 1 | 2>]
```

where the argument to the function is random? Matzo will always keep random choices consistent between branches, but _only if a branch attempts to make a choice based on a random value_. So, in the above case, we are branching on a 2-tuple whose first element might be either `A` or `B`, and whose second element might be either `1` or `2`. When we try to find out if the first branch matches, it first checks to see if the expression is a 2-tuple, and then checks to see if the first element of the tuple is `A` or not. As soon as that second check happens, Matzo will "fix" the first element to either `A` (in which case the branch matches) or `B` (in which case the branch fails, and we go on to the second branch). However, because nothing pattern-matched on `x`, that value is still lazy. If we have to fall back to the second branch, then we _don't_ try to re-roll the first element of the tuple: that's already been fixed by the first branch, but yet again, the second branch hasn't said anything about `x`, so that remains random.


### A more advanced example

To see how we might use these functions, imagine that we want to create random words that are written in an typical orthography but _also_ include their intended pronunciation in IPA. One such example can be found in the [`simple_ipa`](examples/simple_ipa.matzo) example, which we can walk through here. We start with the same typical definitions of consonants and vowels, but notice that some of the consonants are IPA symbols:

```
cons := "p" | "t" | "k"
      | "m" | "n" | "ŋ"
      | "s" | "ʃ" | "ɾ";
vowel ::= a i u;
```

Our next definitions create the [rime](https://en.wikipedia.org/wiki/Syllable#Rime) out of a vowel and an optional glottal stop, and then the syllable by choosing a random consonant and then a vowel. However, notice that we are producing _tuples_ instead of strings here: the result of `rime` will be either `<vowel>` or `<vowel, "ʔ">`, and `syll` will use `tuple/flatten` to combine the rime with a consonant and flatten it down to either `<cons, vowel>` or `<cons, vowel, "ʔ">`.

```
rime := 4: <vowel> | <vowel, "ʔ">;
syll := tuple/flatten[cons, rime];
```

We then produce the `word` by producing 2 to 4 copies of this syllable (which might look like, say, `<<cons, vowel>, <cons vowel, "ʔ">>`) and then flattening it again (to produce `<cons, vowel, cons, vowel, "ʔ">`.)

```
word := tuple/flatten[tuple/rep[2..4, syll]];
```

Now, we define a function called `orthography` which takes a IPA string and replaces it with the orthographic equivalent. We also allow most of the letters to remain unchanged, but the four IPA letters are replaced with corresponding letters in the Latin alphabet.

```
fn orthography {
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
