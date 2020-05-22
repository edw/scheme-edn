# EDN Reader for R7RS Scheme

Edwin Watkeys, edw@poseur.com

[EDN](https://github.com/edn-format/edn) is a data format from the
Clojure ecosystem. It is essentially the format of Clojure programs,
minus surface syntax such as quoting, syntax-quoting, and `@` rest
argument syntax.

This reader depends on the [`(chibi
parse)`](http://synthcode.com/scheme/chibi/lib/chibi/parse.html)
module, part of [Chibi Scheme](http://synthcode.com/wiki/chibi-scheme)
but theoretically portable to any R7RS Scheme.

This library, `(edn)`, exports `parse-edn` and
`edn-value`. `Parse-edn` is procedure of one argument and parses the
passed source, returning a data structure representing the passed EDN
or `#f` if parsing fails. `Edn-value` is a parser combinator that can
be called with
[`call-with-parse`](http://synthcode.com/scheme/chibi/lib/chibi/parse.html#h3_call-with-parse)
or other similar procedures in the `(chibi parse)` library.

Additionally, the library exports predicates, accessors, and
constructors for types that don't have exact analogs in Scheme or
where the analogs are not nearly-universally supported. Of particular
note, the library returns EDN `nil` as the value `edn-nil`, which can
be tested for via `edn-nil?`.

This implementation should parse any valid EDN document. Also, since
Clojure programs may emit ratio values, they are parsed into Scheme
ratios despite their absence from the EDN standard. Finally, note that
`parse-edn` will return `#f` on failure, which is also the result of
parsing the string `"false"`. If this is a problem, use the
`call-with-parse` procedure metioned above.

A test suite is included in the `(edn-test)` library. To execute it,
evaluate `(run-tests)`.
