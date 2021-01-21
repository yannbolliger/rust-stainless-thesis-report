# Introduction \label{intro}

Stainless[^stainless] is a formal verification tool for the Scala programming
language, written in Scala. This project works on the Rust-frontend to
Stainless, which is called `rust-stainless`[^repo] by Georg Schmid and is
written in Rust. Its goal is to extract a subset of the Rust language, translate
it to Stainless's intermediate representation and use the verifier pipeline of
Stainless.

[^stainless]: [stainless.epfl.ch](https://stainless.epfl.ch/)
[^repo]:
    [epfl-lara/rust-stainless on Github](https://github.com/epfl-lara/rust-stainless)

- many features already there: list them

  - expression extraction for integer operations, syntax, etc.
  - function & body extraction
  - pattern match extraction
  - type parameters and generics
  - ADT, enum/struct extraction
  - tuples without pattern matching
  - pre-, post conditions as attributes

- clear restrictions: only immutable, functional code
- no references, no recursive data types
- specs should not interfere with code/borrow-checking

Specs were already there, but without the `measure` one. Big challenge of specs
is disabling the borrow-checker for their code.

# Goals and Added Features

This project aimed at advancing the Rust-frontend of Stainless to support a
sizeable chunk of Stainless' functional constructs. This will involve extending
the frontend's current features, in particular within the Rustc-embedded
extraction phase.

Ultimately, we should be able to port several of the Stainless' existing
verification benchmarks.

- show off some first class feature with type classes

# Implementation

The overall architecture of the code-base has not been changed. All the new
features were implemented as extensions to the existing infrastructure. The
architecture can be concisely described as a pipeline. First, there is the
`libstainless` which contains the macros needed for the attributes like specs
and flags.

Rustc is linked as library to the frontend and its runner is driver is used to
execute the first few phases of compilation until the AST (on which macro
expansion is performed) is converted to the _High-Level Intermediate
Representation (HIR)._ This tree results after type-checking and borrow-checking
have been performed. Hence, the frontend can assume that types and ownership are
checked.

The `rust-stainless` extracts Stainless AST trees from the HIR in its extraction
phase. Those trees are then serialised to a binary format and submitted to a
running instance of Stainless. On the Stainless side, there is a custom frontend
that is able to ingest the serialisation format and with only minor
transformations feed it to the verification pipeline.

## `let` Type Ascriptions and Tuple Pattern Matching [^pr1]

Both of these features were implemented by simply adding a case in the pattern
extraction function. Once matched, the subpatterns of both cases are easily
dealt with by a simple recursion. For tuples, the leaf pattern type
`TyKind::Tuple`[^tuple-path] was needed and for the ascriptions the pattern kind
`PatKind::AscribeUserType`[^pat-path].

[^pr1]:
    [PR#60 on Github](https://github.com/epfl-lara/rust-stainless/pull/60);
    [PR#61 on Github](https://github.com/epfl-lara/rust-stainless/pull/61)

[^pat-path]: `rustc_mir_build::thir::pattern::PatKind::AscribeUserType`
[^tuple-path]: `rustc_middle::ty::TyKind::Tuple`

## Tuple Struct ADTs [^pr7]

[^pr7]: [PR#35 on Github](https://github.com/epfl-lara/rust-stainless/pull/35)

The extraction of tuple `struct`s of Rust to Stainless ADTs was enabled by
solving a smaller naming problem. Stainless's backend Z3 does not allow purely
numerical identifiers for fields of ADTs, which clashed with the numerical
naming scheme of fields in Rust's tuple structs, e.g. `tuple.0`. This was solved
by simply prepending an underscore to such identifiers, i.e. `0` becomes `_0`.

## Stainless Measure Attribute [^pr2]

[^pr2]: [PR#31 on Github](https://github.com/epfl-lara/rust-stainless/pull/31)

As described in \ref{intro}, it is a design goal to be able to state bodies of
specs in Rust code without interfering with the actual function's code, in
particular without having an influence on the borrow-checking of the function's
body. To circumvent the borrow checker, the spec-attributes are desugared by a
proc-macro to nested functions inside the actual function. These nested
functions duplicate the parameters of their parent. In that way, new bindings
are created that cannot interfere with the outer body but have the same types
and identifiers, which makes it simple to extract them to Stainless's `require`,
and `ensuring`.

To extend that and support the `#[measure()]` attribute, a proc-macro was added
to `libstainless`, as well as the corresponding code that extracts a `decreases`
tree from it. Most functionality was already present for pre- and postconditions
and could only be extended by a third case.

The only differences are that there can be at most one measure attribute per
function, which is easily checked in the extraction, and that the return type of
the generated function can be anything, unlike the conditions that always return
a boolean. The latter poses a problem because the macro has no understanding of
what type the given expression is. Therefore, the generated function just
returns `()` by appending a `;` on the expression. Later in the extraction, the
_HIR_ will contain information about the type of that last expression and only
there the type of the measure is inferred.

## Immutable Boxes and References by Erasure [^pr3]

[^pr3]:
    [PR#30 on Github](https://github.com/epfl-lara/rust-stainless/pull/30);
    [PR#45 on Github](https://github.com/epfl-lara/rust-stainless/pull/45)

Support of these two features relies on following the assumptions about the Rust
fragment the frontend supports:

- it is impossible to create mutable values or references
- the only allowed references are immutable borrows and immutable boxes
- the only allowed binding modes are immutable, aliasable by-reference, called
  _shared borrows_ by rustc or by-value.

If the above holds, then it should be safe to treat immutable references as the
objects they refer to. Therefore, boxes and references work by erasure. That is
to say that everywhere an expression like `ExprKind::Borrow`[^expr-kind] or a
type like `TyKind::Ref`[^ty-kind] occurs and satisfies the above conditions, it
is simply extracted to the expression or type it contains. The same goes for
calls to `Box::new`.

[^expr-kind]: `rustc_mir_build::thir::ExprKind::{Borrow, Deref}`
[^ty-kind]: `rustc_middle::ty::TyKind::Ref`

## Stainless Set Type Operations [^pr4]

[^pr4]: [PR#37 on Github](https://github.com/epfl-lara/rust-stainless/pull/37)

All the set operations available for Scala in `stainless.collections` were added
as methods on the type `Set<T>` in `libstainless`. Each method panics with
`unimplemented!()` when run, although it would be simple to provide a runtime
behaviour to these methods.

In the extraction of function calls, the set operations are detected as being
part of the _standard items_ -- items that are specially marked e.g. some Rust
language features like panics, and that have special cases for extraction. Set
operations are simply extracted to their corresponding set operation tree in
Stainless.

Apart from its simplicity, the current implementation made visible two
disadvantages. The first one concerns the detection of standard items by `DefId`
from other crates. The extraction code only knows the names/paths of the
functions it should detect, like `stainless::Set::singleton`. However, rustc
does not seem to have a way of querying items from other crates by name, only by
id. Therefore, the extraction code has to enumerate all `DefId`s from the
`stainless` crate and compare them by name to the desired ones, until it has
found all the needed items.

The other disadvantage of the current `Set` is that while it has a derived
implementation of the `PartialEq` trait, the extraction code does not recognise
calls to `PartialEq::eq` as being equivalent to the primitive equality operator
`==`. Hence, it is not capable of extracting a Stainless `Equals` tree for that
type and one has to resort to using `Set::subset_of` in both directions.

## Implementation Block Methods [^pr5]

[^pr5]: [PR#47 on Github](https://github.com/epfl-lara/rust-stainless/pull/47)

Other than abstract methods and laws, i.e. functions specified on traits and
their implementations which will be covered in \ref{typecls}, it is not
necessary to extract methods from normal `impl` blocks as Stainless methods. As
there is no inheritance outside of the type classes and their implementations,
it is simpler to extract normal methods like top-level functions, that have as
first parameter their `Self` type. In fact, this is also how the Rust compiler
represents those methods, therefore the extraction to Stainless functions is
straightforward and can reuse exactly the same code as top-level function
extraction.

One complication arose for the way of attaching specs to functions which was
done with nested functions.

The nested function approach fails for methods because nested functions in Rust
cannot use arguments nor type parameters from their surrounding function but
methods need the `self` parameter. To solve this issue I rely on the restriction
that function/method names need to be unique in a scope in Rust. In this case,
each method is uniquely identified by its parent `impl` and its name. This makes
it possible to desugar the specs to sibling functions on that `impl` block and
encode the name of the spec'd function in the name of the generated sibling
function. The example shows how the generated spec-function is named, the number
serves to distinguish multiple specs of the same type:

```{.rust caption="Attribute becomes a sibling function."}
#[post(ret > 0)] // this attribute
fn append(&self, a: i32) -> i32 {}

// becomes:
fn __post_1_append(&self, a: i32, ret: i32) -> bool {
  ret > 0
}
```

One disadvantage of this solution is that there are now two separate ways of
encoding specs for certain kinds of functions with Rust macros. On the other
hand, both types share them same extraction code and this approach makes it
possible to add specs on functions that are nested inside a method, as this
example shows.

## Type Classes and Laws \label{typecls} [^pr6]

[^pr6]:
    In order of dependency:
    [PR#57 on Github](https://github.com/epfl-lara/rust-stainless/pull/57);
    [PR#58 on Github](https://github.com/epfl-lara/rust-stainless/pull/58);
    [PR#59 on Github](https://github.com/epfl-lara/rust-stainless/pull/59);
    [PR#52 on Github](https://github.com/epfl-lara/rust-stainless/pull/52)

### Caveats

Other specs and measures don't work on trait/impl blocks currently. This leads
to Stainless not being able to prove one of the properties in the `ListEquals`
implementation, because it cannot infer the measure that it should use.

# Discussion

# Conclusion
