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

Rustc is linked as library to the fronted and its runner is driver is used to
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

## let type ascriptions

## tuple pattern matching

## stainless measure annotation

## box by erasure

## immutable references by erasure

## stainless set type operations

## Implementation Block Methods

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
done with nested functions. As described in \ref{intro}, it is a design goal to
be able to state bodies of specs in Rust code without interfering with the
actual function's code, in particular without having an influence on the
borrow-checking of the function's body. To circumvent the borrow checker, specs
for top-level functions are desugared by a proc-marco to nested functions that
duplicate the parameters of their parent. In that way, new bindings are created
that cannot interfere with the outer body but have the same types and
identifiers, which makes it simple to extract them to Stainless's `require`,
`ensuring` and `decreases`.

The nested function approach fails for methods because nested functions in Rust
cannot use arguments nor type parameters from their surrounding function but
methods need the `self` parameter. To solve this issue I rely on the restriction
that function/method names need to be unique in a scope in Rust. In this case,
each method is uniquely identified by its parent `impl` and its name. This makes
it possible to desugar the specs to sibling functions on that `impl` block and
encode the name of the spec'd function in the name of the generated sibling
function. The example shows how the generated spec-function is named, the number
serves to distinguish multiple specs of the same type:

```{.rust caption="Attribute becomes a sibling function"}
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

## Type Classes and Laws \label{typecls}

### Caveats

Other specs and measures don't work on trait/impl blocks currently. This leads
to Stainless not being able to prove one of the properties in the `ListEquals`
implementation, because it cannot infer the measure that it should use.

# Discussion

# Conclusion
