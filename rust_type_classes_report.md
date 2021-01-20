# Introduction \label{intro}

- short intro on verification and stainless
- clearly establish that this is an extension to the existing project

Say what fragments were already there.

Specs were already there, but without the `measure` one. Big challenge of specs
is disabling the borrow-checker for their code.

## Goals

Goal of this project was to advance the prototypical Rust-frontend of Stainless
to support a sizeable chunk of Stainless' functional constructs. This will
involve extending the frontend's current features, in particular within the
Rustc-embedded extraction phase.

Ultimately, we should be able to port several of the Stainless' existing
verification benchmarks.

- show off some first class feature with type classes

# Added Features

# Implementation

## let type ascriptions

## tuple pattern matching

## box by erasure

## stainless set type operations

## stainless measure annotation

## immutable references by erasure

## impl block functions

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

```rust
#[post(ret > 0)]
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
