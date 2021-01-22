# Introduction \label{intro}

The purpose of project is to extend the Rust-frontend to Stainless,
`rust-stainless`[^repo] by Georg Schmid, written in Rust. Stainless[^stainless]
is a formal verification tool for Scala, written in Scala. The goal of the
frontend is to extract a subset of the Rust language, translate it to
Stainless's intermediate representation and reuse its verification pipeline.

[^stainless]: [stainless.epfl.ch](https://stainless.epfl.ch/)
[^repo]:
    [epfl-lara/rust-stainless on Github](https://github.com/epfl-lara/rust-stainless)

While the main architecture and infrastructure of the frontend already existed,
this project adds numerous features and refactors, in particular it adds the
capability to extract type classes in the Scala-Stainless sense from Rust's
traits and their implementations. To illustrate that, consider Listing
\ref{code1} that describes equality as an abstract class.

```{.scala label="code1" caption="Type class with attached laws in Scala."}
abstract class Equals[T] {
  def equals(x: T, y: T): Boolean
  def notEquals(x: T, y: T): Boolean = !equals(x, y)

  @law
  def law_reflexive(x: T): Boolean =
    equals(x, x)
  @law
  def law_symmetric(x: T, y: T): Boolean =
    equals(x, y) == equals(y, x)
  @law
  def law_transitive(x: T, y: T, z: T): Boolean =
    !(equals(x, y) && equals(y, z)) || equals(x, z)
}
```

Stainless makes it possible to attach _laws_ i.e. algebraic properties to type
classes [@algb] with the `@law` annotation. These laws are also verified by
Stainless which ensures that implementors hold the contract set by the type
class.

The primary goal of this project was to port that feature to the Rust-frontend
of Stainless. In other words, support the code in Listing \ref{code2}. This was
achieved with some drawbacks, discussed in \ref{caveats}.

```{.rust label="code2" caption="Type class with attached laws in Rust."}
trait Equals {
  fn equals(&self, x: &Self) -> bool;
  fn not_equals(&self, x: &Self) -> bool {
    !self.equals(x)
  }

  #[law]
  fn law_reflexive(x: &Self) -> bool {
    x.equals(x)
  }
  #[law]
  fn law_symmetric(x: &Self, y: &Self) -> bool {
    x.equals(y) == y.equals(x)
  }
  #[law]
  fn law_transitive(x: &Self, y: &Self, z: &Self) -> bool {
    !(x.equals(y) && y.equals(z)) || x.equals(z)
  }
}
```

The rest of this report is structured as follows. In \ref{background}, the
existing features of the Rust-frontend as well as a short architecture overview
is given. The added features are introduced on a conceptual, user-perspective in
\ref{features} and their implementation is described in \ref{implementation}.
Lastly, I discuss problems with the current state as well as options for future
work \ref{discussion}.

# Background \label{background}

## Existing features

The targeted Rust fragment underlies some strict restrictions: all code has to
be functional and immutable. The only allowed side-effect is `panic!`. Before
the project, references, heap allocated objects called _boxes_ and recursive
data types were also forbidden. Nonetheless, the majority of features was
already supported, like extraction of most of the basic syntax, top-level
functions and their bodies, integer and boolean expressions and operations,
pattern matching, type parameters and generics, see Listing \ref{code3}.

```{.rust label="code3" caption="Integer operations in Rust."}
pub fn i32_ops(x: i32, y: i32) {
  assert!(x + y == y + x);
  assert!(x + x == 2 * x);
  assert!(x + x == x << 1);
  if x >= 0 && x < 1<<30 {
    assert!(x == (x + x) / 2);
  }
}
```

Support for _algebraic data types (ADTs)_ was als present, including tuples
(without their pattern matching) and generics. In Rust, ADTs are `enum`s, tuples
and `struct`s. They are extracted to Stainless ADTs:

```{.rust caption="An ADT example in Rust."}
enum Maybe<T> {
  Nothing,
  Just { value: T },
}

fn get_or_else<T>(maybe: Maybe<T>, default: T) -> T {
  match maybe {
    Maybe::Nothing => default,
    Maybe::Just { value } => value,
  }
}
```

To state pre- and postconditions on functions, like Scala's `require` and
`ensuring` the crate `libstainless`, contains two attributes that can be added
to functions: `pre` and `post`, in short _specs_. The expression of a spec is
normal Rust code, but the design goal is that specs should have no influence on
the function body. This poses an unsolved problem, because normal Rust code has
to satisfy the type- _and_ borrow-checks, which can be difficult if some
expression in a spec consumes an object. As a work-around, it is allowed to add
multiple specs of the same kind which is equivalent to multiple
`&&`-concatenated expressions.

```{.rust caption="Example of function specs."}
#[pre(x >= 0)]
#[pre(x < 10)]
#[post(ret >= 0)]
pub fn fact(x: i32) -> i32 {
  if x <= 0 { 1 }
  else { fact(x - 1) * x }
}
```

## Architecture

The Rust-frontend is a pipeline of different stages: macros, early compilation,
extraction, serialisation and verification. The programmer depends on
`libstainless` which contains the macros for the attributes like specs and
flags. The frontend can then be run as a `cargo` task.

Rustc is linked as library to the frontend and, at the start of the cargo task,
its driver executes the first few phases of compilation, until the AST (on which
macro expansion is performed) is converted to the _High-Level Intermediate
Representation (HIR)_. This tree results after type- and borrow-checking have
been performed. Hence, the frontend can assume that types and ownership are
checked.

After that, the extraction phase of the frontend converts the HIR to Stainless
AST trees that are serialised to a binary format. Lastly, a JVM instance of
Stainless with a custom frontend ingests the serialisation format and with only
minor transformations feeds the trees to the verification pipeline.

# New Features \label{features}

In this section, the code needed for Listing \ref{code2} and for the translated
insertion sort benchmark (Appendix \ref{insertion}) is introduced
feature-by-feature.
# Implementation \label{implementation}

The overall architecture of the code-base has not changed. All new features were
implemented as extensions to the existing infrastructure.

## `let` Type Ascriptions and Tuple Pattern Matching [^pr1]

Both of these features were implemented by simply adding a case in the pattern
extraction function. Once matched, the subpatterns of both cases are easily
dealt with by a simple recursion. For tuples, the leaf pattern type
`TyKind::Tuple`[^tuple-path] was needed and for the ascriptions the pattern kind
`PatKind::AscribeUserType`[^pat-path].

[^pr1]:
    [PR#60 on Github](https://github.com/epfl-lara/rust-stainless/pull/60);
    [PR#61 on Github](https://github.com/epfl-lara/rust-stainless/pull/61)

[^pat-path]: `rustc_hair::hair::pattern::PatKind::AscribeUserType`
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

### Caveats \label{caveats}

Other specs and measures don't work on trait/impl blocks currently. This leads
to Stainless not being able to prove one of the properties in the `ListEquals`
implementation, because it cannot infer the measure that it should use.

Type classes cannot have multiple implementations for different operations, e.g.
Monoid for `i32` for addition _and_ multiplication.

Type class inheritance doesn't work yet.

# Discussion \label{discussion}

Further work:

- String extraction
- Floating point OPs are missing
- projection types when applying methods to refs

two big problems for usability of type classes:

- Discrepancy between own, spec'd traits with laws and std::traits
- Borrow checking in laws and specs messes up

Further: mutability and vectors (instead of ad-hoc, linked lists)

# Conclusion

# References

<!-- prettier-ignore -->
::: {#refs}
:::

# Appendix

## Insertion Sort Benchmark \label{insertion}

The original Scala benchmark, as taken from
[the Stainless repository](https://github.com/epfl-lara/stainless/blob/master/frontends/benchmarks/verification/valid/InsertionSort.scala).

```{.scala caption="The Scala benchmark from "}
import stainless.annotation._
import stainless.lang._

object InsertionSort {
  sealed abstract class List
  case class Cons(head:Int,tail:List) extends List
  case class Nil() extends List

  sealed abstract class OptInt
  case class Some(value: Int) extends OptInt
  case class None() extends OptInt

  def size(l : List) : BigInt = (l match {
    case Nil() => BigInt(0)
    case Cons(_, xs) => 1 + size(xs)
  }) ensuring(_ >= 0)

  def contents(l: List): Set[Int] = l match {
    case Nil() => Set.empty
    case Cons(x,xs) => contents(xs) ++ Set(x)
  }

  def min(l : List) : OptInt = l match {
    case Nil() => None()
    case Cons(x, xs) => min(xs) match {
      case None() => Some(x)
      case Some(x2) => if(x < x2) Some(x) else Some(x2)
    }
  }

  def isSorted(l: List): Boolean = l match {
    case Nil() => true
    case Cons(x, Nil()) => true
    case Cons(x, Cons(y, ys)) => x <= y && isSorted(Cons(y, ys))
  }

  /* Inserting element 'e' into a sorted list 'l' produces a sorted list with
   * the expected content and size */
  def sortedIns(e: Int, l: List): List = {
    require(isSorted(l))
    l match {
      case Nil() => Cons(e,Nil())
      case Cons(x,xs) => if (x <= e) Cons(x,sortedIns(e, xs)) else Cons(e, l)
    }
  } ensuring(res => contents(res) == contents(l) ++ Set(e)
                    && isSorted(res)
                    && size(res) == size(l) + 1
            )

  /* Insertion sort yields a sorted list of same size and content as the input
   * list */
  def sort(l: List): List = (l match {
    case Nil() => Nil()
    case Cons(x,xs) => sortedIns(x, sort(xs))
  }) ensuring(res => contents(res) == contents(l)
                     && isSorted(res)
                     && size(res) == size(l)
             )

  @extern
  def main(args: Array[String]): Unit = {
    val ls: List = Cons(5, Cons(2, Cons(4, Cons(5, Cons(1, Cons(8,Nil()))))))
    println(ls)
    println(sort(ls))
  }
}
```

The translated Rust test case as taken from
[the frontend repository](https://github.com/epfl-lara/rust-stainless/blob/master/stainless_frontend/tests/pass/insertion_sort.rs).

```{.rust caption="The Rust test case."}
extern crate stainless;
use stainless::*;

pub enum Option<T> {
  None,
  Some(T),
}

pub enum List<T> {
  Nil,
  Cons(T, Box<List<T>>),
}

impl<T> List<T> {
  #[measure(self)]
  pub fn size(&self) -> u32 {
    match self {
      List::Nil => 0,
      List::Cons(_, tail) => 1 + tail.size(),
    }
  }

  #[measure(self)]
  pub fn contents(&self) -> Set<T> {
    match self {
      List::Nil => Set::empty(),
      List::Cons(head, tail) => tail.contents().union(&Set::singleton(head)),
    }
  }
}

impl List<i32> {
  #[measure(self)]
  pub fn is_sorted(&self) -> bool {
    match self {
      List::Nil => true,
      List::Cons(x, tail) => match &**tail {
        List::Nil => true,
        List::Cons(y, ..) => *x <= *y && tail.is_sorted(),
      },
    }
  }

  #[measure(self)]
  pub fn min(&self) -> Option<i32> {
    match self {
      List::Nil => Option::None,
      List::Cons(x, xs) => match xs.min() {
        Option::None => Option::Some(*x),
        Option::Some(y) if *x < y => Option::Some(*x),
        Option::Some(y) => Option::Some(y),
      },
    }
  }

  /// Inserting element 'e' into a sorted list 'l' produces a sorted list with
  /// the expected content and size
  #[pre(self.is_sorted())]
  #[measure(self)]
  #[post(
    ret.size() == self.size() + 1 &&
    ret.is_sorted() &&
    ret.contents().is_subset_of(&self.contents().add(&e)) &&
    self.contents().add(&e).is_subset_of(&ret.contents())
  )]
  pub fn sorted_insert(self, e: i32) -> List<i32> {
    match self {
      List::Cons(head, tail) if head <= e => List::Cons(head, Box::new(tail.sorted_insert(e))),
      _ => List::Cons(e, Box::new(self)),
    }
  }

  /// Insertion sort yields a sorted list of same size and content as the input
  /// list
  #[measure(self)]
  #[post(
    ret.size() == self.size() &&
    ret.is_sorted() &&
    ret.contents().is_subset_of(&self.contents()) &&
    self.contents().is_subset_of(&ret.contents())
  )]
  pub fn sort(self) -> List<i32> {
    match self {
      List::Nil => self,
      List::Cons(x, xs) => xs.sort().sorted_insert(x),
    }
  }
}

#[external]
pub fn main() {
  let list = List::Cons(
    5,
    Box::new(List::Cons(
      2,
      Box::new(List::Cons(
        4,
        Box::new(List::Cons(
          5,
          Box::new(List::Cons(-1, Box::new(List::Cons(8, Box::new(List::Nil))))),
        )),
      )),
    )),
  );
  assert!(list.sort().is_sorted())
}
```
