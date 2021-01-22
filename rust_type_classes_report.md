# Introduction \label{intro}

The goal of this project is to extend the Rust-frontend to Stainless,
`rust-stainless`[^repo] by Georg Schmid, written in Rust. Stainless[^stainless]
is a formal verification tool for Scala, written in Scala. The Rust-frontend is
capable of extracting a subset of the Rust language, translating it to
Stainless's intermediate representation and submitting it to Stainless's
verification pipeline.

[^stainless]: [stainless.epfl.ch](https://stainless.epfl.ch/)
[^repo]:
    [epfl-lara/rust-stainless on Github](https://github.com/epfl-lara/rust-stainless)

While the main architecture and infrastructure of the frontend already existed,
this project adds numerous features. In particular, the primary goal was to
enable extraction of type classes in the Scala Stainless sense from Rust's
traits and their implementations. To illustrate that, consider Listing
\ref{code1} that describes equality as an abstract class in Scala.

```{.scala label="code1" caption="Type class with attached laws in Scala."}
abstract class Equals[T] {
  def equals(x: T, y: T): Boolean
  def notEquals(x: T, y: T): Boolean = !equals(x, y)

  @law
  def reflexive(x: T): Boolean =
    equals(x, x)
  @law
  def symmetric(x: T, y: T): Boolean =
    equals(x, y) == equals(y, x)
  @law
  def transitive(x: T, y: T, z: T): Boolean =
    !(equals(x, y) && equals(y, z)) || equals(x, z)
}
```

Stainless makes it possible to attach _laws_ i.e. algebraic properties to type
classes [@algb] with the `@law` annotation, hence it ensures that implementors
hold the contract set by the type class.

The same is now possible in the Rust-frontend. In other words, the code in
Listing \ref{code2} is now supported, although with some drawbacks discussed in
\ref{caveats}. Furthermore, the newly added features also permitted to translate
and prove two benchmarks from the Scala Stainless repository into Rust:
insertion sort and binary search.

```{.rust label="code2" caption="Type class with laws and implementations in Rust."}
trait Equals {
  fn equals(&self, x: &Self) -> bool;
  fn not_equals(&self, x: &Self) -> bool {
    !self.equals(x)
  }

  #[law]
  fn reflexive(x: &Self) -> bool {
    x.equals(x)
  }
  #[law]
  fn symmetric(x: &Self, y: &Self) -> bool {
    x.equals(y) == y.equals(x)
  }
  #[law]
  fn transitive(x: &Self, y: &Self, z: &Self) -> bool {
    !(x.equals(y) && y.equals(z)) || x.equals(z)
  }
}
impl Equals for i32 {
  fn equals(&self, y: &i32) -> bool {
    *self == *y
  }
}
impl<T: Equals> Equals for List<T> {
  fn equals(&self, other: &List<T>) -> bool {
    match (self, other) {
      (List::Nil, List::Nil) => true,
      (List::Cons(x, xs), List::Cons(y, ys)) =>
        x.equals(y) && xs.equals(ys),
      _ => false,
    }
  }
}
```

The rest of this report is structured as follows. In \ref{background}, the
existing features of the Rust-frontend as well as a short architecture overview
is given. The added features are introduced in the user-perspective in
\ref{features} and their internal implementation is described in
\ref{implementation}. Lastly, I discuss problems with the current state of the
code-base as well as prospects for future work \ref{discussion}.

# Background \label{background}

## Existing features

The targeted Rust fragment underlies strict restrictions: all code has to be
functional and immutable and the only allowed side-effect is `panic!`. Before
this project, references, heap allocated objects called _boxes_ and therefore
recursive data types were impossible. Nonetheless, the majority of features
existed already, like the extraction of most of the syntax, top-level functions
and their bodies, integer and boolean expressions/operations, pattern matching,
type parameters and generics, etc.

```{.rust label="code3" caption="Verifiable integer operations in Rust."}
pub fn i32_ops(x: i32, y: i32) {
  assert!(x + y == y + x);
  assert!(x + x == 2 * x);
  assert!(x + x == x << 1);
  if x >= 0 && x < 1<<30 {
    assert!(x == (x + x) / 2);
  }
}
```

_Algebraic data types (ADTs)_ were supported, including tuples (without their
pattern matching) and generics. ADTs in Rust are `enum`s, tuples and `struct`s.

```{.rust caption="An ADT example in Rust."}
enum Maybe<T> {
  Nothing,
  Just { value: T }
}
fn get_or<T>(maybe: Maybe<T>, default: T) -> T {
  match maybe {
    Maybe::Nothing => default,
    Maybe::Just { value } => value,
  }
}
```

To state pre- and postconditions, in short _specs_, on functions like in Scala
with `require` and `ensuring`, the crate `libstainless` contains two attributes
that can be added to functions: `pre` and `post`. The expression in a spec is
regular Rust code, but it must have no influence on the function body and it
must be able to express anything a Scala spec could. This poses an unsolved
problem, because Rust code has to type- _and_ borrow-check, which can be
difficult if some expression in a spec consumes an object. As a work-around, one
can add multiple specs of the same kind to a function which is equivalent to
multiple `&&`-concatenated expressions.

```{.rust caption="Example of multiple function specs."}
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
`libstainless` which contains the macros for the spec-attributes and flags. The
frontend is invoked as a `cargo` task.

`rustc` is linked as a library to the frontend and, upon invocation of the cargo
task, its driver executes the first few phases of regular compilation. I.e. it
parses the program, expands macros on the _abstract syntax tree (AST)_ and does
type- and borrow-checking. The resulting tree is called _High-Level Intermediate
Representation (HIR)_ and it satisfies the type and ownership system.

The extraction phase of the frontend then converts the HIR to Stainless ASTs
that are serialised to a binary format. Lastly, a JVM instance of Stainless with
a custom frontend ingests the serialised trees and, with minor transformations,
feeds them to the regular verification.

# New Features \label{features}

In this section, the code needed for Listing \ref{code2} and for the translated
insertion sort benchmark (Appendix \ref{insertion}) is introduced
feature-by-feature.

## Notation Improvements

Some small features were added to ease notation: `let` bindings can now
explicitly state a type, it is possible to pattern match on tuples and support
for tuple `struct`s was extended to allow accessing fields by their numerical
identifier.

```{.rust caption="Notational extensions."}
pub enum Option<T> {
  None,
  Some(T),
}
let t: u32 = (2, 3).0;
match (t, Option::Some(-1)) {
  (a, Option::None) => {}
  (_, Option::Some(b)) => {}
}
```

## Immutable References and Heap Allocation

Still under the restriction of strict immutability everywhere, it is now
possible to immutably borrow objects, pass immutable references around and
allocate objects on the heap with `Box::new`. This enables recursive data types
like the typical, functional linked-list.

```{.rust caption="Linked-list as recursive ADT."}
pub enum List<T> {
  Nil,
  Cons(T, Box<List<T>>),
}
```

Additionally, the problem of borrow-checking the spec expressions becomes less
severe because functions that only read an object can now take a reference
instead of consuming it. Hence, many specs that needed multiple attributes can
be written as a single boolean expression. Any layer of referencing is allowed.

```{.rust caption="Taking a double reference as argument."}
fn dont_consume(option: &IntOption) -> bool {}
fn dont_consume_2(option: &&IntOption) -> bool {}
#[pre(
  does_not_consume(&o) &&
  does_not_consume_2(&&o)
)]
fn this_consumes(o: IntOption) {}
```

## Recursive proofs

In Stainless, recursive proofs often require the programmer to state the
induction variable with a `decreases` call. This helps Stainless to infer the so
called _measure_ of the proof. That feature was introduced as an additional spec
attribute on functions which makes it possible to verify recursive functions.

```{.rust caption="Measure attribute."}
#[measure(self)]
fn size<T>(l: &List<T>) -> u32 {
  match l {
    List::Nil => 0,
    List::Cons(_, tail) => 1 + size(tail),
  }
}
```

## Stainless Sets

To reason about contents of lists, Stainless has an infinite set type and
exposes its logical operations in `stainless.collections`. Analogously, all set
operations available in Scala were added as methods on the type `Set<T>` in
`libstainless`. Though unlike for Scala, there is no runtime implementation
available, the methods panic when run. But, they serve for example for proving
correctness of a sorted insert on a list:

```{.rust caption="Specification of an insert function."}
#[measure(l)]
fn is_sorted(l: &List<i32>) -> bool;
#[measure(l)]
fn contents(l: &List<i32>) -> stainless::Set<i32>;

#[pre(is_sorted(l))]
#[measure(l)]
#[post(
size(&ret) == size(l) + 1 &&
is_sorted(&ret) &&
contents(&ret).is_subset_of(&contents(l).add(&e)) &&
contents(l).add(&e).is_subset_of(&contents(&ret))
)]
fn sorted_ins(l: &List<i32>, e: i32) -> List<i32>;
```

## Implementation Block Methods

To add methods on a type in Rust one can define them in an `impl` block. The
methods are then callable on instances of that type. This project enables
extraction of such methods, and their specs, to Stainless. As Rust allows
multiple implementation blocks for a type, this is also supported. With this, we
can define the previously displayed list functions as methods (Listing
\ref{implblocks}).

```{.rust caption="Methods on the list implementation." label="implblocks"}
impl<T> List<T> {
  #[measure(self)]
  pub fn size(&self) -> u32 { ... }
}
impl List<i32> {
  #[measure(self)]
  pub fn is_sorted(&self) -> bool { ... }

  #[pre(self.is_sorted())]
  #[measure(self)]
  #[post(...)]
  pub fn sorted_insert(self, e: i32) -> List<i32>
  { ... }
}
```

The features introduced so far suffice to port the insertion sort
(\ref{insertion}) and binary search benchmark to Rust.

## Type Classes and Laws

The final new features are type classes with attached laws, as described in
\ref{intro}. Not only can the frontend extract classes and objects from Rust
traits and implementations, but it also infers which type class instance needs
to be called at each call site of a trait method. Additionally, the laws
specified on a trait will be proven by Stainless for each implementation. As
illustration, take an example violation of the Liskov Substitution principle
[@liskov] with the following trait:

```{.rust caption="Example trait with laws."}
trait Rectangle {
  fn width(&self) -> u32;
  fn height(&self) -> u32;
  fn set_width(&self, width: u32) -> Self;
  fn set_height(&self, height: u32) -> Self;

  #[law]
  fn preserve_height(&self, any: u32) -> bool {
    self.set_width(any).height() == self.height()
  }
  #[law]
  fn preserve_width(&self, any: u32) -> bool {
    self.set_height(any).width() == self.width()
  }
}
```

The trait defines four methods and states that a change to the width should not
change the height and vice versa. Clearly, the following implementation of a
rectangle holds these properties.

```{.rust caption="Example implementation of the trait."}
struct Rect { width: u32, height: u32 }

impl Rectangle for Rect {
  fn width(&self) -> u32 { self.width }
  fn height(&self) -> u32 { self.height }
  fn set_width(&self, width: u32) -> Self {
    Rect { width, height: self.height }
  }
  fn set_height(&self, height: u32) -> Self {
    Rect { width: self.width, height }
  }
}
```

Without the laws, it would be tempting to add a square implementation. But that
changes both dimensions at once violating the laws. Stainless detects this and
therefore, the square does not pass, the bug is caught.

```{.rust caption="Example implementation violating the laws."}
struct Square { width: u32 }

impl Rectangle for Square {
  fn width(&self) -> u32 { self.width }
  fn height(&self) -> u32 { self.width }
  fn set_width(&self, width: u32) -> Self {
    Square { width }
  }
  fn set_height(&self, height: u32) -> Self {
    Square { width: height }
  }
}
```

# Implementation \label{implementation}

The overall architecture of the code-base has not changed. New features were
implemented as extensions to the existing infrastructure.

## `let` Type Ascriptions and Tuple Pattern Matching [^pr1]

Both features were implemented by simply adding a case in the pattern match
extraction code. Once matched, the subpatterns of both cases are easily dealt
with by recursing once. For tuples, the leaf pattern type
`TyKind::Tuple`[^tuple-path] was needed and for ascriptions the pattern kind
`PatKind::AscribeUserType`[^pat-path].

[^pr1]:
    [PR#60](https://github.com/epfl-lara/rust-stainless/pull/60) and
    [PR#61 on Github](https://github.com/epfl-lara/rust-stainless/pull/61).

[^pat-path]: `rustc_hair:hair:pattern:PatKind:AscribeUserType`
[^tuple-path]: `rustc_middle::ty::TyKind::Tuple`

## Tuple Struct ADTs [^pr7]

[^pr7]: [PR#35 on Github](https://github.com/epfl-lara/rust-stainless/pull/35).

Extraction of Rust's tuple `struct`s was improved by solving a small naming
problem. Stainless's backend Z3 does not allow purely numerical identifiers for
ADT fields, which clashed with the numerical names of tuple struct fields in
Rust, e.g. `example.0`. The solution was to prepend an underscore to such
identifiers: `0` becomes `_0`.

## Immutable Boxes and References by Erasure [^pr3]

[^pr3]:
    [PR#30](https://github.com/epfl-lara/rust-stainless/pull/30) and
    [PR#45 on Github](https://github.com/epfl-lara/rust-stainless/pull/45).

These two features rely on the following assumptions about the allowed Rust
fragment:

- it's impossible to create mutable values or references,
- the only allowed references are immutable borrows and immutable boxes,
- the only allowed binding modes are immutable, aliasable, by-reference (called
  _shared borrows_ by `rustc`) or by-value.

If the above holds, then it should be safe to erase immutable references and
treat them as the objects they refer to. Hence, everywhere an expression like
`ExprKind::Borrow`[^expr-kind] or a type like `TyKind::Ref`[^ty-kind] occurs and
satisfies the above conditions, it is simply extracted to the expression or type
that it contains or refers to. The same goes for calls to `Box::new`.

[^expr-kind]: `rustc_hair::hair::ExprKind::{Borrow, Deref}`
[^ty-kind]: `rustc_middle::ty::TyKind::Ref`

## Measure Attribute [^pr2]

[^pr2]: [PR#31 on Github](https://github.com/epfl-lara/rust-stainless/pull/31).

A design goal of specs, as described in \ref{intro}, is to state their
expressions in Rust code without having an influence on the borrow-checking of
actual the function. To circumvent the borrow checker, the spec attributes are
desugared by a macro to nested functions inside the actual function. The nested
functions duplicate the parameters of their parent. In that way, new bindings
are created without interference but with the same types and identifiers --
simplifying extraction to Stainless's `require` and `ensuring`.

To support the `#[measure()]` attribute, a macro was added, as well as the code
that extracts a `decreases` call it. Most functionality was already present for
conditions and could only be extended by another case.

The only differences between conditions and measure are that there can be at
most one measure per function, which is easily ensured in extraction, and the
return type of the generated measure function can be anything, whereas for
conditions it's always boolean. This poses a problem because macros have no
notion of types, they just see tokens. Therefore, the generated function just
returns `()` by appending a `;` to the expression. Later in extraction, the
_HIR_ will contain the type of that expression and there the type of the measure
is inferred.

## Stainless Set Operations [^pr4]

[^pr4]: [PR#37 on Github](https://github.com/epfl-lara/rust-stainless/pull/37).

The set operations from `libstainless::Set` are detected in function call
extraction as being part of the _standard items_ -- items that are specially
marked and extracted, e.g. some Rust language features like panic. Set operation
calls are simply extracted to their Stainless equivalent.

The described implementation made two disadvantages visible. The first one
concerns the detection of standard items from other crates by `DefId`. The
extraction phase only knows the names/paths of the functions to detect, e.g.
`stainless::Set::singleton`. However, `rustc` does not seem to have an API to
query items from other crates _by name_, only by `DefId`. Therefore, the
frontend has to enumerate all `DefId`s from the `stainless` crate and compare
them by name to the desired ones, until all needed items are found.

The other disadvantage hints at the larger issue of not recognising standard
traits, see \ref{discussion}. While `Set` has a derived implementation of
`PartialEq`, the frontend does not recognise calls to `PartialEq::eq` as being
equivalent to the primitive equality `==` and hence, does not extract them as
Stainless's equality operator. Therefore, to check equality on sets, one has to
resort to using `Set::subset_of` in both directions.

## Implementation Block Methods \label{implblocksref} [^pr5]

[^pr5]: [PR#47 on Github](https://github.com/epfl-lara/rust-stainless/pull/47).

Other than abstract methods and laws, i.e. trait methods covered in
\ref{typecls}, it is not necessary to extract methods on regular `impl` blocks
as Stainless _methods_. As there is no inheritance outside of the type classes
and instances, it is simpler to extract normal methods like top-level functions
that have as first parameter their receiver. In fact, `rustc` also represents
those methods that way, therefore extraction to Stainless functions is
straightforward and can reuse the same code as top-level functions.

A complication arose for the specs that are desugared to nested functions. That
approach failed for methods, because in Rust nested functions cannot access
arguments nor type parameters from the surrounding function, but our methods
need the `self` parameter. To solve the issue, I rely on the restriction that
function/method names in Rust are unique in a scope. Thus, a method is uniquely
identified by its parent `impl` and its name. This makes it possible to desugar
the specs to sibling functions on the same `impl` block and encode the name of
the actual function in the name of the generated sibling spec, as shown in
Listing \ref{sibling}. The number distinguishes multiple specs of the same kind.

```{.rust caption="Attribute becomes a sibling function." label=sibling}
#[post(ret > 0)] // this attribute
fn append(&self, a: i32) -> i32 {}

// becomes:
fn __post_1_append(&self, a: i32, ret: i32) -> bool {
  ret > 0
}
```

This solution has the disadvantage that there are now two separate ways of
encoding specs for functions with Rust macros. At least, both ways share the
same extraction code. And on the other hand, the solution enables specs on
functions that are nested inside a method, as this example shows:

```{.rust caption="Nested function in a method with specs."}
impl NiceStruct {
  fn foo(&self) -> bool {
    #[pre(x > 0 && x < 100 && y > 0 && y < 100)]
    #[post(ret > 0)]
    fn bar(x: i32, y: i32) -> i32 { x * y }
    ...
  }
}
```

## Type Classes and Laws \label{typecls} [^pr6]

[^pr6]:
    These are not all merged yet, they are in review but the code should be
    stable. In order of dependency:
    [PR#877 on Stainless](https://github.com/epfl-lara/stainless/pull/877). On
    the frontend: [PR#57](https://github.com/epfl-lara/rust-stainless/pull/57),
    [PR#58](https://github.com/epfl-lara/rust-stainless/pull/58),
    [PR#59](https://github.com/epfl-lara/rust-stainless/pull/59) and
    [PR#52](https://github.com/epfl-lara/rust-stainless/pull/52) on Github.

### Class Extraction

First, the custom Scala frontend of Stainless used by our pipeline had to be
slightly adapted to ingest _class definitions_ alongside the ADTs and functions
it already supports. Additionally, a new `#[law]` flag was added in all phases
of the pipeline.

Class extraction introduces a distinction between regular `impl` blocks as
discussed in \ref{implblocksref}, and `impl for Trait` blocks that need to be
extracted as type class implementations, i.e. _case classes_ or _case objects_.
Of course, traits have to be extracted as _abstract classes_.

Internally, Rust represents an `impl for Trait` block with a _trait bound_ on
the block, while Scala uses inheritance with `extends`. Furthermore, Rust traits
don't have an explicit type parameter designating the type they act on because
of the intrinsic `Self`, whereas Scala type classes always have at least one
type parameter. Fortunately, `rustc` internally treats the `Self` type parameter
like a regular type parameter, therefore it is already concatenated with
possible other type parameters and the problem is solved. As Listing
\ref{clstranslation} shows, the trait bounds on type parameters of
implementations are converted to evidence parameters. If an implementation has
no type parameters, it can be extracted as a ground case object.

```{.rust caption="Examples of translated type classes." label="clstranslation"}
trait Equals { ... }
// => abstract class Equals[Self]

trait Other<X, Y> { ... }
// => abstract class Other[Self, X, Y]

impl Equals for i32 { ... }
// => case object i32asEquals extends Equals[i32]

impl<T: Equals> Equals for List<T> { ... }
// => case class ListasEquals[T](ev0: Equals[T])
//    extends Equals[List[T]]
```

### Method and Receiver Extraction

Now that there are classes, it is also necessary to distinguish _function calls_
from _method calls_. Most of that distinction could be achieved by adding some
flags like `abstract` and `methodOf(ClassX)` to methods. However, to signal to
Stainless that a method is overriding another one, they need to have the same
symbol, which had to be ensured on the Scala side of the pipeline.

The challenge of method calls is to resolve the receiver instance they are
called on. For example, Listing \ref{code1} shows a call to `this.equals` inside
the type class. In Rust, this is implicitly resolved i.e. trait implementations
just need to be _in scope_ to be used. Therefore, extraction has to resolve
receiver instances itself. To do so, it keeps a map of all method symbols to
their _class definition_. In that way, a method call can be distinguished from a
function call at call site by looking up the function's symbol in the map.

Instance resolution then takes a triple of class identifier, receiver type and
additional types, as wells as the current surrounding class to resolve the
receiver instance. For example, inside a type class, the `this` instance is
accessible, inside classes with evidence parameters, the evidence instances are
available and ground case objects are always in scope. As a last resort,
instance resolution recursively checks whether it can create a new instance of a
class by providing it the required evidence arguments. This happens for example,
if an external function in Listing \ref{code2} called `equals` on a `List<i32>`,
it would get translated to `ListasEquals[i32](i32asEquals).equals`.

### Caveats \label{caveats}

While the described implementation is stable and working, there are some
drawbacks, that are not yet resolved. At the moment it's not possible to add
specs (other than laws) on type class methods, because Rust does not permit
additional methods on trait implementations. This leads to Stainless not being
able to prove one of the properties in the list implementation of Listing
\ref{code2}, because it cannot infer the measure. That problem could be solved
by unifying the way in which specs are encoded by the macros.

The second missing facet is type class inheritance. Stainless uses inheritance
to extend a type class, while Rust uses trait bounds on subtraits. For
simplicity, the frontend could in the future translate these trait bounds to
additional evidence parameters instead of inheritance, for which the
infrastructure is already in place.

The last restriction is due to Rust's type system. Traits cannot have multiple
implementations of the same type. That is a problem if one wants to implement a
`Monoid` type class. For example, one could want two implementations for `i32`,
one for addition, one for multiplication. The Rust work-around for that is to
use marker structs as additional type parameters on the type class, like shown
in the Appendix \ref{monoid2}. That allows the compiler to distinguish between
implementations. Support for that in the Rust-frontend could be implemented by
handling type parameters of top-level functions with evidence parameters and
extending the type parameter extraction of traits to the second level.

# Discussion \label{discussion}

Having discussed the missing parts and drawbacks of the type classes
implementation, I will now turn to the general state and some larger issues of
the Rust-frontend after this project. There are two problems that manifested
themselves multiple times during the project.

Even before the project, it was clear that the borrow-checker and the specs do
not work well together. This was confirmed by the unresolved problem of specs in
type classes (see \ref{caveats}) but it also made expressing some laws on
example type classes nearly impossible if they involved consuming functions. The
question is whether stating the spec expressions in Rust and letting them
type-check is providing more utility than it causes harm? A radically different
approach would be to use a _domain specific language_ for specs, avoiding all
contact with the Rust compiler.

A limitation of the frontend with type classes is that it does not _understand_
Rust's standard library traits. One can now provide separate definitions of such
traits like `Equals` but the Rust-frontend is not capable of verifying
properties on traits that are not part of the currently compiled crate. However,
being able to use actual standard library traits and attaching laws to them
would be an immense productivity gain. This presents an interesting opportunity
for future work.

Some smaller features could provide value without being prohibitively difficult
to implement now. With references, it could be possible to implement some string
operations and expressions. Though, borrow-checking could still be an issue as
long as there is no way of cloning objects. Another small addition would be
projection types. Extracting those would allow methods and functions with
receiver type `T` to be applied to references `&T`.

A bigger challenge for future work providing in turn more utility would be
support for Rust's own arrays, slices or vectors. One could try to extract those
to Stainless's _functional arrays_ instead of using the unidiomatic linked-list
of this report.

Without doubt the most useful feature however, would be support for imperative
code and mutability. A simultaneous project on Stainless explores a _full
imperative phase_ for Scala. Building upon that and extracting imperative Rust
code could therefore be the ultimate goal for the Rust-frontend project.

# Conclusion

Building on a solid foundation of existing infrastructure, in this semester
project I added multiple features to the Rust-frontend of Stainless. Some are
just small improvements for the programmer, others really introduce new
possibilities like the immutable references, the implementation blocks and
especially the type classes.

The given examples show that the current implementation of the type class
extraction is a valid proof-of-concept, even if there are still some rough edges
that need polishing. Indeed, type class inheritance and handling trait bounds on
top-level functions will again improve the utility of type class extraction.

The project also shed light on promising paths for future work. Especially,
making it possible to extract items from other crates or storing already
extracted items for future use could provide good value to users. The other
promising direction is to target the new imperative phase of Scala Stainless.

# References

<!-- prettier-ignore -->
::: {#refs}
:::

\newpage

# Appendix

## Insertion Sort Benchmark \label{insertion}

The original Scala benchmark, as taken from
[the Stainless repository](https://github.com/epfl-lara/stainless/blob/master/frontends/benchmarks/verification/valid/InsertionSort.scala).

```{.scala caption="Insertion Sort in Scala."}
import stainless.annotation._
import stainless.lang._

object InsertionSort {
  sealed abstract class OptInt
  case class Some(value: Int) extends OptInt
  case class None() extends OptInt

  sealed abstract class List
  case class Cons(head:Int, tail:List) extends List
  case class Nil() extends List

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
      case Some(x2) =>
        if(x < x2) Some(x) else Some(x2)
    }
  }
  def isSorted(l: List): Boolean = l match {
    case Cons(x, Cons(y, ys)) =>
      x <= y && isSorted(Cons(y, ys))
    _ => true
  }
  def sortedIns(e: Int, l: List): List = {
    require(isSorted(l))
    l match {
      case Nil() => Cons(e,Nil())
      case Cons(x,xs) =>
        if (x <= e) Cons(x,sortedIns(e, xs))
        else Cons(e, l)
    }
  } ensuring(res =>
    contents(res) == contents(l) ++ Set(e) &&
    isSorted(res) && size(res) == size(l) + 1
  )
  def sort(l: List): List = (l match {
    case Nil() => Nil()
    case Cons(x,xs) => sortedIns(x, sort(xs))
  }) ensuring(res =>
    contents(res) == contents(l) &&
    isSorted(res) && size(res) == size(l)
  )
  @extern
  def main(args: Array[String]): Unit = {
    val ls: List = Cons(5, Cons(2, Cons(4, Cons(5, Cons(1, Cons(8,Nil()))))))
    println(ls)
    println(sort(ls))
  }
}
```

The translated Rust test case, as taken from
[the frontend repository](https://github.com/epfl-lara/rust-stainless/blob/master/stainless_frontend/tests/pass/insertion_sort.rs).

\newpage

```{.rust caption="Insertion Sort in Rust."}
extern crate stainless;
use stainless::*;

pub enum Option<T> { None, Some(T) }
pub enum List<T> { Nil, Cons(T, Box<List<T>>) }

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
      List::Cons(head, tail) =>
        tail.contents().union(&Set::singleton(head))
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
        List::Cons(y, ..) =>
          *x <= *y && tail.is_sorted(),
      },
    }
  }
  #[pre(self.is_sorted())]
  #[measure(self)]
  #[post(
    ret.size() == self.size() + 1 && ret.is_sorted()
    && ret.contents().is_subset_of(
      &self.contents().add(&e)
    ) && self.contents().add(&e).is_subset_of(
      &ret.contents()
  ))]
  pub fn sorted_insert(self, e: i32) -> List<i32> {
    match self {
      List::Cons(head, tail) if head <= e =>
        List::Cons(head,
          Box::new(tail.sorted_insert(e))),
      _ => List::Cons(e, Box::new(self)),
    }
  }
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
pub fn main() {
  let list = List::Cons(5, Box::new(List::Cons(2, Box::new(List::Cons(4, Box::new(List::Cons(5, Box::new(List::Cons(-1, Box::new(List::Cons(8, Box::new(List::Nil))))))))))));
  assert!(list.sort().is_sorted())
}
```

## Monoid Example with Marker Structs \label{monoid2}

```{.rust caption="Example of a monoid type class working around the trait type limitation."}
extern crate stainless;
use stainless::*;

pub trait Op {}
struct Add;
struct Mul;
impl Op for Add {}
impl Op for Mul {}

pub trait Monoid<O: Op>: Sized {
  fn append(&self, other: &Self) -> Self;
  fn neutral() -> Self;

  // This is needed because of the missing type
  // class inheritance. And we need equality in
  // the laws.
  fn equals(&self, other: &Self) -> bool;
  #[law]
  fn assoc(&self, b: &Self, c: &Self) -> bool {
    self.append(b).append(c).equals(
      &self.append(&b.append(c))
    )
  }
  #[law]
  fn left_identity(&self) -> bool {
    Self::neutral().append(self).equals(self)
  }
  #[law]
  fn right_identity(&self) -> bool {
    self.append(&Self::neutral()).equals(self)
  }
}

impl Monoid<Add> for i32 {
  fn append(&self, other: &i32) -> i32 {
    *self + *other
  }
  fn neutral() -> Self { 0 }
  fn equals(&self, other: &i32) -> bool {
    *self == *other
  }
}

impl Monoid<Mul> for i32 {
  fn append(&self, other: &i32) -> i32 {
    *self * *other
  }
  fn neutral() -> Self { 1 }
  fn equals(&self, other: &i32) -> bool {
    *self == *other
  }
}

pub enum List<T> { Nil, Cons(T, Box<List<T>>) }

pub fn combine<O: Op, T: Monoid<O>>(l: &List<T>) -> T {
  match l {
    List::Nil => <T as Monoid<O>>::neutral(),
    List::Cons(h, t) => (*h).append(&combine(&**t)),
  }
}
pub fn main() {
  let list = List::Cons(5, Box::new(List::Cons(2, Box::new(List::Cons(4, Box::new(List::Cons(5, Box::new(List::Cons(-1, Box::new(List::Cons(8, Box::new(List::Nil))))))))))));
  assert!(combine::<Add, _>(&list) == 23);
  assert!(combine::<Mul, _>(&list) == -1600)
}
```
