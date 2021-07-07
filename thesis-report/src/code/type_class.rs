extern crate stainless;
use stainless::*;

pub enum List<T> {
  Nil,
  Cons(T, Box<List<T>>),
}

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

impl<T: Equals> Equals for List<T> {
  fn equals(&self, other: &List<T>) -> bool {
    match (self, other) {
      (List::Nil, List::Nil) => true,
      (List::Cons(x, xs), List::Cons(y, ys)) => x.equals(y) && xs.equals(ys),
      _ => false,
    }
  }

  fn law_reflexive(x: &Self) -> bool {
    match x {
      List::Cons(x, xs) => T::law_reflexive(x) && Self::law_reflexive(xs),
      List::Nil => true,
    }
  }

  fn law_symmetric(x: &Self, y: &Self) -> bool {
    match (x, y) {
      (List::Cons(x, xs), List::Cons(y, ys)) => {
        T::law_symmetric(x, y) && Self::law_symmetric(xs, ys)
      }
      _ => true,
    }
  }

  fn law_transitive(x: &Self, y: &Self, z: &Self) -> bool {
    match (x, y, z) {
      (List::Cons(x, xs), List::Cons(y, ys), List::Cons(z, zs)) => {
        T::law_transitive(x, y, z) && Self::law_transitive(xs, ys, zs)
      }
      _ => true,
    }
  }
}

impl Equals for i32 {
  fn equals(&self, y: &i32) -> bool {
    // Deref integers to force primitive comparison operator
    *self == *y
  }
}

pub fn main() {
  let a = 2;
  let b = 4;

  assert!(a.not_equals(&b));

  let list = List::Cons(123, Box::new(List::Cons(456, Box::new(List::Nil))));
  assert!(list.equals(&list));
}
