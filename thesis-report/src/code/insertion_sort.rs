extern crate stainless;
use stainless::*;

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
}

impl List<i32> {
  #[measure(self)]
  pub fn contents(&self) -> Set<i32> {
    match self {
      List::Nil => Set::new(),
      List::Cons(head, tail) => tail.contents().insert(*head),
    }
  }

  #[measure(self)]
  pub fn is_sorted(&self) -> bool {
    match self {
      List::Nil => true,
      List::Cons(x, tail) => match &**tail {
        List::Nil => true,
        // Deref integers to force primitive comparison operator
        List::Cons(y, ..) => *x <= *y && tail.is_sorted(),
      },
    }
  }

  #[measure(self)]
  pub fn min(&self) -> Option<i32> {
    match self {
      List::Nil => None,
      List::Cons(x, xs) => match xs.min() {
        None => Some(*x),
        Some(y) if *x < y => Some(*x),
        Some(y) => Some(y),
      },
    }
  }

  /// Inserting element 'e' into a sorted list 'l' produces a sorted
  /// list with the expected content and size
  #[pre(self.is_sorted())]
  #[measure(self)]
  #[post(
    ret.size() == self.size() + 1 &&
    ret.is_sorted() &&
    ret.contents().is_subset(&self.contents().insert(e)) &&
    self.contents().insert(e).is_subset(&ret.contents())
  )]
  pub fn sorted_insert(self, e: i32) -> List<i32> {
    match self {
      List::Cons(head, tail)
        if head <= e => List::Cons(head, Box::new(tail.sorted_insert(e))),
      _ => List::Cons(e, Box::new(self)),
    }
  }

  /// Insertion sort yields a sorted list of same size and content
  /// as the input list
  #[measure(self)]
  #[post(
    ret.size() == self.size() &&
    ret.is_sorted() &&
    ret.contents().is_subset(&self.contents()) &&
    self.contents().is_subset(&ret.contents())
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
          Box::new(List::Cons(
            -1,
            Box::new(List::Cons(8, Box::new(List::Nil)))
          )),
        )),
      )),
    )),
  );
  assert!(list.sort().is_sorted())
}
