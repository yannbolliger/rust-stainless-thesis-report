def sorted_insert(self: List[Int], e: Int): List[Int] = {
  require(is_sorted(self))
  decreases(ListPrimitiveSize[Int](self))
  val t: List[Int] = {
    val t: List[Int] = self match {
      case Cons(MutCell(head), MutCell(tail)) if head <= e =>
        Cons[Int](MutCell[Int](head), MutCell[List[Int]](sorted_insert({
          val x: MutCell[List[Int]] = {
            assert({
              assert(self.isInstanceOf[Cons], "Cast error")
              self
            }.isInstanceOf[Cons], "Cast error")
            {
              assert(self.isInstanceOf[Cons], "Cast error")
              self
            }._1
          }
          assert(true, "Cast error")
          x
        }.value, e)))
      case _ =>
        Cons[Int](MutCell[Int](e), MutCell[List[Int]](self))
    }
    assert(t.isInstanceOf[Cons], "Inner refinement lifting")
    t
  }
  val res: List[Int] = {
    val res: List[Int] = t
    assert(res.isInstanceOf[Cons], "Inner refinement lifting")
    res
  }
  res
} ensuring {
  (ret: List[Int]) => {
    val t: Boolean = if (size[Int](ret) == size[Int](self) + 1) {
      is_sorted(ret)
    } else {
      false
    }
    val res: Boolean = t
    val t: Boolean = if (res) {
      contents(ret).subsetOf(contents(self) + e)
    } else {
      false
    }
    val res: Boolean = t
    val t: Boolean = if (res) {
      contents(self) + e.subsetOf(contents(ret))
    } else {
      false
    }
    val res: Boolean = t
    res
  }
}
