@pure def sorted_insert(
  self: List[Int],
  e: Int
): List[Int] = {
  require(is_sorted(self))
  decreases({ self })
  self match {
    case Cons(MutCell(head), MutCell(tail))
      if head <= e => freshCopy(
          Cons[Int](
            MutCell[Int](head),
            MutCell[List[Int]](
              sorted_insert(tail, e)
            )
          )
        )
    case _ => freshCopy(
        Cons[Int](
          MutCell[Int](e),
          MutCell[List[Int]](self)
        )
      )
  }
} ensuring { (ret: List[Int]) =>
  size[Int](ret) == size[Int](self) + 1 &&
  is_sorted(ret) && contents(ret).subsetOf(
    contents(self) ++ Set(e)
  ) &&
  (contents(self) ++ Set(e)) subsetOf contents(ret)
}
