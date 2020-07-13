package bigData
sealed trait Tree[+A] {
  def size: Int

  //Максимальное расстояние от корня до листа
  def depth: Int

  def map[B](f: A => B): Tree[B]

  def fold[B](map: A => B)(reduce: (B, B) => B): B
}


case class Leaf[A](value: A) extends Tree[A] {
  override def size: Int = 1

  override def depth: Int = 1

  override def map[B](f: A => B): Tree[B] = Leaf(f(value))

  override def fold[B](map: A => B)(reduce: (B, B) => B): B = map(value)


}

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A] {
  override def size: Int = 1 + left.size + right.size

  override def depth: Int = left.depth.max(right.depth) + 1

  override def map[B](f: A => B): Tree[B] =
    Branch(left.map(f), right.map(f))

  override def fold[B](map: A => B)(reduce: (B, B) => B): B =
    reduce(left.fold(map)(reduce), right.fold(map)(reduce))
}
