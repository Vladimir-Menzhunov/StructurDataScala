package ru.philit.bigdata.vsu.scalaLang.funcStrucut

trait FuncSetOperations {
  type FunSet[A] = A => Boolean

  def contains[A](set: FunSet[A], elem: A): Boolean

  def singletonSet[A](elem: A): FunSet[A]

  def union[A](set: FunSet[A], another: FunSet[A]): FunSet[A]

  def intersect[A](set: FunSet[A], another: FunSet[A]): FunSet[A]

  def diff[A](set: FunSet[A], another: FunSet[A]): FunSet[A]

  def filter[A](set: FunSet[A], f: A => Boolean): FunSet[A]

  def map[A <: AnyVal, B <: AnyVal](set: FunSet[A], f: A => B)(range: Seq[A]): FunSet[B]

  def forAll[A <: AnyVal](set: FunSet[A], f: A => Boolean)(range: Seq[A]): Boolean

  def exists[A <: AnyVal](set: FunSet[A], f: A => Boolean)(range: Seq[A]): Boolean

  def asString[A <: AnyVal](set: FunSet[A])(range: Seq[A]): String

}

object FuncSetImpl extends FuncSetOperations {

  override def contains[A](set: FuncSetImpl.FunSet[A], elem: A): Boolean = set(elem)

  override def singletonSet[A](elem: A): FunSet[A] = x => x == elem

  override def union[A](set: FuncSetImpl.FunSet[A], another: FuncSetImpl.FunSet[A]): FuncSetImpl.FunSet[A] =
    x => contains(set, x) || contains(another, x)

  override def intersect[A](set: FuncSetImpl.FunSet[A], another: FuncSetImpl.FunSet[A]): FuncSetImpl.FunSet[A] =
    x => set(x) && another(x)

  override def diff[A](set: FuncSetImpl.FunSet[A], another: FuncSetImpl.FunSet[A]): FuncSetImpl.FunSet[A] =
    x => set(x) && !another(x) /*|| (another(x) && !set(x))*/

  override def filter[A](set: FuncSetImpl.FunSet[A], f: A => Boolean): FuncSetImpl.FunSet[A] =
    x => set(x) && f(x)

  override def map[A <: AnyVal, B <: AnyVal](set: FuncSetImpl.FunSet[A], f: A => B)(range: Seq[A]): FuncSetImpl.FunSet[B] =
    x => exists[A](set, value => x == f(value))(range)


  override def forAll[A <: AnyVal](set: FuncSetImpl.FunSet[A], f: A => Boolean)(range: Seq[A]): Boolean = {
    val bounds = range.toIterator
    def loop(elem: A): Boolean = if(set(elem) && !f(elem)) false
      else if(bounds.hasNext) loop(bounds.next()) else true

    loop(bounds.next())
  }

  override def exists[A <: AnyVal](set: FuncSetImpl.FunSet[A], f: A => Boolean)(range: Seq[A]): Boolean =
    !forAll[A](set, x => !f(x))(range)

  override def asString[A <: AnyVal](set: FuncSetImpl.FunSet[A])(range: Seq[A]): String = {
    val bounds = range.toIterator
    def loop(acc: String): String = if(bounds.hasNext) {
      val element = bounds.next()
      loop(if(contains(set, element)) s"$acc $element" else acc)
    } else s"$acc ]"

    loop("[")
  }


}
