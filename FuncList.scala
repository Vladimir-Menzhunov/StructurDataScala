package ru.philit.bigdata.vsu.scalaLang.funcStrucut

sealed trait FList[+A] {

  def head: A

  def tail: FList[A]

}

object FList {
  def tail[A](l: FList[A]): FList[A] = l match {
    case FNil => throw new Exception("Tail of empty list")
    case Cons(_, t) => t
  }

  def apply[A](values: A*): FList[A] = if (values.nonEmpty)
    values.foldLeft(FNil: FList[A])((acc, el) => acc.append(el))
  else FNil

  implicit class FListImpl[A](list: FList[A]) {
    def append(item: A): FList[A] = Cons(item, list)

    def drop(n: Int): FList[A] = list match {
      case FNil => FNil
      case Cons(_, t) => if(n > 0) t.drop(n - 1) else list
    }

    def reverse: FList[A] = foldLeft(FNil: FList[A])((acc, h) => Cons(h, acc))

    def foldLeft[B](acc: B)(f: (B, A) => B): B = list match {
      case FNil => acc
      case Cons(h, t) => t.foldLeft(f(acc, h))(f)
    }

    def foldRight[B](acc: B)(f: (A, B) => B): B = list match {
      case FNil =>
        print("Fnil")
        acc
      case Cons(h, t) => f(h,  t.foldRight(acc)(f))
    }

    def scalaFoldRight[B](acc: B)(f: (A, B) => B): B =
      list.reverse.foldLeft(acc)((x, y) => f(y, x))

    def map[B](f: A => B): FList[B] = list match {
      case FNil => FNil
      case Cons(h, t) => foldRight(FNil: FList[B])((el, acc) => Cons(f(el), acc))
    }

    def flatten[A](list: FList[FList[A]]): FList[A] = list.foldRight(FNil: FList[A])((acc, l) => acc.concat(l))

    def flatMap[B](f: A => FList[B]): FList[B] =
      flatten(list.map(f))

    def length: Int = list match {
      case FNil => 0
      case Cons(h, t) => list.foldRight(0)((_ , acc) => acc + 1)
    }

    def concat(another: FList[A]): FList[A] = list.foldRight(another)(Cons(_, _))
  }

  implicit def optionToFList[A](op: Option[A]): FList[A] = op match {
    case Some(value) => FList(value)
    case None => FNil
  }


}

case object FNil extends FList[Nothing] {

  override def toString: String = "FNil"

  override def tail: FList[Nothing] = throw new Exception("Tail of empty list")

  override def head: Nothing = throw new Exception("Head of empty list")
}

case class Cons[+A](head: A, tail: FList[A]) extends FList[A] {
  override def toString: String = s"Cons($head, $tail)"
}
