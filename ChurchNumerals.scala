package ru.philit.bigdata.vsu.scalaLang.funcStrucut

import java.util

object ChurchNumerals extends App {
  type Func[A] = A => A
  type Church[A] = Func[A] => A => A

  def zero[A]: Church[A] = (f: Func[A]) => (a: A) => a
  def increment[A](num: Church[A]): Church[A] = f => f compose num(f)

  def one[A]: Church[A] = increment(zero)

  def two[A]: Church[A] = increment(increment(zero))


  def plus[A](x: Church[A], y: Church[A]): Church[A] =
    f => x(f) compose y(f)

  def asInt(num: Church[Int]): Int = num((x: Int) => x + 1)(0)

  println(asInt(zero[Int]))

  println(asInt(one[Int]))

  println(asInt(plus[Int](one[Int], one[Int])))

}