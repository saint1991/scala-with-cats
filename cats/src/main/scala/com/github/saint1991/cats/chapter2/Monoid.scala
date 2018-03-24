package com.github.saint1991.cats.chapter2

trait Monoid[T] extends Semigroup[T] {
  def identity: T
}

object Monoid {
  implicit val intsSum: Monoid[Int] = new Monoid[Int] {
    override def combine(x: Int, y: Int): Int = x + y
    override def identity: Int = 0
  }
}
