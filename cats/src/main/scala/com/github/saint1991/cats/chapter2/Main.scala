package com.github.saint1991.cats.chapter2

//import cats.{Semigroup => CSemigroup}
import cats.{Monoid => CMonoid}

object Main extends App {

  catsExample()

  private def catsExample(): Unit = {
    import cats.instances.string._
    import cats.instances.option._
    println(CMonoid[String].combine("Hello ", "cats"))
    println(CMonoid[Option[String]].combine(Some("Hello "), None))
    println(CMonoid[Option[String]].empty)

    import cats.syntax.semigroup._
    // |+| means Semigroup#combine
    println("Hello " |+| "cats" |+| "!!")
  }

}


object Exercise1 {

  // There are 2 Monoids for Boolean
  // Xor does not have identity element so that it is Semigroup.
  implicit val and: Monoid[Boolean] = new Monoid[Boolean] {
    override def combine(x: Boolean, y: Boolean): Boolean = x && y
    override def identity: Boolean = true
  }
  implicit val or: Monoid[Boolean]  = new Monoid[Boolean] {
    override def combine(x: Boolean, y: Boolean): Boolean = x || y
    override def identity: Boolean = false
  }
}

object Exercise2 {

  object Monoid {
    implicit val union: Monoid[Set[Int]] = new Monoid[Set[Int]] {
      override def combine(x: Set[Int], y: Set[Int]): Set[Int] = x union y
      override def identity: Set[Int] = Set.empty
    }
  }

  object Semigroup {
    implicit val intersection: Semigroup[Set[Int]] = {
      (x: Set[Int], y: Set[Int])=> x intersect y
    }
  }

}

object Exercise3 {
  import cats.{Monoid => CMonoid}
  def add[T](items: List[T])(implicit monoid: CMonoid[T]): T = items.foldLeft(monoid.empty) {
    (acc, item) => monoid.combine(acc, item)
  }

  case class Order(totalCost: Double, quantity: Double)
  implicit val orderMonoid: CMonoid[Order] = new CMonoid[Order] {
    override def combine(x: Order, y: Order): Order = Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
    override def empty: Order = Order(0.0, 0.0)
  }
}