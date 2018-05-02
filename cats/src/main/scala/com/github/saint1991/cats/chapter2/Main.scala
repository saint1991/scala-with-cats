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

  implicit val and: Monoid[Boolean] = new Monoid[Boolean] {
    override def combine(x: Boolean, y: Boolean): Boolean = x && y
    override def empty: Boolean = true
  }
  implicit val or: Monoid[Boolean]  = new Monoid[Boolean] {
    override def combine(x: Boolean, y: Boolean): Boolean = x || y
    override def empty: Boolean = false
  }
  implicit val xor: Monoid[Boolean] = new Monoid[Boolean] {
    override def combine(x: Boolean, y: Boolean): Boolean = (x && !y) || (!x && y)
    override def empty: Boolean = false
  }
  implicit val xnor: Monoid[Boolean] = new Monoid[Boolean] {
    override def combine(x: Boolean, y: Boolean): Boolean = (x || !y) && (!x || y)
    override def empty: Boolean = true
  }
}

object Exercise2 {

  object Monoid {
    implicit val union: Monoid[Set[Int]] = new Monoid[Set[Int]] {
      override def combine(x: Set[Int], y: Set[Int]): Set[Int] = x union y
      override def empty: Set[Int] = Set.empty
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

  def main(): Unit = {
    import cats.instances.option._
    import cats.syntax.option._
    import cats.instances.int._
    println(add(List(1.some, 2.some, none[Int])))
  }

  def add[T](items: List[T])(implicit monoid: CMonoid[T]): T = items.foldLeft(monoid.empty) {
    (acc, item) => monoid.combine(acc, item)
  }

  case class Order(totalCost: Double, quantity: Double)
  implicit val orderMonoid: CMonoid[Order] = new CMonoid[Order] {
    override def combine(x: Order, y: Order): Order = Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
    override def empty: Order = Order(0.0, 0.0)
  }
}