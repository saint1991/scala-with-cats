package com.github.saint1991.cats.chapter7

import cats.Monoid
import cats.Applicative
import cats.syntax.applicative._

object Main extends App {

  exercise1()
  exercise2()
  exercise3and4and5()

  def useFoldable(): Unit = {
    import cats.Foldable
    import cats.instances.list._
    import cats.instances.vector._
    import cats.instances.int._

    Foldable[List].foldLeft(List(1, 2, 3), 0)(_ + _)

    import cats.instances.string._
    Foldable[List].foldMap(List(1, 2, 3))(_.toString) // "123"

    (Foldable[List] compose Foldable[Vector]).combineAll(List(Vector(1, 2, 3), Vector(4, 5 ,6)))
  }


  def exercise1(): Unit = {
    val l = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

    // compile error
    // println(l.foldLeft(List.empty[Int])(_ :: _))
    println(l.foldRight(List.empty[Int])(_ :: _))

    println(l.foldLeft (List.empty[Int]) { (acc, x) => x :: acc })
    println(l.foldRight(List.empty[Int]) { (x, acc) => x :: acc })

  }

  def exercise2(): Unit = {

    def map[A, B](l: List[A])(f: A => B): List[B] = l.foldRight(List.empty[B]) { f(_) :: _ }
    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l.foldRight(List.empty[B]) { f(_) ++ _ }
    def filter[A](l: List[A])(f: A => Boolean): List[A] = l.foldRight(List.empty[A]) { (x, acc) => if (f(x)) x :: acc else acc }
    def sum[A: Monoid](l: List[A]): A = l.foldRight(implicitly[Monoid[A]].empty)(implicitly[Monoid[A]].combine(_, _))

    import cats.instances.int._
    val l = List(1, 2, 3)
    println(map(l)(_ + 1))
    println(flatMap(l)(x => List(x + 1, x + 2)))
    println(filter(l)(_ % 2 == 1))
    println(sum(l))
  }

  def exercise3and4and5(): Unit = {

    import cats.syntax.apply._

    def listTraverse[F[_]: Applicative, A, B](list: List[A])(f: A => F[B]): F[List[B]] =
      list.foldLeft(List.empty[B].pure[F]) { (acc, item) => (acc, f(item)).mapN(_ :+ _) }

    def listSequence[F[_]: Applicative, B](list: List[F[B]]): F[List[B]] =
      listTraverse(list)(identity)

    import cats.instances.vector._

    // Vector(List(1, 3), List(1, 4), List(2, 3), List(2, 4))
    println(listSequence(List(Vector(1, 2), Vector(3, 4))))

    // Vector(List(1, 3, 5), List(1, 3, 6), List(1, 4, 5), List(1, 4, 6), List(2, 3, 5), List(2, 3, 6), List(2, 4, 5), List(2, 4, 6))
    println(listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6))))

    import cats.instances.option._

    def process(inputs: List[Int]): Option[List[Int]] = listTraverse(inputs)(n => if (n % 2 == 0) Some(n) else None)

    // Some(List(2, 4, 6))
    println(process(List(2, 4, 6)))

    // None
    println(process(List(1, 2, 3)))


    import cats.data.Validated
    import cats.instances.all._
    type ErrorsOr[A] = Validated[List[String], A]

    def processValidated(inputs: List[Int]): ErrorsOr[List[Int]] = listTraverse(inputs) { n =>
      if (n % 2 == 0) Validated.valid(n) else Validated.invalid(List(s"$n is not even"))
    }

    // Valid(List(2, 4, 6))
    println(processValidated(List(2, 4, 6)))

    // Invalid(List(1 is not even, 3 is not even))
    println(processValidated(List(1, 2, 3)))

  }


}
