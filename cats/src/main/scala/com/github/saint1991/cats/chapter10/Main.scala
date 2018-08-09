package com.github.saint1991.cats.chapter10

import scala.annotation.tailrec

import cats.Semigroup
import cats.data.{NonEmptyList, Validated}
import cats.data.Validated.{Invalid, Valid}


object Exercise1 {

  import cats.Semigroup
  import cats.syntax.semigroup._

  final case class And[E, A](
    left: Check[E, A],
    right: Check[E, A]
  ) extends Check[E, A]

  final case class Or[E, A](
    left: Check[E, A],
    right: Check[E, A]
  ) extends Check[E, A]

  final case class Pure[E, A](
    func: A => Either[E, A]
  ) extends Check[E, A]

  sealed trait Check[E, A] { self =>

    def and(that: Check[E, A]): Check[E, A] = And[E, A](self, that)

    def or(that: Check[E, A]): Check[E, A] = Or[E, A](self, that)

    def apply(a: A)(implicit s: Semigroup[E]): Either[E, A] = this match {
      case And(left, right) => (left(a), right(a)) match {
        case (Left(e1), Left(e2)) => Left(e1 |+| e2)
        case (Left(e1), _) => Left(e1)
        case (_, Left(e2)) => Left(e2)
        case _ => Right(a)
      }
      case Or(left, right) => left(a) match {
        case a @ Right(_) => a
        case Left(e1) => right(a) match {
          case b @ Right(_) => b
          case Left(e2) => Left(e1 |+| e2)
        }
      }
      case Pure(f) => f(a)
    }
  }
}

object Exercise2 {

  import cats.syntax.apply._

  final case class And[E, A](
    left: Check[E, A],
    right: Check[E, A]
  ) extends Check[E, A]

  final case class Or[E, A](
    left: Check[E, A],
    right: Check[E, A]
  ) extends Check[E, A]

  final case class Pure[E, A](
    func: A =>Validated[E, A]
  ) extends Check[E, A]

  trait Check[E, A] {

    def and(that: Check[E, A]): Check[E, A] = And(this, that)

    def or(that: Check[E, A]): Check[E, A] = Or(this, that)

    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] = this match {
      case Pure(f) => f(a)
      case And(left, right) => (left(a), right(a)).mapN((_, _) => a)
      case Or(left, right) => left(a) match {
        case v: Valid[E] => v
        case Invalid(e1) => right(a) match {
          case iv: Valid[E] => iv
          case Invalid(e2) => Invalid(s.combine(e1, e2))
        }
      }

    }

  }
}

object Exercise3 {

  import cats.syntax.apply._
  import cats.syntax.semigroup._

  object Predicate {

    final case class And[E, A](
      left: Predicate[E, A],
      right: Predicate[E, A]
    ) extends Predicate[E, A]

    final case class Or[E, A](
      left: Predicate[E, A],
      right: Predicate[E, A]
    ) extends Predicate[E, A]

    final case class Pure[E, A](f: A => Validated[E, A]) extends Predicate[E, A]

    final case class True[E, A]() extends Predicate[E, A]

    def lift[E, A](error: E, predicate: A => Boolean): Predicate[E, A] =
      Pure { a => if (predicate(a)) Valid(a) else Invalid(error) }
  }

  trait Predicate[E, A] {

    import Predicate._
    import cats.data.Validated._

    def and(that: Predicate[E, A]): Predicate[E, A] = And(this, that)

    def or(that: Predicate[E, A]): Predicate[E, A] = Or(this, that)

    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] = this match {
      case True() => Valid(a)
      case Predicate.Pure(f) => f(a)
      case And(left, right) => (left(a), right(a)).mapN((_, _) => a)
      case Or(left, right) => left(a) match {
        case v: Valid[E] => v
        case Invalid(e1) => right(a) match {
          case iv: Valid[E] => iv
          case Invalid(e2) => Invalid(e1 |+| e2)
        }
      }
    }

    def run()(implicit s: Semigroup[E]): A => Validated[E, A] = apply
  }


  object Check {

    def apply[E, A, B](f: A => Validated[E, B])(implicit s: Semigroup[E]): Check[E, A, B] = new Check[E, A, B] {
      override def apply(a: A)(implicit s: Semigroup[E]): Validated[E, B] = f(a)
    }

    final case class Pure[E, A, B](predicate: Predicate[E, A]) extends Check[E, A, A] {
      override def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] = predicate(a)
    }
    final case class Map[E, A, B, C](c: Check[E, A, B], f: B => C) extends Check[E, A, C] {
      override def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] = c(a).map(f)
    }
    final case class FlatMap[E, A, B, C](c: Check[E, A, B], f: B => Check[E, A, C]) extends Check[E, A, C] {
      override def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] = c(a).withEither(_.flatMap(f(_)(a).toEither))
    }
    final case class AndThen[E, A, B, C](c: Check[E, A, B], following: Check[E, B, C]) extends Check[E, A, C] {
      override def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] = c(a).withEither(_.flatMap(following(_).toEither))
    }
  }

  sealed trait Check[E, A, B] { self =>

    import Check._

    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, B]

    def map[C](f: B => C): Check[E, A, C] = Map(this, f)

    def flatMap[C](f: B => Check[E, A, C]): Check[E, A, C] = FlatMap(this, f)

    def andThen[C](that: Check[E, B, C]): Check[E, A, C] = AndThen(this, that)
  }

}

object Utilities {
  import Exercise3._

  final case class User(username: String, email: String)
  type Errors = NonEmptyList[String]

  def error(s: String) = NonEmptyList(s, Nil)

  def longerThan(n: Int): Predicate[Errors, String] =
    Predicate.lift(error(s"Must be longer than $n characters"), _.length > n)

  val alphanumeric: Predicate[Errors, String] =
    Predicate.lift(error("Must be all alphanumeric characters"), _.forall(_.isLetterOrDigit))

  def contains(char: Char): Predicate[Errors, String] =
    Predicate.lift(error(s"Must contain the character $char"), _.contains(char))

  def containsOnce(char: Char): Predicate[Errors, String] =
    Predicate.lift(error(s"Must contain the character $char only once"), _.count(_ == char) == 1)
}

object Exercise4 extends App {

  import cats.syntax.apply._
  import Utilities._
  import Exercise3._

  val usernameCheck: Check[Errors, String, String] = Check.Pure(longerThan(3) and alphanumeric)

  val emailLeftCheck = Check.Pure(longerThan(0))
  val emailRightCheck = Check.Pure(longerThan(3) and containsOnce('.'))
  val emailSplitCheck: Check[Errors, String, (String, String, String)] = Check.Pure(containsOnce('@')).map {
    email => email.split('@') match {
      case Array(left, right) => (left, right, email)
    }
  }
  val emailCheck: Check[Errors, String, String] = emailSplitCheck andThen[String] {
    Check { case (left: String, right: String, email: String) =>
      (emailLeftCheck(left), emailRightCheck(right)).mapN((_, _) => email)
    }
  }

  def userCheck(username: String, email: String): Validated[Errors, User] =
    (usernameCheck(username), emailCheck(email)).mapN(User.apply)

  println(userCheck("foobar", "foobar@gmail.com"))
  println(userCheck("foo", "foobar@gmail.com"))
  println(userCheck("@foobar", "foobar@gmail.com"))
  println(userCheck("@f", "foobar@gmail.com"))
  println(userCheck("foobar", "foobarcom"))
  println(userCheck("foobar", "@gmail.com"))
  println(userCheck("foobar", "foobar@gmailcom"))
  println(userCheck("foobar", "foobar@.c"))

}

object Exercise5 extends App {
  import Utilities._

  import cats.FlatMap
  import cats.data.Kleisli
  import cats.syntax.apply._

  type Result[A] = Validated[Errors, A]
  type Check[A, B] = Kleisli[Result, A, B]

  implicit val resultFlatMapInstance: FlatMap[Result] = new FlatMap[Result] {
    override def flatMap[A, B](fa: Result[A])(f: A => Result[B]): Result[B] = fa match {
      case iv @ Invalid(_) => iv
      case Valid(a) => f(a)
    }

    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Result[Either[A, B]]): Result[B] = f(a) match {
      case iv @ Invalid(_) => iv
      case Valid(Right(b)) => Valid(b)
      case Valid(Left(l)) => tailRecM(l)(f)
    }

    override def map[A, B](fa: Result[A])(f: A => B): Result[B] = fa.map(f)
  }

  def check[A, B](f: A => Result[B]): Check[A, B] = Kleisli(f)

  val usernameCheck = check( (longerThan(4) and alphanumeric).run() )

  val emailLeftCheck = check( longerThan(0).run() )
  val emailRightCheck = check( (longerThan(3) and containsOnce('.')).run() )
  val emailCheck = check( containsOnce('@').run() ).map { email =>
    email.split('@') match {
      case Array(left, right) => (left, right, email)
    }
  } andThen {
    check[(String, String, String), String] { case (left: String, right: String, email: String) =>
      (emailLeftCheck(left), emailRightCheck(right)).mapN((_, _) => email)
    }
  }

  def userCheck(username: String, email: String): Validated[Errors, User] =
    (usernameCheck(username), emailCheck(email)).mapN(User.apply)

  println(userCheck("foobar", "foobar@gmail.com"))
  println(userCheck("foo", "foobar@gmail.com"))
  println(userCheck("@foobar", "foobar@gmail.com"))
  println(userCheck("@f", "foobar@gmail.com"))
  println(userCheck("foobar", "foobarcom"))
  println(userCheck("foobar", "@gmail.com"))
  println(userCheck("foobar", "foobar@gmailcom"))
  println(userCheck("foobar", "foobar@.c"))

}


object KleisliExample extends App {
  import cats.data.Kleisli
  import cats.instances.list._

  val step1 = Kleisli[List, Int, Int](x => List(x + 1, x - 1))
  val step2 = Kleisli[List, Int, Int](x => List(x, -x))
  val step3 = Kleisli[List, Int, Int](x => List(x * 2, x / 2))

  val pipeline = step1 andThen step2 andThen step3
  val result = pipeline.run(20)
  println(result)
}

// List(20)
// List(21, 19)
// List(21, -21, 19, -19)
// List(42, 10, -42, -10, 38, 9 , -38, -9)