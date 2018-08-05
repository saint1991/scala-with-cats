package com.github.saint1991.cats.chapter10

import cats.Semigroup
import cats.data.{Kleisli, NonEmptyList, Validated, ValidatedNel}
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

object Exercise3 extends App {

  import cats.syntax.apply._
  import cats.syntax.semigroup._
  import cats.instances.all._

  final case class And[E, A](
    left: Predicate[E, A],
    right: Predicate[E, A]
  ) extends Predicate[E, A]

  final case class Or[E, A](
    left: Predicate[E, A],
    right: Predicate[E, A]
  ) extends Predicate[E, A]

  final case class Pure[E, A](f: A => Validated[E, A]) extends Predicate[E, A]

  trait Predicate[E, A] {

    def and(that: Predicate[E, A]): Predicate[E, A] = And(this, that)

    def or(that: Predicate[E, A]): Predicate[E, A] = Or(this, that)

    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] = this match {
      case Pure(f) => f(a)
      case And(left, right) => (left(a), right(a)).mapN((_, _) => a)
      case Or(left, right) => left(a) match {
        case v: Valid[E] => v
        case Invalid(e1) => right(a) match {
          case iv: Valid[E] => iv
          case Invalid(e2) => Invalid(e1 |+| e2)
        }
      }
    }

    def run: A => Validated[E, A] = apply

  }

  object Predicate {
    def lift[E, A](error: E, predicate: A => Boolean): Predicate[E, A] =
      Pure { a => if (predicate(a)) Valid(a) else Invalid(error) }
  }


  case class Transform[E, A, B](pred: Predicate[E, A], f: A => B) extends Check[E, A, B]

  sealed trait Check[E, A, B] { self =>

    import cats.syntax.applicative._

    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, B] = this match {
      case Transform(pred, f) => pred(a).map(f)
    }

    def map[C](f: B => C)(implicit s: Semigroup[E]): Check[E, A, C] = new Check[E, A, C] {
      override def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] = self(a).map(f)
    }

    def flatMap[C](f: B => Check[E, A, C])(implicit s: Semigroup[E]): Check[E, A, C] = new Check[E, A, C]{
      override def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] = self(a) match {
        case iv @ Invalid(_) => iv
        case Valid(b) => f(b).apply(a)
      }
    }

    def andThen[C](that: Check[E, B, C])(implicit s: Semigroup[E]): Check[E, A, C] = new Check[E, A, C] {
      override def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] = self(a) match {
        case iv @ Invalid(_) => iv
        case Valid(b) => that(b)
      }
    }
  }

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


  private def useCheck(): Unit = {

    val usernameCheck: Check[Errors, String, String] = Transform(longerThan(4) and alphanumeric, identity)

    val emailWholeCheck: Check[Errors, String, (String, String, String)] = Transform(containsOnce('@'), email => email.split('@') match {
      case Array(left, right) => (left, right, email)
    })
    val emailLeftCheck: Check[Errors, String, String] = Transform(longerThan(0), identity)
    val emailRightCheck: Check[Errors, String, String] = Transform(longerThan(3) and containsOnce('.'), identity)

    val shortUserName = "foo"
    val nonAlphaNumericUserName = "@foobar"
    val nonAlphaNumericShortUserName = "@f"

    println(usernameCheck(shortUserName))
    println(usernameCheck(nonAlphaNumericUserName))
    println(usernameCheck(nonAlphaNumericShortUserName))
  }

  private def useKleisli(): Unit = {

    import cats.instances.all._

    type Nel[A] = Validated[Errors, A]

    val usernameCheck = Kleisli((longerThan(4) and alphanumeric).run)

    val shortUserName = "foo"
    val nonAlphaNumericUserName = "@foobar"
    val nonAlphaNumericShortUserName = "@f"

    println(usernameCheck(shortUserName))
    println(usernameCheck(nonAlphaNumericUserName))
    println(usernameCheck(nonAlphaNumericShortUserName))
  }

  useCheck()
  useKleisli()
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