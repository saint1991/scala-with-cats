package com.github.saint1991.cats.chapter6

import scala.util.Try

import cats.Semigroupal
import cats.data.Validated
import cats.instances.option._
import cats.instances.list._
import cats.instances.either._


object Main extends App {

  useSemigroupal()
  useApplicative()
  exercise1()
  exercise2()

  private def useSemigroupal(): Unit = {

    println(Semigroupal[Option].product(Option(1), Option("foo")))
    println(Semigroupal.tuple3(Option(1), Option(19), Option(2)))
    println(Semigroupal.map3(Option(1), Option(1), Option(2))(_ + _ + _))

    import cats.syntax.apply._
    import cats.instances.option._

    println((Option(1), None).tupled)

    case class Cat(name: String, age: Int)

    println((Option("mike"), Option(18)).mapN(Cat.apply))

    // List((1, 3), (1, 4), (2, 3), (2, 4))
    println(Semigroupal[List].product(List(1, 2), List(3, 4)))

    // Left(Vector(Error 1))
    type ErrorOr[A] = Either[Vector[String], A]
    println(Semigroupal[ErrorOr].product(Left(Vector("Error 1")), Right(1)))

    // Left(Vector(Error 1)) <- fail fast
    println(Semigroupal[ErrorOr].product(Left(Vector("Error 1")), Left(Vector("Error 2"))))
    cats.Eval
  }

  private def useApplicative(): Unit = {

  }


  private def exercise1(): Unit = {

    import cats.Monad
    import cats.syntax.flatMap._
    import cats.syntax.functor._
    def product[M[_]: Monad, A, B](x: M[A], y: M[B]): M[(A, B)] = for {
      a <- x
      b <- y
    } yield (a, b)
  }

  private def exercise2(): Unit = {

    import cats.syntax.apply._
    import cats.syntax.either._

    case class User(name: String, age: Int)
    type FormInput = Map[String, String]

    def readName(form: FormInput): Either[List[String], String] =
      Either.fromOption(form.get("name"), "name is required" :: Nil)
        .ensure("name must not be blank" :: Nil)(_.nonEmpty)

    def readAge(form: FormInput): Either[List[String], Int] =
      Either.fromOption(form.get("age"), "age is required" :: Nil)
        .map(age => Try(age.toInt).filter(_ >= 0))
        .ensure("age must be a non negative integer" :: Nil)(_.isSuccess)
        .map(_.get)

    def validate(form: FormInput): Validated[List[String], User] =
      (readName(form).toValidated, readAge(form).toValidated).mapN(User.apply)

    println(validate(Map.empty))
    println(validate(Map("name" -> "Mike")))
    println(validate(Map("age" -> "18")))
  }

  private def exercise3(): Unit = {

    import cats.syntax.apply._
    import cats.syntax.either._

    case class User(name: String, age: Int)
    type FormData = Map[String, String]
    type FailFast[A] = Either[List[String], A]
    type FailSlow[A] = Validated[List[String], A]
    type NumFmtExn = NumberFormatException

    def getValue(name: String)(form: FormData): Either[List[String], String] =
      Either.fromOption(form.get("name"), "name is required" :: Nil)

    def parseInt(name: String)(data: String): FailFast[Int] =
      Either.catchOnly[NumFmtExn](data.toInt).
        leftMap(_ => List(s"$name must be an integer"))

    def nonBlank(name: String)(data: String): FailFast[String] =
      Right[List[String], String](data).
        ensure(List(s"$name cannot be blank"))(_.nonEmpty)

    def nonNegative(name: String)(data: Int): FailFast[Int] =
      Right[List[String], Int](data).
        ensure(List(s"$name must be non-negative"))(_ >= 0)



    def readName(data: FormData): FailFast[String] = for {
      name <- getValue("name")(data)
      _ <- nonBlank("name")(name)
    } yield name

    def readAge(data: FormData): Either[List[String], Int] = for {
      age <- getValue("age")(data)
      iage <- parseInt("age")(age)
      vage <- nonNegative("age")(iage)
    } yield vage

    def validate(form: FormData): Validated[List[String], User] =
      (readName(form).toValidated, readAge(form).toValidated).mapN(User.apply)


  }

}
