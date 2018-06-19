package com.github.saint1991.cats.chapter6

import cats.Semigroupal
import cats.data.{NonEmptyVector, Validated}
import cats.instances.list._
import cats.syntax.validated._

object ValidatedExample extends App {

  type AllErrorsOr[A] = Validated[List[String], A]

  val result: AllErrorsOr[(Nothing, Int)] = Semigroupal[AllErrorsOr].product(
    Validated.invalid(List("Error 1")),
    10.valid
  )

  val result2 = Semigroupal[AllErrorsOr].product(
    1.valid,
    2.valid
  )

  type NelError[A] = Validated[NonEmptyVector[String], A]
  val result3 = Semigroupal[NelError].product(
    NonEmptyVector.of("Error 1").invalid[Int],
    NonEmptyVector.of("Error 2").invalid[Int]
  )

  val result4 = Semigroupal[NelError].product(
    1.valid,
    2.valid
  )

  println(result)
  println(result2)
  println(result3)
  println(result4)
}
