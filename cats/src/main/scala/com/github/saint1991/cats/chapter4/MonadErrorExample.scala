package com.github.saint1991.cats.chapter4

object MonadErrorExample extends App {

  import cats.MonadError
  import cats.instances.either._

  type ErrorOr[A] = Either[String, A]
  val e = MonadError[ErrorOr, String]
  println(e.raiseError("error"))

}
