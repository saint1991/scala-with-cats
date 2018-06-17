package com.github.saint1991.cats.chapter5

import cats.Monad
import cats.data.EitherT
import cats.syntax.applicative._
import cats.syntax.option._
import cats.instances.list._
import cats.instances.option._

object Main extends App {

  composing()
  useOptionT()
  exercise()

  type Composed[A] = List[Option[A]]
  private def composing(): Monad[Composed] = new Monad[Composed] {

    override def pure[A](a: A): Composed[A] = a.pure[Option].pure[List]

    override def flatMap[A, B](fa: Composed[A])(f: A => Composed[B]): Composed[B] = fa.flatMap { x =>
      x.fold[Composed[B]](none.pure[List])(f)
    }

    override def tailRecM[A, B](a: A)(f: A => Composed[Either[A, B]]): Composed[B] = {
      def loop(fab: Composed[Either[A, B]]): List[Option[B]] = fab.flatMap {
        case None => None.pure[List]
        case Some(Right(x)) => Some(x).pure[List]
        case Some(Left(x)) => loop(f(x))
      }
      loop(f(a))
    }
  }

  private def useOptionT(): Unit = {

    import cats.data.OptionT
    import cats.syntax.option._
    type ListOption[A] = OptionT[List, A]

    val opt: ListOption[Int] = OptionT(List(
      10.some,
      none,
      1.some,
      2.some,
      none
    ))

    val mappedOpt: OptionT[List, Int] = opt.map(_ + 1) 
    println(s"map: $mappedOpt")
    
    val result: List[Option[Int]] = mappedOpt.value
    println(s"$result")
  }

  private def exercise(): Unit = {

    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent._
    import scala.concurrent.duration._
    import scala.language.postfixOps
    import cats.instances.future._
    import cats.syntax.either._


    type Response[A] = EitherT[Future, String, A]

    val powerLevels = Map(
      "Jazz"      -> 6,
      "Bumblebee" -> 8,
      "Hot Rod"   -> 10
    )

    def getPowerLevel(autobot: String): Response[Int] = EitherT(powerLevels.get(autobot) match {
      case None => s"Comms error: $autobot unreacable".asLeft.pure[Future]
      case Some(level) => level.asRight.pure[Future]
    })

    def canSpecialMove(ally1: String, ally2: String): Response[Boolean] = {
      val levelOfAlly1 = getPowerLevel(ally1)
      val levelOfAlly2 = getPowerLevel(ally2)
      for {
        l1 <- levelOfAlly1
        l2 <- levelOfAlly2
      } yield l1 + l2 > 15
    }

    def tacticalReport(ally1: String, ally2: String): String = Await.result(canSpecialMove(ally1, ally2).map {
      case true => s"$ally1 and $ally2 are ready to roll out!"
      case false => s"$ally1 and $ally2 need a recharge"
    }.value.map {
      case Left(unreachable) => unreachable
      case Right(report) => report
    }, 10 seconds)

    println(tacticalReport("Jazz", "Bumblebee"))
    println(tacticalReport("Bumblebee", "Hot Rod"))
    println(tacticalReport("Jazz", "Ironhide"))
  }
}
