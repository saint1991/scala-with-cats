package com.github.saint1991.cats.chapter9

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.language.postfixOps

import cats.Monoid

object Main extends App {

  exercise1()
  exercise2()
  exercise3()

  def foldMap[A, B: Monoid](v: Vector[A])(f: A => B): B =
    v.map(f).fold(implicitly[Monoid[B]].empty)(implicitly[Monoid[B]].combine)

  def exercise1(): Unit = {
    import cats.instances.int._
    import cats.instances.string._
    println(foldMap(Vector(1, 2, 3))(identity))
    println(foldMap(Vector(1, 2, 3))(_.toString + "! "))
    println(foldMap("Hello world!".toVector)(_.toString.toUpperCase))
  }

  def exercise2(): Unit = {

    import scala.concurrent.ExecutionContext.Implicits.global
    import cats.instances.int._

    def parallelFoldMap[A, B : Monoid](values: Vector[A])(f: A => B): Future[B] = {

      val cores = Runtime.getRuntime.availableProcessors()
      val groups: Iterator[Vector[A]] = values.grouped(values.length / cores + 1)

      val futures: Iterator[Future[B]] = groups.map { v => Future ( foldMap(v)(f) )}

      val finalResult = Future.sequence(futures) map { r =>
        foldMap(r.toVector)(identity)
      }
      finalResult
    }

    println(Await.result(parallelFoldMap((1 to 1000).toVector)(identity), 10 seconds))
  }

  def exercise3(): Unit = {

    import scala.concurrent.ExecutionContext.Implicits.global

    import cats.syntax.foldable._
    import cats.instances.vector._
    import cats.instances.int._

    def parallelFoldMap[A, B : Monoid](values: Vector[A])(f: A => B): Future[B] = {

      val cores = Runtime.getRuntime.availableProcessors()
      val batches: Seq[Vector[A]] = values.grouped(values.length / cores + 1).toList

      Future.traverse(batches) { v =>
        Future(v.foldMap(f))
      }.map { r => r.toVector.foldMap(identity) }
    }

    println(Await.result(parallelFoldMap((1 to 1000).toVector)(identity), 10 seconds))
  }

}
