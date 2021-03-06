package com.github.saint1991.cats.chapter4

trait Monad[F[_]] {

  def pure[A](x: A): F[A]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  // exercise1
  def map[A, B](x: F[A])(f: A => B): F[B] = flatMap(x)(f andThen pure)

  // def tailRecM[A, B](a: A)(fn: A => F[Either[A, B]]): F[B]
}

object MonadSyntax {

  def pure[F[_], A](a: A)(implicit monad: Monad[F]): F[A] = monad.pure(a)

  implicit class MonadOpts[F[_], A](val self: F[A]) extends AnyVal {

    def flatMap[B](f: A => F[B])(implicit monad: Monad[F]): F[B] = monad.flatMap(self)(f)

    def map[B](f: A => B)(implicit monad: Monad[F]): F[B] = monad.map(self)(f)
  }
}

object OptionMonad extends Monad[Option] {
  override def pure[A](x: A): Option[A] = Option(x)

  override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa match {
    case None => None
    case Some(a) => f(a)
  }
}

object M extends App {
  val m = OptionMonad
  m.map(m.pure(1)) { x => x + 1 }
}