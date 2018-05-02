package com.github.saint1991.cats.chapter4

trait MonadError[F[_], E] extends Monad[F] {

  // Lift an error into the `F` context:
  def raiseError[A](e: E): F[A]

  // Handle an error, potentially recovering from it:
  def handleError[A](fa: F[A])(f: E => A): F[A]

  // failing if the predicate is not satisfied:
  def ensure[A](fa: F[A])(e: E)(f: A => Boolean): F[A]
}
