package com.github.saint1991.cats.chapter3

trait InvariantFunctor[F[_]] {
  def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B]
}
