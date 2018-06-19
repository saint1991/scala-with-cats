package com.github.saint1991.cats.chapter6

trait Applicative[F[_]] extends Apply[F] {
  def pure[A](a: A): F[A]
}
