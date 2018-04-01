package com.github.saint1991.cats.chapter3

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}
