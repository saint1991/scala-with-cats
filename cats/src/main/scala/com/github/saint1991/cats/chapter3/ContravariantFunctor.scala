package com.github.saint1991.cats.chapter3

trait ContravariantFunctor[F[_]] {
  def contramap[A, B](fa: F[A])(f: B  => A): F[B]
}
