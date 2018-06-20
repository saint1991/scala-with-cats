package com.github.saint1991.cats.chapter7

trait Foldable[F[_]] {
  def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B
  def foldRight[A, B](fa: F[A], b: B)(f: (A, B) => B): B
}
