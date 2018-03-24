package com.github.saint1991.cats.chapter2

trait Semigroup[T] {
  def combine(x: T, y: T): T
}
