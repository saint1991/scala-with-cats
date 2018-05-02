package com.github.saint1991.cats.chapter4

import org.scalatest.{Matchers, WordSpec}


class MonadTest extends WordSpec with Matchers {
//
//  import MonadSyntax._
//
//  private def leftIdentityLaw[F[_], A, B](a: A)(func: A => F[B])(implicit monad: Monad[F]): Unit = {
//    monad.pure(a).flatMap(func) should equal (func(a))
//  }
//
//  private def rightIdentityLaw[F[_], A, B](fa: F[A])(f: A => F[B])(implicit monad: Monad[F]): Unit = {
//    monad.flatMap(fa)(monad.pure) should equal (fa)
//  }
//
//  private def associavityLaw[F[_], A, B, C](fa: F[A])(f: A => F[B])(g: B => F[C])(implicit monad: Monad[F]): Unit = {
//    monad.flatMap(fa)(f).flatMap(g) should equal (monad.flatMap(fa)(x => f(x).flatMap(g)))
//  }
}
