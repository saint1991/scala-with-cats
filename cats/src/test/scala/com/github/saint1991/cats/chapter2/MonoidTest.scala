package com.github.saint1991.cats.chapter2

import org.scalatest.{Matchers, WordSpec}

class MonoidTest extends WordSpec with Matchers {

  private def isClosed[T](a: T, b: T)(implicit monoid: Semigroup[T]): T = monoid.combine(a, b)
  private def isAssociative[T](a: T, b: T, c: T)(implicit monoid: Monoid[T]): Boolean =
    monoid.combine(monoid.combine(a, b), c) == monoid.combine(a, monoid.combine(b, c))
  private def hasIdentity[T](a: T)(implicit monoid: Monoid[T]) =
    monoid.combine(a, monoid.empty) == a && monoid.combine(monoid.empty, a) == a


  {
    import Monoid.intsSum

    "Monoid should be closed" in {
      isClosed(1, 2) match {
        case _: Int => succeed
      }
    }

    "Monoid should be associative" in {
      isAssociative(1, 2, -3) shouldBe true
    }

    "Monoid should has identity" in {
      hasIdentity(-1) shouldBe true
    }
  }

  {
    import Exercise1.and
    "AndMonoid should be closed" in {
      isClosed(true, false) match {
        case _: Boolean => succeed
      }
    }

    "AndMonoid should be associative" in {
      isAssociative(true, true, true) shouldBe true
      isAssociative(false, true, true) shouldBe true
      isAssociative(true, false, true) shouldBe true
      isAssociative(true, true, false) shouldBe true
    }

    "AndMonoid should has identity" in {
      hasIdentity(true) shouldBe true
      hasIdentity(false) shouldBe true
    }
  }

  {
    import Exercise1.or
    "OrMonoid should be closed" in {
      isClosed(true, false) match {
        case _: Boolean => succeed
      }
    }

    "OrMonoid should be associative" in {
      isAssociative(true, true, true) shouldBe true
      isAssociative(false, true, true) shouldBe true
      isAssociative(true, false, true) shouldBe true
      isAssociative(true, true, false) shouldBe true
    }

    "OrMonoid should has identity" in {
      hasIdentity(true) shouldBe true
      hasIdentity(false) shouldBe true
    }
  }

  {
    import Exercise2.Monoid.union
    "UnionMonoid should be closed" in {
      isClosed(Set(1, 2, 3), Set(2, 4)) match {
        case _: Set[Int] => succeed
      }
    }

    "UnionMonoid should be associative" in {
      isAssociative(Set(1, 2), Set(-3, 1), Set(5, 2)) shouldBe true
    }

    "UnionMonoid should has identity" in {
      hasIdentity(Set(1, 2, 3)) shouldBe true
    }
  }

  {
    import Exercise2.Semigroup.intersection
    "IntersectionMonoid should be closed" in {
      isClosed(Set(1, 2, 3), Set(2, 4)) match {
        case _ => succeed
      }
    }
  }

  {
    import cats.instances.int._
    import cats.instances.option._
    import cats.syntax.option._
    import Exercise3.add
    "add should sum all integers in a provided List" in {
      add(List(1, 3, -1 ,2)) should equal (5)
    }

    "add should sum all option integers in a provided List" in {
      add(List(1.some, none, 4.some, (-1).some, none)) should equal (4.some)
    }
  }

  {
    import Exercise3._
    add(List(Order(100.0, 2.0), Order(200, 5.0), Order(800, 10.0))) should equal (Order(1100.0, 17.0))
  }

}
