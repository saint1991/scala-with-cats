package com.github.saint1991.cats.chapter11

import cats.{Id, MonoidK}
import cats.kernel.Monoid

object Exercise1 {

  final case class GCounter(counters: Map[String, Int]) {

    def increment(machine: String, amount: Int): GCounter = GCounter(
      counters.updated(machine, counters.getOrElse(machine, 0) + amount)
    )

    def merge(that: GCounter): GCounter = GCounter(
      that.counters.foldLeft(this.counters) { case (acc, (machine, amount)) =>
        acc.updated(machine, Math.max(acc.getOrElse(machine, 0), amount))
      }
    )

    def total: Int = counters.values.sum
  }
}

object Exercise2 {

  trait BoundedSemiLattice[A] extends Monoid[A] {
    def combine(x: A, y: A): A
    def empty: A
  }

  trait BoundedSemiLatticeK[F[_]] extends MonoidK[F] {
    def combine[A](x: F[A], y: F[A]): F[A]
    def empty[A]: F[A]
  }

  object BoundedSemiLattice {

    implicit val intMergeInstance: BoundedSemiLattice[Int] = new BoundedSemiLattice[Int] {
      override def combine(x: Int, y: Int): Int = Math.max(x, y)
      override def empty: Int = Int.MinValue
    }

    implicit val setMergeInstance: BoundedSemiLatticeK[Set] = new BoundedSemiLatticeK[Set] {
      override def combineK[A](x: Set[A], y: Set[A]): Set[A] = x union y
      override def combine[A](x: Set[A], y: Set[A]): Set[A] = combineK(x, y)
      override def empty[A]: Set[A] = Set.empty[A]
    }
  }
}

object Exercise3 {

  import cats.syntax.monoid._
  import Exercise2._

  final case class GCounter[A](counters: Map[String, A]) {
    def increment(machine: String, amount: A)(implicit m: Monoid[A]): GCounter[A] = GCounter[A](
      counters.updated(machine, counters.getOrElse(machine, m.empty) |+| amount)
    )

    def merge(that: GCounter[A])(implicit bsl: BoundedSemiLattice[A]): GCounter[A] =  GCounter(
      that.counters.foldLeft(this.counters) { case (acc, (machine, amount)) =>
        acc.updated(machine, bsl.combine(acc.getOrElse(machine, bsl.empty), amount))
      }
    )

    def total(implicit m: Monoid[A]): A = counters.values.foldLeft(m.empty)(_ |+| _)
  }

}

object Exercise4 extends App {

  import Exercise2._
  import cats.syntax.monoid._

  trait GCounter[F[_, _], K, V] {
    def increment(f: F[K, V])(k: K, v: V)(implicit m: Monoid[V]): F[K, V]

    def merge(f1: F[K, V], f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V]

    def total(f: F[K, V])(implicit m: Monoid[V]): V
  }

  object GCounter {
    def apply[F[_, _], K, V](implicit counter: GCounter[F, K, V]): GCounter[F, K, V] = counter

    type CaseClassGCounter = Exercise3.GCounter[Int]

    implicit val mapInstance: GCounter[Map, String, Int] = new GCounter[Map, String, Int] {
      override def increment(f: Map[String, Int])(k: String, v: Int)(implicit m: Monoid[Int]): Map[String, Int] =
        f.updated(k, f.getOrElse(k, m.empty) |+| v)

      override def merge(f1: Map[String, Int], f2: Map[String, Int])(implicit b: BoundedSemiLattice[Int]): Map[String, Int] =
        f2.foldLeft(f1) { case (acc, (tk, tv)) =>
          acc.updated(tk, b.combine(acc.getOrElse(tk, b.empty), tv))
        }

      override def total(f: Map[String, Int])(implicit m: Monoid[Int]): Int = f.values.foldLeft(m.empty)(_ |+| _)
    }
  }

  private def useGCounter(): Unit = {
    import cats.instances.int._

    val g1 = Map("a" -> 7, "b" -> 3)
    val g2 = Map("a" -> 2, "b" -> 5)

    val counter = GCounter[Map, String, Int]

    val merged = counter.merge(g1, g2)
    println(merged)

    val total = counter.total(merged)
    println(total)
  }

  useGCounter()
}

object Exercise5 {

  import Exercise2._
  import Exercise4.GCounter
  import cats.syntax.monoid._

  trait KeyValueStore[F[_, _]] {

    def put[K, V](f: F[K, V])(k: K, v: V): F[K, V]

    def get[K, V](f: F[K, V])(k: K): Option[V]

    def getOrElse[K, V](f: F[K, V])(k: K, default: V): V = get(f)(k).getOrElse(default)

    def values[K, V](f: F[K, V]): List[V]
  }

  object KeyValueStore {
    val mapInstance: KeyValueStore[Map] = new KeyValueStore[Map] {

      override def put[K, V](f: Map[K, V])(k: K, v: V): Map[K, V] = f.updated(k, v)

      override def get[K, V](f: Map[K, V])(k: K): Option[V] = f.get(k)

      override def values[K, V](f: Map[K, V]): List[V] = f.values.toList
    }
  }

  implicit class KvsOps[F[_, _], K, V](f: F[K, V]) {
    def put(key: K, value: V)(implicit kvs: KeyValueStore[F]): F[K, V] = kvs.put(f)(key, value)

    def get(key: K)(implicit kvs: KeyValueStore[F]): Option[V] = kvs.get(f)(key)

    def getOrElse(key: K, default: V)(implicit kvs: KeyValueStore[F]): V = kvs.getOrElse(f)(key, default)

    def values(implicit kvs: KeyValueStore[F]): List[V] = kvs.values(f)
  }

  implicit def gcounterInstance[F[_, _], K, V](implicit kvs: KeyValueStore[F], km: Monoid[F[K, V]]): GCounter[F, K, V] = new GCounter[F, K, V] {
    override def increment(f: F[K, V])(k: K, v: V)(implicit m: Monoid[V]): F[K, V] = f.put(k, f.getOrElse(k, m.empty) |+| v)

    override def merge(f1: F[K, V], f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V] = f1 |+| f2

    override def total(f: F[K, V])(implicit m: Monoid[V]): V = m.combineAll(f.values)
  }
}

