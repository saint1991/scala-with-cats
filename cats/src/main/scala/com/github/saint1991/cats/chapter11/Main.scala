package com.github.saint1991.cats.chapter11

object Exercise1 {

  type Machine = String

  final case class GCounter(counters: Map[Machine, Int]) {

    def increment(machine: Machine, amount: Int): GCounter = GCounter(
      counters.updated(machine, counters.getOrElse(machine, 0) + amount)
    )

    def merge(that: GCounter): GCounter = GCounter(
      that.counters.foldLeft(this.counters) { case (acc, (machine, amount)) =>
        acc.updated(machine, acc.getOrElse(machine, 0) + amount)
      }
    )

    def total: Int = counters.values.sum
  }
}
