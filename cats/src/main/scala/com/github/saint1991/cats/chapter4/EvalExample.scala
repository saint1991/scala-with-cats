package com.github.saint1991.cats.chapter4

object EvalExample extends App {

  import cats.Eval

  // eager computation
  val now = Eval.now {
    println("now")
    1
  }.map { x =>
    println("map in now")
    x + 1
  }

  // lazy computation and memorized (computed only once)
  val later = Eval.later {
    println("later")
    2
  }.map { x =>
    println("map in later")
    x + 1
  }

  // lazy computation
  val always = Eval.always {
    println("always")
    3
  }.map { x =>
    println("map in always")
    x + 1
  }

  println("values")
  println(now.value)
  println(later.value)
  println(later.value)
  println(always.value)
  println(always.value)

  // stack safe factorial
  def factorial(n: BigInt): Eval[BigInt] =
    if(n == 1) Eval.now(n)
    else Eval.defer(factorial(n - 1).map(_ * n))

  factorial(10).value

}
