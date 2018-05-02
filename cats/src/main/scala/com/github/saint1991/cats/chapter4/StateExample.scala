package com.github.saint1991.cats.chapter4

object StateExample extends App {
  import cats.data.State

  val s: State[Int, String] = State[Int, String] { state =>
    (state + 1, "result")
  }

  val result: (Int, String) = s.run(0).value
  println(result)

  val step1 = State[Int, String] { num =>
    val ans = num + 1
    (ans, s"Result of step1: $ans")
  }

  val step2 = State[Int, String] { num =>
    val ans = num * 2
    (ans, s"Result of step2: $ans")
  }

  // result contains zipped state without explicitly returning (states ++ new state) in flatMap
  val steps = for {
    x <- step1
    y <- step2
  } yield (x, y)

  step1.flatMap { x =>
    step2.map( y => (x, y))
  }

  // state
  println(steps.run(20).value)

  val getDemo = State.get[Int].run(10).value
  val setDemo = State.set[Int](30).run(20).value
  val pureDemo = State.pure[Int, String]("Result").run(5).value
  val inspectDemo = State.inspect[Int, String](_ + "!").run(1).value
  val modifyDemo = State.modify[Int](_ + 1).run(30).value
}
