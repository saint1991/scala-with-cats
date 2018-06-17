package com.github.saint1991.cats.chapter4

object WriterExample extends App {
  import cats.data.Writer
  import cats.instances.vector._   // for Monoid
  import cats.syntax.WriterSyntax

  val writer = Writer(Vector("error1", "success", "error2"), 2)
  val result = writer.value
  val logs = writer.written
  val both = writer.run
  val after = writer.flatMap { x =>
    Writer(Vector("error3"), x + 1)
  }
  val reset = after.reset
  println((result, logs, both))
  println(after)
  println(reset)
}
