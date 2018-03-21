package com.github.saint1991.cats.chapter1

// type class
trait Printable[A] {
  def format(value: A): String
}

object PrintableInstances {
  implicit val intPrintable: Printable[Int] = new Printable[Int] {
    override def format(value: Int): String = value.formatted("%03d")
  }
  implicit val stringPrintable: Printable[String] = new Printable[String] {
    override def format(value: String): String = value
  }
}

object Printable {
  def format[A](value: A)(implicit printable: Printable[A]): String = printable.format(value)
  def print[A](value: A)(implicit printable: Printable[A]): Unit = println(format(value))
}

object PrintableSyntax {
  implicit class PrintableOps[A](val self: A) extends AnyVal {
    def format(implicit printable: Printable[A]): String = printable.format(self)
    def print(implicit printable: Printable[A]): Unit = println(format)
  }
}