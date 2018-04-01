package com.github.saint1991.cats.chapter3

import cats.{Functor => CatsFunctor}

object Main extends App {

  catsFunctors()
  ex2()
  ex3()

  private def catsFunctors(): Unit = {
    import cats.instances.function._
    import cats.syntax.functor._

    val f1: Function1[Int, Int] = (x: Int) => x + 1
    val f2: Function1[Int, Int] = (x: Int) => x * 2
    val f3: Function1[Int, String] = (x: Int) => s"the number is $x"
    val f4 = f1 map f2 map f3
    println(f4(10))

    import scala.concurrent.{Future, ExecutionContext}
    implicit def futureFunctor(implicit ec: ExecutionContext): CatsFunctor[Future] =
      new CatsFunctor[Future] {
        def map[A, B](value: Future[A])(func: A => B): Future[B] =
          value.map(func)
      }

    import scala.concurrent.ExecutionContext.Implicits.global
    CatsFunctor[Future].map(Future(2)) { x => x * 2 }
  }

  private def catsContravariants(): Unit = {
    import cats.Contravariant
    import cats.Show
    import cats.instances.string._

    val showStr: Show[String] = Show[String]
    val showSym: Show[Symbol] = Contravariant[Show[String]].contramap(showStr) { sym: Symbol =>
      s"'${sym.toString()}"
    }

    showSym.show('cats)
  }

  private def catsInvariants(): Unit = {
    import cats.Monoid
    import cats.instances.string._
    import cats.syntax.invariant._
    import cats.syntax.semigroup._

    implicit val symbolMonoid: Monoid[Symbol] =
      Monoid[String].imap(Symbol.apply)(_.name)

    println(symbolMonoid.empty)
    println('a |+| 'few |+| 'words)
  }

  private def ex2(): Unit = {
    import Exercise2._
    printing(true)
    printing("success")

    printing(Box(true))
    printing(Box("cat"))
  }

  private def ex3(): Unit = {
    import Exercise3._

    println(encode(1.2812))
    println(decode[Double]("1.2812"))
    println(encode(Box(1.2812)))
    println(decode[Box[Double]]("1.2812"))

  }
}

object Exercise1 {
  sealed trait Tree[+A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]

  implicit val treeFunctor: CatsFunctor[Tree] = new CatsFunctor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Leaf(a) => Leaf(f(a))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }
  }
}


final case class Box[A](value: A)

object Exercise2 {

  trait Printable[A] { self =>
    def format(value: A): String
    //def contramap[B](f: B => A): Printable[B] = { value => (f andThen format)(value) }
    def contramap[B](f: B => A): Printable[B] = new Printable[B] {
      override def format(value: B): String = (f andThen self.format)(value)
    }
  }
  implicit val stringPrintable: Printable[String] = new Printable[String] {
    def format(value: String): String = "\"" + value + "\""
  }
  implicit val booleanPrintable: Printable[Boolean] = new Printable[Boolean] {
    def format(value: Boolean): String = if(value) "yes" else "no"
  }


  implicit def boxStringPrintable(implicit sp: Printable[String]): Printable[Box[String]] =
    sp.contramap { (box: Box[String]) => s"${box.value}" }

  implicit def boxBooleanPrintable(implicit bp: Printable[Boolean]): Printable[Box[Boolean]] =
    bp.contramap( (box: Box[Boolean]) => box.value )


  def printing[T](value: T)(implicit printable: Printable[T]): Unit = println(printable.format(value))

}

object Exercise3 {

  trait Codec[A] { self =>
    def encode(value: A): String
    def decode(value: String): A
    def imap[B](dec: A => B, enc: B => A): Codec[B] = new Codec[B] {
      override def encode(value: B): String = (enc andThen self.encode)(value)
      override def decode(value: String): B = (self.decode _ andThen dec)(value)
    }
  }

  def encode[A](value: A)(implicit c: Codec[A]): String = c.encode(value)
  def decode[A](value: String)(implicit c: Codec[A]): A = c.decode(value)

  implicit val doubleCodec: Codec[Double] = new Codec[Double] {
    override def encode(value: Double): String = value.formatted("%.3f")
    override def decode(value: String): Double = value.toDouble
  }

  implicit val boxCodec: Codec[Box[Double]] = doubleCodec.imap(value => Box(value), box => box.value)
}

