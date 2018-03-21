package com.github.saint1991.cats.chapter1

import JsonSyntax._ // import implicit conversion toJson
import PrintableSyntax._

final case class Cat(name: String, age: Int, color: String)

object Main extends App {

  examineTypeClass()
  eq()

  println("--ex1--")
  exercise1()
  println("--ex2--")
  exercise2()
  println("--ex3--")
  exercise3()


  private def examineTypeClass(): Unit = {

    import JsonWriter._ // import implicit scope of JsonWriter[String]

    // capture the implicit JsonWriter[String] exists in this scope
    val writer = implicitly[JsonWriter[String]]
    println(writer.write("implicitly"))

    val json = "jsonString".toJson()
    println(json)
  }

  private def eq(): Unit = {
    import cats.Eq
    import cats.instances.int._
    import cats.instances.option._
    import cats.syntax.eq._
    import cats.syntax.option._

    val eqInt = Eq[Int]
    println(eqInt.eqv(1, 1))
    println(1 === 2)
    println(1 =!= 2)

    println(1.some =!= none[Int])
  }

//  private def variance(): Unit = {
//    trait Printable[+A] // covariance: Printable[B] is considered as a subtype of Printable[A] if B is subtype of A.
//    trait Showable[-A]  // contravariance: Showble[B] is considered as a subtype of Showable[A] if A is subtype of B.
//  }

  private def exercise1(): Unit = {

    import PrintableInstances._
    Printable.print(12)

    implicit val catPrintable: Printable[Cat] = new Printable[Cat] {
      override def format(cat: Cat): String = s"${cat.name} is a ${cat.age} year-old ${cat.color} cat."
    }

    Cat("mike", 10, "white").print
  }

  private def exercise2(): Unit = {
    import cats._               // for importing type classes
    // import cats.instances.all._ // for importing type class instances
    import cats.syntax.all._    // for importing implicit classes
    // import cats.implicits._  // for importing type class instances and syntax


    implicit val showCat: Show[Cat] = Show.show[Cat] { cat =>
      s"${cat.name} is a ${cat.age} year-old ${cat.color} cat."
    }

    println(Cat("mike", 10, "white").show)
  }

  private def exercise3(): Unit = {
    import cats._
    import cats.syntax.eq._
    import cats.instances.option._

    implicit val catEq = Eq.instance[Cat] { (c1, c2) =>
      c1.name == c2.name && c1.age == c2.age && c1.color == c2.color
    }

    val cat1 = Cat("Garfield",   38, "orange and black")
    val cat2 = Cat("Heathcliff", 33, "orange and black")
    val optionCat1 = Option(cat1)
    val optionCat2 = Option.empty[Cat]

    println(cat1 =!= cat2)
    println(optionCat1 =!= optionCat2)
  }
}

