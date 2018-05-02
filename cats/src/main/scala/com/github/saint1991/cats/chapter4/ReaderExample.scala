package com.github.saint1991.cats.chapter4

import java.io.File

import scala.io.Source

object ReaderExample extends App {
  import cats.data.Reader


  case class Config(ip: String, port: Int)


  val valueConfigReader: Reader[(String, Int), Config] = Reader(Config.tupled)
  val fileConfigReader: Reader[File, Config] = Reader { f =>
    val lines = Source.fromFile(f, "UTF-8").getLines().toSeq
    Config(lines.head, Integer.parseInt(lines(1)))
  }


  case class Cat(name: String, favoriteFood: String)

  val catName: Reader[Cat, String] = Reader(cat => cat.name)

  val feedKitty: Reader[Cat, String] = Reader(cat => s"Have a nice bowl of ${cat.favoriteFood}")
  val greetKitty: Reader[Cat, String] = catName.map { name =>
    println("greeting")
    s"Hello $name"
  }

  val combined: Reader[Cat, String] = for {
    greet <- greetKitty
    feed  <- feedKitty
  } yield s"$greet. $feed."

  val flatMapped: Reader[Cat, String] = greetKitty.flatMap { greet =>
    feedKitty.map { feed =>
      s"$greet. $feed"
    }
  }

  println("Main")
  println(combined(Cat("Garfield", "lasagne")))
  println(flatMapped(Cat("Garfield", "lasagne")))


}
