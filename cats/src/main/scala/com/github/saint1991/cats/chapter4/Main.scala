package com.github.saint1991.cats.chapter4

import scala.concurrent.{Await, Future}
import scala.language.postfixOps


object Main extends App {

//  catsMonad()
//  catsMonadSyntax()
//  catsEither()

  Exercise5.useFoldRight()
//  Exercise6.useFactorial()
//  Exercise7.useCheckLogin()
//  Exercise8.useEvalInput()

  private def catsMonad(): Unit = {
    import cats.Monad
    import cats.instances.option._
    import cats.instances.list._

    val opt1 = Monad[Option].pure(2)
    val opt2 = Monad[Option].flatMap(opt1)(x => Some(x + 2))

    val list = Monad[List].pure(1)
    println(opt1)
    println(opt2)
    println(list)

    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration._
    import cats.instances.future._

    val fm = Monad[Future]
    val future: Future[Int] = fm.flatMap(fm.pure(1))(x => fm.pure(x + 1))
    Await.result(future, 2 seconds)

  }

  private def catsMonadSyntax(): Unit = {
    import cats.instances.option._
    import cats.instances.list._
    import cats.syntax.applicative._

    println(1.pure[Option])
    println(1.pure[List])

    import cats.syntax.functor._
    import cats.syntax.flatMap._
    import scala.language.higherKinds

    def sumSquare[F[_]: cats.Monad](a: F[Int], b: F[Int]): F[Int] =
      a.flatMap(x => b.map(y => x * x  + y * y))

    import cats.instances.option._
    import cats.instances.list._
    println(sumSquare(Option(2), Option(4)))
    println(sumSquare(List(1, 2, 4), List(5, 6)))

    import cats.Id
    sumSquare(1: Id[Int], 2: Id[Int])
  }

  private def catsEither(): Unit = {
    import cats.syntax.either._

    val a: String = 1.asLeft[String].getOrElse("a")
    val b: Either[String, Int] = 2.asRight[String].leftMap(a => a + a).bimap(a => a + a, b => b + 1)
    println(a)
    println(b)
  }

}

object Exercise2 {
  type Id[A] = A
  val idMonad: Monad[Id] = new Monad[Id] {
    override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)
    override def pure[A](x: A): Id[A] = x
    override def map[A, B](x: Id[A])(f: A => B): Id[B] = f(x)
  }
}

object Exercise3 {

}

object Exercise4 {

}

object Exercise5 {

  import cats.Eval

//  def foldRight[A, B](as: List[A], acc: B)(fn: (A, Eval[B]) => Eval[B]): Eval[B] = as match {
//    case Nil => Eval.now(acc)
//    case head :: tail => Eval.defer(fn(head, foldRight(tail, acc)(fn)))
//  }

  def useFoldRight(): Unit = {
    val result = foldRight(Iterator.fill(5)(2).toList, Eval.now(2)) { (el, acc) => acc.map(x => x + el) }
    println(result.value)
  }

//  def foldRight[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] = as match {
//    case Nil => acc
//    case head :: tail => Eval.defer(fn(head, foldRight(tail, acc)(fn)))
//  }

//  foldRight implementation in cats
  def foldRight[A, B](fa: List[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
    def loop(as: List[A]): Eval[B] =
      as match {
        case Nil => lb
        case h :: t => f(h, Eval.defer(loop(t)))
      }
    Eval.defer(loop(fa))
  }


//  def foldRight2[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] = as match {
//    case Nil => acc
//    case head :: tail => Eval.defer(fn(head, foldRight2(tail, acc)(fn)))
//  }
//
//  def useFoldRight2(): Unit = {
//    val result = foldRight2(List(1, 2, 3, 4, 5), Eval.later(0)) { (el, acc) => acc.map(x => x + el) }
//    println(result.value)
//  }
}

object Exercise6 {
  import cats.data.Writer

  def slowly[A](body: => A): A = try body finally Thread.sleep(100)

  def factorial(n: Int): Writer[Vector[String], Int] = slowly {
    if (n <= 1) Writer(Vector(s"fact $n 1"), 1)
    else {
      val factN = factorial(n - 1).map(fact => n * fact)
      val value = factN.value
      factN.mapWritten(logs => logs :+ s"fact $n $value")
    }
  }

  def useFactorial(): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration._

    val result1 = Future(factorial(5))
    val result2 = Future(factorial(3))

    val res = Await.result(for {
      r1 <- result1
      r2 <- result2
    } yield (r1, r2), 10 seconds)


    println(s"r1: ${res._1.value}")
    println(res._1.written.mkString("\n"))

    println(s"r2: ${res._2.value}")
    println(res._2.written.mkString("\n"))
  }

}

object Exercise7 {

  import cats.data.Reader

  case class Db (
    usernames: Map[Int, String],
    passwords: Map[String, String]
  )

  type DbReader[T] = Reader[Db, T]

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader[Db, Option[String]]( _.usernames.get(userId) )

  def checkPassword(username: String, password: String): DbReader[Boolean] =
    Reader[Db, Boolean](_.passwords.get(username).contains(password))

  def checkLogin(userId: Int, password: String): DbReader[Boolean] = findUsername(userId).flatMap {
    case None => Reader(_ => false)
    case Some(username) => checkPassword(username, password)
  }

  def useCheckLogin(): Unit = {
    val users = Map(
      1 -> "dade",
      2 -> "kate",
      3 -> "margo"
    )
    val passwords = Map(
      "dade"  -> "zerocool",
      "kate"  -> "acidburn",
      "margo" -> "secret"
    )
    val db = Db(users, passwords)
    println(checkLogin(1, "zerocool").run(db))
    println(checkLogin(4, "davinci").run(db))
  }
}

object Exercise8 {
  import cats.data.State

  type CalcState[A] = State[List[Int], A]

  private def evaluate(op1: Int, operator: String, op2: Int): Int = operator match {
    case "+" => op1 + op2
    case "-" => op1 - op2
    case "*" => op1 * op2
    case "/" => op1 / op2
  }

  def evalOne(sym: String): CalcState[Int] = sym match {
    case operator @ ("+" | "-" | "*" | "/") =>
      State[List[Int], Int] {
        case op2 :: op1 :: stack =>
          val result = evaluate(op1, operator, op2)
          (result :: stack, result)
        case _ => throw new IllegalStateException("invalid formula")
      }
    case number =>
      val integer = number.toInt
      State[List[Int], Int] { stack => (integer :: stack, integer) }
  }

  def evalAll(input: List[String]): CalcState[Int] = input match {
    case head :: tail => tail.foldLeft(evalOne(head)) { (acc, sym) =>
      acc.flatMap(_ => evalOne(sym))
    }
    case _ => State(_ => (Nil, 0))
  }

  def evalInput(input: String): CalcState[Int] = evalAll(input.split(" ").toList)

  def useEvalInput(): Unit = {
    println(evalAll(List("1", "2", "+", "3", "*")).runA(Nil).value)
  }
}

object Exercise9 {

  sealed trait Tree[+A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)
  def leaf[A](value: A): Tree[A] = Leaf(value)

  import cats.{Monad => CMonad}
  cats.instances.list

  val nonTailRecTreeMonad: CMonad[Tree] = new CMonad[Tree] {

    override def pure[A](x: A): Tree[A] = Leaf(x)

    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
      case Leaf(x) => f(x)
      case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f))
    }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {

      def loop(t: Tree[Either[A, B]]): Tree[B] = {
        t match {
          case Leaf(Left(x)) => loop(f(x))
          case Leaf(Right(x)) => Leaf(x)
          case Branch(left, right) => Branch(loop(left), loop(right))
        }
      }

      loop(f(a))
    }
  }
//
//  val tailRecTreeMonad: CMonad[Tree] = new CMonad[Tree] {
//
//    override def pure[A](x: A): Tree[A] = Leaf(x)
//
//    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = ???
//
//    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
//
//      @tailrec
//      def loop(stack: List[Tree[Either[A, B]]], acc: Tree[B]): Tree[B] = stack match {
//        case head :: tail => head match {
//          case Leaf(Left(x)) => loop(f(x) :: tail, acc)
//          case Leaf(Right(x)) => loop(tail, acc) // TODO add this node to acc
//          case Branch(left, right) => loop(left :: right :: tail, acc)
//        }
//        case Nil => acc
//      }
//
//      // loop(f(a) :: Nil, Leaf(B())) // TODO what should be put as the 2nd arg ???
//    }
//  }

}
