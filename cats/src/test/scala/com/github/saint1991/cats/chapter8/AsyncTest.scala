package com.github.saint1991.cats.chapter8

import scala.concurrent.Future

import cats.{Applicative, Functor, Id}
import cats.syntax.functor._
import cats.instances.list._
import cats.syntax.traverse._
import org.scalatest.{Matchers, WordSpec}


object TestSubjectives {

  // service to test
  class UptimeService[F[_]: Applicative](client: UptimeClient[F]) {
    def getTotalUptime(hostnames: List[String]): F[Int] =  hostnames.traverse(client.getUptime).map(_.sum)
  }


//  trait UptimeClient {
//    def getUptime(hostname: String): Future[Int]
//  }

  trait UptimeClient[F[_]] {
    def getUptime(hostname: String)(implicit f: Functor[F]): F[Int]
  }

  trait RealUptimeClient extends UptimeClient[Future] {
    override def getUptime(hostname: String)(implicit f: Functor[Future]): Future[Int]
  }

  // mock client
  class TestUptimeClient(hosts: Map[String, Int]) extends UptimeClient[Id] {
    override def getUptime(hostname: String)(implicit f: Functor[Id]): Id[Int] = hosts.getOrElse(hostname, 0)
  }

}


class AsyncTest extends WordSpec with Matchers {

  import TestSubjectives._

  "UptimeService#getTotalUptime" when {

    "all hosts exist" should {
      "sum up their uptime values correctly" in {

        val hosts = Map(
          "host1" -> 10,
          "host2" -> 6
        )
        val client = new TestUptimeClient(hosts)
        val service = new UptimeService(client)


        val expected = hosts.values.sum // = 16
        val actual = service.getTotalUptime(hosts.keys.toList)

        actual should equal (expected) // ERROR: comparing Future[Int] with Int!!!
      }
    }

  }

}
