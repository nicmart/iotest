import cats.effect.{Fiber, IO}
import org.scalatest.{Matchers, WordSpec}
import cats.syntax.all._
import cats.instances.all._

import scala.collection.immutable
import scala.util.Random
import scala.concurrent.ExecutionContext.Implicits.global

class IoAlgSpec extends WordSpec with Matchers {

  "adasd" should {
    "asdasd " in {
      val ios: List[IO[Boolean]] = (1 to 10).map { i =>
        IO {
          println(s"IO $i")
          Thread.sleep(Random.nextInt(100))
          if (i == 5) true
          else false
        }
      }.toList
      val n: IO[Unit] = IO { println("hi") } *> IO { println("bye")}
      val shifted: List[IO[Boolean]] = ios.map(IO.shift *> _)
      val t: IO[List[Fiber[IO, Boolean]]] = shifted.traverse(_.start)
      ios.parSequence.unsafeRunSync()
      //shifted.parTraverse()
      val seq = CatsIoAlg.seq(shifted, identity[Boolean], IO.pure(false))
      //val result = seq.unsafeRunSync()
      true shouldBe true
    }
  }

  def pairSeq[A](io1: IO[A], io2: IO[A], p: A => Boolean, z: IO[A]): IO[A] = {
    //io1.
    ???
  }

  // Start writing your ScalaFiddle code here
  import cats._, implicits._
  import cats.effect._
  import scala.concurrent.duration._

  val tasks = List.range(1, 10).map(i =>
    IO.sleep(i.seconds) *> IO { println(s"$i") } *> IO.pure(i)
  )

  def findInParallel[A](fas: List[IO[A]], p: A ⇒ Boolean, z: A): IO[A] =
    fas.traverse(_.start).flatMap { fibers =>
      fibers.foldLeft(IO.pure(z)) { (result, fiber) =>
        result.map(p).ifM(
          result,
          fiber.join
        )
      }
    }

  def fastestInParallel[A](fas: List[IO[A]], p: A ⇒ Boolean, z: A): IO[A] =
    fas match {
      case Nil => IO.pure(z)
      case head :: tail => IO.racePair(head, fastestInParallel(tail, p, z)).flatMap {
          case Left((a, fiberB)) => if (p(a)) IO.pure(a) else fiberB.join
          case Right((fiberA, b)) => if (p(b)) IO.pure(b) else fiberA.join
        }
    }

  def findInParallelC[A](fas: List[IO[A]], p: A ⇒ Boolean, z: A): IO[A] =
    fas.traverse(_.start).flatMap { fibers =>
      fibers.foldLeft(IO.pure(z)) { (result, fiber) =>
        result.map(p).ifM(
          fiber.cancel *> result,
          fiber.join
        )
      }
    }

  findInParallel[Int](tasks, _ > 5, 0)
    .flatMap(i => IO(println(s"found $i")))
    .unsafeRunAsync(_ => ())
}
