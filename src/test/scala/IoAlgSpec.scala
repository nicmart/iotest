import cats.effect.IO
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
      val shifted: List[IO[Boolean]] = ios.map(IO.shift *> _)
      //shifted.parSequence.unsafeRunSync()
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
}
