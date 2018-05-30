import cats.effect.IO

//noinspection ScalaFileName
trait IoAlg[F[_]] {
  def seq[A](fas: List[F[A]], p: A => Boolean, z: F[A]): F[A]
  def par[A](fas: List[F[A]], p: A => Boolean, z: F[A]): F[A]
}

object CatsIoAlg extends IoAlg[IO] {
  override def seq[A](fas: List[IO[A]], p: A => Boolean, z: IO[A]): IO[A] =
    fas match {
      case Nil => z
      case head :: tail => head.flatMap(a => if (p(a)) head else seq(tail, p, z))
    }
  override def par[A](fas: List[IO[A]], p: A => Boolean, z: IO[A]): IO[A] =
    z
}
