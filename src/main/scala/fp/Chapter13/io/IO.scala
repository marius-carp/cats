package fp.Chapter13.io

import fp.Chapter11.Monad


trait IO[F[_], +A]

trait Runnable[A] {
  def run: A
}

object Delay {
  def apply[A](a: => A): Runnable[A] = new Runnable[A] {
    override def run: A = a
  }
}

case class Pure[F[_], +A](get: A) extends IO[F, A]
case class Request[F[_], I, +A](expr: F[I],
                                receive: I => IO[F, A]) extends IO[F, A]

trait Console[A]

case object ReadLine extends Console[Option[String]]
case class PrintLine(s: String) extends Console[Unit]

trait Run[F[_]] {
  def apply[A](expr: F[A]): (A, Run[F])
}

object IO {

  def run[F[_], A](r: Run[F])(io: IO[F, A]): A = io match {
    case Pure(a) => a
    case Request(expr, recv) =>
      r(expr) match {
        case (e, r2) => run(r2)(recv(e))
      }
  }

  def monad[F[_]] = new Monad[IO[F, ?]] {
    override def unit[A](a: => A): IO[F, A] =
      Pure(a)

    override def flatMap[A, B](ma: IO[F, A])(f: A => IO[F, B]): IO[F, B] = {
       ma match {
        case Pure(a) => f(a)
        case Request(expr, recv) => ??? //Request(expr, recv andThen(_.flatMap f))
      }
    }
  }

  def console(lines: List[String]): Run[Console] = {
    new Run[Console] {
      def apply[A](c: Console[A]) = c match {
        case ReadLine => ???

        case PrintLine(_) => ???
      }
    }
  }

  def run[F[_], A](f: Monad[F])(io: IO[F, A]): F[A] = {
    io match {
      case Pure(a) => f.unit(a)
      case Request(exr, recv) =>
        f.flatMap(exr)(e => run(f)(recv(e)))
    }
  }

}