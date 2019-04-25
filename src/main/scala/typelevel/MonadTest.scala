package typelevel

import cats.{Applicative, Monad}

import scala.annotation.tailrec
import scala.reflect.runtime.universe

object MonadTest {

  implicit def optionMonad(implicit app: Applicative[Option]) = new Monad[Option] {
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
      app.map(fa)(f).flatten

    override def pure[A](x: A): Option[A] =
      app.pure(x)

    @tailrec
    override def tailRecM[A, B](init: A)(f: A => Option[Either[A, B]]): Option[B] =
      f(init) match {
        case None => None
        case (Some(Right(b))) => Some(b)
        case (Some(Left(a))) => tailRecM(a)(f)
      }
  }

  val result = universe.reify(
    for {
      x <- Some(1)
      y <- Some(2)
    } yield x + y
  ).tree

  val result2 = Some.apply(1).flatMap((x) => Some.apply(2).map((y) => x.$plus(y)))

  case class OptionT[F[_], A](value: F[Option[A]])

  implicit def optionTMonad[F[_]](implicit F: Monad[F]) = new Monad[OptionT[F, ?]] {
    override def flatMap[A, B](fa: OptionT[F, A])(f: A => OptionT[F, B]): OptionT[F, B] =
      OptionT {
        F.flatMap(fa.value) {
          case None => F.pure(None)
          case Some(a) => f(a).value
        }
      }

    override def pure[A](x: A): OptionT[F, A] =
      OptionT(F.pure(Some(x)))

    override def tailRecM[A, B](a: A)(f: A => OptionT[F, Either[A, B]]): OptionT[F, B] =
      OptionT {
        F.tailRecM(a)(a0 => F.map(f(a0).value) {
          case None => Right(None)
          case Some(b0) => b0.map(Some(_))
        })
      }
  }

}
