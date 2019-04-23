package typelevel

import java.sql.Connection

import cats.Applicative
import cats.data.Nested
import cats.implicits._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object ApplicativeTest {


  /*def fct[A, B]()(f: A => B): Option[A => B] = {
    Some {
      a => f(a)
    }
  }

  val result = fct[Int, String]()(_.toString)

  val result2 = Applicative.optionApplicative.ap(fct[Int, String]()(_.toString))(Some(32))

  val result3 = Applicative.optionApplicative.product(Some(3), Some(2))
  */

  val f: (Int, Char) => Double = (i, c) => (i + c).toDouble

  val int: Option[Int] = Some(5)
  val char: Option[Char] = Some('a')

  val result = int.map(i => (c: Char) => f(i, c))


  val x: Future[Option[Int]] = Future.successful(Some(5))
  val y: Future[Option[Char]] = Future.successful(Some('a'))

  val composed = Applicative[Future].compose[Option].map2(x, y)(_ + _)
  val nested = Applicative[Nested[Future, Option, ?]].map2(Nested(x), Nested(y))(_ + _)

  val username: Option[String] = Some("username")
  val password: Option[String] = Some("password")
  val url: Option[String] = Some("some.login.url.here")

  def attemptConnect(username: String, password: String, url: String): Option[Connection] = None

  Applicative[Option].map3(username, password, url)(attemptConnect)

  def traverseOption[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight(Some(List.empty[B]): Option[List[B]]) { (a: A, acc:Option[List[B]]) =>
      val optB = f(a)

      Applicative[Option].map2(optB, acc)(_ :: _)
    }

  val result2 = traverseOption(List(1, 2, 3, 4))(i => Some(i))

  def traverseEither[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight(Right(List.empty[B]): Either[E, List[B]]) { (a: A, acc: Either[E, List[B]]) =>
      val eitherB: Either[E, B] = f(a)
      Applicative[Either[E, ?]].map2(eitherB, acc)(_ :: _)
    }

  val result3 = traverseEither(List(1, 2, 3))(i => if (i % 2 != 0) Left(s"${i} is not even") else Right(i / 2))

  def traverse[F[_]: Applicative, A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(Applicative[F].pure(List.empty[B])) { (a: A, acc: F[List[B]]) =>
      val fb: F[B] = f(a)
      Applicative[F].map2(fb, acc)(_ :: _)
    }

  val result4 = List(1, 2, 3).traverse(i => Some(i): Option[Int])

  final case class Id[A](value: A)

  implicit val applicativeForId: Applicative[Id] = new Applicative[Id] {
    override def pure[A](x: A): Id[A] = Id(x)

    override def ap[A, B](ff: Id[A => B])(fa: Id[A]): Id[B] =
      Id(ff.value(fa.value))
  }

}
