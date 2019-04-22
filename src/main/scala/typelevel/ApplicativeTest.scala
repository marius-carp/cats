package typelevel

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

}
