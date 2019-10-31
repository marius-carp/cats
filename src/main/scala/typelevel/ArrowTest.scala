package typelevel

import cats._
import cats.arrow.Arrow
import cats.data._
import cats.implicits._

object ArrowTest extends App {

  val double: Int => Int = _ * 2
  val addFive: Int => Int = _ + 10

  val doubleAndAddFive = double.andThen(addFive)

  println(doubleAndAddFive(10))

  val fct1: Function1[Int, String] = _.toString

  println(fct1(21))

  def combine[F[_, _]: Arrow, A, B, C](fab: F[A, B], fac: F[A, C]): F[A, (B, C)] =
    Arrow[F].lift((a: A) => (a, a)) >>> (fab *** fac)


}
