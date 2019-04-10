package catsLearn.day9

import cats._
import cats.data._
import cats.implicits._

object FoldMTest {

  def binSmalls(acc: Int, x: Int): Option[Int] =
    if(x > 9)
      none[Int]
    else
      (acc + x).some

  val result = Foldable[List].foldM(List(2, 8, 3, 1), 0)(binSmalls)
  val result2 = Foldable[List].foldM(List(2, 11, 3, 1), 0)(binSmalls)

  def parseInt(x: String): Option[Int] =
    (scala.util.Try(x.toInt) map { Some(_) }
      recover { case _: NumberFormatException => None }).get

  def foldingFunction(list: List[Double], next: String): Option[List[Double]] =
    (list, next) match {
      case (x :: y :: ys, "*") => ((y * x) :: ys).some
      case (x :: y :: ys, "+") => ((y + x) :: ys).some
      case (x :: y :: ys, "-") => ((y - x) :: ys).some
      case (xs, numString) => parseInt(numString) map {_ :: xs}
    }

  def solveRPN(s: String): Option[Double] =
    for {
      List(x) <- Foldable[List].foldM(s.split(' ').toList, Nil: List[Double])(foldingFunction)
    } yield x

  val result3 = solveRPN("1 2 * 4 + 10 -")
}
