package typelevel

import cats.Monoid
import cats.data.NonEmptyList._
import cats.data._
import cats.implicits._


object MonoidTest {


  def combineAll[A: Monoid](as: List[A]): A =
    as.foldLeft(Monoid[A].empty)(Monoid[A].combine)

  val result: Int = combineAll(List(1, 2, 3, 4))

  /*final case class NonEmptyList[A](head: A, tail: List[A]) {

    def ++(other: NonEmptyList[A]) =
      NonEmptyList(head, tail ++ other.toList)

    def toList: List[A] = head :: tail
  }

  object NonEmptyList {
    implicit def nonEmptyListSemigroup[A]: Semigroup[NonEmptyList[A]] =
      new Semigroup[NonEmptyList[A]] {
        def combine(x: NonEmptyList[A], y: NonEmptyList[A]): NonEmptyList[A] = x ++ y
      }
  }

  implicit def optionMonad[A: Semigroup]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def empty: Option[A] = None

    override def combine(x: Option[A], y: Option[A]): Option[A] =
      x match {
        case None => y
        case Some(xs) =>
          y match {
            case None => x
            case Some(ys) => Some(xs |+| ys)
          }
      }
  }
*/

  val list = List(
    NonEmptyList(1, List(2, 3)),
    NonEmptyList(4, List(5, 6))
  )
  val lifted: List[Option[NonEmptyList[Int]]] = list.map(nel => Option(nel))

  val result2: Option[NonEmptyList[Int]] = Monoid.combineAll(lifted)




}
