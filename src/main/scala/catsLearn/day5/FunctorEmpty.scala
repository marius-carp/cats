package catsLearn.day5

import cats._
import cats.data._
import cats.implicits._

trait FunctorEmpty[F[_]] extends Serializable {

  val functor: Functor[F]

  def mapFilter[A, B](fa: F[A])(f: A => Option[B]): F[B]

  def collect[A, B](fa: F[A])(f: PartialFunction[A, B]): F[B]

  def flattenOption[A](fa: F[Option[A]]): F[A]

  def filter[A](fa: F[A])(f: A => Boolean): F[A]

}

object FunctorEmpty {

  val eng = Map(
    1 -> "one",
    3 -> "three",
    10 -> "ten"
  )

  (1 to 50).toList mapFilter(eng.get(_))

}
