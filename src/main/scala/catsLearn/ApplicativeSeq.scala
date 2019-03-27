package catsLearn

import cats._
import cats.data._
import cats.implicits._

object ApplicativeSeq {

  def sequanceA[F[_]: Applicative, A](list: List[F[A]]): F[List[A]] = list match {
    case Nil =>
      Applicative[F].pure(Nil: List[A])
    case x :: xs =>
      (x, sequanceA(xs)) mapN {_ :: _}
  }

}
