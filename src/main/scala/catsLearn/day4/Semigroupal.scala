package catsLearn.day4

import cats._
import simulacrum.typeclass

@typeclass trait Semigroupal[F[_]] {
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
}

trait CartesianLaws[F[_]] {
  implicit def F: Cartesian[F]

  def cartesianAssociativity[A, B, C](fa: F[A], fb: F[B], fc: F[C]): (F[(A, (B, C))], F[((A, B), C)]) =
    (F.product(fa, F.product(fb, fc)), F.product(F.product(fa, fb), fc))


}
