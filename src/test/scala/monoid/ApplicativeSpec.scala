package monoid

import myimpl.Applicative
import org.specs2.execute.Result
import org.specs2.mutable.Specification

class ApplicativeSpec extends Specification {

  "associativity" in associativity(Applicative.optionApplicative, Some(1), Some(2), Some(3))
  "leftIdentity" in leftIdentity(Applicative.optionApplicative, Some(1))
  "rightIdentity" in rightIdentity(Applicative.optionApplicative, Some(1))

  def associativity[F[_], A, B, C](fa: Applicative[F], a: F[A], b: F[B], c: F[C]): Result = {
    fa.product(a, fa.product(b, c)) should beEqualTo {
      fa.map(fa.product(fa.product(a, b), c)){ case ((a, b), c) => (a, (b, c))}
    }
  }

  def leftIdentity[F[_], A](fa: Applicative[F], value: F[A]): Result = {
    fa.map(fa.product(fa.pure(()), value))(_._2) should beEqualTo(value)
  }

  def rightIdentity[F[_], A](fa: Applicative[F], value: F[A]): Result =
    fa.map(fa.product(value, fa.pure(())))(_._1) should beEqualTo(value)
}
