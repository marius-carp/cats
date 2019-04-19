package monoid

import myimpl.Functor
import org.specs2.execute.Result
import org.specs2.mutable.Specification

class FunctorSpec extends Specification {

  "composition" should composition(Functor.functorOption, Some(21))((i: Int) => i.toString, (s: String) => s.toInt + 1 )
  "identity" should identity(Functor.functorOption, Some(21))

  def composition[F[_], A, B, C](fa: Functor[F], value: F[A])(f: A => B, g: B => C): Result = {
    val bs = fa.map(value)(f)
    val lhs = fa.map(bs)(g)

    val rhs = fa.map(value)(f.andThen(g))

    lhs should beEqualTo(rhs)
  }

  def identity[F[_], A](fa: Functor[F], value: F[A]): Result = {
    fa.map(value)(x => x) should beEqualTo(value)
  }

}
