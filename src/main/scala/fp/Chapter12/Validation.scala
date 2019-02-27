package fp.Chapter12

import java.util.Date

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E]) extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]

object Validation {
  def valid[E]: Applicative[Validation[E, ?]] = new Applicative[Validation[E, ?]] {

    override def unit[A](a: => A): Validation[E, A] = Success(a)

    override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] = {
      (fa, fb) match {
        case (Success(a), Success(b)) =>  unit(f(a, b))
        case (Failure(h1, t1), Failure(h2, t2)) => Failure(h1, t1 ++ t2 :+ h2)
        case (e@Failure(_, _), _) => e
        case (_, e@Failure(_, _)) => e
      }
    }
  }
}
