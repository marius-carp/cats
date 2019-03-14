package video

import simulacrum._

import scala.language.higherKinds


@typeclass trait Functor[F[_]] { self =>

  def map[A, B](fa: F[A])(f: A => B): F[B]

  def lift[A, B](f: A => B): F[A] => F[B] =
    fa => map(fa)(f)

  def as[A, B](fa: F[A], b: => B): F[B] =
    map(fa)(_ => b)

  def void[A](fa: F[A]): F[Unit] =
    map(fa)(_ => ())

  def compose[G[_]](implicit G: Functor[G]): Functor[Lambda[X => F[G[X]]]] =
    new Functor[Lambda[X => F[G[X]]]] {
      override def map[A, B](fga: F[G[A]])(f: A => B): F[G[B]] =
        self.map(fga)(ga => G.map(ga)(a => f(a)))
    }
}

object Functor {

  implicit val listFunctor: Functor[List] = new Functor[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B] =
      fa.map(f)
  }

  implicit val optionFunctor: Functor[Option] = new Functor[Option] {
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] =
      fa.map(f)
  }

  implicit def function1Functor[X]: Functor[X => ?] = new Functor[X => ?] {
    override def map[A, B](fa: X => A)(f: A => B): X => B = fa andThen f
  }
}

