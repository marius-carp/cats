package cats

import simulacrum.typeclass

@typeclass trait Monad[F[_]] extends Applicative[F] { self =>

  def pure[A](a: A): F[A]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  override def apply[A, B](fa: F[A])(ff: F[A => B]): F[B] =
    flatMap(ff)((f: A => B) => map(fa)(f))

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => pure(f(a)))

  def flatten[A](ffa: F[F[A]]): F[A] =
    flatMap(ffa)(fa => fa)

  def compose[G[_]](implicit G: Monad[G]): Monad[Lambda[X => F[G[X]]]] =
    new Monad[Lambda[X => F[G[X]]]] {
      override def pure[A](a: A): F[G[A]] = self.pure(G.pure(a))

      override def flatMap[A, B](fga: F[G[A]])(f: A => F[G[B]]): F[G[B]] = {
        val nested = self.map(fga) {ga => G.map(ga) {a => f(a): F[G[B]] }: G[F[G[B]]] }: F[G[F[G[B]]]]
        flatten(nested)
      }
    }
}


object Monad {

  implicit val listMonad: Monad[List] = new Monad[List] {
    override def pure[A](a: A): List[A] = List(a)

    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = {
      fa.flatMap(f)
    }
  }

  implicit val optionMonad: Monad[Option] = new Monad[Option] {
    override def pure[A](a: A): Option[A] = Some(a)

    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
      fa.flatMap(f)
  }

}


