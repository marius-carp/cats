package cats

import simulacrum.typeclass

@typeclass trait Applicative[F[_]] extends Functor[F]{ self =>

  //take 1 and lift it to List(1)
  def pure[A](a: A): F[A]

  def apply[A, B](fa: F[A])(ff: F[A => B]): F[B]

  def apply2[A, B, Z](fa: F[A], fb: F[B])(ff: F[(A, B) => Z]): F[Z] =
    apply(fa)(apply(fb)(map(ff)(f => b => a => f(a, b))))

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    apply(fa)(pure(f))

  def map2[A, B, Z](fa: F[A], fb: F[B])(f: (A, B) => Z): F[Z] =
    apply(fa)(map(fb)(b => f(_, b)))

  def map3[A, B, C, Z](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => Z): F[Z] =
    apply(fa)(map2(fb, fc)((b, c) => a => f(a, b, c)))

  def map4[A, B, C, D, Z](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => Z): F[Z] =
    map2(tuple2(fa, fb), tuple2(fc, fd)) {
      case ((a, b), (c, d)) => f(a, b, c, d)
    }

  def map5[A, B, C, D, E, Z](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E])(f: (A, B, C, D, E) => Z): F[Z] =
    map2(tuple2(fa, fb), tuple3(fc, fd, fe)) {
      case((a, b), (c, d, e)) => f(a, b, c, d, e)
    }

  def tuple2[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((a, b) => (a, b))

  def tuple3[A, B, C](fa: F[A], fb: F[B], fc: F[C]): F[(A, B, C)] =
    map3(fa, fb, fc)((a, b, c) => (a, b, c))

  def flip[A, B](ff: F[A => B]): F[A] => F[B] =
    fa => apply(fa)(ff)

  def compose[G[_]](implicit G: Applicative[G]): Applicative[Lambda[X => F[G[X]]]] =
    new Applicative[Lambda[X => F[G[X]]]] {
      override def pure[A](a: A): F[G[A]] = self.pure(G.pure(a))

      override def apply[A, B](fga: F[G[A]])(ff: F[G[A => B]]): F[G[B]] = {
        val x: F[G[A] => G[B]] = self.map(ff)(gab => G.flip(gab))
        self.apply(fga)(x)
      }
    }
}

object Applicative {

  implicit val optionalApplicative: Applicative[Option] = new Applicative[Option] {
    override def pure[A](a: A): Option[A] = Some(a)

    override def apply[A, B](fa: Option[A])(ff: Option[A => B]): Option[B] = (fa, ff) match {
      case (None, _) => None
      case (Some(a), None) => None
      case (Some(a), Some(f)) => Some(f(a))
    }

  }

  implicit val listApplicative: Applicative[List] = new Applicative[List] {
    override def pure[A](a: A): List[A] = List(a)

    override def apply[A, B](fa: List[A])(ff: List[A => B]): List[B] =
      for {
        a <- fa
        f <- ff
      } yield f(a)
  }

}
