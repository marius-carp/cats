package fp.Chapter12

import fp.Chapter11.Functor

trait Applicative[F[_]] extends Functor[F] {

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(map(fa)(f.curried))(fb)

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)((a, b) =>  a(b))

  def unit[A](a: A): F[A]

  def map[A, B](a: F[A])(f: A => B): F[B] =
    apply(unit(f))(a)

  def mapAsMap2AndUnit[A, B](a: F[A])(f: A => B): F[B] =
    map2(a, unit(f))((a, b) => b(a))

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(x => x)

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] = {
    as.foldRight(unit(List.empty[B])) {(a, mList) =>
      map2(f(a), mList) { (a, b) => a :: b}
    }
  }

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = {
    sequence(List.fill(n)(fa))
  }

  def factor[A, B](fa: F[A], fb: F[B]): F[(A, B)] = {
    map2(fa, fb)((a, b) => (a, b))
  }
}

object Applicative {
  def apply[A, B](oab: Option[A => B])(oa: Option[A]): Option[B] =
    (oab, oa) match {
      case (Some(f), Some(a)) => Some(f(a))
      case _ => None
    }




}


