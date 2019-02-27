package fp.Chapter12

import fp.Chapter10.{Foldable, Monoid}
import fp.Chapter11.Functor

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {

  def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: Applicative[G]): G[F[B]] =
    sequence(map(fa)(f))

  def sequence[M[_]: Applicative, A](fma: F[M[A]]): M[F[A]] =
    traverse(fma)(ma => ma)


  type Id[A] = A

  implicit val idMonad: Monad[Id] = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = a

    override def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = f(ma)
  }

  override def map[A, B](fa: F[A])(f: A => B): F[B] = {
    traverse(fa){ r =>
      val k = f(r)
      idMonad.unit(k)
    }
  }

  type Const[A, B] = A

  implicit def monoidApplicative[M](M : Monoid[M]) = new Applicative[Traverse.this.Const[M, ?]] {
    override def unit[A](a: => A): Const[M, A] = M.zero
  }

}

object Traverse {

  def listTraverse = new Traverse[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B] = Functor.listFunctor.map(fa)(f)

    override def traverse[G[_], A, B](fa: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] = {
      fa.foldLeft(G.unit(List.empty[B]))((bList, a) => G.map2(f(a), bList)(_ :: _))
    }
  }

  def optionList = new Traverse[Option] {
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] =
      fa match {
        case Some(a) => Some(f(a))
        case None => None
      }

    override def traverse[G[_], A, B](fa: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] =
      fa match {
        case Some(a) => G.map(f(a))(Some(_))
        case None => G.unit(None)
      }
  }


}
