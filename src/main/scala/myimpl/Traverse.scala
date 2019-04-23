package myimpl

import cats.{Applicative => ApplicativeT}
import cats.implicits._
import typelevel.ApplicativeTest.Id

trait Traverse[F[_]] extends Functor[F] {
  def traverse[G[_]: ApplicativeT, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse(fa)(a => Id(f(a))).value
}

object Traverse {

  implicit val traverseForList: Traverse[List] = new Traverse[List] {
    def traverse[G[_]: ApplicativeT, A, B](fa: List[A])(f: A => G[B]): G[List[B]] =
      fa.foldRight(ApplicativeT[G].pure(List.empty[B])) { (a, acc) =>
        ApplicativeT[G].map2(f(a), acc)(_ :: _)
      }

    override def map[A, B](fa: List[A])(f: A => B): List[B] =
      fa.map(f)
  }



}
