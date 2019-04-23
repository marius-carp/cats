package typelevel

import cats.Applicative
import cats.implicits._

object TraverseTest {

  /*def traverse[F[_]: Applicative, A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(Applicative[F].pure(List.empty[B])) { (a: A, acc: F[List[B]]) =>
      val fb: F[B] = f(a)
      Applicative[F].map2(fb, acc)(_ :: _)
    }

  val result = Traverse.traverseForList.traverse(List(1,2,3))(i => Some(i.toString): Option[String])*/

  val list: List[Option[Int]] = List(Some(1), Some(2), Some(21))

  val traversed = list.traverse(identity)




}

