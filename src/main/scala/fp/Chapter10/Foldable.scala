package fp.Chapter10

import fp.Chapter3.{Branch, Leaf, Tree}
import fp.Chapter10.Monoid._

trait Foldable[F[_]] {

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](fa: F[A]): List[A] = {
    foldRight(fa)(List.empty[A])((a, b) => a :: b)
  }
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)((a, b) => f(a, b))

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)((b, a) => f(b, a))

  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    Monoid.foldMap(as, mb)(f)
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A,B](as: IndexedSeq[A])(z: B)(f: (A,B) => B): B =
    as.foldRight(z)(f)

  override def foldLeft[A,B](as: IndexedSeq[A])(z: B)(f: (B,A) => B): B =
    as.foldLeft(z)(f)

  override def foldMap[A,B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    foldMapV(as, mb)(f)
}

object StreamFoldable extends Foldable[Stream] {

  override def foldRight[A,B](as: Stream[A])(z: B)(f: (A,B) => B): B =
    as.foldRight(z)(f)

  override def foldLeft[A,B](as: Stream[A])(z: B)(f: (B,A) => B): B =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: Stream[A])(f: A => B)(mb: Monoid[B]): B = ???
}

object TreeFoldable extends Foldable[Tree] {

  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B =
    as match {
      case Leaf(value) => f(value, z)
      case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
    }

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B =
    as match {
      case Leaf(v) => f(z, v)
      case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
    }

  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    as match {
      case Leaf(v) => f(v)
      case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
    }
}

object OptionFoldable extends Foldable[Option] {
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B =
    as match {
      case None => z
      case Some(v) => f(v, z)
    }

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B =
    as match {
      case None => z
      case Some(v) => f(z, v)
    }

  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    as match {
      case None => mb.zero
      case Some(v) => f(v)
    }
}
