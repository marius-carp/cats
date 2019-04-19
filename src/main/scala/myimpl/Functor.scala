package myimpl

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def lift[A, B](f: A => B): F[A] => F[B] =
    fa => map(fa)(f)
}

object Functor {
  implicit val functorOption: Functor[Option] = new Functor[Option] {
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] =
      fa match {
        case None => None
        case Some(value) => Some(f(value))
      }
  }

  implicit val functorList: Functor[List] = new Functor[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B] =
      fa.map(f)
  }

}


