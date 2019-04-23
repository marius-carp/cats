package myimpl

trait Applicative[F[_]] extends Functor[F] {

  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]

  def pure[A](a: A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] = ap(pure(f))(fa)

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    ap(map(fa)(a => (b: B) => (a, b)))(fb)

  def map2[A, B, Z](fa: F[A], fb: F[B])(f: (A, B) => Z): F[Z] =
    ap(map(fb)(b => f(_: A, b)))(fa)
}

object Applicative {

  implicit val optionApplicative: Applicative[Option] = new Applicative[Option] {
    override def pure[A](a: A): Option[A] = Some(a)

    override def ap[A, B](ff: Option[A => B])(fa: Option[A]): Option[B] =
      (ff, fa) match {
        case (None, _) => None
        case (Some(_), None) => None
        case (Some(f), Some(a)) => Some(f(a))
      }
  }

  implicit val listApplicative: Applicative[List] = new Applicative[List] {
    override def pure[A](a: A): List[A] = List(a)

    override def ap[A, B](ff: List[A => B])(fa: List[A]): List[B] = {
      for {
        f <- ff
        a <- fa
      } yield f(a)
    }
  }

  final case class Id[A](value: A)

  implicit val applicativeForId: Applicative[Id] = new Applicative[Id] {
    override def pure[A](x: A): Id[A] = Id(x)

    override def ap[A, B](ff: Id[A => B])(fa: Id[A]): Id[B] =
      Id(ff.value(fa.value))
  }
}
