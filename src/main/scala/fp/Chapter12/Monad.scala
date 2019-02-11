package fp.Chapter12

trait Monad[M[_]] extends Applicative[M] {

  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] =
    join(map(ma)(f))

  def join[A](mma: M[M[A]]): M[A] = flatMap(mma)(ma => ma)

  def compose[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] =
    a => flatMap(f(a))(g)

  override def apply[A, B](mf: M[A => B])(ma: M[A]): M[B] =
    flatMap(mf)(f => flatMap(ma)(a => unit(f(a))))

}
