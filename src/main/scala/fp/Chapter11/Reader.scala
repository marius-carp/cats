package fp.Chapter11

case class Reader[R, A](run: R => A)

object Reader {
  def readerMonad[R] = new Monad[Reader[R, ?]] {
    override def unit[A](a: => A): Reader[R, A] = Reader(_ => a)

    override def flatMap[A, B](ma: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] =
      Reader { r =>
        val a = ma.run(r)

        f(a).run(r)
      }
  }
}
