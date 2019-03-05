package fp.Chapter13

import fp.Chapter12.Monad

trait IO[+A] { self =>

  def run(): A

  def map[B](f: A => B): IO[B] = new IO[B] {
    override def run(): B = f(self.run())
  }

  def flatMap[B](f: A => IO[B]): IO[B] = new IO[B] {
    override def run(): B = f(self.run()).run()
  }

}

object IO extends Monad [IO] {
  override def unit[A](a: => A): IO[A] = new IO[A] {
    override def run(): A = a
  }

  override def flatMap[A, B](ma: IO[A])(f: A => IO[B]): IO[B] =
    ma.flatMap(f)

  def apply[A](a: => A): IO[A] = unit(a)
}



