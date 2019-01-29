package fp.Chapter11

import fp.Chapter7.Par
import fp.Chapter7.Par.Par
import fp.Chapter8.Gen

trait Monad[M[_]] extends Functor[M] {

  def unit[A](a: => A): M[A]
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  def map[A, B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma) { a =>
      unit(f(a))
    }

  def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma) { a =>
      map(mb) { b =>
        f(a, b)
      }
    }

  def sequence[A](lma: List[M[A]]): M[List[A]] =
    traverse(lma)(x => x)

  def traverse[A, B](la: List[A])(f: A => M[B]): M[List[B]] = {
    la.foldRight(unit(List.empty[B])){ (a, mList) =>
      map2(f(a), mList){ (a, b) => a :: b }
    }
  }

  def replicateM[A](n: Int, ma: M[A]): M[List[A]] =
    sequence(List.fill(n)(ma))

  def factor[A, B](ma: M[A], mb: M[B]): M[(A, B)] =
    map2(ma, mb)((_, _))

  def cofactor[A, B](e: Either[M[A], M[B]]): M[Either[A, B]] =
    e match {
      case Left(ma) =>
        map(ma)(Left(_))
      case Right(mb) =>
        map(mb)(Right(_))
    }

  def compose[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] =
    a => flatMap(f(a))(g)

  def flatMapAsCompose[A, B](ma: M[A])(f: A => M[B]): M[B] =
    compose((_: Unit) => ma, f)(())

  def join[A](mma: M[M[A]]): M[A] =
    flatMap(mma)(r => r)

  def flatMapAsJoin[A, B](ma: M[A])(f: A => M[B]): M[B] =
    join(map(ma)(f))

  def composeAsJoin[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] =
    a => {
      join(map(f(a))(g))
    }
}

object Monad {
  val genMonad: Monad[Gen] = new Monad[Gen] {
    override def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] = ma.flatMap(f)

    override def unit[A](a: => A): Gen[A] = Gen.unit(a)
  }

  val parMonad: Monad[Par] = new Monad[Par] {
    override def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] = Par.flatMap(ma)(f)

    override def unit[A](a: => A): Par[A] = Par.unit(a)
  }

  val optionMonad: Monad[Option] = new Monad[Option] {
    override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma.flatMap(f)

    override def unit[A](a: => A): Option[A] = Some(a)
  }

  val streamMonad: Monad[Stream] = new Monad[Stream] {
    override def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]): Stream[B] = ma.flatMap(f)

    override def unit[A](a: => A): Stream[A] = Stream.apply(a)
  }

  val listMonad: Monad[List] = new Monad[List] {
    override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma.flatMap(f)

    override def unit[A](a: => A): List[A] = List(a)
  }

}
