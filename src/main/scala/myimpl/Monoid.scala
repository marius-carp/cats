package myimpl

trait Monoid[A] {
  def zero: A

  def op(a1: A, a2: A): A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    override def zero: String = ""
    override def op(a1: String, a2: String): String = a1 + a2
  }

  val intAdditionMonoid = new Monoid[Int] {
    override def zero: Int = 0

    override def op(a1: Int, a2: Int): Int = a1 + a2
  }

  def listMonoid[A] = new Monoid[List[A]] {
    override def zero: List[A] = List.empty[A]

    override def op(a1: List[A], a2: List[A]): List[A] =
      a1 ::: a2
  }

  def optionMonoid[A] = new Monoid[Option[A]] {
    override def zero: Option[A] = None

    override def op(a1: Option[A], a2: Option[A]): Option[A] =
      a1.flatMap(_ => a2)
  }

  def foldMap[A, B](list: List[A], m: Monoid[B])(f: A => B): B = {
    list.map(f).foldLeft(m.zero)(m.op)
  }

  def productMonoid[A, B](a: Monoid[A], b: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    override def zero: (A, B) = (a.zero, b.zero)

    override def op(a1: (A, B), a2: (A, B)): (A, B) =
      (a.op(a1._1, a2._1), b.op(a1._2, a2._2))
  }

  def functionMonoid[A, B](b: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    override def zero: A => B = _ => b.zero

    override def op(a1: A => B, a2: A => B): A => B =
      a => b.op(a1(a), a2(a))
  }

  def optionCompose[A](ma: Monoid[A]): Monoid[Option[A]] = new Monoid[Option[A]] {
    override def zero: Option[A] = None

    override def op(a1: Option[A], a2: Option[A]): Option[A] =
      optionMonoid.op(a1, a2)
  }
}
