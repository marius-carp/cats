package fp.Chapter10

trait Monoid[A] {

  def op(a1: A, a2: A): A
  def zero: A

}

object Monoid {

  val stringMonoid: Monoid[String] = new Monoid[String] {
    override def op(a1: String, a2: String): String = a1 + a2

    override def zero: String = ""
  }

  def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    override def op(a1: List[A], a2: List[A]): List[A] = a1 ::: a2

    override def zero: List[A] = List.empty[A]
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def zero: Int = 0

    override def op(a1: Int, a2: Int): Int = a1 + a2
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def zero: Int = 1

    override def op(a1: Int, a2: Int): Int = a1 * a2
  }

  def booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def zero: Boolean = false

    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
  }

  def booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def zero: Boolean = true

    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] =
      a1.flatMap(_ => a2)

    override def zero: Option[A] = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def zero: A => A = { a => a }

    override def op(a1: A => A, a2: A => A): A => A = {
      a1 compose a2
    }
  }

  def wordsMonoid(s: String): Monoid[String] = new Monoid[String] {
    override def zero: String = s

    override def op(a1: String, a2: String): String = {
      a1.trim + " " + a2.trim
    }
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A = {
    as.foldLeft(m.zero)(m.op)
  }

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = {
    as.map(f).foldLeft(m.zero)(m.op)
  }

  def foldMapAsFold[A, B](as: List[A], m: Monoid[B])(f: A => B): B = {
    as.foldLeft(m.zero){(a, b) =>
      m.op(a, f(b))
    }
  }

  def foldLeftFold[A, B](as: List[A], z: B)(f: (B,A) => B): B = {
    val mb: Monoid[B => B] = endoMonoid[B]
    foldMap(as, mb)(a => b => f(b, a))(z)
  }
}
