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

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    override def op(x: A, y: A): A = m.op(y, x)
    override def zero: A = m.zero
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

  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if(v.isEmpty)
      m.zero
    else if(v.size == 1)
      f(v(0))
    else {
      val a = v.splitAt(v.size / 2)
      m.op(foldMapV(a._1, m)(f), foldMapV(a._2, m)(f))
    }
  }

  def ordered(ints: IndexedSeq[Int]): Boolean = {
    // Reference implementation
    // (Int, Int, Boolean) = (Min, Max, IsOrdered)
    val mon = new Monoid[Option[(Int, Int, Boolean)]] {
      override def op(o1: Option[(Int, Int, Boolean)], o2: Option[(Int, Int, Boolean)]): Option[(Int, Int, Boolean)] =
        (o1, o2) match {
          case (Some((x1, y1, p)), Some((x2, y2, q))) =>
            Some((x1 min x2, y1 max y2, p && q && y1 <= x2))
          case (x, None) => x
          case (None, x) => x
        }
      override def zero: None.type = None
    }
    foldMapV(ints, mon)(i => Some((i, i, true))).forall(_._3)
  }

  def productMonoid[A, B](a: Monoid[A], b: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    override def zero: (A, B) = (a.zero, b.zero)

    override def op(a1: (A, B), a2: (A, B)): (A, B) = {
      (a.op(a1._1, a2._1), b.op(a1._2, a2._2))
    }
  }

  def coproductMonoid[A, B](a: Monoid[A], b: Monoid[B]): Monoid[Either[A, B]] = new Monoid[Either[A, B]] {
    override def zero: Either[A, B] = Right(b.zero)

    override def op(a1: Either[A, B], a2: Either[A, B]): Either[A, B] = (a1, a2) match {
      case (Left(al1), Left(al2)) => Left(a.op(al1, al2))
      case (Right(br1), Right(br2)) => Right(b.op(br1, br2))
      case (Left(al), _) => Left(al)
      case (_, Left(al)) => Left(al)
    }
  }

  def mapMergeMonoid[K, V](v: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      override def zero: Map[K, V] = Map.empty[K, V]

      override def op(a1: Map[K, V], a2: Map[K, V]): Map[K, V] =
        a1.map {
          case (k, v1) => (k, v.op(v1, a2.getOrElse(k, v.zero)))
        }
    }

  def functionMonoid[A, B](b: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    override def zero: A => B = a => b.zero

    override def op(a1: A => B, a2: A => B): A => B = {
      a => b.op(a1(a), a2(a))
    }
  }

  def frequencyMap(strings: IndexedSeq[String]): Map[String, Int] = {
    bag(strings)
  }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    foldMapV(as, mapMergeMonoid[A, Int](intAddition))((a: A) => Map(a -> 1))
}
