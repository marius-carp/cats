package fp.Chapter5

trait Stream[+A] { self =>
  def uncons: Option[(A, Stream[A])]
  def isEmpty: Boolean = uncons.isEmpty

  def headOption: Option[A] =
    foldRight(Option.empty[A])((a, _) => Some(a))

  def toList: List[A] = {
    this.uncons match {
      case None => Nil
      case Some((head, tail)) => head +: tail.toList
    }
  }

  def take(n: Int): Stream[A] = this.uncons match {
    case None => Stream.empty
    case Some((head, tail)) =>
      if(n == 0)
        Stream.empty
      else if(n == 1)
        Stream(head)
      else
        Stream.cons(head, tail.take(n - 1))
  }

  def takeWhile(f: A => Boolean): Stream[A] = {
    this.uncons match {
      case None => Stream.empty
      case Some((head, tail)) =>
        if(f(head))
          Stream.cons(head, tail.takeWhile(f))
        else
          Stream.empty
    }
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    uncons match {
      case Some((head, tail)) => f(head, tail.foldRight(z)(f))
      case None => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(f: A => Boolean): Boolean =
    foldRight(true)((a, b) => f(a) && b)

  def takeWhileFoldRight(f: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A]){(a, b) =>
      if(f(a))
        Stream.cons(a, b.takeWhileFoldRight(f))
      else
        Stream.empty
    }

  def map[B](f: A => B): Stream[B] = {
    foldRight(Stream.empty[B])((a, b) => Stream.cons(f(a), b))
  }

  def filter(f: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A]){ (a, b) =>
      if(f(a))
        Stream.cons(a, b)
      else
        b
    }
  }

  def append[B >: A](as: => Stream[B]): Stream[B] =
    as.foldRight(this: Stream[B])((a, b) => Stream.cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    map(f).foldRight(Stream.empty[B])((a, b) =>
      a.foldRight(b)((x, y) => Stream.cons(x, y)))

  def mapUnfold[B](f: A => B): Stream[B] =
    Stream.unfold(this) { (a) =>
      a.uncons.map { case (head, tail) =>
        (f(head), tail)
      }
    }

}

object Stream {
  def empty[A]: Stream[A] =
    new Stream[A] {
      override def uncons: Option[(A, Stream[A])] = None
    }

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
    new Stream[A] {
      override def uncons: Option[(A, Stream[A])] = Some((hd, tl))
    }

  def apply[A](as: A*): Stream[A] =
    if(as.isEmpty)
      empty
    else
      cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] =
    cons(a, constant(a))

  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def go(current: Int, next: Int): Stream[Int] =
      cons(current, go(next, current + next))

    go(0, 1)
  }

  def unfold[A, B](zero: A)(f: A => Option[(B, A)]): Stream[B] = {
    def loop(currentInput: A): Stream[B] = f(currentInput) match {
      case Some((b, a)) => Stream.cons(b, loop(a))
      case _ => empty // terminate
    }

    loop(zero)
  }

  def fibsUnfold: Stream[Int] = {
    unfold((0, 1)) {
      case (current, next) => Some((current, (next, current + next)))
    }
  }

  def fromUnfold(n: Int): Stream[Int] =
    unfold(n)((a) => Some(a, a + 1))

  def constantUnfold[A](a: A): Stream[A] =
    unfold(a)((a) => Some(a, a))

}
