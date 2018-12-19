package fp.Chapter5

trait Stream[+A] { self =>
  def uncons: Option[(A, Stream[A])]
  def isEmpty: Boolean = uncons.isEmpty

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

  def takeWhileFoldRight(f: A => Boolean): Stream[A] = ???

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


}
