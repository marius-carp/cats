package fp.Chapter3

sealed trait List[+A]

case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def head[A](list: List[A]): A =
    list match {
      case Nil => sys.error("head of empty list")
      case Cons(x, _) => x
    }

  def isEmpty[A](list: List[A]): Boolean =
    list match {
      case Nil => true
      case _ => false
    }

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1
    case Cons(0, _) => 0
    case Cons(x, xs) => x * product(xs)
  }

  def sum2(l: List[Int]): Double =
    foldRight(l, 0.0)(_ + _)

  def product2(l: List[Double]): Double =
    foldRight(l, 1.0)(_ * _)

  def sum3(l: List[Int]): Double =
    foldLeft(l, 0.0)(_ + _)

  def product3(l: List[Double]): Double =
    foldLeft(l, 1.0)(_ * _)

  def apply[A](as: A*): List[A] =
    if(as.isEmpty)
      Nil
    else
      Cons(as.head, apply(as.tail: _*))

  def tail[A](list: List[A]): List[A] =
    list match {
      case Nil => Nil
      case Cons(_, xs) => xs
    }

  def drop[A](n: Int, list: List[A]): List[A] =
    list match {
      case Nil => Nil
      case Cons(_, xs) =>
        if(n > 0)
          drop(n - 1, xs)
        else
          list
    }

  def dropWhile[A](list: List[A])(f: A => Boolean): List[A] =
    list match {
      case Nil => Nil
      case Cons(x, xs) =>
        if(f(x))
          dropWhile(xs)(f)
        else
          list
    }

  def setHead[A](x: A, list: List[A]): List[A] =
    Cons(x, list)

  def init[A](list: List[A]): List[A] =
    list match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }

  def foldRight[A, B](list: List[A], z: B)(f: (A, B) => B): B =
    list match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def length[A](list: List[A]): Int =
    foldRight(list, 0)((_, b) => b + 1)

  def length2[A](list: List[A]): Int =
    foldLeft(list, 0)((_, b) => b + 1)

  def foldLeft[A, B](list: List[A], z: B)(f: (A, B) => B): B =
    list match {
      case Nil => z
      case Cons(x, xs) =>
        foldLeft(xs, f(x, z))(f)
    }

  def revers[A](list: List[A]): List[A] =
    foldLeft(list, Nil: List[A])((a, b) => setHead(a, b))

  def appendViaFoldRight[A](a: List[A], b: List[A]): List[A] =
    foldRight(a, b)((x, y) => Cons(x, y))

  def concat[A](a: List[List[A]]): List[A] =
    foldRight(a, Nil: List[A])((a, b) => appendViaFoldRight(a, b))

  def add1(list: List[Int]): List[Int] =
    foldRight(list, Nil: List[Int])((a, b) => Cons(a + 1, b))

  def doubleToString(list: List[Double]): List[String] =
    foldRight(list, Nil: List[String])((a, b) => Cons(a.toString, b))

  def map[A, B](list: List[A])(f: A => B): List[B] =
    foldRight(list, Nil: List[B])((a, b) => Cons(f(a), b))

  def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] =
    foldRight(list, Nil: List[B])((a, b) => appendViaFoldRight(f(a), b))

  def filter[A](list: List[A])(f: A => Boolean): List[A] =
    flatMap(list) { a =>
      if(f(a))
        Cons(a, Nil: List[A])
      else
        Nil: List[A]
    }

  def zipish(a: List[Int], b: List[Int]): List[Int] =
    (a, b) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h, t), Cons(h2, t2)) => Cons(h + h2, zipish(t, t2))
    }

  def zipishGeneral[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] =
    (a, b) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h, t), Cons(h2, t2)) => Cons(f(h, h2), zipishGeneral(t, t2)(f))
    }

  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = {
    def go(l: List[A], subSub: List[A], found: Boolean): Boolean =
      l match {
        case Nil =>
          if(isEmpty(subSub))
            found
          else
            false
        case Cons(x, xs) =>
          subSub match {
            case Nil =>
              found
            case Cons(subX, subXs) if subX == x =>
              go(xs, subXs, found = true)
            case Cons(subX, subXs) =>
              go(xs, sub, found = false)
          }
      }

    go(l, sub, found = false)
  }
}
