package fp.Chapter1

import scala.annotation.tailrec

class Functions {

  def fib(n: Int): Int = {
    @tailrec
    def fib(n: Int, a: Int, b: Int): Int = {
      if(n == 0)
        a
      else if(n == 1)
        b
      else
        fib(n - 1, b, a + b)

    }

    fib(n, 0, 1)
  }

  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {

    @tailrec
    def go(as: Array[A], last: A, current: A): Boolean = {
      if (as.isEmpty)
        true
      else if (gt(last, current))
        go(as.tail, current, as.head)
      else
        false
    }

    val tail = as.tail


    go(tail.tail, as.head, tail.head)

  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = {
    f(a, _: B)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => f(a, _: B)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }


}
