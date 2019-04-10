package catsLearn.day9

import cats._
import cats.data._
import cats.implicits._

import scala.annotation.tailrec

case class Prob[A](list: List[(A, Double)])

trait ProbInstance { self =>

  def flatten[B](xs: Prob[Prob[B]]): Prob[B] = {
    def multall(innerxs: Prob[B], p: Double) =
      innerxs.list map { case (x, r) => (x, p * r) }
    Prob((xs.list map { case (innerxs, p) => multall(innerxs, p) }).flatten)
  }

  implicit val probInstance: Monad[Prob] = new Monad[Prob] {
    override def pure[A](x: A): Prob[A] = Prob((x, 1d) :: Nil)

    override def flatMap[A, B](fa: Prob[A])(f: A => Prob[B]): Prob[B] = {
      self.flatten(map(fa)(f))
    }

    override def map[A, B](fa: Prob[A])(f: A => B): Prob[B] =
      Prob(fa.list map { case (x, p) => (f(x), p) })

    def tailRecM[A, B](a: A)(f: A => Prob[Either[A, B]]): Prob[B] = {
      val buf = List.newBuilder[(B, Double)]
      @tailrec def go(lists: List[List[(Either[A, B], Double)]]): Unit =
        lists match {
          case (ab :: abs) :: tail => ab match {
            case (Right(b), p) =>
              buf += ((b, p))
              go(abs :: tail)
            case (Left(a), p) =>
              go(f(a).list :: abs :: tail)
          }
          case Nil :: tail => go(tail)
          case Nil => ()
        }
      go(f(a).list :: Nil)
      Prob(buf.result)
    }
  }

  implicit def probShow[A]: Show[Prob[A]] = Show.fromToString
}

case object Prob extends ProbInstance {

  val result = Prob(List((3, 0.5), (5, 0.25), (9, 0.25))).map(- _)

}


