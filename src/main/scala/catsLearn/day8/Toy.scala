package catsLearn.day8

import cats._
import cats.data._
import cats.free.Free
import cats.implicits._
import catsLearn.day8.Toy.Output

trait Toy[+A, +NEXT]

object Toy {
  case class Output[A, Next](a: A, next: Next) extends Toy[A, Next]
  case class Bell[Next](next: Next) extends Toy[Nothing, Next]
  case class Done() extends Toy[Nothing, Nothing]
}

sealed trait CharToy[+Next]

object CharToy {
  case class CharOutput[Next](a: Char, next: Next) extends CharToy[Next]
  case class CharBell[Next](next: Next) extends CharToy[Next]
  case class CharDone() extends CharToy[Nothing]

  def output[Next](a: Char, next: Next): CharToy[Next] = CharOutput(a, next)
  def bell[Next](next: Next): CharToy[Next] = CharBell(next)
  def done: CharToy[Nothing] = CharDone()

  val result = output('A', bell(done))

  implicit val charToyFunctor: Functor[CharToy] = new Functor[CharToy] {
    override def map[A, B](fa: CharToy[A])(f: A => B): CharToy[B] =
      fa match {
        case o: CharOutput[A] => CharOutput(o.a, f(o.next))
        case b: CharBell[A] => CharBell(f(b.next))
        case CharDone() => CharDone()
      }
  }

  def outputFree(a: Char): Free[CharToy, Unit] =
    Free.liftF[CharToy, Unit](CharOutput(a, ()))
  def bellFree: Free[CharToy, Unit] = Free.liftF[CharToy, Unit](CharBell(()))
  def doneFree: Free[CharToy, Unit] = Free.liftF[CharToy, Unit](CharDone())
  def pure[A](a: A): Free[CharToy, A] = Free.pure[CharToy, A](a)

}




