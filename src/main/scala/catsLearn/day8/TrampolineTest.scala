package catsLearn.day8

import cats._
import cats.data._
import cats.implicits._
import cats.free.{Free, Trampoline}
import Trampoline._

object TrampolineTest {

  def even[A](ns: List[A]): Trampoline[Boolean] =
    ns match {
      case Nil => done(true)
      case x :: xs => suspend(odd(xs))
    }

  def odd[A](ns: List[A]): Trampoline[Boolean] =
    ns match {
      case Nil => done(false)
      case x :: xs => suspend(even(xs))
    }

  val evn = even(List(1, 2, 3)).run

}