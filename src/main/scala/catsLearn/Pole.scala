package catsLearn

import catsLearn.Pole.Birds
import cats._
import cats.data._
import cats.implicits._


object Pole {
  type Birds = Int

  val rlr: Option[Pole] = Monad[Option].pure(Pole(0, 0)) >>=
    {_.landRight(1)} >>=
    {_.landLeft(2)} >>=
    {_.landRight(2)}

  def routine: Option[Pole] =
    for {
      start <- Monad[Option].pure(Pole(0, 0))
      first <- start.landLeft(2)
      second <- first.landRight(2)
      third <- second.landLeft(1)
    } yield third


  none[Int] >> 3.some //none
  3.some >> 4.some // Some(4)
  3.some >> none[Int] //none

  3.some >>= { x => "!".some >>= { y => (x.show + y).some} }

  for {
    x <- 3.some
    y <- "!".some
  } yield x.show + y


  def justH: Option[Char] =
    for {
      (x :: xs) <- "hello".toList.some
    } yield x
}

case class Pole(left: Birds, right: Birds) {

  def landLeft(n: Birds): Option[Pole] =
    if(math.abs((left + n) - right) < 4)
      copy(left = left + n).some
    else
      none[Pole]


  def landRight(n: Birds): Option[Pole] =
    if(math.abs(left - (right + n)) < 4)
      copy(right = right + n).some
    else
      none[Pole]



}

