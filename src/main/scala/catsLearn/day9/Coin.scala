package catsLearn.day9

import cats._
import cats.data._
import cats.implicits._

sealed trait Coin

object Coin {
  case object Heads extends Coin
  case object Tails extends Coin

  implicit val coinEq: Eq[Coin] = new Eq[Coin] {
    override def eqv(x: Coin, y: Coin): Boolean = x == y
  }

  def heads: Coin = Heads
  def tails: Coin = Tails

  def coin: Prob[Coin] = Prob(heads -> 0.5 :: tails -> 0.5 :: Nil)
  def loadedCoin: Prob[Coin] = Prob(heads -> 0.1 :: tails -> 0.9 :: Nil)

  def flipThree: Prob[Boolean] = for {
    a <- coin
    b <- coin
    c <-loadedCoin
  } yield {
     List(a, b, c).forall(_ === tails)
  }

  val result = flipThree

}
