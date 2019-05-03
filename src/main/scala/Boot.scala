import catsLearn._
import catsLearn.day10.Main
import catsLearn.day7.{IorTest, ValidatedTest}
import catsLearn.day8.{CharToy, FixE, TrampolineTest}
import catsLearn.day9.{Coin, FoldMTest, KleisliTest, Prob}
import typelevel._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}


object Boot extends App {

  implicit val ec = scala.concurrent.ExecutionContext.global



  println(ContravariantMonoidalTest.result3)

}
