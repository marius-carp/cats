import catsLearn._
import catsLearn.day10.Main
import catsLearn.day7.{IorTest, ValidatedTest}
import catsLearn.day8.{CharToy, FixE, TrampolineTest}
import catsLearn.day9.{Coin, FoldMTest, KleisliTest, Prob}
import typelevel.SemigroupTest


object Boot extends App {

  val result = SemigroupTest.result2

  println(result)

}
