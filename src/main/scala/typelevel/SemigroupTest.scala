package typelevel

import cats.Semigroup
import cats.implicits._

object SemigroupTest {


  val map1 = Map("hello" -> 0, "world" -> 1)
  val map2 = Map("hello" -> 2, "cats"  -> 3)

  val result: Map[String, Int] = map1 |+| map2

  def optionCombine[A: Semigroup](a: A, opt: Option[A]): A =
    opt.map(a |+| _).getOrElse(a)

  def mergeMap[K, V: Semigroup](lhs: Map[K, V], rhs: Map[K, V]): Map[K, V] = {
    lhs.foldLeft(rhs) {
      case (acc, (k, v)) => acc.updated(k, optionCombine(v, acc.get(k)))
    }
  }

  val xm1 = Map('a' -> 1, 'b' -> 2)
  val xm2 = Map('b' -> 3, 'c' -> 4)
  val result2 = mergeMap(xm1, xm2)

}
