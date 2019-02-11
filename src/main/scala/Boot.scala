import fp.Chapter11.Id
import fp.Chapter11.Monad._

object Boot extends App {


  val f: (Int, Int ,Int) => Int = _ + _ + _

  val k: Int => Int => Int => Int = f.curried


  def r(i: Int): Int => Int = {
    a => i + 10
  }

  def s(r: Int => Int): Int => Int = {
    a => r(a)
  }

  val result = s(r(21))

  println(result(12))
}

