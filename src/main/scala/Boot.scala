import fp.Chapter6.SimpleRNG._
import fp.Chapter6.RNG._
import fp.Chapter6._

object Boot extends App {

  val seed = RNG.Simple(231)

  val result = intsSequence(21)

  println(result(seed)._1)




}

