import fp.Chapter11.Monad._

object Boot extends App {


  val result = optionMonad.sequence(List(Some(1), Some(2), Some(3), Some(4)))

  println(result)


}

