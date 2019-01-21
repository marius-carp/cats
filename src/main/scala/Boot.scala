import fp.Chapter10.Monoid

object Boot extends App {


  val result = Monoid.concatenate(List("a", "b", "c"), Monoid.stringMonoid)

  println(s"result $result")

}

