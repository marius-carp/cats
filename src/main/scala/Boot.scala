import fp.Chapter3._

object Boot extends App {

  val list = List(1,0,3,5,6)
  val list2 = List(3, 5, 6)

  println(List.hasSubsequence(list, list2))

}

