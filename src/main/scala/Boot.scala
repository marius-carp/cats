import fp.Chapter4._
import fp.Chapter3._
import fp.Chapter4.Option._

object Boot extends App {

  val list: List[Option[Int]] = List(Some(1), Some(2), None, Some(3))

  println(List.sequence[Int](list))
}

