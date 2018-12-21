import fp.Chapter5._

object Boot extends App {

  val stream: Stream[Int] = Stream.constantUnfold(2)

  val result = stream.take(6)

  println(result.toList)

}

