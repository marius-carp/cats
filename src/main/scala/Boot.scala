import fp.Chapter5._

object Boot extends App {


  val stream: Stream[Int] = Stream(1,2,3,4,5)


  val result = stream.takeWhile(_ != 4)

  println(result.toList)

}

