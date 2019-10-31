package pattern

object Pattern extends App {


  val str = "THIS IS A TEST TEXT TEXT TEXT TEST TEXT"
  val toFind = "TEST"

  val result = (str + List.fill(toFind.length)(" ").mkString("")).sliding(toFind.length, 1).zipWithIndex.filter(_._1 == toFind).map(_._2).toList

  println(result)

}
