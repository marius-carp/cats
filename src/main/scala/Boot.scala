import java.util.concurrent.ForkJoinPool

import fp.Chapter7._

object Boot extends App {


  private val forkJoinPool = new ForkJoinPool()

  val m = Par.unit("frunza")
  val fork = Par.fork(m)
  val materialized = fork(forkJoinPool).get()

  val list = Par.sortPar(Par.unit(List(1,5,3,2,6,4)))
  val materializedList = Par.run(forkJoinPool)(list)

  println(s"m $materializedList")



}

