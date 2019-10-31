package monix

import monix.eval.Task
import monix.execution.{CancelableFuture, Scheduler}
import monix.reactive.{Consumer, Observable}

object Boot extends App {

  implicit val monixScheduler: Scheduler = monix.execution.Scheduler.global



  {
    val source = Observable.range(1, 500)

    def triggerKillSwitch(): CancelableFuture[Unit] = {
      println("triggered the kill switch")
      source.consumeWith(Consumer.cancel).runAsync
    }

    source.mapAsync(parallelism = 1) { i =>
      println(s"Visiting page number: $i")

      Thread.sleep(2000)

      source.consumeWith(Consumer.cancel)
    }.toListL.foreach(i => println(s"Processed page number: $i"))


    def here(tks: () => CancelableFuture[Unit]) =
      tks()

  }

}
