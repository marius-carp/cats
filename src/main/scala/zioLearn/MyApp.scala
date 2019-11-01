package zioLearn

import java.io.IOException

import zio.{App, ZIO, console}
import zio.console._

object MyApp extends App {

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    myAppLogic.fold(_ => 1, _ => 0)

  val myAppLogic: ZIO[console.Console, IOException, Unit] =
    for {
      _ <- putStr("What's your name")
      name <- getStrLn
      _ <- putStr(name)
    } yield ()

}
