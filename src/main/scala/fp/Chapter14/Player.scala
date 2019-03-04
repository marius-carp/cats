package fp.Chapter14

case class Player(name: String, score: Int)

object Player {
  def winnerMsg(p: Player): String =
    p.name + " is the winner!"

  def winner(p1: Player, p2: Player): Player =
    if (p1.score > p2.score) p1 else p2

  def declareWinner(p1: Player, p2: Player): IO[Unit] =
    printWinner(winner(p1, p2))

  def PrintLine(msg: String): IO[Unit] = {
    new IO[Unit] {
      def run() = println(msg)
    }
  }

  def printWinner(p: Player): IO[Unit] =
    PrintLine(winnerMsg(p))

  def ReadLine: IO[String] = IO {
    scala.io.StdIn.readLine()
  }
}

