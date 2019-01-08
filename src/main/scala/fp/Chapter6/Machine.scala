package fp.Chapter6

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {

  def run(input: Input): State[Machine, Int] = (input, this) match {
    case (_, s@Machine(_, 0, _)) => State(_ => (coins, s)) //no candy
    case (Coin, s@Machine(false, _, _)) => State(_ => (coins, s)) //inserting coin in unlocked machine
    case (Turn, s@Machine(true, _, _)) => State(_ => (coins, s)) // turn a locked machine
    case (Coin, Machine(true, candies, _)) => State(_ => (coins + 1, Machine(locked = false, candies, coins + 1)))
    case (Turn, Machine(false, candies, coins)) => State(_ => (coins, Machine(true, candies - 1, coins)))
  }

}

case object Machine {
  def t(inputs: List[Input]): List[Machine => Machine] = {
    inputs.map(i => (s: Machine) => (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) => Machine(locked = false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) => Machine(locked = true, candy - 1, coin)
    })
  }

  def simulateMachine(inputs: List[Input]): State[Machine, Int] = {
    inputs.map(i => (s: Machine) => (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) => Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) => Machine(true, candy - 1, coin)
    })
    State.unit[Machine, Int](0)
  }
}


