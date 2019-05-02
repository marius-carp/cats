package typelevel

import cats._
import cats.implicits._
import scala.math.Ordered._

object ContravariantTest {

  case class Money(amount: Int)
  case class Salary(size: Money)

  implicit val showMoney: Show[Money] = Show.show(m => s"$$${m.amount}")

  implicit val showSalary: Show[Salary] = showMoney.contramap(_.size)

  val result = Salary(Money(321)).show

  implicit val moneyOrdering: Ordering[Money] = Ordering.by(_.amount)

  val result2 = Money(100) < Money(200)

}
