package typelevel

import cats._
import cats.implicits._

object ContravariantMonoidalTest {

  case class Predicate[A](run: A => Boolean)

  implicit val contravariantMonoidalPredicate: ContravariantMonoidal[Predicate] = new ContravariantMonoidal[Predicate] {
    override def unit: Predicate[Unit] = Predicate[Unit](Function.const(true))

    override def product[A, B](fa: Predicate[A], fb: Predicate[B]): Predicate[(A, B)] =
      Predicate(x => fa.run(x._1) && fb.run(x._2))

    override def contramap[A, B](fa: Predicate[A])(f: B => A): Predicate[B] =
      Predicate(x => fa.run(f(x)))
  }

  case class Money(value: Long)

  def isEven: Predicate[Long] = Predicate(_ % 2 == 0)
  def isEvenMoney: Predicate[Money] = isEven.contramap(_.value)

  val result = isEvenMoney.run(Money(55))

  def times2Predicate: Predicate[Long] => Predicate[Long] =
    ContravariantMonoidal[Predicate].liftContravariant((x: Long) => 2 * x)

  def liftMoney: Predicate[Long] => Predicate[Money] =
    ContravariantMonoidal[Predicate].liftContravariant(_.value)

  def trivial = times2Predicate(isEven)

  val result2 = trivial.run(321)

  case class Transaction(value: Money, payee: String)

  def isEvan: Predicate[String] = Predicate(_ == "Evan")

  def isGraterThan50Dollars: Predicate[Money] = liftMoney(Predicate(_ > 50))

  def isEvenPaymentToEvanOfMoreThan50 =
    (isEvenMoney, isGraterThan50Dollars, isEvan).contramapN((trans: Transaction) => (trans.value, trans.value, trans.payee))

  val result3 = isEvenPaymentToEvanOfMoreThan50.run(Transaction(Money(56), "Evan"))

}
