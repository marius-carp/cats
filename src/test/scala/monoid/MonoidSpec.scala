package monoid

import myimpl.Monoid
import org.specs2.execute.Result
import org.specs2.mutable.Specification

class MonoidSpec extends Specification {

  "identity" should identity(Monoid.stringMonoid, "frunza")
  "asociativity" should asociativity(Monoid.stringMonoid, "frunza", "frz", "asd")

  "identity" should identity(Monoid.intAdditionMonoid, 21)
  "asociativity" should asociativity(Monoid.intAdditionMonoid, 22, 23, 25)


  def identity[A](m: Monoid[A], value: A): Result = {
    m.op(value, m.zero) must beEqualTo(value)
  }

  def asociativity[A](m: Monoid[A], x: A, y: A, z: A): Result =
    m.op(x, m.op(y, z)) must beEqualTo(m.op(m.op(x, y), z))


}
