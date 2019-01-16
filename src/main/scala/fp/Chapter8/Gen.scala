package fp.Chapter8

import fp.Chapter6.{RNG, SimpleRNG, State}

trait Prop { self =>

  def check(): Boolean

  def &&(p: Prop): Prop = new Prop {
    override def check(): Boolean = self.check() && p.check()
  }

}

object Prop {
  type FailedCase = String
  type SuccessCount = Int

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = ???
}


case class Gen[A](sample: State[RNG, A]) {

  def map[B](f: A => B): Gen[B] = {
    Gen(sample.map(f))
  }

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] = {
    Gen(sample.map2(g.sample)(f))
  }

  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    val state = sample.flatMap(r => f(r).sample)

    Gen(state)
  }

  def listOfN(size: Gen[Int]): Gen[List[A]] = {
    size.flatMap{ r =>
      Gen.listOfN(r, this)
    }
  }





}

object Gen {

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    val state = State((st) => SimpleRNG.double(st)).map(i =>
      ((stopExclusive - start) * i + start).toInt)

    Gen(state)
  }

  def unit[A](a: => A): Gen[A] = {
    Gen(State.unit(a))
  }

  def boolean: Gen[Boolean] = {
    Gen(choose(0, 2).sample.map(_ > 0))
  }


  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] = {
    val state = List.fill(n)(a.sample)
    Gen(State.sequence(state))
  }

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    Gen.boolean.flatMap(r => if(r) g1 else g2)
  }

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val ((gen1, pr1), (gen2, pr2)) = (g1, g2)

    val state = State(st => SimpleRNG.double(st)).flatMap { d =>
      val rand = d * (pr1 + pr2)

      if(rand < pr1)
        gen1.sample
      else
        gen2.sample
    }

    Gen(state)
  }
}
