package catsLearn.day7

import cats._
import cats.data._
import cats.implicits._
import catsLearn.day7


object StateTest {

  type Stack = List[Int]

  def stackyStack: State[Stack, Unit] = for {
    stackNow <- State.get[Stack]
    r <- if(stackNow === List(1, 2, 3))
        State.set[Stack](List(8, 3, 1))
      else
        State.set[Stack](List(9, 2, 1))
  } yield r


  val pop: State[Stack, Int] = for {
    s <- State.get[Stack]
    x :: xs = s
    _ <- State.set[Stack](xs)
  } yield x

  def push(x: Int): State[Stack, Unit] = for {
    xs <- State.get[Stack]
    r <- State.set(x :: xs)
  } yield r


  val result = day7.StateTest.stackyStack

  result.run(List(1, 2, 3)).value

  val a = for {
    _ <- day7.StateTest.push(32)
    _ <- day7.StateTest.push(33)
    a <- day7.StateTest.pop
  } yield a

  a.run(List(5, 6, 7)).value
}
