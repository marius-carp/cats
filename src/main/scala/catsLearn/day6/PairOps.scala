package catsLearn.day6

import cats._
import cats.data._
import cats.implicits._

object Day6 {

  //Writer monad

  def isBigGang(x: Int): (Boolean, String) =
    (x > 9, "Compared gang size to 9")

  implicit class PairOps[A, B: Semigroup](pair: (A, B)) {
    def applyLog[C](f: A => (C, B)): (C, B) = {
      val (x, log) = pair
      val (y, newLog) = f(x)

      (y, log |+| newLog)
    }
  }


  val bigBang = (3, "Smashing gang") applyLog isBigGang


  type Writer[L, V] = WriterT[Id, L, V]

  object Writer {
    def apply[L, V](l: L, v: V): WriterT[Id, L, V] = new WriterT[Id, L, V]((l, v))

    def value[L: Monoid, V](v: V): Writer[L, V] = WriterT.value(v)

    def tell[L](l: L): Writer[L, Unit] = WriterT.tell(l)
  }

  val w = Writer("Smallish gang", 3)
  val v = Writer.value[String, Int](3)
  val l = Writer.tell[String]("Log Something")

  def logNumber(x: Int): Writer[List[String], Int] =
    Writer(List("Got number: " + x.show), 3)

  def multWithLog: Writer[List[String], Int] =
    for {
      a <- logNumber(3)
      b <- logNumber(5)
    } yield a * b


  def gcd(a: Int, b: Int): Writer[List[String], Int] =
    if(b == 0)
      for {
        _ <- Writer.tell(List("Finished with " + a.show))
      } yield a
    else
      Writer.tell(List(s"${a.show} mod ${b.show} = ${(a % b).show}")) >>= {_ =>
        gcd(b, a % b)
      }

  //Reader monad

  val f1 = (_: Int) * 2
  val f2 = (_: Int) + 10

  val h1 = (f1 |@| f2) map {_ + _}

  def addStuff: Int => Int = for {
    a <- (_: Int) * 2
    b <- (_: Int) + 10
  } yield a + b

  case class User(id: Long, parentId: Long, name: String, email: String)
  trait UserRepo {
    def get(id: Long): User
    def find(name: String): User
  }

  trait Users {
    def getUser(id: Long): UserRepo => User = {
      case repo => repo.get(id)
    }

    def findUsers(name: String): UserRepo => User = {
      case repo => repo.find(name)
    }

  }

  object UserInfo extends Users {
    def userInfo(name: String): UserRepo => Map[String, String] =
      for {
        user <- findUsers(name)
        boss <- getUser(user.parentId)
      } yield Map(
        "name" -> s"${user.name}",
        "email" -> s"${user.email}",
        "boss_name" -> s"${boss.name}"
      )
  }

  trait Program {
    def app: UserRepo => String =
      for {
        fredo <- UserInfo.userInfo("Fredo")
      } yield fredo.toString
  }

  val testUsers = List(User(0, 0, "Vito", "vito@example.com"),
    User(1, 0, "Michael", "michael@example.com"),
    User(2, 0, "Fredo", "fredo@example.com"))

  object Main extends Program {
    def run: String = app(mkUserRepo)
    def mkUserRepo: UserRepo = new UserRepo {
      def get(id: Long): User = (testUsers find { _.id === id }).get
      def find(name: String): User = (testUsers find { _.name === name }).get
    }
  }

  val readerResult = Main.run

}

