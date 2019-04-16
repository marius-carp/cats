package catsLearn.day10

import java.net.URI

import cats._
import cats.data._
import cats.implicits._
import catsLearn.day10.ReaderTest.ReaderTOption


case class User(id: Long, parentId: Long, name: String, email: String)

trait UserRepo {
  def get(id: Long): Option[User]
  def find(name: String): Option[User]
}

trait HttpService {
  def get(uri: URI): String
}

trait Config {
  def userRepo: UserRepo
  def httpService: Option[HttpService]
}

trait Users {
  def getUser(id: Long): ReaderTOption[Config, User] =
    ReaderTOption.ro(config => config.userRepo.get(id))

  def findUser(name: String): ReaderTOption[Config, User] =
    ReaderTOption.ro(config => config.userRepo.find(name))
}

trait Https {
  def getHttp(uri: URI): ReaderTOption[Config, String] =
    ReaderTOption.ro(config => config.httpService.map(_.get(uri)))
}

object ReaderTest {
  type ReaderTOption[A, B] = Kleisli[Option, A, B]
  object ReaderTOption {
    def ro[A, B](f: A => Option[B]): ReaderTOption[A, B] = Kleisli(f)
  }
}

trait Program extends Users with Https {
  def userSearch(id: Long): ReaderTOption[Config, String] =
    for {
      u <- getUser(id)
      r <- getHttp(new URI(s"http://www.google.com/?q=${u.name}"))
    } yield r
}

object Main extends Program {
  def run(config: Config): Option[String] =
    userSearch(2).run(config)

  val dummyConfig: Config = new Config {

    val testUsers = List(User(0, 0, "Vito", "vito@example.com"),
      User(1, 0, "Michael", "michael@example.com"),
      User(2, 0, "Fredo", "fredo@example.com"))

    override def userRepo: UserRepo = new UserRepo {
      override def find(name: String): Option[User] =
        testUsers.find(_.name == name)

      override def get(id: Long): Option[User] =
        testUsers.find(_.id == id)
    }

    override def httpService: Option[HttpService] = Some(new HttpService {
      override def get(uri: URI): String = "Fredo man"
    })
  }
}