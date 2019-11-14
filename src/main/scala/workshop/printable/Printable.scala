package workshop.printable

import workshop.printable.TestPrintable.Cat

trait Printable[A] {
  def format(a: A): String
}

object PrintableInstances {

  implicit val printableString: Printable[String] =
    (a: String) => a

  implicit val printableInt: Printable[Int] =
    (a: Int) => a.toString

  implicit val printableCat: Printable[Cat] =
    (cat: Cat) => {
      val name  = Printable.format(cat.name)
      val age   = Printable.format(cat.age)
      val color = Printable.format(cat.color)

      s"$name is a $age year-old $color cat."
    }
}

object Printable {

  def format[A](a: A)(implicit printable: Printable[A]): String =
    printable.format(a)

  def print[A](a: A)(implicit printable: Printable[A]): Unit =
    println(format(a))

}

object PrintableSyntax {

  implicit class PrintableOps[A](a: A) {
    def format()(implicit p: Printable[A]): String =
      p.format(a)

    def print()(implicit p: Printable[A]): Unit =
      println(format())
  }

}

object TestPrintable extends App {
  import PrintableInstances._
  import PrintableSyntax._

  final case class Cat(name: String, age: Int, color: String)
  final case class Dog(name: String, age: Int, color: String)

  val cat = Cat("frunza", 3, "red")
  val dog = Dog("frunza", 3, "red")

  cat.print()
}
