import cats.data.Validated.{Invalid, Valid}
import cats.{Functor, Semigroupal}
import cats.data.ValidatedNec
import cats.implicits._
import validated.CellResult

package object validated {

  type ValidationResult[A] = ValidatedNec[DomainValidation, CellResult[A]]

  implicit val functor: Functor[ValidationResult] = new Functor[ValidationResult] {
    override def map[A, B](fa: ValidationResult[A])(f: A => B): ValidationResult[B] =
      fa match {
        case Valid(cell) =>
          cell match {
            case CellResultSuccess(a) => CellResultSuccess(f(a)).validNec
            case CellResultError => UsernameHasSpecialCharacters.invalidNec
          }
        case i@Invalid(e) => i
      }
  }

  implicit val semigroup = new Semigroupal[ValidationResult] {
    override def product[A, B](fa: ValidationResult[A], fb: ValidationResult[B]): ValidationResult[(A, B)] = {
      (fa, fb) match {
        case (Valid(a), Valid(b)) => CellResultSuccess((a.asInstanceOf[CellResultSuccess[A]].a, b.asInstanceOf[CellResultSuccess[B]].a)).validNec
      }
    }
  }


}
 /*
 fa.map {
        case CellResultSuccess(a) => CellResultSuccess(f(a))
        case CellResultError => CellResultError
      }
  */