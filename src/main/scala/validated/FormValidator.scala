package validated

import cats.data._
import cats.data.Validated._
import cats.implicits._



object FormValidator {

  final case class RegistrationData(username: String, password: String, firstName: String, lastName: String, age: Int)

  private def validateUserName(userName: String): ValidationResult[String] =
    if (userName.matches("^[a-zA-Z0-9]+$")) CellResultSuccess(userName).validNec else UsernameHasSpecialCharacters.invalidNec

  private def validatePassword(password: String): ValidationResult[String] =
    if (password.matches("(?=^.{10,}$)((?=.*\\d)|(?=.*\\W+))(?![.\\n])(?=.*[A-Z])(?=.*[a-z]).*$")) CellResultSuccess(password).validNec
    else PasswordDoesNotMeetCriteria.invalidNec

  private def validateFirstName(firstName: String): ValidationResult[String] =
    if (firstName.matches("^[a-zA-Z]+$")) CellResultSuccess(firstName).validNec else FirstNameHasSpecialCharacters.invalidNec

  private def validateLastName(lastName: String): ValidationResult[String] =
    if (lastName.matches("^[a-zA-Z]+$")) CellResultSuccess(lastName).validNec else LastNameHasSpecialCharacters.invalidNec

  private def validateAge(age: Int): ValidationResult[Int] =
    if (age >= 18 && age <= 75) CellResultSuccess(age).validNec else AgeIsInvalid.invalidNec

  def validateForm(username: String, password: String, firstName: String, lastName: String, age: Int): ValidationResult[RegistrationData] = {
    (validateUserName(username),
      validatePassword(password),
      validateFirstName(firstName),
      validateLastName(lastName),
      validateAge(age)).mapN(RegistrationData)
  }

}

object Test extends App {

  val res = FormValidator.validateForm(
    username = "Joe",
    password = "Passw0r$1234",
    firstName = "John",
    lastName = "Doe",
    age = 21
  )


  println(res)
}
