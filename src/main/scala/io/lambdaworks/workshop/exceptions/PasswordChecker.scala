package io.lambdaworks.workshop.exceptions

object PasswordChecker {

  def validate(password: String): Either[List[Throwable], String] = {
    val errors = List (
      minNumberOfChars(password, 6).left.toOption,
      containsNumber(password).left.toOption,
      containsLowerCase(password).left.toOption,
      containsUpperCase(password).left.toOption
    ).flatten

    if(errors.isEmpty) Right(password) else Left(errors)
  }

  private def minNumberOfChars(password: String, length: Int): Either[Throwable, String] =
    if(password.length < length) Left(InvalidLength) else Right(password)

  private def containsUpperCase(password: String): Either[Throwable, String] =
    if(!password.exists(_.isUpper)) Left(MissingUppercase) else Right(password)

  private def containsLowerCase(password: String): Either[Throwable, String] =
    if(!password.exists(_.isLower)) Left(MissingLowercase) else Right(password)

  private def containsNumber(password: String): Either[Throwable, String] =
    if (!password.exists(_.isDigit)) Left(MissingNumber) else Right(password)

  object InvalidLength    extends Throwable("Password must contain at least 5 characters.")
  object MissingUppercase extends Throwable("Password must contain uppercase letter.")
  object MissingLowercase extends Throwable("Password must contain lowercase letter.")
  object MissingNumber    extends Throwable("Password must contain number.")

}
