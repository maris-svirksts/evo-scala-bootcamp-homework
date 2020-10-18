package error_handling

import cats.data.ValidatedNec
import cats.syntax.all._
import java.util.Calendar

/** Homework. Place the solution under `error_handling` package in your homework repository.
  *
  * 1. Model `PaymentCard` class as an ADT (protect against invalid data as much as it makes sense).
  * 2. Add `ValidationError` cases (at least 5, may be more).
  * 3. Implement `validate` method to construct `PaymentCard` instance from the supplied raw data.
  */

object Homework7 {
  // Using string for PaymentCardNumber because the input might contain empty spaces.
  // Using string for securityCode because it could start with 0 and leading 0 for ints means that it's an octal number:
  // https://stackoverflow.com/questions/565634/why-are-integer-literals-with-leading-zeroes-interpreted-strangely

  final case class NameOnPaymentCard(nameOnPaymentCard: String) extends AnyVal
  final case class PaymentCardNumber(paymentCardNumber: String) extends AnyVal
  final case class ExpiryDate(expiryDate: String) extends AnyVal
  final case class SecurityCode(securityCode: String) extends AnyVal

  case class PaymentCard(
      nameOnPaymentCard: NameOnPaymentCard,
      paymentCardNumber: PaymentCardNumber,
      expiryDate: ExpiryDate,
      securityCode: SecurityCode
  )

  sealed trait ValidationError
  object ValidationError {
    // Data taken from https://community.developer.visa.com/t5/Implementation-API-Sample-Code/Credit-card-name-validation/td-p/11546
    // It looks like the card holder name might be optional in some parts of the world. Assuming it's required for the purposes of this exercise.
    final case object nameOnCardLengthIsInvalid extends ValidationError {
      override def toString: String =
        "Name on card, including whitespace, must be between 7 and 49 characters."
    }
    final case object nameOnCardInvalidCharacters extends ValidationError {
      override def toString: String =
        "Name on card can contain only specific characters."
    }

    final case object paymentCardInvalidCharacters extends ValidationError {
      override def toString: String =
        "Payment card number should contain only digits or whitespace."
    }
    final case object paymentCardLengthIsInvalid extends ValidationError {
      override def toString: String =
        "Payment card number should contain 14 to 19 digits."
    }
    final case object paymentCardNumberWrong extends ValidationError {
      override def toString: String =
        "Payment card did not pass Luhn validation."
    }

    final case object expiryDateInvalidCharacters extends ValidationError {
      override def toString: String =
        "Expiry date field should look like this: 09/23 or 9/22"
    }
    final case object expiryDateInThePast extends ValidationError {
      override def toString: String = "The card has expired."
    }

    final case object securityCodeWrong extends ValidationError {
      override def toString: String =
        "Security code should contain 3 to 4 digits."
    }
  }

  object PaymentCardValidator {
    import ValidationError._

    type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

    private def validateNameOnCard(
        nameOnPaymentCard: String
    ): AllErrorsOr[NameOnPaymentCard] = {

      def validateNameLength: AllErrorsOr[String] =
        if (nameOnPaymentCard.length >= 7 && nameOnPaymentCard.length <= 49)
          nameOnPaymentCard.validNec
        else nameOnCardLengthIsInvalid.invalidNec

      def validateNameContents: AllErrorsOr[String] =
        if (
          nameOnPaymentCard.matches(
            "^([A-Za-z\\s'.-~`]{3,})\\s([A-Za-z'.-~`]{3,})$"
          )
        ) nameOnPaymentCard.validNec
        else nameOnCardInvalidCharacters.invalidNec

      validateNameLength.productR(validateNameContents) map NameOnPaymentCard
    }

    //https://github.com/wix/credit-card/blob/master/src/main/scala/com/wix/pay/creditcard/validators/LuhnValidator.scala
    //https://en.wikipedia.org/wiki/Payment_card_number
    private def validatePaymentCardNumber(
        paymentCardNumber: String
    ): AllErrorsOr[PaymentCardNumber] = {
      val cleanedPaymentCardNumber = paymentCardNumber.filter(_.isDigit)

      def validateCardNumberLength: AllErrorsOr[String] =
        if (
          cleanedPaymentCardNumber.length >= 14 && cleanedPaymentCardNumber.length <= 19
        )
          cleanedPaymentCardNumber.validNec
        else nameOnCardLengthIsInvalid.invalidNec

      def validateLuhnSum: AllErrorsOr[String] = {
        val digitalRoot =
          Seq(0, 0, 1, 2, 2, 4, 3, 6, 4, 8, 5, 1, 6, 3, 7, 5, 8, 7, 9, 9)

        val calculateLuhnSum: String => Int = cleanedPaymentCardNumber => {
          cleanedPaymentCardNumber.reverse.zipWithIndex.map {
            case (digit, index) => digitalRoot(digit * 2 + index % 2)
          }.sum
        }

        if (calculateLuhnSum(cleanedPaymentCardNumber) % 10 == 0)
          cleanedPaymentCardNumber.validNec
        else paymentCardNumberWrong.invalidNec
      }

      validateCardNumberLength.productR(validateLuhnSum) map PaymentCardNumber
    }

    private def validateExpiryDate(
        expiryDate: String
    ): AllErrorsOr[ExpiryDate] = {

      def validateExpiryDateContents: AllErrorsOr[String] =
        if (expiryDate.matches("^(0{0,1}[1-9]|1[0-2])/([2-9][0-9])$"))
          expiryDate.validNec
        else expiryDateInvalidCharacters.invalidNec

      def validateExpiryDateInThePast: AllErrorsOr[String] = {
        val cal = Calendar.getInstance()
        val currentYear = cal.get(Calendar.YEAR).toString.takeRight(2).toInt
        val monthTemp = cal.get(Calendar.MONTH)
        val month = monthTemp + 1
        val Array(cardMonth, cardYear) = expiryDate.split("/")
        if (
          cardYear.toIntOption.getOrElse(
            0
          ) <= currentYear && cardMonth.toIntOption.getOrElse(0) < month
        ) expiryDate.validNec
        else expiryDateInThePast.invalidNec
      }

      validateExpiryDateContents.productR(
        validateExpiryDateInThePast
      ) map ExpiryDate
    }

    private def validateSecurityCode(
        securityCode: String
    ): AllErrorsOr[SecurityCode] = {

      def validateSecurityCodeContents: AllErrorsOr[String] =
        if (securityCode.matches("^[0-9]{3,4}$"))
          securityCode.validNec
        else securityCodeWrong.invalidNec

      validateSecurityCodeContents map SecurityCode
    }

    def validate(
        name: String,
        number: String,
        expirationDate: String,
        securityCode: String
    ): AllErrorsOr[PaymentCard] =
      (
        validateNameOnCard(name),
        validatePaymentCardNumber(number),
        validateExpiryDate(expirationDate),
        validateSecurityCode(securityCode)
      ).mapN(PaymentCard)
  }
}
