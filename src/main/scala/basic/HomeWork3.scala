package basic

import basic.HomeWork3.Command._

import scala.io.Source
import scala.util.control.Exception.allCatch

object HomeWork3 {
  // Homework

  sealed trait Command {
    def calculations: Either[ErrorMessage, Result]
  }

  object Command {
    final case class Divide(dividend: Double, divisor: Double) extends Command {
      override def calculations: Either[ErrorMessage, Result] = {
        if (divisor.toInt == 0) Left(ErrorMessage("Division by 0."))
        else
          Right(
            PolishResult(
              "divided",
              List(dividend, divisor),
              dividend / divisor
            )
          )
      }
    }

    final case class Sum(numbers: List[Double]) extends Command {
      override def calculations: Either[ErrorMessage, Result] =
        Right(PolishResult("sum", numbers, numbers.sum))
    }
    final case class Average(numbers: List[Double]) extends Command {
      override def calculations: Either[ErrorMessage, Result] =
        Right(PolishResult("average", numbers, numbers.sum / numbers.length))
    }
    final case class Min(numbers: List[Double]) extends Command {
      override def calculations: Either[ErrorMessage, Result] =
        Right(PolishResult("minimum", numbers, numbers.min))
    }
    final case class Max(numbers: List[Double]) extends Command {
      override def calculations: Either[ErrorMessage, Result] =
        Right(PolishResult("maximum", numbers, numbers.max))
    }
  }

  final case class ErrorMessage(value: String)

  sealed trait Result {
    def action: String
    def elements: List[Double]
    def value: Double
  }
  final case class PolishResult(
      action: String,
      elements: List[Double],
      value: Double
  ) extends Result

  //Thanks to Juris Krikis for suggestion.
  def isDoubleNumber(s: String): Boolean = s.toDoubleOption.isDefined

  /*
   * Checks are done for the following issues:
   * - Extra whitespace. It's cleared out.
   * - Empty line: call Left()
   * - Only one argument: call Left()
   * - Something else entered where a number is expected: call Left()
   * - Wrong upper/lower case letters: transformed to lower case.
   * - Too many numbers for division: call Left()
   * - Unknown command: call Left()
   */
  def parseCommand(x: String): Either[ErrorMessage, Command] = {
    // Consider how to handle extra whitespace gracefully (without errors).
    val line = x.split("\\s+").toList

    line match {
      case Nil => Left(ErrorMessage("Empty line."))
      case ::(head, tail) if (tail.isEmpty) =>
        Left(ErrorMessage("Zero numbers."))
      case ::(head, tail) if (tail.length < 2) =>
        Left(ErrorMessage("Only one argument."))
      case ::(head, tail) if !tail.forall(isDoubleNumber) =>
        Left(ErrorMessage("At least one of the elements is not a number."))
      case head :: divider :: divisor :: Nil
          if (head.toLowerCase == "divide") =>
        Right(Divide(divider.toDouble, divisor.toDouble))
      case ::(head, tail) if (head.toLowerCase == "divide") =>
        Left(ErrorMessage("Too many numbers for division."))
      case ::(head, tail) if (head.toLowerCase == "sum") =>
        Right(Sum(tail.map(str => str.toDouble)))
      case ::(head, tail) if (head.toLowerCase == "average") =>
        Right(Average(tail.map(str => str.toDouble)))
      case ::(head, tail) if (head.toLowerCase == "min") =>
        Right(Min(tail.map(str => str.toDouble)))
      case ::(head, tail) if (head.toLowerCase == "max") =>
        Right(Max(tail.map(str => str.toDouble)))
      case default => Left(ErrorMessage("Unknown command."))
    }

  }

  def calculate(x: Command): Either[ErrorMessage, Result] = {
    x.calculations
  }

  def renderResult(x: Result): String = {
    if (x.action == "divided")
      x.elements.head.toString.replaceFirst(
        ".0",
        ""
      ) + " " + x.action + " by " + x.elements.tail.head.toString
        .replaceFirst(".0", "") + " is " + x.value.toString
        .replaceFirst(".0", "")
    else
      "the " + x.action + " of " + x.elements
        .map(double => double.toString.replaceFirst(".0", ""))
        .mkString(" ") + " is " + x.value.toString.replaceFirst(".0", "")

  }

  def process(x: String): String = {
    /*
     * My first idea, but, couldn't make it work together with 'for' ( a requirement for this method ): it was complaining that it didn't work together with map from main method.
     *
    val result = parseCommand( x ) match {
      case Left(value)  => ???
      case Right(value) => calculate( value ) match {
        case Left(value)  => ???
        case Right(value) => renderResult(value)
      }
    }

    result.toString

     */

    val results = for {
      parsedCommand <- parseCommand(x)
      result <- calculate(parsedCommand)
    } yield result

    results match {
      case Left(value) =>
        s"Error: ${value.value}" // Probably a bad idea to return it as a string right away.
      case Right(value) => renderResult(value)
    }
  }

  // This `main` method reads lines from stdin, passes each to `process` and outputs the return value to stdout
  def main(args: Array[String]): Unit =
    Source.stdin.getLines() map process foreach println
}
