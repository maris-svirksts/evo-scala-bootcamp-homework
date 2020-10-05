package basic

import basic.HomeWork3._
import org.scalatest.flatspec._
import org.scalatest.matchers._

class HomeWork3Spec extends AnyFlatSpec with should.Matchers {
  // Errors.
  "Errors" should "no arguments" in {
    process("divide") shouldEqual "Error: Zero numbers."
  }
  "Errors" should "extra whitespace" in {
    process("divide 4  5") shouldEqual "4 divided by 5 is 0.8"
  }

  // Calculations.
  "Division" should "divide first element by second element" in {
    process("divide 4 5") shouldEqual "4 divided by 5 is 0.8"
  }
  "Sum" should "count all elements" in {
    process("sum 5 5 6 8.5") shouldEqual "the sum of 5 5 6 8.5 is 24.5"
  }
  "Average" should "show average number" in {
    process("average 4 3 8.5 4") shouldEqual "the average of 4 3 8.5 4 is 4.875"
  }
  "Min" should "return min number" in {
    process("min 4 -3 -17") shouldEqual "the minimum of 4 -3 -17 is -17"
  }
  "Max" should "return max number" in {
    process("max 4 -3 -17") shouldEqual "the maximum of 4 -3 -17 is 4"
  }
}
