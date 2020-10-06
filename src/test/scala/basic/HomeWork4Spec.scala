package basic

import basic.HomeWork4._
import org.scalatest.flatspec._
import org.scalatest.matchers._

class HomeWork4Spec extends AnyFlatSpec with should.Matchers {
  "Sort" should "equals" in {
    sortConsideringEqualValues(
      Map("a" -> 1, "b" -> 2, "c" -> 4, "d" -> 1, "e" -> 0, "f" -> 2, "g" -> 2)
    ) shouldEqual "List(Set(\"e\") -> 0, Set(\"a\", \"d\") -> 1, Set(\"b\", \"f\", \"g\") -> 2, Set(\"c\") -> 4)"
  }
}
