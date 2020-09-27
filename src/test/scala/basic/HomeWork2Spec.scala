package basic

import basic.HomeWork2._
import org.scalatest.flatspec._
import org.scalatest.matchers._

class HomeWork2Spec extends AnyFlatSpec with should.Matchers {
  /*
   * Shape tests.
   */
  "Point" should "move by 1 point" in {
    Point(1, 2, 0).move(1, 1, 1) shouldEqual Point(2.0, 3.0, 1.0)
  }
}
