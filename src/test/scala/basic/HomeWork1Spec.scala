package basic

import HomeWork1._
import org.scalatest._
import flatspec._
import matchers._

class HomeWork1Spec extends AnyFlatSpec with should.Matchers {
  /*
   * Greatest common divisor tests.
   */
  "gcd" should "get greatest common divisor for 4 and 2" in {
    gcd(4, 2) shouldEqual 2
  }

  "gcd" should "get greatest common divisor for -2 and -4" in {
    gcd(-2, -4) shouldEqual 2
  }

  "gcd" should "get greatest common divisor for 48 and 180" in {
    gcd(48, 180) shouldEqual 12
  }

  "gcd" should "get greatest common divisor for -48 and 180" in {
    gcd(-48, 180) shouldEqual 12
  }

  "gcd" should "get greatest common divisor for 180 and 48" in {
    gcd(180, 48) shouldEqual 12
  }

  "gcd" should "get greatest common divisor for 180 and -48" in {
    gcd(180, -48) shouldEqual 12
  }

  "gcd" should "get greatest common divisor for 0 and 180" in {
    gcd(0, 180) shouldEqual 180
  }

  "gcd" should "get greatest common divisor for 180 and 0" in {
    gcd(180, 0) shouldEqual 180
  }

  "gcd" should "return -1 instead of a greatest common divisor for 0 and 0" in {
    gcd(0, 0) shouldEqual -1
  }

  /*
   * Lowest common denominator tests.
   */
  "lcm" should "get lowest common denominator for 0 and 0" in {
    lcm(0, 0) shouldEqual 0
  }

  "lcm" should "get lowest common denominator for 5 and 0" in {
    lcm(5, 0) shouldEqual 0
  }

  "lcm" should "get lowest common denominator for 0 and 5" in {
    lcm(0, 5) shouldEqual 0
  }

  "lcm" should "get lowest common denominator for 2 and 3" in {
    lcm(2, 3) shouldEqual 6
  }

  "lcm" should "get lowest common denominator for 3 and 2" in {
    lcm(3, 2) shouldEqual 6
  }

  "lcm" should "get lowest common denominator for 5 and -17" in {
    lcm(5, -17) shouldEqual 85
  }

  "lcm" should "get lowest common denominator for -17 and 5" in {
    lcm(-17, 5) shouldEqual 85
  }

  "lcm" should "get lowest common denominator for -6 and -17" in {
    lcm(-6, -17) shouldEqual 102
  }
}
