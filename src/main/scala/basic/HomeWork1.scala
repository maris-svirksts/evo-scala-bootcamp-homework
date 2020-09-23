package basic

import scala.annotation.tailrec

object HomeWork1 {
  /*
   * Lowest common denominator.
   *
   * Calculated by multiplying the integers given, then dividing by Greatest common divisor for the same integers.
   * Greatest common divisor is defined within gcd method.
   *
   * 0 is returned if either of the two integers are equal to 0.
   */
  def lcm(a: Int, b: Int): Int = {
    if (0 == a || 0 == b) 0
    Math.abs(a * b) / gcd(a, b)
  }

  /*
   * Greatest common divisor.
   *
   * Calculated by using Euclidean algorithm: https://en.wikipedia.org/wiki/Euclidean_algorithm
   * Using modulus version (original) because subtraction based version requires positive integers as input.
   *
   * -1 is returned if both integers are equal to 0.
   */
  @tailrec
  def gcd(a: Int, b: Int): Int = {

    if (0 == a && 0 == b) -1
    else if (0 == b) Math.abs(a)
    else if (0 == a) Math.abs(b)
    else gcd(b, a % b)
  }
}
