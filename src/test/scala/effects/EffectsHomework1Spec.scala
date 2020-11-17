package effects

import EffectsHomework1Executable._
import org.scalatest._
import flatspec._
import matchers._

class EffectsHomework1Spec extends AnyFlatSpec with should.Matchers {
  "map" should "work" in {
    val a = IO(1)
    val b = 2
    val c = a.map(_ => b)
    c.unsafeRunSync() shouldEqual b
  }

  "flatMap" should "work" in {
    val a = IO(List(1, 2))
    val b = IO(List(3, 4))
    val c = a.flatMap(_ => b)
    c.unsafeRunSync() shouldEqual List(3, 4)
  }

  "*>" should "work" in {
    val a = IO(List(1, 2))
    val b = IO(List(3, 4))
    val c = a *> b
    c.unsafeRunSync() shouldEqual List(3, 4)
  }

  "as" should "work" in {
    val a = IO(List(1, 2))
    val b = IO(List(3, 4))
    val c = a as b
    c.unsafeRunSync() shouldEqual b
  }

  "void" should "work" in {
    val a = IO(List(1, 2))
    val c = a.void
    c.unsafeRunSync() shouldEqual ()
  }
}
