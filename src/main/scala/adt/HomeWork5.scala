package adt

object HomeWork5 {

  // Homework. Define all algebraic data types, which would be needed to implement “Hold’em Hand Strength”
  // task you completed to join the bootcamp. Use your best judgement about particular data types to include
  // in the solution, you can model concepts like:
  //
  // 1. Suit
  // 2. Rank
  // 3. Card
  // 4. Hand (Texas or Omaha)
  // 5. Board
  // 6. Poker Combination (High Card, Pair, etc.)
  // 7. Test Case (Board & Hands to rank)
  // 8. Test Result (Hands ranked in a particular order for a particular Board, accounting for splits)
  //
  // Make sure the defined model protects against invalid data. Use value classes and smart constructors as
  // appropriate. Place the solution under `adt` package in your homework repository.

  // Copy/Pasted from 3rd homework.
  final case class ErrorMessage(value: String)

  final case class Suit private (value: String) extends AnyVal
  object Suit {
    private val options: List[String] =
      List("d", "s", "c", "h")
    def create(value: String): Either[ErrorMessage, Suit] = {
      value match {
        case correct if options.contains(value) => Right(Suit(value))
        case _ =>
          Left(
            ErrorMessage(
              "Suit value should be one of the following: d, s, c, h."
            )
          )
      }
    }
  }

  final case class Rank private (value: String) extends AnyVal
  object Rank {
    private val options: List[String] =
      List("2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K", "A")
    def create(value: String): Either[ErrorMessage, Rank] = {
      value match {
        case correct if options.contains(value) => Right(Rank(value))
        case _ =>
          Left(
            ErrorMessage("Recheck if rank is from 2 till 10 or J, Q, K or A.")
          )
      }
    }
  }

  final case class Card(rank: Rank, suit: Suit)

  //Might be better if done like Suit and Rank ( more unified ), did it this way to try it out.
  sealed trait Hand {
    def create: Option[Hand]
  }
  object Hand {
    final case class Texas(value: List[Card]) extends Hand {
      override def create: Option[Hand] = {
        if (value.length == 2) Some(Texas(value))
        else None
      }
    }
    final case class Omaha(value: List[Card]) extends Hand {
      override def create: Option[Hand] = {
        if (value.length == 4) Some(Omaha(value))
        else None
      }
    }
  }

  final case class Board private (value: List[Card])
  object Board {
    def create(value: List[Card]): Option[Board] = {
      if (value.length == 5) Some(Board(value))
      else None
    }
  }

  // TODO: Need to add additional checks to Combinations.
  sealed trait Combinations
  object Combinations {
    final case object HighCard extends Combinations
    final case object Pair extends Combinations
    final case object TwoPairs extends Combinations
    final case object ThreeOfaKind extends Combinations
    final case object Straight extends Combinations
    final case object Flush extends Combinations
    final case object FullHouse extends Combinations
    final case object FourOfaKind extends Combinations
    final case object StraightFlush extends Combinations
    final case object RoyalFlush extends Combinations
  }

  final case class TestCase(board: Board, hands: List[Hand])
  object TestCase {
    def create(board: Board, hands: List[Hand]): String = {
      board.toString + " " + hands.toString
    }
  }

  //TODO: do the actual comparison, sorting that depends on the input and the combinations possible.
  final case class TestResult(value: TestCase)
  object TestResult {
    def create(value: TestCase): String = {
      ???
    }
  }
}
