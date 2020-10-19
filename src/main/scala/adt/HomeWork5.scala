package adt

import cats.data.NonEmptySet

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

  sealed trait Suit
  object Suit {
    final case object Diamonds extends Suit
    final case object Spades extends Suit
    final case object Clubs extends Suit
    final case object Hearts extends Suit

    def create(value: Char): Either[ErrorMessage, Suit] = {
      value.toLower match {
        case 'd' => Right(Diamonds)
        case 's' => Right(Spades)
        case 'c' => Right(Clubs)
        case 'h' => Right(Hearts)
        case _ =>
          Left(
            ErrorMessage(
              "Suit value should be one of the following: d, s, c, h."
            )
          )
      }
    }
  }

  sealed trait Rank
  object Rank {
    final case object Two extends Rank
    final case object Three extends Rank
    final case object Four extends Rank
    final case object Five extends Rank
    final case object Six extends Rank
    final case object Seven extends Rank
    final case object Eight extends Rank
    final case object Nine extends Rank
    final case object Ten extends Rank
    final case object Jack extends Rank
    final case object Queen extends Rank
    final case object King extends Rank
    final case object Ace extends Rank

    def create(value: String): Either[ErrorMessage, Rank] = {
      value.toUpperCase match {
        case "2"  => Right(Two)
        case "3"  => Right(Three)
        case "4"  => Right(Four)
        case "5"  => Right(Five)
        case "6"  => Right(Six)
        case "7"  => Right(Seven)
        case "8"  => Right(Eight)
        case "9"  => Right(Nine)
        case "10" => Right(Ten)
        case "J"  => Right(Jack)
        case "Q"  => Right(Queen)
        case "K"  => Right(King)
        case "A"  => Right(Ace)
        case _ =>
          Left(
            ErrorMessage(
              "Recheck if rank is from 2 till 10, J, Q, K or A."
            )
          )
      }
    }
  }

  final case class Card(rank: Rank, suit: Suit)

  sealed trait Hand[H <: Hand[H]]
  object Hand {
    final case class Texas(value: Set[Card]) extends Hand[Texas] {
      def create: Option[Texas] = {
        if (value.size == 2) Some(Texas(value))
        else None
      }
    }
    final case class Omaha(value: Set[Card]) extends Hand[Omaha] {
      def create: Option[Omaha] = {
        if (value.size == 4) Some(Omaha(value))
        else None
      }
    }
  }

  final case class Board private (value: Set[Card])
  object Board {
    def create(value: Set[Card]): Option[Board] = {
      if (value.size == 5) Some(Board(value))
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

  final case class TestCase[H <: Hand[H]](
      board: Board,
      hands: NonEmptySet[Hand[H]]
  )
  object TestCase {
    def create[H](
        board: Board,
        hands: NonEmptySet[Hand[H]]
    ): Either[ErrorMessage, String] = {
      Right(board.toString + " " + hands.toString)
    }
  }

  //TODO: do the actual comparison, sorting that depends on the input and the combinations possible.
  final case class TestResult[H <: Hand[H]](
      value: TestCase[Hand[H]]
  )
  object TestResult {
    def create[H <: Hand[H]](value: TestCase[Hand[H]]): String = {
      ???
    }
  }
}
