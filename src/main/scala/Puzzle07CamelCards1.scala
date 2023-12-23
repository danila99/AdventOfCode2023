import CamelCards._

import scala.annotation.tailrec

object Puzzle07CamelCards1 extends App with inputFileArgs with CommonUtils {
  val hands = for {
    line <- getLines
    (cards, bet) = splitInTwo(" ", line)
  } yield Hand(cards, bet.toInt)

  val totalWinnings: BigInt = hands.sorted.zipWithIndex.foldLeft(BigInt(0)) {
    case (total, (hand, index)) => total + (hand.bet * (index + 1))
  }

  Console.println(totalWinnings)
}

object CamelCards {
  // A, K, Q, J, T, 9, 8, 7, 6, 5, 4, 3, 2
  sealed trait Label
  case object Ace extends Label
  case object King extends Label
  case object Queen extends Label
  case object Jack extends Label
  case object TEN extends Label
  case object NINE extends Label
  case object EIGHT extends Label
  case object SEVEN extends Label
  case object SIX extends Label
  case object FIVE extends Label
  case object FOUR extends Label
  case object THREE extends Label
  case object TWO extends Label

  final case class Card(label: Label) {
    lazy val rank: Byte = label match {
      case Ace => 12
      case King => 11
      case Queen => 10
      case Jack => 9
      case TEN => 8
      case NINE => 7
      case EIGHT => 6
      case SEVEN => 5
      case SIX => 4
      case FIVE => 3
      case FOUR => 2
      case THREE => 1
      case TWO => 0
    }
  }

  object Card {
    def apply(face: Char): Card = face match {
      case 'A' => Card(Ace)
      case 'K' => Card(King)
      case 'Q' => Card(Queen)
      case 'J' => Card(Jack)
      case 'T' => Card(TEN)
      case '9' => Card(NINE)
      case '8' => Card(EIGHT)
      case '7' => Card(SEVEN)
      case '6' => Card(SIX)
      case '5' => Card(FIVE)
      case '4' => Card(FOUR)
      case '3' => Card(THREE)
      case '2' => Card(TWO)
      case unknown => throw new IllegalArgumentException(s"unknown face: $unknown")
    }
  }

  sealed trait HandType
  case object FiveOfAKind extends HandType
  case object FourOfAKind extends HandType
  case object FullHouse extends HandType
  case object ThreeOfAKind extends HandType
  case object TwoPair extends HandType
  case object OnePair extends HandType
  case object HighCard extends HandType

  final case class Hand(cards: List[Card], bet: Int) extends Comparable[Hand] {
    lazy val handType: HandType = {
      val grouped = cards.groupBy(_.label)
      val sameCardsSize: Seq[Int] = grouped.map { case (_, labels) => labels.size }.toSeq.sorted.reverse
      val maxTwoRanks = (sameCardsSize.head, sameCardsSize.tail.headOption.getOrElse(0))
      maxTwoRanks match {
        case (5, _) => FiveOfAKind
        case (4, _) => FourOfAKind
        case (3, 2) => FullHouse
        case (3, _) => ThreeOfAKind
        case (2, 2) => TwoPair
        case (2, _) => OnePair
        case (1, _) => HighCard
        case (x, y) => throw new IllegalArgumentException(s"unknown ranks: $x $y")
      }
    }

    lazy val handTypeRank: Byte = handType match {
      case FiveOfAKind => 6
      case FourOfAKind => 5
      case FullHouse => 4
      case ThreeOfAKind => 3
      case TwoPair => 2
      case OnePair => 1
      case HighCard => 0
    }

    lazy val rank: List[Byte] = List(handTypeRank) ++ cards.map(_.rank)

    @tailrec
    private def compareRanks(otherRank: List[Byte], index: Int): Int = otherRank(index) match {
      case other if other == rank(index) => compareRanks(otherRank, index + 1)
      case other => rank(index).compareTo(other)
    }

    override def compareTo(otherHand: Hand): Int = compareRanks(otherHand.rank, 0)
  }

  object Hand {
    def apply(cards: String, bet: Int): Hand = Hand(cards.toList.map(Card(_)), bet)
  }
}

