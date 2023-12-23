import CamelCards._

import scala.annotation.tailrec

object Puzzle07CamelCards1 extends App with inputFileArgs with CommonUtils {
  def createCard(face: Char): Card = face match {
    case 'A' => Ace
    case 'K' => King
    case 'Q' => Queen
    case 'J' => Jack
    case 'T' => Ten
    case '9' => Nine
    case '8' => Eight
    case '7' => Seven
    case '6' => Six
    case '5' => Five
    case '4' => Four
    case '3' => Three
    case '2' => Two
    case unknown => throw new IllegalArgumentException(s"unknown face: $unknown")
  }

  def handTypeStrategy(cards: List[Card]): HandType = {
    val grouped = cards.groupBy(_.getClass.getName)
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

  val hands = for {
    line <- getLines
    (cardsStr, betStr) = splitInTwo(" ", line)
    cards = cardsStr.toList.map(createCard)
  } yield Hand(cards, betStr.toInt, handTypeStrategy)

  val totalWinnings: BigInt = hands.sorted.zipWithIndex.foldLeft(BigInt(0)) {
    case (total, (hand, index)) => total + (hand.bet * (index + 1))
  }

  Console.println(totalWinnings)
}

object CamelCards {
  // A, K, Q, J, T, 9, 8, 7, 6, 5, 4, 3, 2, Joker
  sealed trait Card {
    val rank: Byte
  }

  case object Ace extends Card { val rank = 13 }
  case object King extends Card { val rank = 12 }
  case object Queen extends Card { val rank = 11 }
  case object Jack extends Card { val rank = 10 }
  case object Ten extends Card { val rank = 9 }
  case object Nine extends Card { val rank = 8 }
  case object Eight extends Card { val rank = 7 }
  case object Seven extends Card { val rank = 6 }
  case object Six extends Card { val rank = 5 }
  case object Five extends Card { val rank = 4 }
  case object Four extends Card { val rank = 3 }
  case object Three extends Card { val rank = 2 }
  case object Two extends Card { val rank = 1 }
  case object Joker extends Card { val rank = 0 }

  sealed trait HandType
  case object FiveOfAKind extends HandType
  case object FourOfAKind extends HandType
  case object FullHouse extends HandType
  case object ThreeOfAKind extends HandType
  case object TwoPair extends HandType
  case object OnePair extends HandType
  case object HighCard extends HandType

  final case class Hand(cards: List[Card], bet: Int, handTypeStrategy: List[Card] => HandType)
    extends Comparable[Hand] {
    lazy val handTypeRank: Byte = handTypeStrategy(cards) match {
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
}

