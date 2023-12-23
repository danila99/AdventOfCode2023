import CamelCards._

import scala.annotation.tailrec

object Puzzle07CamelCards2 extends App with inputFileArgs with CommonUtils {
  def createCardWithJoker(face: Char): Card = face match {
    case 'A' => Ace
    case 'K' => King
    case 'Q' => Queen
    case 'J' => Joker // here it goes
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

  def handTypeStrategyWithJokers(cards: List[Card]): HandType = {
    // The idea: obtain handType for all cards minus Jokers.
    // Then improve handType with Jokers, applying them one by one recursively
    val cardsMinusJokers = cards.filter(_ != Joker)

    if (cardsMinusJokers.isEmpty) FiveOfAKind // five Jokers detected, the special case
    else {
      val handTypeInitial = Puzzle07CamelCards1.handTypeStrategy(cardsMinusJokers)
      val jokersCount = cards.size - cardsMinusJokers.size
      ImproveHandType(handTypeInitial, jokersCount)
    }
  }

  def improveByOneJoker(handType: HandType): HandType = handType match {
    case FiveOfAKind => FiveOfAKind
    case FourOfAKind => FiveOfAKind
    case FullHouse => FourOfAKind
    case ThreeOfAKind => FourOfAKind
    case TwoPair => FullHouse
    case OnePair => ThreeOfAKind
    case HighCard => OnePair
  }

  @tailrec
  def ImproveHandType(handType: HandType, jokersCount: Int): HandType = jokersCount match {
    case 0 => handType
    case count => ImproveHandType(improveByOneJoker(handType), count - 1)
  }

  val hands = for {
    line <- getLines
    (cardsStr, betStr) = splitInTwo(" ", line)
    cards = cardsStr.toList.map(createCardWithJoker)
  } yield Hand(cards, betStr.toInt, handTypeStrategyWithJokers)

  val totalWinnings: BigInt = hands.sorted.zipWithIndex.foldLeft(BigInt(0)) {
    case (total, (hand, index)) => total + (hand.bet * (index + 1))
  }

  Console.println(totalWinnings)
}

