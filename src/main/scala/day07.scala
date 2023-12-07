package day07

// part 1
enum HandType:
  case FiveOfAKind
  case FourOfAKind
  case FullHouse
  case ThreeOfAKind
  case TwoPair
  case OnePair
  case HighCard

case class Hand(hand: String)
case class Rules(val cardValue: Char => Int, handType: Hand => HandType)

val noJokerRules = Rules(cardValue, handTypeWithoutJoker)
def totalWinningsNoJoker(input: String) =
  totalWinnings(noJokerRules, input)

def totalWinnings(rules: Rules, input: String) =
  import scala.math.Ordered.orderingToOrdered
  val handTypeOrdering: Ordering[HandType] = Ordering.by(t => t.ordinal)
  given Ordering[(HandType, Iterable[Int])] =
    Ordering.Tuple2(handTypeOrdering.reverse, Ordering.Iterable[Int])
  given Ordering[Hand] = Ordering.by((hand: Hand) =>
    (rules.handType(hand), hand.hand.map(rules.cardValue))
  )

  parseHandBids(input).sorted
    .map(_._2)
    .zipWithIndex
    .map { (bid, index) => bid * (index + 1) }
    .sum

def parseHandBids(input: String): Seq[(Hand, Int)] =
  input.linesIterator
    .map(_.span(x => !x.isSpaceChar))
    .map((hand, bid) => (Hand(hand), bid.strip.toInt))
    .toSeq

def cardValue(char: Char): Int = char match
  case 'A'            => 14
  case 'K'            => 13
  case 'Q'            => 12
  case 'J'            => 11
  case 'T'            => 10
  case x if x.isDigit => x.asDigit

def handTypeWithoutJoker(hand: Hand): HandType =
  val handType = sizeType(hand.hand.groupBy(x => x))
  handType match
    case Seq(5)             => HandType.FiveOfAKind
    case Seq(1, 4)          => HandType.FourOfAKind
    case Seq(2, 3)          => HandType.FullHouse
    case Seq(1, 1, 3)       => HandType.ThreeOfAKind
    case Seq(1, 2, 2)       => HandType.TwoPair
    case Seq(1, 1, 1, 2)    => HandType.OnePair
    case Seq(1, 1, 1, 1, 1) => HandType.HighCard

def sizeType(map: Map[Char, String]) = map.values.map(_.size).toSeq.sorted

// part 2
val jokerRules = Rules(cardValueWithJoker, handTypeWithJoker)
def totalWinningsWithJoker(input: String) = totalWinnings(jokerRules, input)

def cardValueWithJoker(char: Char) = if char == 'J' then 1 else cardValue(char)

def handTypeWithJoker(hand: Hand): HandType =
  val handType = sizeType(hand.hand.groupBy(x => x).filterKeys(_ != 'J').toMap)

  handType match
    case Seq() | Seq(_)  => HandType.FiveOfAKind
    case Seq(1, _)       => HandType.FourOfAKind
    case Seq(2, 2)       => HandType.FullHouse
    case Seq(1, 1, _)    => HandType.ThreeOfAKind
    case Seq(1, 1, 1, 1) => HandType.OnePair
    case _               => handTypeWithoutJoker(hand)
