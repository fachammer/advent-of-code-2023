package day07

import debug._
import scala.math.Ordered.orderingToOrdered

// part 1
enum HandType:
  case FiveOfAKind
  case FourOfAKind
  case FullHouse
  case ThreeOfAKind
  case TwoPair
  case OnePair
  case HighCard

case class Hand(values: Seq[Int]):
  def handType: HandType =
    val groups = values.groupBy(x => x)
    if groups.size == 1 then HandType.FiveOfAKind
    else if groups.size == 2 then
      if groups.values.map(_.size).toSet == Set(1, 4) then HandType.FourOfAKind
      else HandType.FullHouse
    else if groups.size == 3 then
      if groups.values.map(_.size).toSet == Set(3, 1, 1) then
        HandType.ThreeOfAKind
      else HandType.TwoPair
    else if groups.size == 4 then HandType.OnePair
    else HandType.HighCard

object HandOrdering extends Ordering[Hand]:
  override def compare(left: Hand, right: Hand): Int =
    val comparison = left.handType.compare(right.handType)
    if comparison != 0 then comparison
    else right.values.compare(left.values)

object HandTypeOrdering extends Ordering[HandType]:
  override def compare(left: HandType, right: HandType): Int =
    left.ordinal.compare(right.ordinal)

given Ordering[Hand]     = HandOrdering
given Ordering[HandType] = HandTypeOrdering

def totalWinnings(input: String) =
  val bids = input.linesIterator
    .map(_.span(x => !x.isSpaceChar))
    .map((hand, bid) => (Hand(hand.map(cardValue)), bid.strip.toInt))
    .toSeq

  bids.sorted.reverse.zipWithIndex
    .map { case ((hand, bid), index) =>
      (hand, hand.handType, bid * (index + 1))
    }
    .d
    .map((_, _, winning) => winning)
    .sum

def cardValue(char: Char): Int = char match
  case 'A'            => 14
  case 'K'            => 13
  case 'Q'            => 12
  case 'J'            => 11
  case 'T'            => 10
  case x if x.isDigit => x.asDigit
