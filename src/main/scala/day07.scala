package day07

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
      if groups.values.map(_.size).toSeq.sorted.reverse == Seq(4, 1) then
        HandType.FourOfAKind
      else HandType.FullHouse
    else if groups.size == 3 then
      if groups.values.map(_.size).toSeq.sorted.reverse == Seq(3, 1, 1) then
        HandType.ThreeOfAKind
      else HandType.TwoPair
    else if groups.size == 4 then HandType.OnePair
    else HandType.HighCard

class HandOrdering(using handTypeOrdering: Ordering[HandType])
    extends Ordering[Hand]:
  def compare(left: Hand, right: Hand): Int =
    val comparison = left.handType.compare(right.handType)
    if comparison != 0 then comparison
    else right.values.compare(left.values)

object HandTypeOrdering extends Ordering[HandType]:
  override def compare(left: HandType, right: HandType): Int =
    left.ordinal.compare(right.ordinal)

def totalWinnings(input: String) =
  val bids = input.linesIterator
    .map(_.span(x => !x.isSpaceChar))
    .map((hand, bid) => (Hand(hand.map(cardValue)), bid.strip.toInt))
    .toSeq

  given Ordering[HandType] = HandTypeOrdering
  given Ordering[Hand]     = HandOrdering()
  bids.sorted.reverse.zipWithIndex.map { case ((_, bid), index) =>
    bid * (index + 1)
  }.sum

def cardValue(char: Char): Int = char match
  case 'A'            => 14
  case 'K'            => 13
  case 'Q'            => 12
  case 'J'            => 11
  case 'T'            => 10
  case x if x.isDigit => x.asDigit

// part 2
case class HandWithJoker(hand: String):
  def handType: HandType =
    val groupsTypeWithoutJoker = hand
      .groupBy(x => x)
      .filterKeys(_ != 'J')
      .values
      .map(_.size)
      .toSeq
      .sorted
      .reverse

    val numberOfJokers = hand.groupBy(x => x).get('J').map(_.size).getOrElse(0)
    import HandType.*
    (groupsTypeWithoutJoker, numberOfJokers) match
      case (_, 5) | (_, 4) | (Seq(2), 3) | (Seq(3), 2) | (Seq(4), 1) =>
        FiveOfAKind
      case (Seq(1, 1), 3) | (Seq(2, 1), 2) | (Seq(3, 1), 1) => FourOfAKind
      case (Seq(2, 2), 1)                                   => FullHouse
      case (Seq(1, 1, 1), 2) | (Seq(2, 1, 1), 1)            => ThreeOfAKind
      case (Seq(1, 1, 1, 1), 1)                             => OnePair
      case (_, 0) => Hand(hand.map(cardValue)).handType

class HandWithJokerOrdering(using handTypeOrdering: Ordering[HandType])
    extends Ordering[HandWithJoker]:
  def compare(left: HandWithJoker, right: HandWithJoker): Int =
    val comparison = left.handType.compare(right.handType)
    def cardValueWithJoker(char: Char) =
      if char == 'J' then 1 else cardValue(char)
    if comparison != 0 then comparison
    else
      right.hand
        .map(cardValueWithJoker)
        .compare(left.hand.map(cardValueWithJoker))

def totalWinningsWithJoker(input: String) =
  val bids = input.linesIterator
    .map(_.span(x => !x.isSpaceChar))
    .map((hand, bid) => (HandWithJoker(hand), bid.strip.toInt))
    .toSeq

  given Ordering[HandType]      = HandTypeOrdering
  given Ordering[HandWithJoker] = HandWithJokerOrdering()
  bids.sorted.reverse.zipWithIndex
    .map { case ((hand, bid), index) =>
      (hand, hand.handType, index + 1, bid, bid * (index + 1))
    }
    .map((_, _, _, _, winning) => winning)
    .sum
