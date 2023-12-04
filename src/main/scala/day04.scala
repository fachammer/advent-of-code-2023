package day04

// part 1
def points(input: String): Int =
  input.linesIterator
    .map(parseCard)
    .map((_, winning, own) => cardPoints(winning, own))
    .sum

def parseCard(input: String): (Int, Set[Int], Set[Int]) =
  input match
    case s"Card $card: $winningNumbers | $ownNumbers" =>
      (
        card.strip.toInt,
        winningNumbers
          .split(" ")
          .filterNot(_.isEmpty)
          .map(_.strip)
          .map(_.toInt)
          .toSet,
        ownNumbers
          .split(" ")
          .filterNot(_.isBlank)
          .map(_.strip)
          .map(_.toInt)
          .toSet
      )

def cardPoints(winningNumbers: Set[Int], ourNumbers: Set[Int]): Int =
  winningNumbers.intersect(ourNumbers).size match
    case 0 => 0
    case n => scala.math.pow(2, n - 1).toInt

// part2
def numberOfScratchCards(input: String): Int =
  val numberOfCards = input.linesIterator.length
  var cardAmounts =
    scala.collection.mutable.Map[Int, Int]((1 to numberOfCards).map((_, 1))*)

  for line <- input.linesIterator do
    val (card, winningNumbers, ownNumbers) = parseCard(line)
    val intersection    = winningNumbers.intersect(ownNumbers)
    val numberOfMatches = intersection.size
    for
      i <- (card + 1) to (card + numberOfMatches)
      if i <= numberOfCards
    do cardAmounts(i) += cardAmounts(card)

  cardAmounts.values.sum
