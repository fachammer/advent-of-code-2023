package day04

// part 1
def points(input: String): Int =
  input.linesIterator.map(parseCard).map(cardPoints.tupled).sum

def parseCard(input: String): (Set[Int], Set[Int]) =
  input match
    case s"$card: $winningNumbers | $ownNumbers" =>
      (
        winningNumbers.split(" ").filterNot(_.isEmpty).map(_.toInt).toSet,
        ownNumbers.split(" ").filterNot(_.isBlank).map(_.toInt).toSet
      )

def cardPoints(winningNumbers: Set[Int], ourNumbers: Set[Int]): Int =
  winningNumbers.intersect(ourNumbers).size match
    case 0 => 0
    case n => scala.math.pow(2, n - 1).toInt
