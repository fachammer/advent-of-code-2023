package day04

// part 1
case class Card(val number: Int, winning: Set[Int], our: Set[Int]):
  def matches: Int = winning.intersect(our).size
  def points: Int = matches match
    case 0 => 0
    case n => scala.math.pow(2, n - 1).toInt

def points(input: String) = input.linesIterator.map(parseCard).map(_.points).sum

def parseCard(input: String): Card =
  val pattern = raw"Card +(\d+): +(.*) \| +(.*)".r
  input match
    case pattern(cardNumber, winning, our) =>
      def parseNumbers(n: String) = n.split(" +").map(_.toInt).toSet
      Card(cardNumber.toInt, parseNumbers(winning), parseNumbers(our))

// part 2
def scratchCards(input: String): Int =
  val cards              = input.linesIterator.map(parseCard).toSeq
  val initialCardAmounts = for Card(n, _, _) <- cards yield (n, 1)
  val cardAmounts = scala.collection.mutable.Map[Int, Int](initialCardAmounts*)

  for
    card @ Card(number, _, _) <- cards
    nextCard                  <- number + 1 to number + card.matches
    if cardAmounts.contains(nextCard)
  do cardAmounts(nextCard) += cardAmounts(number)

  cardAmounts.values.sum
