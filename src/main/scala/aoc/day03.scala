object day03:
  given Day = 3

  sumOfPartNumbers.testCases(file("example") -> 4361, file("input") -> 532428)
  def sumOfPartNumbers(input: String): Int =
    given schematic: Schematic = parseSchematic(input)
    val partNumbers = numberPositions.filter(x => isPartNumber(x._1, x._2))
    partNumbers.map(_._1).sum

  case class Schematic(val schematic: Seq[String])

  def parseSchematic(input: String): Schematic =
    Schematic(input.linesIterator.toArray)

  def numberPositions(using schematic: Schematic): Seq[(Int, (Int, Int))] =
    val Schematic(lines) = schematic
    extension (s: String)
      def unsignedIntsWithIndex =
        "[0-9]+".r.findAllMatchIn(s).map(x => (x.matched.toInt, x.start))

    val result = for
      (line, row)   <- lines.zipWithIndex
      (number, col) <- line.unsignedIntsWithIndex
    yield (number, (col, row))

    result

  def isPartNumber(using schematic: Schematic)(
      number: Int,
      position: (Int, Int)
  ): Boolean =
    val adjacents = adjacentPositions(number, position)
    adjacents.map((col, row) => schematic.schematic(row)(col)).exists(isSymbol)

  def adjacentPositions(using schematic: Schematic)(
      number: Int,
      position: (Int, Int)
  ): Seq[(Int, Int)] =
    val Schematic(lines) = schematic
    val width            = lines(0).length
    val height           = lines.length
    val numberWidth      = number.toString.length
    def inBounds(position: (Int, Int)) =
      val (col, row) = position
      (0 until width).contains(col) && (0 until height).contains(row)

    val leftBorder  = Seq((-1, -1), (-1, 0), (-1, 1))
    val rightBorder = Seq((numberWidth, -1), (numberWidth, 0), (numberWidth, 1))
    val upperBorder = (0 until numberWidth).map((_, -1))
    val lowerBorder = (0 until numberWidth).map((_, 1))
    val border      = leftBorder ++ rightBorder ++ upperBorder ++ lowerBorder
    val (col, row)  = position
    val result      = border.map(p => (col + p._1, row + p._2)).filter(inBounds)
    result

  def isSymbol(char: Char): Boolean = char match
    case '.'            => false
    case x if x.isDigit => false
    case _              => true
