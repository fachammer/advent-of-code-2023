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

  extension (s: String)
    def unsignedIntsWithIndex =
      "[0-9]+".r.findAllMatchIn(s).map(x => (x.matched.toInt, (x.start, x.end)))

  def numberPositions(using schematic: Schematic): Seq[(Int, (Int, Int))] =
    val Schematic(lines) = schematic
    val result = for
      (line, row)        <- lines.zipWithIndex
      (number, (col, _)) <- line.unsignedIntsWithIndex
    yield (number, (col, row))

    result

  def isPartNumber(using schematic: Schematic)(
      number: Int,
      position: (Int, Int)
  ): Boolean =
    val adjacents = numberAdjacentPositions(number, position)
    adjacents.map((col, row) => schematic.schematic(row)(col)).exists(isSymbol)

  def inBounds(using schematic: Schematic)(position: (Int, Int)) =
    val Schematic(lines) = schematic
    val width            = lines(0).length
    val height           = lines.length
    val (col, row)       = position
    (0 until width).contains(col) && (0 until height).contains(row)

  def numberAdjacentPositions(using schematic: Schematic)(
      number: Int,
      position: (Int, Int)
  ): Seq[(Int, Int)] =
    val numberWidth = number.toString.length
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

  sumOfGearRatios.testCases(
    file("example") -> 467835,
    file("input")   -> 84051670
  )
  def sumOfGearRatios(input: String): Int =
    given schematic: Schematic = parseSchematic(input)
    val partNumbers = numberPositions.filter(x => isPartNumber(x._1, x._2))
    asterisks.flatMap(gearRatio).sum

  def asterisks(using schematic: Schematic): Seq[(Int, Int)] =
    val Schematic(lines) = schematic
    for
      (line, row) <- lines.zipWithIndex
      (char, col) <- line.zipWithIndex
      if char == '*'
    yield (col, row)

  def gearRatio(using schematic: Schematic)(position: (Int, Int)): Option[Int] =
    val border =
      Seq((-1, 0), (-1, -1), (0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1))
    val adjacentPositions =
      border.map(p => (position._1 + p._1, position._2 + p._2)).filter(inBounds)
    val adjacentPartNumbers = adjacentPositions
      .flatMap(numberAtPosition)
      .filter(x => isPartNumber(x._1, (x._2._2._1, x._2._1)))
      .distinct
    adjacentPartNumbers match
      case Seq((first, _), (second, _)) => Some(first * second)
      case _                            => None

  def numberAtPosition(using schematic: Schematic)(
      position: (Int, Int)
  ): Option[(Int, (Int, (Int, Int)))] =
    val Schematic(lines) = schematic
    val (col, row)       = position
    val char             = lines(row)(col)
    if (char.isDigit)
      val (number, (start, end)) = lines(row).unsignedIntsWithIndex
        .find((_, r) => (r._1 until r._2).contains(col))
        .get
      Some((number, (row, (start, end))))
    else None
