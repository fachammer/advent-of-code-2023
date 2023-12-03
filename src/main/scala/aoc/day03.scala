object day03:
  given Day = 3

  sumOfPartNumbers.testCases(file("example") -> 4361, file("input") -> 532428)
  def sumOfPartNumbers(input: String): Int =
    given Schematic = parseSchematic(input)
    partNumbers.map(_.number).sum

  type Schematic = Seq[String]
  def parseSchematic(input: String): Schematic     = input.linesIterator.toArray
  def getRow(using schematic: Schematic)(row: Int) = schematic(row)
  def get(using schematic: Schematic)(col: Int, row: Int) = schematic(row)(col)

  case class SchematicNumber(val number: Int, val row: Int, val startCol: Int):
    def width = number.toString.length

    def adjacents(using Schematic): Seq[(Int, Int)] =
      val upperBorder = for col <- -1 to width yield (col, -1)
      val lowerBorder = for col <- -1 to width yield (col, 1)
      val border      = upperBorder ++ lowerBorder :+ (-1, 0) :+ (width, 0)
      border.offset(startCol, row).filter(isInBounds.tupled)

    def asPartNumber(using Schematic) =
      if adjacents.map(get.tupled).exists(isSymbol) then Some(this) else None

  extension (s: String)
    def unsignedIntsWithRange = "[0-9]+".r
      .findAllMatchIn(s)
      .map(x => (x.matched.toInt, x.start until x.end))

    def unsignedIntAtPositionWithRange(i: Int): Option[(Int, Range)] =
      unsignedIntsWithRange.find((_, range) => range.contains(i))

  def partNumbers(using schematic: Schematic) = for
    (line, row)     <- schematic.zipWithIndex
    (number, range) <- line.unsignedIntsWithRange
    partNumber      <- partNumberAt(range.start, row)
  yield partNumber

  def isInBounds(using schematic: Schematic)(col: Int, row: Int) =
    val width  = getRow(0).length
    val height = schematic.length
    (0 until width).contains(col) && (0 until height).contains(row)

  def isSymbol(char: Char) = char match
    case '.'            => false
    case x if x.isDigit => false
    case _              => true

  gearRatiosSum.testCases(file("example") -> 467835, file("input") -> 84051670)
  def gearRatiosSum(input: String): Int =
    given Schematic = parseSchematic(input)
    gears.map(_.ratio).sum

  case class Gear(val first: SchematicNumber, val second: SchematicNumber):
    def ratio = first.number * second.number

  def gears(using schematic: Schematic): Seq[Gear] = for
    (line, row) <- schematic.zipWithIndex
    (char, col) <- line.zipWithIndex
    gear        <- gearAt(col, row)
  yield gear

  def gearAt(using Schematic)(col: Int, row: Int): Option[Gear] =
    if get(col, row) != '*' then return None
    val adjacentPartNumbers = neighborhood8(col, row)
      .filter(isInBounds.tupled)
      .flatMap(partNumberAt.tupled)
      .distinct
    adjacentPartNumbers match
      case Seq(first, second) => Some(Gear(first, second))
      case _                  => None

  extension (seq: Seq[(Int, Int)])
    def offset(col: Int, row: Int) = seq.map((x, y) => (x + col, y + row))

  def neighborhood8(using Schematic)(col: Int, row: Int): Seq[(Int, Int)] =
    for i <- -1 to 1; j <- -1 to 1 if (i, j) != (0, 0)
    yield (col + i, row + j)

  def partNumberAt(using Schematic)(col: Int, row: Int) = for
    (number, range) <- getRow(row).unsignedIntAtPositionWithRange(col)
    partNumber      <- SchematicNumber(number, row, range.start).asPartNumber
  yield partNumber
