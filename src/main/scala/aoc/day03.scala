package day03

// part 1
type Schematic = Array[String]
case class SchematicNumber(val number: Int, val row: Int, val startCol: Int):
  def range = startCol until (startCol + number.toString.length)

  def asPartNumber(using Schematic): Option[SchematicNumber] =
    adjacentPositions.map(charAt.tupled).find(isSymbol).map(_ => this)

  private def adjacentPositions(using Schematic): Set[(Int, Int)] =
    val upperBorder = for col <- -1 to range.length yield (col, -1)
    val lowerBorder = for col <- -1 to range.length yield (col, 1)
    val border      = upperBorder ++ lowerBorder :+ (-1, 0) :+ (range.length, 0)
    border.offset(startCol, row).withinBounds.toSet

def sumOfPartNumbers(input: String): Int =
  given Schematic = input.linesIterator.toArray
  partNumbers.map(_.number).sum

def partNumbers(using schematic: Schematic) = for
  (line, row)     <- schematic.zipWithIndex
  (number, range) <- line.unsignedIntsWithRange
  partNumber      <- SchematicNumber(number, row, range.start).asPartNumber
yield partNumber

def lineAt(using schematic: Schematic)(row: Int) = schematic(row)
def charAt(using Schematic)(col: Int, row: Int)  = lineAt(row)(col)
def isInBounds(using schematic: Schematic)(col: Int, row: Int) =
  val width  = lineAt(0).length
  val height = schematic.length
  (0 until width).contains(col) && (0 until height).contains(row)

def isSymbol(char: Char) = char != '.' && !char.isDigit

extension (s: String)
  def unsignedIntsWithRange = for numberMatch <- "[0-9]+".r.findAllMatchIn(s)
  yield (numberMatch.matched.toInt, numberMatch.start until numberMatch.end)

  def unsignedIntAtIndexWithRange(i: Int): Option[(Int, Range)] =
    unsignedIntsWithRange.find((_, range) => range.contains(i))

extension (seq: Seq[(Int, Int)])
  def offset(col: Int, row: Int)    = for (x, y) <- seq yield (x + col, y + row)
  def withinBounds(using Schematic) = seq.filter(isInBounds.tupled)

// part 2
case class Gear(val first: SchematicNumber, val second: SchematicNumber):
  def ratio = first.number * second.number

def gearRatioSum(input: String): Int =
  given Schematic = input.linesIterator.toArray
  gears.map(_.ratio).sum

def gears(using schematic: Schematic): Seq[Gear] = for
  (line, row) <- schematic.zipWithIndex
  (char, col) <- line.zipWithIndex
  gear        <- gearAt(col, row)
yield gear

def gearAt(using Schematic)(col: Int, row: Int): Option[Gear] =
  if charAt(col, row) != '*' then return None
  for Seq(first, second) <- Some(adjacentPartNumbersAt(col, row))
  yield Gear(first, second)

def adjacentPartNumbersAt(using Schematic)(col: Int, row: Int) =
  val neighborhood8 = for i <- -1 to 1; j <- -1 to 1 yield (col + i, row + j)
  neighborhood8.withinBounds.flatMap(partNumberAt.tupled).distinct

def partNumberAt(using Schematic)(col: Int, row: Int) = for
  (number, range) <- lineAt(row).unsignedIntAtIndexWithRange(col)
  partNumber      <- SchematicNumber(number, row, range.start).asPartNumber
yield partNumber
