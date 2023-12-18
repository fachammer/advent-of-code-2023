package day18

import scala.collection.mutable

// part 1
def part1(input: String) =
  val steps =
    for case s"$step $amount $_" <- input.linesIterator
    yield (step, amount.toInt)
  lagoonSize(steps)

def part2(input: String) =
  val steps =
    for
      case s"$_ $_ (#$color)" <- input.linesIterator
      step = color.last match
        case '0' => "R"
        case '1' => "D"
        case '2' => "L"
        case '3' => "U"
      amount = Integer.parseInt(color.init, 16)
    yield (step, amount.toInt)
  lagoonSize(steps)

def lagoonSize(steps: Iterator[(String, Int)]) =
  enum Line:
    case Right(from: Position, length: Int)
    case Down(from: Position, length: Int)
    case Left(from: Position, length: Int)
    case Up(from: Position, length: Int)

    def fromPosition = this match
      case Right(from, length) => from
      case Down(from, length)  => from
      case Left(from, length)  => from
      case Up(from, length)    => from
    def toPosition = fromPosition + Direction(
      direction.col * (lineLength - 1),
      direction.row * (lineLength - 1),
    )
    def direction = this match
      case Right(from, length) => Direction(1, 0)
      case Down(from, length)  => Direction(0, 1)
      case Left(from, length)  => Direction(-1, 0)
      case Up(from, length)    => Direction(0, -1)
    def lineLength = this match
      case Right(from, length) => length
      case Down(from, length)  => length
      case Left(from, length)  => length
      case Up(from, length)    => length

    def extended = this match
      case Right(from, length) => Right(from - direction, length + 1)
      case Down(from, length)  => Down(from - direction, length + 1)
      case Left(from, length)  => Left(from - direction, length + 1)
      case Up(from, length)    => Up(from - direction, length + 1)

    val minCol = fromPosition.col.min(toPosition.col)
    val maxCol = fromPosition.col.max(toPosition.col)
    val minRow = fromPosition.row.min(toPosition.row)
    val maxRow = fromPosition.row.max(toPosition.row)

  import Line.*
  case class Direction(col: Int, row: Int):
    def rightNormal = Direction(-row, col)
  case class Position(col: Int, row: Int):
    def +(direction: Direction) =
      Position(col + direction.col, row + direction.row)
    def -(direction: Direction) =
      Position(col - direction.col, row - direction.row)
    def -(position: Position) =
      Direction(col - position.col, row - position.row)
    def neighborhood4 =
      Seq((col + 1, row), (col, row + 1), (col - 1, row), (col, row - 1)).map(
        (c, r) => Position(c, r),
      )

  var position = Position(0, 0)
  val lines    = mutable.Buffer[Line]()
  for (step, amount) <- steps do
    val line = step match
      case "R" => Right(position, amount + 1)
      case "D" => Down(position, amount + 1)
      case "L" => Left(position, amount + 1)
      case "U" => Up(position, amount + 1)
    lines.append(line)
    position = line.toPosition

  val sortedLines =
    lines.sortWith((l, k) =>
      l.minCol < k.minCol || l.minCol == k.minCol && l.minRow < k.minRow || l.minCol == k.minCol && l.minRow == k.minRow && ((
        l,
        k,
      ) match
        case (Up(_, _) | Down(_, _), _) => true
        case _                          => false
      ),
    )

  def rowIntersectingLines(row: Int) =
    sortedLines.filter { line =>
      (line.minRow to line.maxRow).contains(row)
    }

  val positions = lines.flatMap(l => Seq(l.fromPosition, l.toPosition))
  val minCol    = positions.minBy(_.col).col
  val minRow    = positions.minBy(_.row).row
  val maxRow    = positions.maxBy(_.row).row
  var area      = 0L
  for row <- minRow to maxRow do
    val linesInRow = rowIntersectingLines(row)
    case class Crossing(col: Int, length: Int, afterwardsInside: Boolean)

    val crossings = mutable.Buffer[Crossing]()
    var index     = 0
    var inside    = false
    while index < linesInRow.length do
      if index == linesInRow.length - 1 then
        val line = linesInRow(index)
        crossings.append(
          Crossing(line.minCol, line.maxCol - line.minCol + 1, false),
        )
        index += 1
      else
        val line     = linesInRow(index)
        val nextLine = linesInRow(index + 1)

        line match
          case Up(_, _) | Down(_, _) =>
          case _                     => throw Exception("line must be vertical")

        (line, nextLine) match
          case (Up(_, _), Down(_, _)) | (Down(_, _), Up(_, _)) =>
            inside = !inside
            crossings.append(Crossing(line.minCol, 1, inside))
            index += 1
          case (Up(_, _) | Down(_, _), Left(_, _) | Right(_, _)) =>
            assert(index + 2 < linesInRow.length)
            linesInRow(index + 2) match
              case (Up(_, _) | Down(_, _)) =>
              case _ => throw Exception("next next must be vertical")

            (line, linesInRow(index + 2)) match
              case (Up(_, _), Up(_, _)) | (Down(_, _), Down(_, _)) =>
                inside = !inside
              case _ =>

            crossings.append(
              Crossing(nextLine.minCol, nextLine.lineLength, inside),
            )
            index += 3
          case _ => throw Exception("should not happen")

    inside = false
    var col = minCol
    for crossing <- crossings do
      if inside then area += (crossing.col - col)
      inside = crossing.afterwardsInside
      col = crossing.col + crossing.length
      area += crossing.length

  area
