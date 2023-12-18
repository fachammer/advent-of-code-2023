package day18

import scala.collection.mutable

// part 1
def part1(input: String) =
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
  type Color = String
  case class Grid(width: Int, height: Int):
    val grid: Array[Array[Color]] = Array.fill(height, width)(".")
    val rows                      = 0 until grid.length
    val cols                      = 0 until grid(0).length
    def update(position: Position, updated: Color) =
      val pos = position
      grid(pos.row)(pos.col) = updated
    def apply(position: Position) =
      val pos = position
      grid(pos.row)(pos.col)
    def contains(position: Position) =
      cols.contains(position.col) && rows.contains(position.row)
    def mkString =
      val builder = mutable.StringBuilder()
      for row <- rows do
        for col <- cols do builder.addAll(grid(row)(col))
        builder.addOne('\n')
      builder.mkString

  var position  = Position(0, 0)
  val positions = mutable.LinkedHashSet(Position(0, 0))
  for case s"$step $amount (#$color)" <- input.linesIterator do
    val direction = step match
      case "R" => Direction(1, 0)
      case "D" => Direction(0, 1)
      case "L" => Direction(-1, 0)
      case "U" => Direction(0, -1)
    (1 to amount.toInt).foreach: _ =>
      position += direction
      positions.add(position)

  val minCol          = positions.minBy(_.col).col
  val maxCol          = positions.maxBy(_.col).col
  val minRow          = positions.minBy(_.row).row
  val maxRow          = positions.maxBy(_.row).row
  val origin          = Direction(minCol, minRow)
  val grid            = Grid(maxCol - minCol + 1, maxRow - minRow + 1)
  val offsetPositions = positions.map(_ - origin)
  offsetPositions.foreach: pos =>
    grid(pos) = "#"

  val insidePosition = offsetPositions.sliding(2).flatMap { s =>
    val (a, b)         = (s.head, s.last)
    val possibleInside = a + (b - a).rightNormal // assuming right-wound path
    Option.when(s.head.col == s.last.col && grid(possibleInside) == ".")(
      possibleInside,
    )
  }.next

  def floodFillFrom(point: Position) =
    val queue = mutable.Queue(point)
    while queue.nonEmpty do
      val pos = queue.dequeue()
      if grid.contains(pos) && grid(pos) == "." then
        grid(pos) = "#"
        queue.enqueueAll(pos.neighborhood4)

  floodFillFrom(insidePosition)

  grid.grid.flatten.count(_ == "#")
