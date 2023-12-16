package day16

import scala.collection.mutable.Stack

// part 1
type Cave = Vector[Vector[Char]]
case class Position(col: Int, row: Int)
def numberOfEnergizedTiles(input: String) =
  val cave = input.linesIterator.map(_.toVector).toVector
  energizedTiles(cave).size

extension (cave: Cave)
  def apply(col: Int, row: Int) = cave(row)(col)
  def isInBounds(col: Int, row: Int) =
    (0 until cave.length).contains(row)
      && (0 until cave(0).length).contains(col)

def energizedTiles(cave: Cave): Set[Position] =
  enum Direction:
    case Up
    case Left
    case Down
    case Right

  import Direction.*

  var id = 0
  def nextId =
    id += 1
    id

  case class Beam(id: Int, startCol: Int, startRow: Int, direction: Direction):
    def position = Position(startCol, startRow)
    def nextPosition =
      val (col, row) = direction match
        case Up    => (startCol, startRow - 1)
        case Left  => (startCol - 1, startRow)
        case Right => (startCol + 1, startRow)
        case Down  => (startCol, startRow + 1)
      Position(col, row)

    def positionDirection = (position, direction)

  object Beam:
    def fresh(startCol: Int, startRow: Int, direction: Direction): Beam =
      Beam(nextId, startCol, startRow, direction)

  import scala.collection.mutable
  val stack          = Stack(Beam.fresh(0, 0, Right))
  val visitedBeams   = mutable.Set.empty[(Position, Direction)]
  val energizedTiles = mutable.Set.empty[Position]

  while stack.nonEmpty do
    val beam = stack.pop()

    if !visitedBeams.contains((beam.position, beam.direction))
      && cave.isInBounds(beam.startCol, beam.startRow)
    then
      visitedBeams.add(beam.positionDirection)
      energizedTiles.add(beam.position)

      (cave(beam.startCol, beam.startRow), beam.direction) match
        case ('.', _) | ('|', Up) | ('|', Down) | ('-', Left) | ('-', Right) =>
          stack.push(
            Beam(
              beam.id,
              beam.nextPosition.col,
              beam.nextPosition.row,
              beam.direction,
            ),
          )

        case ('|', Left) | ('|', Right) =>
          val upBeam   = Beam.fresh(beam.startCol, beam.startRow - 1, Up)
          val downBeam = Beam.fresh(beam.startCol, beam.startRow + 1, Down)
          stack.push(upBeam)
          stack.push(downBeam)
        case ('-', Up) | ('-', Down) =>
          val leftBeam  = Beam.fresh(beam.startCol - 1, beam.startRow, Left)
          val rightBeam = Beam.fresh(beam.startCol + 1, beam.startRow, Right)
          stack.push(leftBeam)
          stack.push(rightBeam)

        case ('/', Up) =>
          stack.push(Beam(beam.id, beam.startCol + 1, beam.startRow, Right))
        case ('/', Left) =>
          stack.push(Beam(beam.id, beam.startCol, beam.startRow + 1, Down))
        case ('/', Down) =>
          stack.push(Beam(beam.id, beam.startCol - 1, beam.startRow, Left))
        case ('/', Right) =>
          stack.push(Beam(beam.id, beam.startCol, beam.startRow - 1, Up))

        case ('\\', Up) =>
          stack.push(Beam(beam.id, beam.startCol - 1, beam.startRow, Left))
        case ('\\', Left) =>
          stack.push(Beam(beam.id, beam.startCol, beam.startRow - 1, Up))
        case ('\\', Down) =>
          stack.push(Beam(beam.id, beam.startCol + 1, beam.startRow, Right))
        case ('\\', Right) =>
          stack.push(Beam(beam.id, beam.startCol, beam.startRow + 1, Down))

        case _ => ???

  energizedTiles.toSet
