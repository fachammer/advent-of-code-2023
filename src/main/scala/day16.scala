package day16

// part 1
case class Cave(cave: Vector[Vector[Char]]):
  def at(position: Position) = cave(position.row)(position.col)
  def cols                   = 0 until cave(0).length
  def rows                   = 0 until cave.length
  def isInBounds(position: Position) =
    rows.contains(position.row) && cols.contains(position.col)

object Cave:
  def parse(input: String) = Cave(input.linesIterator.map(_.toVector).toVector)

case class Position(col: Int, row: Int):
  def +(d: Direction) = Position(col + d.col, row + d.row)

case class Direction(col: Int, row: Int):
  def opposite      = Direction(-col, -row)
  def flipUpRight   = Direction(-row, -col)
  def flipDownRight = flipUpRight.opposite
  def orthogonals   = Seq(flipUpRight, flipDownRight)
object Direction:
  val Up    = Direction(0, -1)
  val Left  = Direction(-1, 0)
  val Down  = Direction(0, 1)
  val Right = Direction(1, 0)
import Direction.*

case class Beam(position: Position, direction: Direction):
  def directTowards(dir: Direction) = Beam(position + dir, dir)

  def step          = directTowards(direction)
  def flipUpRight   = directTowards(direction.flipUpRight)
  def flipDownRight = directTowards(direction.flipDownRight)
  def split         = direction.orthogonals.map(directTowards)
  def isHorizontal  = direction.row == 0
  def isVertical    = direction.col == 0

def numberOfEnergizedTiles(input: String) =
  energizedTiles(Cave.parse(input), Beam(Position(0, 0), Right)).size

def energizedTiles(cave: Cave, initialBeam: Beam): Set[Position] =
  extension (beam: Beam)
    def nextBeams = cave.at(beam.position) match
      case '/'                      => Seq(beam.flipUpRight)
      case '\\'                     => Seq(beam.flipDownRight)
      case '|' if beam.isHorizontal => beam.split
      case '-' if beam.isVertical   => beam.split
      case _                        => Seq(beam.step)

  val beamsToProcess = scala.collection.mutable.Stack(initialBeam)
  val visitedBeams   = scala.collection.mutable.Set.empty[Beam]
  while beamsToProcess.nonEmpty do
    val beam = beamsToProcess.pop()
    if cave.isInBounds(beam.position) && !visitedBeams.contains(beam) then
      visitedBeams.add(beam)
      beamsToProcess.pushAll(beam.nextBeams)

  visitedBeams.map(_.position).toSet

// part 2
def maxNumberOfEnergizedTiles(input: String) =
  val cave = Cave.parse(input)
  val boundaryBeams = cave.rows.map(r => Beam(Position(0, r), Right))
    ++ cave.rows.map(r => Beam(Position(cave.cols.last, r), Left))
    ++ cave.cols.map(c => Beam(Position(c, cave.rows.last), Up))
    ++ cave.cols.map(c => Beam(Position(c, 0), Down))

  boundaryBeams.map(energizedTiles(cave, _).size).max
