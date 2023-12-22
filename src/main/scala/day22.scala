package day22

import debug.*
import scala.collection.SortedSetOps.*

// part 1
def part1(input: String) =
  disintegrableBricks(parseBricks(input)).size

def parseBricks(input: String): Seq[Brick] =
  input.linesIterator.zipWithIndex.map:
    case (s"$startX,$startY,$startZ~$endX,$endY,$endZ", row) =>
      Brick(
        row.toString,
        Position(startX.toInt, startY.toInt, startZ.toInt),
        Position(endX.toInt, endY.toInt, endZ.toInt),
      )
  .toSeq
case class Position(x: Int, y: Int, z: Int)
case class Brick(id: String, from: Position, to: Position):
  def downOne = Brick(id, from.copy(z = from.z - 1), to.copy(z = to.z - 1))
  def minX    = from.x min to.x
  def maxX    = from.x max to.x
  def minY    = from.y min to.y
  def maxY    = from.y max to.y
  def minZ    = from.z min to.z
  def maxZ    = from.z max to.z

  def xRange = minX to maxX
  def yRange = minY to maxY
  def zRange = minZ to maxZ

  def contains(position: Position) =
    xRange.contains(position.x)
      && yRange.contains(position.y)
      && zRange.contains(position.z)

  def intersects(other: Brick) =
    xRange.interects(other.xRange)
      && yRange.interects(other.yRange)
      && zRange.interects(other.zRange)

  def occupiedPositions =
    for
      x <- xRange
      y <- yRange
      z <- zRange
    yield Position(x, y, z)

def fallenBricks(bricks: Iterable[Brick]) =

  def fallDownIn(bricks: Iterable[Brick], brick: Brick): Brick =
    var br = brick
    while br.downOne.minZ > 0 && !bricks.exists(br.downOne.intersects) do
      br = br.downOne

    br

  given Ordering[Brick] = Ordering.by(brick => brick.minZ)
  val sortedBricks      = bricks.toSeq.sorted
  val brickSet          = bricks.toSet
  sortedBricks.foldLeft(brickSet): (acc, next) =>
    assert(acc.contains(next))
    val withoutNext = acc.diff(Set(next))
    val fallenBrick = fallDownIn(withoutNext, next)
    withoutNext ++ Set(fallenBrick)

def disintegrableBricks(bricks: Seq[Brick]) =
  val fallen = fallenBricks(bricks)
  fallen.filter(numberOfFallenBricksWithout(fallen, _) == 0)

def numberOfFallenBricksWithout(fallen: Set[Brick], brick: Brick) =
  val withoutBrick       = fallen.diff(Set(brick))
  val fallenWithoutBrick = fallenBricks(withoutBrick)
  assert(withoutBrick.size == fallenWithoutBrick.size)
  fallenWithoutBrick.diff(withoutBrick).size.toLong.d

extension (range: Range)
  def interects(other: Range) =
    range.contains(other.start)
      || range.contains(other.end)
      || other.contains(range.start)
      || other.contains(range.end)

