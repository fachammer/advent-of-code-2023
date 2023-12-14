package day14

type Platform = Vector[Vector[Char]]
type Position = (Int, Int)
enum Direction:
  case North
  case West
  case South
  case East

import Direction.*

// part 1
def totalLoadWhenTiltingNorth(input: String) =
  parsePlatform(input).tiltInDirection(North).totalLoad

def parsePlatform(input: String) = input.linesIterator.map(_.toVector).toVector

extension (platform: Platform)
  def updated(col: Int, row: Int, value: Char): Platform =
    platform.updated(row, platform(row).updated(col, value))

  def totalLoad: Int =
    platform.zipWithIndex
      .map((line, row) => (platform.length - row) * line.count(_ == 'O'))
      .sum

// part 2
def totalLoadAfterCycles(cycles: Int)(input: String): Int =
  parsePlatform(input).spinCycle(cycles).totalLoad

extension (direction: Direction)
  def positionOrdering: Ordering[Position] = direction match
    case North => Ordering.by((_, row) => row)
    case West  => Ordering.by((col, _) => col)
    case South => Ordering.by[Position, Int]((_, row) => row).reverse
    case East  => Ordering.by[Position, Int]((col, _) => col).reverse

extension (platform: Platform)
  def moveRockInDirection(
      col: Int,
      row: Int,
      direction: Direction,
  ): Platform =
    val path = direction match
      case North => (row - 1 to 0 by -1).map(r => (col, r))
      case West  => (col - 1 to 0 by -1).map(c => (c, row))
      case South => (row + 1 until platform.length).map(r => (col, r))
      case East  => (col + 1 until platform(0).length).map(c => (c, row))

    val (newCol, newRow) = path
      .takeWhile((c, r) => platform(r)(c) == '.')
      .lastOption
      .getOrElse((col, row))

    platform
      .updated(col, row, '.')
      .updated(newCol, newRow, 'O')

  def tiltInDirection(direction: Direction): Platform =
    val movableRockPositions = for
      (line, row) <- platform.zipWithIndex
      case ('O', col) <- line.zipWithIndex
    yield (col, row)

    movableRockPositions
      .sorted(using direction.positionOrdering)
      .foldLeft(platform) { case (p, (col, row)) =>
        p.moveRockInDirection(col, row, direction)
      }

  def spin = platform
    .tiltInDirection(North)
    .tiltInDirection(West)
    .tiltInDirection(South)
    .tiltInDirection(East)

  def spinCycle(cycles: Int): Platform =
    var currentPlatform = platform
    import scala.collection.mutable
    val encounteredPlatforms = mutable.Buffer(currentPlatform)
    while true do
      currentPlatform = currentPlatform.spin
      if encounteredPlatforms.contains(currentPlatform) then
        val platforms   = encounteredPlatforms.toSeq
        val phase       = platforms.indexOf(currentPlatform)
        val cycleLength = platforms.length - phase
        return platforms(phase + ((cycles - phase) % cycleLength))

      encounteredPlatforms += currentPlatform

    ???
