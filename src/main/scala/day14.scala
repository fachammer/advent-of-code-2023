package day14

import debug.*

type Platform = Vector[Vector[Char]]
type Position = (Int, Int)

// part 1
def totalLoadWhenTiltingNorth(input: String) =
  parsePlatform(input).tiltNorth.totalLoad

def parsePlatform(input: String): Platform =
  input.linesIterator.map(_.toVector).toVector

extension (platform: Platform)
  def northestPositionAfterNorthTilt(col: Int, row: Int): Position =
    val r = (0 to row - 1).reverse.takeWhile(platform(_)(col) == '.').lastOption
      .getOrElse(row)
    (col, r)

  def updated(col: Int, row: Int, value: Char): Platform =
    platform.updated(row, platform(row).updated(col, value))

  def moveRockNorth(rockCol: Int, rockRow: Int): Platform =
    val (newCol, newRow) = northestPositionAfterNorthTilt(rockCol, rockRow)
    platform.updated(rockCol, rockRow, '.').updated(newCol, newRow, 'O')

  def tiltNorth: Platform =
    val movableRockPositions =
      for
        (line, row) <- platform.zipWithIndex
        case ('O', col) <- line.zipWithIndex
      yield (col, row)

    movableRockPositions.foldLeft(platform) { case (p, (col, row)) =>
      p.moveRockNorth(col, row)
    }
    // val fixedRockPositions =
    //   for
    //     (line, row) <- platform.zipWithIndex
    //     case ('#', col) <- line.zipWithIndex
    //   yield (col, row)

  def totalLoad: Int =
    platform.zipWithIndex
      .map((line, row) => (platform.length - row) * line.count(_ == 'O')).sum
