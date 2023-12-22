package day22

case class Position(x: Int, y: Int, z: Int)
case class Brick(id: Int, from: Position, to: Position):
  val xRange = from.x to to.x
  val yRange = from.y to to.y
  val zRange = from.z to to.z
  val maxZ   = zRange.end
  val xy     = for x <- xRange; y <- yRange yield (x, y)

  def withNewZ(newZ: Int) =
    Brick(id, from.copy(z = newZ), to.copy(z = newZ + to.z - from.z))

  def intersects(other: Brick) =
    xRange.interects(other.xRange)
      && yRange.interects(other.yRange)
      && zRange.interects(other.zRange)

type BricksSnapshot = Set[Brick]
object BricksSnapshot:
  def parse(input: String): BricksSnapshot =
    Set.from:
      input.linesIterator.zipWithIndex.map:
        case (s"$fromX,$fromY,$fromZ~$toX,$toY,$toZ", row) =>
          val from = Position(fromX.toInt, fromY.toInt, fromZ.toInt)
          val to   = Position(toX.toInt, toY.toInt, toZ.toInt)
          Brick(row, from, to)

def settle(bricks: Iterable[Brick]) =
  def settleBrick(maxHeights: Map[(Int, Int), Int], brick: Brick) =
    val intersectingZ = brick.xy.map(maxHeights).max
    brick.withNewZ(intersectingZ + 1)

  val sortedBricks = bricks.toSeq.sortBy(b => (b.from.z, b.from.y, b.from.x))
  val maxHeights   = Map[(Int, Int), Int]().withDefaultValue(0)
  val (settledBricks, _) =
    sortedBricks.foldLeft((Vector.empty[Brick], maxHeights)): (acc, brick) =>
      val (settledBricks, maxHeights) = acc
      val settledBrick                = settleBrick(maxHeights, brick)
      val updatedMaxHeights = settledBrick.xy.foldLeft(maxHeights):
        (acc, key) => acc.updated(key, acc(key).max(settledBrick.maxZ))

      (settledBricks :+ settledBrick, updatedMaxHeights)

  settledBricks

def unsettledBricksWhenRemoving(settled: Set[Brick], brick: Brick) =
  val withoutBrick        = settled - brick
  val settledWithoutBrick = settle(withoutBrick).toSet
  settledWithoutBrick -- withoutBrick

extension (range: Range)
  def interects(other: Range) =
    range.contains(other.start) || range.contains(other.end)
      || other.contains(range.start) || other.contains(range.end)

extension (a: Int) def minMax(b: Int) = if a <= b then (a, b) else (b, a)

// part 1
def safelyDisintegrableBricks(input: String) =
  val settled = settle(BricksSnapshot.parse(input)).toSet
  settled.count(unsettledBricksWhenRemoving(settled, _).isEmpty)

// part 2
def sumOfSinglyUnsettledBricks(input: String) =
  val settled = settle(BricksSnapshot.parse(input)).toSet
  settled.iterator.map(unsettledBricksWhenRemoving(settled, _).size).sum
