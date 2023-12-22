package day22

// part 1
def part1(input: String) =
  safelyDisintegrableBricks(parseBricks(input)).size

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

  def intersects(other: Brick) =
    xRange.interects(other.xRange)
      && yRange.interects(other.yRange)
      && zRange.interects(other.zRange)

def settle(bricks: Iterable[Brick]) =
  def settleBrick(bricks: Iterable[Brick])(brick: Brick): Brick =
    val next = brick.downOne
    if next.minZ == 0 || bricks.exists(next.intersects) then brick
    else settleBrick(bricks)(next)

  given Ordering[Brick] = Ordering.by(b => (b.from.z, b.from.y, b.from.x))
  val sortedBricks      = bricks.toSeq.sorted
  val brickSet          = Set(sortedBricks*)
  sortedBricks.foldLeft(brickSet): (acc, next) =>
    assert(acc.contains(next))
    val withoutNext  = acc.diff(Set(next))
    val settledBrick = settleBrick(withoutNext)(next)
    // withoutNext.d
    // withoutNext(settledBrick).d
    // withoutNext.find(_.to == settledBrick.to).d
    // settledBrick.d
    assert(!withoutNext.contains(settledBrick))
    val withSettled = withoutNext ++ Set(settledBrick)
    assert(withSettled.size == withoutNext.size + 1)
    withSettled

def safelyDisintegrableBricks(bricks: Seq[Brick]) =
  val settled = settle(bricks)
  settled.filter(numberOfFallenBricksWithout(settled, _) == 0)

def numberOfFallenBricksWithout(fallen: Set[Brick], brick: Brick) =
  assert(fallen.contains(brick))
  val withoutBrick = fallen.diff(Set(brick))
  assert(withoutBrick.size == fallen.size - 1)
  val settledWithoutBrick = settle(withoutBrick)
  assert(withoutBrick.size == settledWithoutBrick.size)
  val numberOfFallenBricks = settledWithoutBrick.diff(withoutBrick).size.toLong
  assert(numberOfFallenBricks <= settledWithoutBrick.size)
  numberOfFallenBricks

extension (range: Range)
  def interects(other: Range) =
    range.contains(other.start)
      || range.contains(other.end)
      || other.contains(range.start)
      || other.contains(range.end)

// part 2
def part2(input: String) =
  val bricks = parseBricks(input)
  assert(bricks.forall(b => b.from.z <= b.to.z))
  val fallen = settle(bricks)
  assert(bricks.size == fallen.size)
  fallen.iterator.map(numberOfFallenBricksWithout(fallen, _)).sum
