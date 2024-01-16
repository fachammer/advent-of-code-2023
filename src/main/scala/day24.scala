package day24

import debug.*
import math.Numeric.Implicits.infixNumericOps
import breeze.linalg.DenseMatrix
import breeze.linalg.support.LiteralRow
import breeze.linalg.DenseVector

// part 1
case class Vec2[T: Numeric](x: T, y: T):
  import math.Numeric.Implicits.infixNumericOps
  def toDouble = Vec2(x.toDouble, y.toDouble)
  def +(other: Vec2[T]) =
    Vec2(x + other.x, y + other.y)
  def dotProduct(other: Vec2[T]) =
    x * other.x + y * other.y
  def normSquared = dotProduct(this)
  def isParallelTo(other: Vec2[T]) =
    dotProduct(other).abs == normSquared

  def lineParameter(point: Vec2[T], direction: Vec2[T]): Option[Double] =
    if direction.x == 0 then
      if x == point.x then
        if direction.y == 0 then
          if y == point.y then Some(0.0)
          else None
        else Some((y - point.y).toDouble / direction.y.toDouble)
      else None
    else if direction.y == 0 then
      if y == point.y then Some((x - point.x).toDouble / direction.x.toDouble)
      else None
    else if direction.y * (x - point.x) == direction.x * (y - point.y) then
      Some((x - point.x).toDouble / direction.x.toDouble)
    else None

extension [T: Numeric](number: T)
  def scalarMultiply(other: Vec2[T]) = Vec2(number * other.x, number * other.y)

case class HailStone(position: Vec2[Long], velocity: Vec2[Long])

def hailStoneIntersections(min: Long, max: Long)(input: String) =
  val hailStones = parseHailStones(input)

  val intersections = hailStones.combinations(2).map:
    case Seq(left, right) => intersection(left, right)

  intersections.flatten.toSeq.count(i =>
    i.timeFromLeft >= 0 && i.timeFromRight >= 0
      && min <= i.position.x && i.position.x <= max &&
      min <= i.position.y && i.position.y <= max,
  )

case class HailStoneIntersection(
    left: HailStone,
    right: HailStone,
    timeFromLeft: Double,
    timeFromRight: Double,
    position: Vec2[Double],
)

def intersection(
    left: HailStone,
    right: HailStone,
): Option[HailStoneIntersection] =
  val determinant =
    left.velocity.x * (-right.velocity.y) - left.velocity.y * (-right.velocity.x)
  if determinant == 0 then
    left.position.lineParameter(right.position, right.velocity) match
      case None => None
      case Some(timeB) =>
        timeB.d
        right.position.lineParameter(left.position, left.velocity) match
          case None => ???
          case Some(timeA) =>
            Some(
              HailStoneIntersection(
                left,
                right,
                timeA,
                timeB,
                right.position.toDouble + timeB.scalarMultiply(
                  right.velocity.toDouble,
                ),
              ),
            )
  else
    val timeFromLeft =
      (-right.velocity.y * (right.position.x - left.position.x) + right.velocity.x * (right.position.y - left.position.y)).toDouble / determinant
    val timeFromRight =
      (-left.velocity.y * (right.position.x - left.position.x) + left.velocity.x * (right.position.y - left.position.y)).toDouble / determinant

    Some(
      HailStoneIntersection(
        left,
        right,
        timeFromLeft,
        timeFromRight,
        left.position.toDouble + timeFromLeft.scalarMultiply(
          left.velocity.toDouble,
        ),
      ),
    )

def parseHailStones(input: String) =
  input.linesIterator.map:
    case s"$px, $py, $_ @ $vx, $vy, $_" =>
      HailStone(
        Vec2(px.strip.toLong, py.strip.toLong),
        Vec2(vx.strip.toLong, vy.strip.toLong),
      )
  .toSeq

def parseHailStones3d(input: String) =
  input.linesIterator.map:
    case s"$px, $py, $pz @ $vx, $vy, $vz" =>
      (
        Vec3(px.strip.toLong, py.strip.toLong, pz.strip.toLong),
        Vec3(vx.strip.toLong, vy.strip.toLong, vz.strip.toLong),
      )
  .toSeq

case class Vec3(x: Long, y: Long, z: Long):
  def +(other: Vec3)   = Vec3(x + other.x, y + other.y, z + other.z)
  def -(other: Vec3)   = Vec3(x - other.x, y - other.y, z - other.z)
  def dot(other: Vec3) = x * other.x + y * other.y + z * other.z
  def cross(other: Vec3) = Vec3(
    y * other.z - z * other.y,
    z * other.x - x * other.z,
    x * other.y - y * other.x,
  )
  def normSquared = dot(this)
  def toVector    = Vector(x, y, z)

extension (t: Long) def *(vec: Vec3) = Vec3(t * vec.x, t * vec.y, t * vec.z)

def oneShotHitAllPositionSum(input: String) =
  val hailStones = parseHailStones3d(input)

  val rhs = DenseVector[Long]:
    hailStones.combinations(2).take(2).flatMap:
      case Seq((lp, lv), (rp, rv)) => (lp.cross(lv) - rp.cross(rv)).toVector
    .toArray

  val rows = hailStones.combinations(2).take(2).flatMap {
    case Seq((lp, lv), (rp, rv)) =>
      Seq(
        Seq(0, lv.z - rv.z, -(lv.y - rv.y), 0, -(lp.z - rp.z), lp.y - rp.y),
        Seq(-(lv.z - rv.z), 0, lv.x - rv.x, lp.z - rp.z, 0, -(lp.x - rp.x)),
        Seq(lv.y - rv.y, -(lv.x - rv.x), 0, -(lp.y - rp.y), lp.x - rp.x, 0),
      )
  }.map(_.map(_.toDouble)).toSeq

  val matrix = DenseMatrix(rows*)
  val result = matrix \ (rhs.map(_.toDouble))
  breeze.linalg.sum(result(0 until 3)).round
