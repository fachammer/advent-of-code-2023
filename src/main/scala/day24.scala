package day24

import breeze.linalg.*
import scala.util.Try

// part 1
case class HailStone(position: Counter[Int, Long], velocity: Counter[Int, Long])

def hailStoneIntersections(min: Long, max: Long)(input: String) =
  val hailStones = parseHailStones(input)

  val intersections = hailStones.combinations(2).flatMap:
    case Seq(left, right) => intersection(left, right)

  intersections.count: i =>
    i.timeFromLeft >= 0 && i.timeFromRight >= 0
      && min <= i.position(0) && i.position(0) <= max
      && min <= i.position(1) && i.position(1) <= max

case class HailStoneIntersection(
    timeFromLeft: Double,
    timeFromRight: Double,
    position: Counter[Int, Double],
)

def intersection(left: HailStone, right: HailStone) =
  val t = AffineExpression.variable("t")
  val s = AffineExpression.variable("s")
  val linearSystem =
    t * left.velocity + left.position === s * right.velocity + right.position

  val result = linearSystem.solve
  result.toOption.map: solution =>
    val timeFromLeft  = solution("t")
    val timeFromRight = solution("s")
    val leftPosition  = convert(left.position, Double)
    val leftVelocity  = convert(left.velocity, Double)
    val position      = leftPosition + leftVelocity * timeFromLeft
    HailStoneIntersection(timeFromLeft, timeFromRight, position)

def parseHailStones(input: String) =
  input.linesIterator.map:
    case s"$px, $py, $_ @ $vx, $vy, $_" =>
      HailStone(
        Counter((0, px.strip.toLong), (1, py.strip.toLong)),
        Counter((0, vx.strip.toLong), (1, vy.strip.toLong)),
      )
  .toSeq

def parseHailStones3d(input: String) =
  input.linesIterator.map:
    case s"$px, $py, $pz @ $vx, $vy, $vz" =>
      (
        Counter(
          (0, px.strip.toLong),
          (1, py.strip.toLong),
          (2, pz.strip.toLong),
        ),
        Counter(
          (0, vx.strip.toLong),
          (1, vy.strip.toLong),
          (2, vz.strip.toLong),
        ),
      )
  .toSeq

extension (vector: Counter[Int, Long])
  def length                    = vector.size
  def *(that: AffineExpression) = that * vector
  def +(that: AffineExpression) = that + vector
  def -(that: AffineExpression) = -that + vector

  def cross(other: Counter[Int, Long]) =
    assert(vector.length == 3)
    assert(other.length == 3)
    crossProductMatrix(vector) * other

  def cross(other: AffineExpression) =
    assert(vector.length == 3)
    assert(other.rows == 3)

    AffineExpression(crossProductMatrix(vector) * other.matrix)

  def concat(that: Counter[Int, Long]) =
    val thisEntries = vector.activeIterator.toSeq
    val maxRow      = thisEntries.map(_._1).maxOption.getOrElse(-1)
    val thatEntries = that.activeIterator.map:
      case (r, v) => (r + maxRow + 1, v)

    val entries = thisEntries.concat(thatEntries).toSeq
    Counter(entries*)

extension [T](matrix: Counter2[Int, T, Long])
  def concat(that: Counter2[Int, T, Long]) =
    val thisEntries = matrix.activeIterator.map:
      case ((r, c), v) => (r, c, v)
    .toSeq
    val maxRow = thisEntries.map(_._1).maxOption.getOrElse(-1)
    val thatEntries = that.activeIterator.map:
      case ((r, c), v) => (r + maxRow + 1, c, v)
    .toSeq

    val entries = thisEntries.concat(thatEntries).toSeq
    Counter2(entries*)

def crossProductMatrix(v: Counter[Int, Long]) =
  assert(v.activeSize == 3)
  Counter2[Int, Int, Long](
    (0, 1, -v(2)),
    (0, 2, v(1)),
    (1, 0, v(2)),
    (1, 2, -v(0)),
    (2, 0, -v(1)),
    (2, 1, v(0)),
  )

case class AffineExpression(
    matrix: Counter2[Int, String, Long],
):
  def rows = matrix.activeKeysIterator.map(_._1).distinct.size
  def cols = matrix.activeKeysIterator.map(_._2).distinct.size
  def +(that: AffineExpression): AffineExpression =
    AffineExpression(matrix + that.matrix)
  def +(that: Counter[Int, Long]): AffineExpression =
    this + AffineExpression.constant(that)

  def unary_-                                     = AffineExpression(-matrix)
  def -(that: AffineExpression): AffineExpression = this + (-that)
  def -(that: Counter[Int, Long]): AffineExpression =
    this - AffineExpression.constant(that)

  def *(that: Counter[Int, Long]): AffineExpression =
    AffineExpression(Counter2(matrix.activeIterator.flatMap:
      case ((row, name), value) =>
        that.activeIterator.map((i, v) =>
          (row * that.size + i, name, value * v),
        )
    .toSeq*))

  def cross(that: Counter[Int, Long]): AffineExpression =
    assert(matrix.activeKeysIterator.map(_._1).distinct.size == 3)
    assert(that.activeSize == 3)

    AffineExpression(crossProductMatrix(-that) * matrix)

  def ===(that: AffineExpression): LinearSystem =
    val expr          = this - that
    val inhomogeneity = -expr.matrix(::, "1")
    expr.matrix.data.mapValuesInPlace { (_, v) =>
      v.data.remove("1")
      v
    }
    LinearSystem(expr.matrix, inhomogeneity)

  def ++(that: AffineExpression) = AffineExpression(matrix.concat(that.matrix))

object AffineExpression:
  def zero                   = AffineExpression(Counter2())
  def variable(name: String) = AffineExpression(Counter2((0, name, 1L)))
  def constant(vector: Counter[Int, Long]) =
    AffineExpression(
      Counter2(
        vector.iterator.map((row, value) => (row, "1", value)).toSeq*,
      ),
    )

case class LinearSystem(
    matrix: Counter2[Int, String, Long],
    inhomogeneity: Counter[Int, Long],
):
  def solve: Try[Counter[String, Double]] =
    val rows = matrix.keySet.map(_._1).toArray.sorted
    val cols = matrix.keySet.map(_._2).toArray.sorted
    val colNamesToIndices = Counter2(
      cols.zipWithIndex.map((c, i) => (c, i, 1L)),
    )
    val rowsToIndices = Counter2(
      rows.zipWithIndex.map((r, i) => (i, r, 1L)),
    )
    val mat = rowsToIndices * matrix * colNamesToIndices

    val A =
      DenseMatrix.tabulate(rows.size, cols.size)((r, c) => mat(r, c).toDouble)
    val b =
      DenseVector.tabulate(inhomogeneity.activeSize)(inhomogeneity(_).toDouble)

    Try(A \ b).map: solution =>
      Counter(solution.activeIterator.map((i, v) => (cols(i), v)).toSeq*)

  def ++(that: LinearSystem) = LinearSystem(
    matrix.concat(that.matrix),
    inhomogeneity.concat(that.inhomogeneity),
  )

object LinearSystem:
  def empty = LinearSystem(Counter2(), Counter())

// part 2
def oneShotHitAllPositionSum(input: String) =
  val hailStones = parseHailStones3d(input).take(3)

  import AffineExpression.*
  import scala.language.implicitConversions
  val px = variable("px")
  val py = variable("py")
  val pz = variable("pz")
  val p  = px ++ py ++ pz
  val vx = variable("vx")
  val vy = variable("vy")
  val vz = variable("vz")
  val v  = vx ++ vy ++ vz

  val linearSystem = hailStones.combinations(2).take(2).map:
    case Seq((lp, lv), (rp, rv)) =>
      -p.cross(lv) - lp.cross(v) + lp.cross(lv) ===
        -p.cross(rv) - rp.cross(v) + rp.cross(rv)
  .foldLeft(LinearSystem.empty)(_ ++ _)

  val result = linearSystem.solve.get
  result("px").round + result("py").round + result("pz").round
