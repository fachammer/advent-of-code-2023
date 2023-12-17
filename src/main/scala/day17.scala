package day17

// part 1
case class Grid(grid: Vector[Vector[Int]]):
  def at(col: Int, row: Int)         = grid(row)(col)
  def height                         = grid.length
  def width                          = grid(0).length
  def cols                           = 0 until width
  def rows                           = 0 until height
  def isInBounds(col: Int, row: Int) = cols.contains(col) && rows.contains(row)
  def neighborhood4(col: Int, row: Int) = neighborhood4Offset(col, row, 1)

  def neighborhood4Offset(col: Int, row: Int, offset: Int) =
    Seq(
      (col + offset, row),
      (col, row + offset),
      (col - offset, row),
      (col, row - offset),
    ).filter(isInBounds.tupled)
object Grid:
  def parse(input: String): Grid =
    Grid(input.linesIterator.map(_.map(_.asDigit).toVector).toVector)

type Position = (Int, Int)
extension (pos: Position)
  def col                       = pos._1
  def row                       = pos._2
  def -(other: Position)        = (pos._1 - other._1, pos._2 - other._2)
  def dot(other: Position): Int = pos._1 * other._1 + pos._2 * other._2
  def isOrthogonalTo(other: Position): Boolean = dot(other) == 0
  def manhattanLength: Int                     = col.abs + row.abs

case class Node(last2: Seq[Position]):
  def position = last2.head

def minimumHeatLossWithStandardCrucible(input: String) =
  minimumHeatLoss(input, 1 to 3)

// part 2
def minimumHeatLossWithUltraCrucible(input: String) =
  minimumHeatLoss(input, 4 to 10)

def minimumHeatLoss(input: String, movementRange: Range) =
  val grid = Grid.parse(input)
  dijkstra[Node](adjacentNodes(grid, movementRange))(Node(Seq((0, 0)))).flatMap:
    (k, v) => Option.when(k.position == (grid.cols.last, grid.rows.last))(v)
  .min

def adjacentNodes(grid: Grid, offsets: Iterable[Int])(node: Node) =
  val current @ (col, row) = node.position
  val possibleNeighbors = for
    offset   <- offsets.toSet
    neighbor <- grid.neighborhood4Offset(col, row, offset)
  yield neighbor

  val adjacent = node.last2 match
    case (col, row) :: Nil => possibleNeighbors
    case (current :: previous :: Nil) =>
      possibleNeighbors.filter: neighbor =>
        (neighbor - current).isOrthogonalTo(current - previous)

  def heatLossInStraightLine(from: Position, toPos: Position) =
    val difference = toPos - from
    val colOffset  = if difference.col.sign == 0 then 1 else difference.col.sign
    val rowOffset  = if difference.row.sign == 0 then 1 else difference.row.sign
    val colRange   = from.col to toPos.col by colOffset
    val rowRange   = from.row to toPos.row by rowOffset
    val heatLosses = for col <- colRange; row <- rowRange
    yield grid.at(col, row)
    heatLosses.sum - grid.at.tupled(from)

  adjacent.map: neighbor =>
    (Node(Seq(neighbor, current)), heatLossInStraightLine(current, neighbor))

def dijkstra[T](adjacent: T => Iterable[(T, Int)])(start: T) =
  import scala.collection.mutable
  given Ordering[(T, Int)] = Ordering.by((_, length) => -length)
  val nodesToProcess       = mutable.PriorityQueue((start, 0))
  val visitedNodes         = mutable.HashMap.empty[T, Int]
  while nodesToProcess.nonEmpty do
    val (node, totalWeight) = nodesToProcess.dequeue()
    if !visitedNodes.contains(node) || visitedNodes(node) > totalWeight then
      visitedNodes.put(node, totalWeight)
      val nextNodes =
        adjacent(node).map: (n, weight) =>
          (n, totalWeight + weight)
      nextNodes.foreach: n =>
        nodesToProcess.enqueue(n)

  visitedNodes.toMap
