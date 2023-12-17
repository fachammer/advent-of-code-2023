package day17

// part 1
def minimumHeatLossWithStandardCrucible(input: String) =
  minimumHeatLoss(input, 1, 3)

// part 2
def minimumHeatLossWithUltraCrucible(input: String) =
  minimumHeatLoss(input, 4, 10)

case class Grid(grid: Vector[Vector[Int]]):
  def at(position: Position) = grid(position.row)(position.col)
  def height                 = grid.length
  def width                  = grid(0).length
  def cols                   = 0 until width
  def rows                   = 0 until height
  def isInBounds(position: Position) =
    cols.contains(position.col) && rows.contains(position.row)

object Grid:
  def parse(input: String): Grid =
    Grid(input.linesIterator.map(_.map(_.asDigit).toVector).toVector)

case class Direction(col: Int, row: Int):
  def orthogonals = Seq(Direction(-row, col), Direction(row, -col))
case class Position(col: Int, row: Int):
  def +(other: Direction) = Position(col + other.col, row + other.row)
case class Node(position: Position, direction: Option[Direction], length: Int)

def minimumHeatLoss(input: String, minSteps: Int, maxStepsUntilTurn: Int) =
  val grid      = Grid.parse(input)
  val adjacency = adjacent(grid, minSteps, maxStepsUntilTurn)
  def isDestination(node: Node) =
    node.length >= minSteps
      && node.position == Position(grid.cols.last, grid.rows.last)

  dijkstra(adjacency, isDestination, Node(Position(0, 0), None, 0)).get

def adjacent(grid: Grid, minSteps: Int, maxStepsUntilTurn: Int)(node: Node) =
  val adjacents = scala.collection.mutable.Buffer.empty[Node]

  if node.direction.isEmpty then
    adjacents.appendAll:
      Seq(Direction(0, 1), Direction(1, 0)).map: direction =>
        Node(node.position + direction, Some(direction), 1)
  else
    if node.length < maxStepsUntilTurn then
      adjacents.append:
        Node(
          node.position + node.direction.get,
          node.direction,
          node.length + 1,
        )

    if node.length >= minSteps then
      node.direction.get.orthogonals.foreach: direction =>
        val nextPosition = node.position + direction
        adjacents.append(Node(nextPosition, Some(direction), 1))

  adjacents
    .filter(node => grid.isInBounds(node.position))
    .map(node => (node, grid.at(node.position)))

def dijkstra[T](
    adjacent: T => Iterable[(T, Int)],
    isDestination: T => Boolean,
    start: T,
): Option[Int] =
  import scala.collection.mutable
  given Ordering[(T, Int)] = Ordering.by((_, length) => -length)
  val nodesToProcess       = mutable.PriorityQueue((start, 0))
  val visitedNodes         = mutable.HashMap.empty[T, Int]
  while nodesToProcess.nonEmpty do
    val (node, totalLength) = nodesToProcess.dequeue()
    if isDestination(node) then return Some(totalLength)
    if !visitedNodes.contains(node) || visitedNodes(node) > totalLength then
      visitedNodes.put(node, totalLength)
      for (next, weight) <- adjacent(node)
      do nodesToProcess.enqueue((next, totalLength + weight))

  None
