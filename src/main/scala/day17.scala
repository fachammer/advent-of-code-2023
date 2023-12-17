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
    )
      .filter(isInBounds.tupled)
object Grid:
  def parse(input: String): Grid =
    Grid(input.linesIterator.map(_.map(_.asDigit).toVector).toVector)

def minimumHeatLoss(input: String) =
  val grid = Grid.parse(input)
  type Position = (Int, Int)
  case class Node(last4: Seq[Position])
  val initialNode = Node(Seq((0, 0)))

  def areOnStraightLine(positions: Position*) =
    positions.forall { case (col, _) => col == positions.head._1 }
      || positions.forall { case (_, row) => row == positions.head._2 }

  extension (node: Node)
    def adjacentNodes: Seq[(Node, Int)] =
      val adjacent = node.last4 match
        case current :: Nil => grid.neighborhood4.tupled(current)
        case all @ (current :: previous :: _) =>
          grid.neighborhood4.tupled(current).filterNot(_ == previous).filter(
            p => all.size < 4 || !areOnStraightLine((p +: all)*),
          )

      adjacent.map(p => (Node((p +: node.last4).take(4)), grid.at.tupled(p)))

  import scala.math.Ordering.Implicits.*
  given Ordering[(Node, Option[Node], Int)] = Ordering.by {
    case (_, _, weight) =>
      -weight
  }
  import scala.collection.mutable
  val nodesToProcess =
    mutable.PriorityQueue((initialNode, Option.empty[Node], 0))
  val visitedNodes = mutable.HashMap.empty[Node, (Option[Node], Int)]
  while nodesToProcess.nonEmpty do
    val (node, predecessor, totalWeight) = nodesToProcess.dequeue()
    if !visitedNodes.contains(node) || visitedNodes(node)._2 > totalWeight then
      visitedNodes.put(node, (predecessor, totalWeight))
      val nextNodes = node.adjacentNodes.map { case (n, weight) =>
        (n, Some(node), totalWeight + weight)
      }
      nodesToProcess.enqueue(nextNodes*)

  visitedNodes.filter { case (node, _) =>
    node.last4.head == (grid.cols.last, grid.rows.last)
  }.map { case (_, (_, pathLength)) => pathLength }.min

// part 2
def minimumHeatLossWithUltraCrucible(input: String) =
  val grid = Grid.parse(input)
  type Position = (Int, Int)
  case class Node(last2: Seq[Position])
  val initialNode = Node(Seq((0, 0)))

  extension (pos: Position)
    def col                  = pos._1
    def row                  = pos._2
    def -(other: Position)   = (pos._1 - other._1, pos._2 - other._2)
    def dot(other: Position) = pos._1 * other._1 + pos._2 * other._2

  extension (node: Node)
    def adjacentNodes: Seq[(Node, Int)] =
      val (col, row) = node.last2.head
      val possibleNeighbors = for
        offset   <- 4 to 10
        neighbor <- grid.neighborhood4Offset(col, row, offset)
      yield neighbor
      val adjacent = node.last2 match
        case (col, row) :: Nil => possibleNeighbors
        case all @ (current :: previous :: _) =>
          val direction = current - previous
          def isInOrthogonalDirection(p: Position) =
            direction.dot(p - current) == 0

          possibleNeighbors.filter(isInOrthogonalDirection)

      def heatLossStraightLine(from: Position, toPos: Position) =
        if from.row == toPos.row then
          val offset = if from.col < toPos.col then 1 else -1
          ((from.col + offset) to toPos.col by offset).map(
            grid.at(_, from.row),
          ).sum
        else if from.col == toPos.col then
          val offset = if from.row < toPos.row then 1 else -1
          ((from.row + offset) to toPos.row by offset).map(
            grid.at(from.col, _),
          ).sum
        else ???

      adjacent.map(p =>
        (
          Node(Seq(p, node.last2.head)),
          heatLossStraightLine(node.last2.head, p),
        ),
      )

  import scala.math.Ordering.Implicits.*
  given Ordering[(Node, Option[Node], Int)] = Ordering.by {
    case (_, _, weight) =>
      -weight
  }
  import scala.collection.mutable
  val nodesToProcess =
    mutable.PriorityQueue((initialNode, Option.empty[Node], 0))
  val visitedNodes = mutable.HashMap.empty[Node, (Option[Node], Int)]
  while nodesToProcess.nonEmpty do
    val (node, predecessor, totalWeight) = nodesToProcess.dequeue()
    if !visitedNodes.contains(node) || visitedNodes(node)._2 > totalWeight then
      visitedNodes.put(node, (predecessor, totalWeight))
      val nextNodes = node.adjacentNodes.map { case (n, weight) =>
        (n, Some(node), totalWeight + weight)
      }
      nodesToProcess.enqueue(nextNodes*)

  visitedNodes.filter { case (node, _) =>
    node.last2.head == (grid.cols.last, grid.rows.last)
  }.map { case (_, (_, pathLength)) => pathLength }.min
