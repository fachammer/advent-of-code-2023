package day17

// part 1
case class Grid(grid: Vector[Vector[Int]]):
  def at(col: Int, row: Int)         = grid(row)(col)
  def height                         = grid.length
  def width                          = grid(0).length
  def cols                           = 0 until width
  def rows                           = 0 until height
  def isInBounds(col: Int, row: Int) = cols.contains(col) && rows.contains(row)
  def neighborhood4(col: Int, row: Int) =
    Seq((col + 1, row), (col, row + 1), (col - 1, row), (col, row - 1))
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
    def adjacentNodes: Seq[(Node, Int)] = node.last4 match
      case Seq(current @ (col, row)) =>
        val nextPositions = grid.neighborhood4(col, row)
        nextPositions.map(p => (Node(Seq(p, current)), grid.at.tupled(p)))
      case Seq(current @ (col, row), previous) =>
        val nextPositions =
          grid.neighborhood4(col, row).filterNot(_ == previous)
        nextPositions.map(p =>
          (Node(Seq(p, current, previous)), grid.at.tupled(p)),
        )
      case Seq(current @ (col, row), previous, prePrevious) =>
        val nextPositions =
          grid.neighborhood4(col, row)
            .filterNot(_ == previous)
        nextPositions.map(p =>
          (Node(Seq(p, current, previous, prePrevious)), grid.at.tupled(p)),
        )
      case Seq(current @ (col, row), previous, prePrevious, prePrePrevious) =>
        val nextPositions =
          grid.neighborhood4(col, row)
            .filterNot(_ == previous)
            .filterNot(p =>
              areOnStraightLine(
                p,
                current,
                previous,
                prePrevious,
                prePrePrevious,
              ),
            )
        nextPositions.map(p =>
          (Node(Seq(p, current, previous, prePrevious)), grid.at.tupled(p)),
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
    node.last4.head == (grid.cols.last, grid.rows.last)
  }.map { case (_, (_, pathLength)) => pathLength }.min
