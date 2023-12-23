package day23

import debug.*

// part 1
def part1(input: String) =
  val grid           = input.linesIterator.map(_.toVector).toVector
  val startCol       = (0 until grid(0).length).find(grid(0)(_) == '.').get
  val startPosititon = (startCol, 0)
  val endCol         = (0 until grid.last.length).find(grid.last(_) == '.').get
  val endPosition    = (endCol, grid.length - 1)

  startPosititon.d
  endPosition.d

  def isInBounds(col: Int, row: Int) =
    (0 until grid.length).contains(row) && (0 until grid(0).length).contains(
      col,
    )

  def manhattanDistance(from: (Int, Int), to: (Int, Int)) =
    (from._1 - to._1).abs + (from._2 - to._2).abs

  def adjacentNodes(col: Int, row: Int) =
    Seq((col + 1, row), (col, row + 1), (col - 1, row), (col, row - 1))
      .filter(isInBounds)
      .flatMap((c, r) =>
        grid(r)(c) match
          case '#' => None
          case '.' => Some((c, r))
          case '>' => Option.when(isInBounds(c + 1, r))((c + 1, r))
          case '<' => Option.when(isInBounds(c - 1, r))((c - 1, r))
          case '^' => Option.when(isInBounds(c, r - 1))((c, r - 1))
          case 'v' => Option.when(isInBounds(c, r + 1))((c, r + 1)),
      )

  adjacentNodes.tupled(startPosititon).d
  import scala.collection.mutable
  type Node = (Int, (Int, Int), Set[(Int, Int)])
  given Ordering[Node] = Ordering.by(_._1)
  val queue            = mutable.PriorityQueue((0, startPosititon, Set.empty))
  val lengths          = mutable.HashMap.empty[(Int, Int), Int]
  while queue.nonEmpty do
    val (length, position, visitedNodes) = queue.dequeue()
    if length < lengths.getOrElse(position, Int.MaxValue) then
      lengths(position) = length
      for
        nextPos <- adjacentNodes.tupled(position)
        if !visitedNodes.contains(nextPos)
      do
        val distance = manhattanDistance(position, nextPos)
        queue.enqueue((length - distance, nextPos, visitedNodes + position))

  -lengths(endPosition).d
