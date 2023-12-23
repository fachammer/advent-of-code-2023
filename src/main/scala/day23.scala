package day23

// part 1
def part1(input: String) =
  val grid           = input.linesIterator.map(_.toVector).toVector
  val startCol       = (0 until grid(0).length).find(grid(0)(_) == '.').get
  val startPosititon = (startCol, 0)
  val endCol         = (0 until grid.last.length).find(grid.last(_) == '.').get
  val endPosition    = (endCol, grid.length - 1)

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

  -lengths(endPosition)

// part 2
def part2(input: String) =
  val grid           = input.linesIterator.map(_.toVector).toVector
  val startCol       = (0 until grid(0).length).find(grid(0)(_) == '.').get
  val startPosititon = (startCol, 0)
  val endCol         = (0 until grid.last.length).find(grid.last(_) == '.').get
  val endPosition    = (endCol, grid.length - 1)

  def isInBounds(col: Int, row: Int) =
    (0 until grid.length).contains(row) && (0 until grid(0).length).contains(
      col,
    )

  def adjacentNodes(col: Int, row: Int) =
    Seq((col + 1, row), (col, row + 1), (col - 1, row), (col, row - 1))
      .filter(isInBounds)
      .flatMap((c, r) =>
        grid(r)(c) match
          case '#' => None
          case _   => Some((c, r)),
      )

  import scala.collection.mutable
  def adjacentCrossings(col: Int, row: Int) =
    val adjacentStarts = adjacentNodes(col, row)
    val crossings      = mutable.Set[((Int, Int), Int)]()

    for start <- adjacentStarts do
      var length = 1
      val queue  = mutable.Queue((start, (col, row)))
      while queue.nonEmpty do
        val ((c, r), previous) = queue.dequeue()
        val adjacent           = adjacentNodes(c, r)
        if adjacent.size > 2 || adjacent.size == 1 then
          crossings.add(((c, r), length))
          queue.clear()
        else
          length += 1
          val nextNode = adjacent.find(_ != previous).get
          queue.enqueue((nextNode, (c, r)))

    crossings.toSet

  val crossingsGraph =
    val visited = mutable.HashMap[(Int, Int), Set[((Int, Int), Int)]]()
    val queue   = mutable.Queue(startPosititon)

    while queue.nonEmpty do
      val (col, row) = queue.dequeue()
      if !visited.contains((col, row)) then
        val nextCrossings = adjacentCrossings(col, row)
        visited((col, row)) = nextCrossings
        nextCrossings.foreach: (pos, _) =>
          queue.enqueue(pos)

    visited.toMap

  type Node = (Int, (Int, Int), Set[(Int, Int)])
  given Ordering[Node] = Ordering.by(_._1)
  val queue            = mutable.PriorityQueue((0, startPosititon, Set.empty))
  val lengths =
    mutable.HashMap.empty[((Int, Int), Set[(Int, Int)]), Int].withDefaultValue(
      Int.MaxValue,
    )
  while queue.nonEmpty do
    val (length, position, visitedNodes) = queue.dequeue()
    if length < lengths((position, visitedNodes)) then
      lengths((position, visitedNodes)) = length
      for
        (nextPos, weight) <- crossingsGraph(position)
        if !visitedNodes.contains(nextPos)
      do queue.enqueue((length - weight, nextPos, visitedNodes + position))

  lengths.filter:
    case ((p, _), _) => p == endPosition
  .map((_, l) => -l)
    .max
