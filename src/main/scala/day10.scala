package day10

// part 1
type Map      = Seq[String]
type Position = (Int, Int)
def farthestDistanceOnMainLoop(input: String) =
  given Map = input.linesIterator.toSeq
  (mainLoop.length / 2.0).ceil.toInt

def mainLoop(using Map) =
  val start = startPosition
  val next  = adjacentPipes(start).head
  accumulate(Seq(next, start)) { case path @ now :: prev :: _ =>
    val nextPossibilities = adjacentPipes(now).filter(_ != prev)
    assert(nextPossibilities.length == 1)
    val next = nextPossibilities.head
    Option.when(next != start)(next +: path)
  }.reverse

def startPosition(using map: Map): Position =
  val startPositions =
    for
      (line, row) <- map.zipWithIndex
      (char, col) <- line.zipWithIndex if char == 'S'
    yield (col, row)
  assert(startPositions.length == 1)
  startPositions.head

def adjacentPipes(using map: Map)(position: Position): Seq[Position] =
  val pipe = charAt.tupled(position)
  val differences = Seq((-1, 0), (0, -1), (1, 0), (0, 1))
    .filter { (colDiff, rowDiff) =>
      isInBounds(position._1 + colDiff, position._2 + rowDiff)
    }.map { case (colDiff, rowDiff) =>
      (colDiff, rowDiff, charAt(position._1 + colDiff, position._2 + rowDiff))
    }
  val adj =
    for
      difference @ (colDiff, rowDiff, char) <- differences
      connects = difference match
        case (-1, 0, p) => connectsLeft(pipe, p)
        case (0, -1, p) => connectsUp(pipe, p)
        case (1, 0, p)  => connectsRight(pipe, p)
        case (0, 1, p)  => connectsDown(pipe, p)
      if connects
    yield (position._1 + colDiff, position._2 + rowDiff)
  adj

def connectsUp(char: Char): Boolean    = Seq('|', 'L', 'J', 'S').contains(char)
def connectsLeft(char: Char): Boolean  = Seq('-', 'J', '7', 'S').contains(char)
def connectsDown(char: Char): Boolean  = Seq('|', 'F', '7', 'S').contains(char)
def connectsRight(char: Char): Boolean = Seq('-', 'F', 'L', 'S').contains(char)

def connectsLeft(from: Char, to: Char): Boolean =
  connectsLeft(from) && connectsRight(to)
def connectsUp(from: Char, to: Char): Boolean =
  connectsUp(from) && connectsDown(to)
def connectsRight(from: Char, to: Char): Boolean =
  connectsRight(from) && connectsLeft(to)
def connectsDown(from: Char, to: Char): Boolean =
  connectsDown(from) && connectsUp(to)

extension (positions: Seq[Position])
  def withinBounds(using Map) = positions.filter(isInBounds.tupled)

def lineAt(using map: Map)(row: Int)      = map(row)
def charAt(using Map)(col: Int, row: Int) = lineAt(row)(col)
def isInBounds(using map: Map)(col: Int, row: Int) =
  val width  = lineAt(0).length
  val height = map.length
  (0 until width).contains(col) && (0 until height).contains(row)

def accumulate[S](initial: S)(op: S => Option[S]): S =
  var a = initial
  while true do
    op(a) match
      case None    => return a
      case Some(b) => a = b
  ???

// part 2
def numberOfEnclosedTiles(input: String) =
  given map: Map            = input.linesIterator.toSeq
  given loop: Set[Position] = mainLoop.toSet
  val enclosedTiles =
    for
      (line, row) <- map.zipWithIndex
      (char, col) <- line.zipWithIndex
      if !loop.contains((col, row)) && isInsideMainLoop(col, row)
    yield (col, row)
  enclosedTiles.size

def isInsideMainLoop(using
    loop: Set[Position],
    map: Map,
)(col: Int, row: Int): Boolean =
  def isOnMainLoop(pos: Position) = loop.contains(pos)
  var pos                         = (col, row)
  var intersectionCount           = 0
  while true do
    if !isInBounds.tupled(pos) then return intersectionCount % 2 == 1

    if isInBounds.tupled(pos) && !isOnMainLoop(pos) then
      while isInBounds.tupled(pos) && !isOnMainLoop(pos) do
        pos = (pos._1 + 1, row)
    else if isInBounds.tupled(pos) && isOnMainLoop(pos) then
      if connectsRight(charAt.tupled(pos)) then
        val loopEnterPos  = pos
        val loopEnterPipe = charAt.tupled(loopEnterPos)
        while isInBounds.tupled(pos) && isOnMainLoop(pos) &&
          connectsRight(charAt.tupled(pos))
        do pos = (pos._1 + 1, row)

        val pipe = charAt.tupled(pos)
        if connectsUp(loopEnterPipe) && connectsUp(pipe) ||
          connectsDown(loopEnterPipe) && connectsDown(pipe)
        then intersectionCount += 2
        else if connectsUp(loopEnterPipe) && connectsDown(pipe) ||
          connectsDown(loopEnterPipe) && connectsUp(pipe)
        then intersectionCount += 1
        else ???

        pos = (pos._1 + 1, row)
      else
        pos = (pos._1 + 1, row)
        intersectionCount += 1

  ???
