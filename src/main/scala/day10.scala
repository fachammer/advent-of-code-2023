package day10

// part 1
type Map      = IndexedSeq[String]
type Position = (Int, Int)
def farthestDistanceOnMainLoop(input: String) =
  given Map = input.linesIterator.toIndexedSeq
  (mainLoop.length / 2.0).ceil.toInt

def mainLoop(using Map) =
  val start @ (startCol, startRow) = startPosition
  val next                         = adjacentPipes(startCol, startRow).head
  accumulate(Seq(next, start)) { case path @ (col, row) :: prev :: _ =>
    val nextPossibilities = adjacentPipes(col, row).filter(_ != prev)
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
  startPositions.head

def adjacentPipes(using map: Map)(col: Int, row: Int): Seq[Position] =
  Seq((col - 1, row), (col, row - 1), (col + 1, row), (col, row + 1)).filter {
    (c, r) => canConnectTo(row, col, c, r)
  }

def canConnectTo(using
    Map,
)(fromRow: Int, fromCol: Int, toCol: Int, toRow: Int): Boolean =
  val canConnectIfInBounds =
    for
      fromPipe <- charAt(fromCol, fromRow)
      toPipe   <- charAt(toCol, toRow)
      colDiff = toCol - fromCol
      rowDiff = toRow - fromRow
    yield (colDiff, rowDiff) match
      case (-1, 0) => canConnectHorizontally(fromPipe, toPipe)
      case (0, -1) => canConnectVertically(fromPipe, toPipe)
      case (1, 0)  => canConnectHorizontally(toPipe, fromPipe)
      case (0, 1)  => canConnectVertically(toPipe, fromPipe)

  canConnectIfInBounds.getOrElse(false)

def canConnectUp(char: Char)    = Seq('|', 'L', 'J', 'S').contains(char)
def canConnectLeft(char: Char)  = Seq('-', 'J', '7', 'S').contains(char)
def canConnectDown(char: Char)  = Seq('|', 'F', '7', 'S').contains(char)
def canConnectRight(char: Char) = Seq('-', 'F', 'L', 'S').contains(char)

def canConnectHorizontally(from: Char, to: Char) =
  canConnectLeft(from) && canConnectRight(to)
def canConnectVertically(from: Char, to: Char) =
  canConnectUp(from) && canConnectDown(to)

def lineAt(using map: Map)(row: Int)      = map.lift(row)
def charAt(using Map)(col: Int, row: Int) = lineAt(row).flatMap(_.lift(col))
def isInBounds(using map: Map)(col: Int, row: Int) = charAt(col, row).isDefined

def accumulate[S](initial: S)(op: S => Option[S]): S =
  var a = initial
  while true do
    op(a) match
      case None    => return a
      case Some(b) => a = b
  ???

// part 2
def numberOfEnclosedTiles(input: String) =
  given map: Map            = input.linesIterator.toIndexedSeq
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
  var currentCol        = col
  var loopIntersections = 0

  def insideBounds = isInBounds(currentCol, row)
  def isOnMainLoop = loop.contains((currentCol, row))
  def pipe         = charAt(currentCol, row).get
  def stepRight()  = currentCol += 1
  def goRightUntilLoopCornerOrOutOfBounds() =
    while insideBounds && isOnMainLoop && canConnectRight(pipe) do stepRight()
  def canGoRightOnLoop = canConnectRight(pipe)
  def isCrossing(enteredPipe: Char) =
    !(canConnectUp(enteredPipe) && canConnectUp(pipe)) &&
      !(canConnectDown(enteredPipe) && canConnectDown(pipe))
  def goRightUntilMainLoopOrOutOfBounds() =
    while insideBounds && !isOnMainLoop do stepRight()

  while insideBounds do
    if !isOnMainLoop then goRightUntilMainLoopOrOutOfBounds()
    else if canGoRightOnLoop then
      val loopEnterPipe = pipe
      goRightUntilLoopCornerOrOutOfBounds()

      if isCrossing(loopEnterPipe) then loopIntersections += 1

      stepRight()
    else // on loop but can't go further without going off loop
      loopIntersections += 1
      stepRight()

  loopIntersections % 2 == 1
