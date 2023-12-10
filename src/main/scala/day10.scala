package day10

// part 1
type Map      = Seq[String]
type Position = (Int, Int)
def farthestDistanceOnMainLoop(input: String) =
  given Map = input.linesIterator.toSeq
  val start = startPosition
  val next  = adjacentPipes(start).head
  val loop = accumulate(Seq(next, start)) { case path @ now :: prev :: _ =>
    val nextPossibilities = adjacentPipes(now).filter(_ != prev)
    assert(nextPossibilities.length == 1)
    val next = nextPossibilities.head
    Option.when(next != start)(next +: path)
  }
  (loop.length / 2.0).ceil.toInt

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
