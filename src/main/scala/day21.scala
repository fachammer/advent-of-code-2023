package day21

// part 1
def reachableGardenPlotsOnFiniteLand(requiredSteps: Int)(input: String) =
  val grid                     = Grid.parse(input)
  val gridVisitor: GridVisitor = grid.isGardenPlotFinite
  gridVisitor.reachableGardenPlots(grid.startPosition, requiredSteps)

case class Grid(grid: Vector[Vector[Char]]):
  val startPosition = (for
    (line, row) <- grid.zipWithIndex
    (char, col) <- line.zipWithIndex
    if char == 'S'
  yield (col, row)).head
  val cols = 0 until grid(0).length
  val rows = 0 until grid.length

  def apply(col: Int, row: Int)    = grid(row)(col)
  def isInside(col: Int, row: Int) = cols.contains(col) && rows.contains(row)
  def isGardenPlotFinite(col: Int, row: Int) =
    isInside(col, row) && this(col, row) != '#'

  def isGardenPlotInfinite(col: Int, row: Int) =
    this(mod(col, cols.end), mod(row, rows.end)) != '#'

object Grid:
  def parse(input: String) = Grid(input.linesIterator.map(_.toVector).toVector)

// part 2
def reachableGardenPlotsWithSpecialInput(gridLengthMultiple: Int)(
    input: String,
) =
  val grid = Grid.parse(input)
  assert(grid.startPosition == (65, 65))
  assert(grid.cols.end == grid.rows.end)
  assert(grid.cols.end == 131)
  val startPosition        = grid.startPosition
  val visitor: GridVisitor = grid.isGardenPlotInfinite

  val steps = (0 to 3).map(65 + _ * 131)
  val sizes = steps.map(visitor.reachableGardenPlots(startPosition, _))
  val Array(_, diff1, diff2, diff3) = sizes.higherOrderDifferencesUpTo(3)
  assert(diff3.forall(_ == 0))

  val quadratic     = diff2.head / 2
  val linear        = diff1.head - quadratic
  val constant      = sizes.head
  val quadraticPoly = (n: Long) => quadratic * n * n + linear * n + constant

  quadraticPoly(gridLengthMultiple)

def reachableGardenPlotsOnInfiniteLand(requiredSteps: Int)(input: String) =
  val grid                     = Grid.parse(input)
  val gridVisitor: GridVisitor = grid.isGardenPlotInfinite
  gridVisitor.reachableGardenPlots(grid.startPosition, requiredSteps)

@FunctionalInterface
trait GridVisitor:
  def isGardenPlot(col: Int, row: Int): Boolean
  def reachableGardenPlots(startPosition: (Int, Int), steps: Int) =
    val initialSteps           = Set(startPosition)
    val initialEvenVisited     = 0
    val initialOddVisited      = 0
    val initialPreviousVisited = Set.empty[(Int, Int)]
    val (_, _, evenVisited, oddVisited) = (0 to steps).foldLeft(
      (
        initialSteps,
        initialPreviousVisited,
        initialEvenVisited,
        initialOddVisited,
      ),
    ) { case ((currentSteps, previousVisited, evenVisited, oddVisited), step) =>
      val nextSteps = currentSteps.flatMap: (col, row) =>
        neighborhood4(col, row).filter(isGardenPlot)
          .filterNot(previousVisited.contains)
      val evenAdded = ((step + 1) % 2) * currentSteps.size
      val oddAdded  = (step       % 2) * currentSteps.size
      (nextSteps, currentSteps, evenVisited + evenAdded, oddVisited + oddAdded)
    }

    if steps % 2 == 0 then evenVisited else oddVisited

def mod(x: Int, y: Int) =
  val m = x % y
  if m >= 0 then m else m + y

def neighborhood4(col: Int, row: Int) =
  Seq((col + 1, row), (col, row + 1), (col - 1, row), (col, row - 1))

extension (seq: Seq[Int])
  def differences(n: Int = 1): Seq[Int] =
    assert(n >= 0)
    if n == 0 then seq
    else
      val diff = seq.differences(n - 1)
      diff.tail.zip(diff).map(_ - _)

  def higherOrderDifferencesUpTo(order: Int): Array[Seq[Int]] =
    (0 to order).map(seq.differences).toArray
