package day11

// part 1
def sumOfShortestPaths(input: String) =
  val universe = expandedUniverse(input)
  val galaxyPositions =
    for
      (line, row) <- universe.zipWithIndex
      case ('#', col) <- line.zipWithIndex
    yield (col, row)

  galaxyPositions.combinations(2).map { case Seq((colA, rowA), (colB, rowB)) =>
    (colB - colA).abs + (rowB - rowA)
  }.sum

def expandedUniverse(input: String) =
  val lines = input.linesIterator.toSeq
  val cols  = lines.transpose

  val expandedCols = cols.flatMap(column =>
    if column.forall(_ == '.') then Seq(column, column) else Seq(column),
  )
  expandedCols.transpose.flatMap(line =>
    if line.forall(_ == '.') then Seq(line, line) else Seq(line),
  )
