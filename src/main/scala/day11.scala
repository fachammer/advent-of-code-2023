package day11

// part 1
def sumOfShortestPaths(input: String) =
  sumOfShortestPathsWithExpansion(2)(input)

// part 2
def sumOfShortestPathsWithExpansion(expansion: Int)(input: String) =
  val galaxyPositions = (for
    (line, row) <- input.linesIterator.zipWithIndex
    case ('#', col) <- line.zipWithIndex
  yield (col, row)).toSeq
  val universe           = input.linesIterator.toSeq
  val transposedUniverse = universe.transpose

  galaxyPositions.combinations(2).map { case Seq((colA, rowA), (colB, rowB)) =>
    val colDistance = ((colA.min(colB) + 1) to colA.max(colB)).map(col =>
      if transposedUniverse(col).forall(_ == '.') then expansion else 1,
    ).sum.toLong
    val rowDistance = ((rowA.min(rowB) + 1) to rowA.max(rowB))
      .map(row => if universe(row).forall(_ == '.') then expansion else 1).sum
      .toLong
    colDistance + rowDistance
  }.sum
