package day11

// part 1
def sumOfShortestPaths(input: String) =
  sumOfShortestPathsWithExpansion(2)(input)

// part 2
def sumOfShortestPathsWithExpansion(expansion: Int)(input: String) =
  val lines = input.linesIterator.toSeq
  val expansionRows = lines.zipWithIndex
    .filter((line, _) => line.forall(_ == '.')).map((_, row) => row).toSet
  val expansionCols = lines.transpose.zipWithIndex
    .filter((line, _) => line.forall(_ == '.')).map((_, col) => col).toSet
  val rowDistance = distance(expansion, expansionRows)
  val colDistance = distance(expansion, expansionCols)

  val galaxyPositions =
    for
      (line, row) <- lines.zipWithIndex
      case ('#', col) <- line.zipWithIndex
    yield (col, row)
  val galaxyPairs = galaxyPositions.toSeq.combinations(2)
  galaxyPairs.map { case Seq((colA, rowA), (colB, rowB)) =>
    (colDistance(colA, colB) + rowDistance(rowA, rowB)).toLong
  }.sum

def distance(expansion: Int, expansionIndices: Set[Int])(from: Int, to: Int) =
  val (min, max) = from.minMax(to)
  val expansions = (min + 1 until max).count(expansionIndices.contains)
  (to - from).abs + expansions * (expansion - 1)

extension (a: Int) def minMax(b: Int) = if a <= b then (a, b) else (b, a)
