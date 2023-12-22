package day21

// part 1
def numberOfPossibleEndPositions(requiredSteps: Int)(input: String) =
  val grid = input.linesIterator.map(_.toVector).toVector

  val startPosition = grid.zipWithIndex.flatMap: (line, row) =>
    line.zipWithIndex.map: (char, col) =>
      (col, row, char)
  .find((_, _, char) => char == 'S')
    .map((col, row, _) => (col, row))
    .get

  import scala.collection.mutable
  val memo = mutable.HashMap.empty[((Int, Int), Int), Set[(Int, Int)]]
  def reachablePositionsInSteps(
      startPosition: (Int, Int),
      remainingSteps: Int,
  ): Set[(Int, Int)] =
    if memo.contains((startPosition, remainingSteps)) then
      return memo((startPosition, remainingSteps))
    val result =
      if remainingSteps == 0 then Set(startPosition)
      else if remainingSteps == 1 then
        val (col, row) = startPosition
        Set((col + 1, row), (col, row + 1), (col - 1, row), (col, row - 1))
          .filter: (c, r) =>
            (0 until grid.length).contains(r)
              && (0 until grid(0).length).contains(c)
          .filter((c, r) => grid(r)(c) != '#')
      else
        val lowerHalf  = remainingSteps / 2
        val upperHalf  = remainingSteps - lowerHalf
        val (col, row) = startPosition
        reachablePositionsInSteps(startPosition, lowerHalf).flatMap(
          reachablePositionsInSteps(_, upperHalf),
        )
    memo((startPosition, remainingSteps)) = result
    result

  (0 until grid.length).flatMap(row =>
    (0 until grid(0).length).map(col => (col, row)),
  ).count((c, r) => grid(r)(c) != '#')
  reachablePositionsInSteps(startPosition, requiredSteps).size

// part 2
def numberOfPossibleEndPositionsWithInfiniteMap(requiredSteps: Int)(
    input: String,
) =
  val grid = input.linesIterator.map(_.toVector).toVector

  val startPosition = grid.zipWithIndex.flatMap: (line, row) =>
    line.zipWithIndex.map: (char, col) =>
      (col, row, char)
  .find((_, _, char) => char == 'S')
    .map((col, row, _) => (col, row))
    .get

  def visited(startPosition: (Int, Int), steps: Int) =
    import scala.collection.mutable
    var previousSteps = mutable.HashSet[(Int, Int)]()
    var currentSteps  = mutable.HashSet[(Int, Int)](startPosition)
    var nextSteps     = mutable.HashSet[(Int, Int)]()
    var step          = 0
    val evenVisited   = mutable.HashSet[(Int, Int)]()
    val oddVisited    = mutable.HashSet[(Int, Int)]()
    while step <= steps do
      for (col, row) <- currentSteps do
        if step % 2 == 0 then evenVisited.add((col, row))
        else oddVisited.add((col, row))
        val nextStepsFromCurrent =
          Set((col + 1, row), (col, row + 1), (col - 1, row), (col, row - 1))
            .filter: (c, r) =>
              grid(mod(r, grid.length))(mod(c, grid(0).length)) != '#'
            .filterNot(previousSteps.contains)

        nextSteps.addAll(nextStepsFromCurrent)

      previousSteps = currentSteps
      currentSteps = nextSteps
      nextSteps = mutable.HashSet.empty
      step += 1

    if steps % 2 == 0 then evenVisited else oddVisited

  val sizes = (0 to 3).map(i => visited(startPosition, 65 + i * 131).size)

  extension (seq: Seq[Int])
    def differences =
      seq.zip(seq.tail).map((left, right) => right - left)

  val firstOrderDifferences  = sizes.differences
  val secondOrderDifferences = firstOrderDifferences.differences
  val thirdOrderDifferences  = secondOrderDifferences.differences
  assert(thirdOrderDifferences.forall(_ == 0))

  val quadraticCoefficient = secondOrderDifferences.head / 2
  val linearCoefficient    = firstOrderDifferences.head - quadraticCoefficient
  val constantCoefficient  = sizes.head

  val quadratic: Long => Long = n =>
    quadraticCoefficient * n * n
      + linearCoefficient * n
      + constantCoefficient

  assert((requiredSteps - startPosition._1) % grid.length == 0)
  val repetitions = (requiredSteps.toLong - startPosition._1) / grid.length

  quadratic(repetitions)

def mod(x: Int, y: Int) =
  val m = x % y
  if m >= 0 then m else m + y
