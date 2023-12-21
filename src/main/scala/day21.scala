package day21

import debug.*

// part 1
def numberOfPossibleEndPositions(requiredSteps: Int)(input: String) =
  val grid = input.linesIterator.map(_.toVector).toVector

  val startPosition = grid.zipWithIndex.flatMap: (line, row) =>
    line.zipWithIndex.map: (char, col) =>
      (col, row, char)
  .find((_, _, char) => char == 'S')
    .map((col, row, _) => (col, row))
    .get
    .d

  import scala.collection.mutable
  val stepFront = mutable.HashSet.empty[(Int, Int)]
  val queue     = mutable.Stack((0, startPosition))
  val memo      = mutable.HashMap.empty[((Int, Int), Int), Set[(Int, Int)]]

  def reachableSteps(
      startPosition: (Int, Int),
      remainingSteps: Int,
  ): Set[(Int, Int)] =
    if memo.contains((startPosition, remainingSteps)) then
      return memo((startPosition, remainingSteps))
    val result =
      if remainingSteps == 0 then Set(startPosition)
      else
        val (col, row) = startPosition
        Set((col + 1, row), (col, row + 1), (col - 1, row), (col, row - 1))
          .filter((c, r) =>
            (0 until grid.length).contains(r)
              && (0 until grid(0).length).contains(c),
          )
          .filter((c, r) => grid(r)(c) != '#')
          .map(p => reachableSteps(p, remainingSteps - 1))
          .flatten
    memo((startPosition, remainingSteps)) = result
    result

  reachableSteps(startPosition, requiredSteps).size
  // while queue.nonEmpty do
  //   val (steps, (col, row)) = queue.pop()
  //   if steps == requiredSteps then stepFront.add((col, row))
  //   else
  //     Seq((col + 1, row), (col, row + 1), (col - 1, row), (col, row - 1))
  //       .filter((c, r) =>
  //         (0 until grid.length).contains(r)
  //           && (0 until grid(0).length).contains(c),
  //       )
  //       .filter((c, r) => grid(r)(c) != '#')
  //       .foreach: (c, r) =>
  //         queue.push((steps + 1, (c, r)))

  // stepFront.size
