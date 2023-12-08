package day08

import debug.*

// part 1
def requiredSteps(input: String) =
  val steps = input.linesIterator.next
  val nodes = input.linesIterator
    .drop(2)
    .map(line =>
      val s"$node = ($leftNode, $rightNode)" = line: @unchecked
      (node, (leftNode, rightNode))
    )
    .toMap

  LazyList
    .concat(LazyList.continually(steps.toSeq)*)
    .scanLeft("AAA") {
      case (node, 'L') => nodes(node)._1
      case (node, 'R') => nodes(node)._2
    }
    .takeWhile(_ != "ZZZ")
    .length

// part 2
def requiredStepsAsGhost(input: String) =
  val steps = input.linesIterator.next.d
  steps.length.d
  val nodes = input.linesIterator
    .drop(2)
    .map(line =>
      val s"$node = ($leftNode, $rightNode)" = line: @unchecked
      (node, (leftNode, rightNode))
    )
    .toMap

  def requiredStepsToAnEndNode(startNode: String, stepOffset: Int) =
    val stepsIter = LazyList
      .concat(LazyList.continually(steps.toSeq.zipWithIndex)*)
      .toIterator
      .drop(stepOffset)
    var node       = startNode.d
    var count      = 0
    var stepsIndex = stepOffset
    while count == 0 || !node.endsWith("Z") do
      assert(node != "XXX")
      count += 1
      val (step, index) = stepsIter.next
      stepsIndex = index
      node = step match
        case 'L' => nodes(node)._1
        case 'R' => nodes(node)._2

    (count, node, stepsIndex)

  def gcd(a: Long, b: Long): Long = if (b == 0) a else gcd(b, a % b)
  def lcm(a: Long, b: Long)       = (a * b) / gcd(a, b)

  val startNodes = nodes.filterKeys(_.endsWith("A")).keys
  val requiredSteps = lcm(
    startNodes
      .map { startNode =>
        val (phase, endNode, stepsIndex) =
          requiredStepsToAnEndNode(startNode, 0).d
        val (cycleLength, _, endNodeStepsIndex) =
          requiredStepsToAnEndNode(endNode, stepsIndex + 1).d
        (
          startNode,
          phase,
          stepsIndex,
          endNode,
          cycleLength,
          endNodeStepsIndex
        )
      }
      .foldLeft(1L) { case (a, (_, _, _, _, b, _)) => lcm(a.toLong, b.toLong) },
    steps.length
  )
  requiredSteps.d
