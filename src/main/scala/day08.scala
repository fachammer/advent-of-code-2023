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
    .d
    .length
  // .scanLeft("AAA")(x => "X")
  // .d
