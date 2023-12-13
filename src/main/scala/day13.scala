package day13

import debug.*

// part 1
def summarize(input: String) =
  val patterns = input.split("\n\n")
    .map(_.linesIterator.map(_.toVector).toVector)
  patterns.map(reflectionCount).sum

def reflectionCount(pattern: Vector[Vector[Char]]): Int =
  verticalReflectionColumn(pattern)
    .orElse(horizontalReflectionRow(pattern).map(_ * 100)).get

def verticalReflectionColumn(pattern: Vector[Vector[Char]]): Option[Int] =
  horizontalReflectionRow(pattern.transpose)

def horizontalReflectionRow(pattern: Vector[Vector[Char]]): Option[Int] =
  val result = (1 to pattern.length / 2).find { i =>
    pattern.take(i).sameElements(pattern.drop(i).take(i).reverse)
  }
  val reversed = pattern.reverse
  val reverseResult = (1 to reversed.length / 2).find { i =>
    reversed.take(i).sameElements(reversed.drop(i).take(i).reverse)
  }.map(pattern.length - _)
  result.orElse(reverseResult)
