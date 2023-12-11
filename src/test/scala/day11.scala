package day11.test

import test.*
import day11.*

given Day = Day(11)
class Test extends DayTest:
  def parts =
    Seq(
      Part(sumOfShortestPaths, "example" -> 374, "input" -> 9536038),
      Part(sumOfShortestPathsWithExpansion(10), "example"         -> 1030),
      Part(sumOfShortestPathsWithExpansion(100), "example"        -> 8410),
      Part(sumOfShortestPathsWithExpansion(1), "example2"         -> 10),
      Part(sumOfShortestPathsWithExpansion(1_000_000), "example2" -> 8_000_002),
      Part(sumOfShortestPathsWithExpansion(1_000_000), "input" -> 447744640566L),
    )
