package day10.test
import test.*
import day10.*
given Day = Day(10)
class Test extends DayTest:
  def parts =
    Seq(
      Part(
        farthestDistanceOnMainLoop,
        "example1" -> 4,
        "example2" -> 8,
        "input"    -> 6890,
      ),
      Part(
        numberOfEnclosedTiles,
        "example3" -> 4,
        "example4" -> 8,
        "example5" -> 10,
        "input"    -> 453,
      ),
    )
