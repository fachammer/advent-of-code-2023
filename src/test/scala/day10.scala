package day10.test
import test.*
import day10.*
given Day = Day(10)
class Test extends DayTest:
  def parts =
    Seq(Part(
      farthestDistanceOnMainLoop,
      "example1" -> 4,
      "example2" -> 8,
      "input"    -> 6890,
    ))
