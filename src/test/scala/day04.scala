package day04.test
import test.*
import day04.*
given Day = Day(4)
class Test extends DayTest:
  def parts =
    Seq(
      Part(day04.points, "example"       -> 13, "input" -> 32609),
      Part(day04.scratchCards, "example" -> 30, "input" -> 14624680),
    )
