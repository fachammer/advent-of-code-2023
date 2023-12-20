package day20.test
import test.{Day, DayTest, Part}
import day20.*
given Day = Day(20)
class Test extends DayTest:
  def parts = Seq(
    Part(
      part1,
      "example"  -> 32000000,
      "example2" -> 11687500,
      "input"    -> 730797576,
    ),
  )
