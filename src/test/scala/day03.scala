package day03.test
import test.*
import day03.*
given Day = Day(3)
class Test extends DayTest:
  def parts =
    Seq(
      Part(partNumberSum, "example" -> 4361, "input"   -> 532428),
      Part(gearRatioSum, "example"  -> 467835, "input" -> 84051670),
    )
