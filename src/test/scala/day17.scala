package day17.test
import test.*
import day17.*
given Day = Day(17)
class Test extends DayTest:
  def parts = Seq(
    // 692 is too low
    Part(minimumHeatLoss, "example" -> 102, "input" -> 694),
  )
