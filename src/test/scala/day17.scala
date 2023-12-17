package day17.test
import test.*
import day17.*
given Day = Day(17)
class Test extends DayTest:
  def parts = Seq(
    Part(minimumHeatLoss, "example" -> 102, "input" -> 694),
    Part(
      minimumHeatLossWithUltraCrucible,
      "example"  -> 94,
      "example2" -> 71,
      "input"    -> 829,
    ),
  )
