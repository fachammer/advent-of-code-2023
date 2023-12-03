package day03

import test.*

class Test extends DayTest(3):
  def parts = Seq(
    Part(sumOfPartNumbers, "example" -> 4361, "input"   -> 532428),
    Part(gearRatiosSum, "example"    -> 467835, "input" -> 84051670)
  )
