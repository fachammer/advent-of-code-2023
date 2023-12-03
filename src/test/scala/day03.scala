package day03

import test.*

class Test extends DayTest(3):
  def parts = Seq(
    Part(partNumberSum, "example" -> 4361, "input"   -> 532428),
    Part(gearRatioSum, "example"  -> 467835, "input" -> 84051670)
  )
