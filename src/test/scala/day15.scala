package day15.test
import test.*
import day15.*
given Day = Day(15)
class Test extends DayTest:
  def parts = Seq(
    Part(hashValuesSum, "example" -> 1320, "input" -> 511215),
    Part(focusingPower, "example" -> 145, "input"  -> 236057),
  )
