package day09.test
import test.*
import day09.*
given Day = Day(9)
class Test extends DayTest:
  def parts =
    Seq(
      Part(extrapolatedSum, "example"         -> 114, "input" -> 1479011877),
      Part(extrapolatedPreviousSum, "example" -> 2, "input"   -> 973),
    )
