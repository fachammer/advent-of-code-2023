package day09.test
import test.{*, given}
import day09.*
given Day = 9
class Day09 extends DayTest:
  def parts = Seq(
    Part(extrapolatedValuesSum, "example" -> 114, "input" -> 1479011877),
    Part(extrapolatedPreviousValuesSum, "example" -> 2, "input" -> 973)
  )
