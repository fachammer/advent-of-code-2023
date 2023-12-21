package day21.test
import test.{Day, DayTest, Part}
import day21.*
given Day = Day(21)
class Test extends DayTest:
  def parts = Seq(
    Part(
      numberOfPossibleEndPositions(6),
      "example" -> 16,
    ),
    Part(
      numberOfPossibleEndPositions(64),
      "input" -> 0,
    ),
  )
