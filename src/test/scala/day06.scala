package day06.test
import test.*
import day06.*
given Day = Day(6)
class Test extends DayTest:
  def parts =
    Seq(
      Part(
        productOfNumberOfWaysToBeatRecord,
        "example" -> 288,
        "input"   -> 4811940,
      ),
      Part(numberOfWaysToBeatRecord, "example" -> 71503, "input" -> 30077773),
    )
