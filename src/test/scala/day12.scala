package day12.test
import test.*
import day12.*
given Day = Day(12)
class Test extends DayTest:
  def parts =
    Seq(
      Part(sumOfArrangements, "example" -> 21, "input" -> 8270),
      Part(
        sumOfUnfoldedSpringConfigurations,
        "example" -> 525152,
        "input"   -> 204640299929836L,
      ),
    )
