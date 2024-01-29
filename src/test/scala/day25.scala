package day25.test
import test.*
import day25.*
given Day = Day(25)
class Test extends DayTest:
  def parts = Seq(
    Part(
      productOfThreeCutComponentSizes,
      "example" -> 54,
      "input"   -> 600225,
    ),
  )
