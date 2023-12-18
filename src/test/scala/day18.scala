package day18.test
import test.*
import day18.*
given Day = Day(18)
class Test extends DayTest:
  def parts = Seq(
    Part(
      lagoonSizeByNormalInstructions,
      "example" -> 62,
      "input"   -> 35401,
    ),
    Part(
      lagoonSizeBySwappedInstructions,
      "example" -> 952408144115L,
      "input"   -> 48020869073824L,
    ),
  )
