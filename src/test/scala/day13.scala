package day13.test
import test.*
import day13.*
given Day = Day(13)
class Test extends DayTest:
  def parts = Seq(Part(summarize, "example" -> 405, "input" -> 0))
