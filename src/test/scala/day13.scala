package day13.test
import test.*
import day13.*
given Day = Day(13)
class Test extends DayTest:
  def parts =
    Seq(
      Part(summarize, "example"                  -> 405, "input" -> 27202),
      Part(summarizeWithSmudgeRemoved, "example" -> 400, "input" -> 41566),
    )

  reflectionWithSmudgeRemoved.testCases(
    "a" :> (patterns(file("example"))(1) -> Reflection.Horizontal(1)),
    "b" :> (patterns(file("input"))(0)   -> Reflection.Vertical(16)),
  )
