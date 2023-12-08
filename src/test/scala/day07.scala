package day07.test
import test.{*, given}
import day07.{*, given}
given Day = 7
class Test extends DayTest:
  def parts = Seq(
    Part(totalWinningsNoJoker, "example"   -> 6440, "input" -> 241344943),
    Part(totalWinningsWithJoker, "example" -> 5905, "input" -> 243101568)
  )
