class Day07 extends DayTest(7):
  import day07.*
  def parts = Seq(
    Part(totalWinningsNoJoker, "example"   -> 6440, "input" -> 241344943),
    Part(totalWinningsWithJoker, "example" -> 5905, "input" -> 243101568)
  )
