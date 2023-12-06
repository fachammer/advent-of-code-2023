class Day06 extends DayTest(6):
  import day06.*
  def parts = Seq(
    Part(
      productOfNumberOfWaysToBeatRecord,
      "example" -> 288,
      "input"   -> 4811940
    ),
    Part(
      numberOfWaysToBeatRecord,
      "example" -> 71503,
      "input"   -> 30077773
    )
  )
