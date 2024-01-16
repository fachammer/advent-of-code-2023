package day24.test
import test.*
import day24.*
given Day = Day(24)
class Test extends DayTest:
  def parts = Seq(
    Part(hailStoneIntersections(7L, 27L), "example" -> 2),
    Part(
      hailStoneIntersections(200000000000000L, 400000000000000L),
      "input" -> 15262,
    ),
    Part(oneShotHitAllPositionSum, "example" -> 47, "input" -> 695832176624149L),
  )
