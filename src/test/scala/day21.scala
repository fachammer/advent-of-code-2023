package day21.test
import test.{Day, DayTest, Part}
import day21.*
given Day = Day(21)
class Test extends DayTest:
  def parts = Seq(
    Part(
      numberOfPossibleEndPositions(6),
      "example" -> 16,
    ),
    Part(
      numberOfPossibleEndPositions(64),
      "input" -> 3733,
    ),
    // Part(numberOfPossibleEndPositionsWithInfiniteMap(6), "example"  -> 16),
    // Part(numberOfPossibleEndPositionsWithInfiniteMap(10), "example" -> 50),
    // Part(numberOfPossibleEndPositionsWithInfiniteMap(50), "example" -> 1594),
    // Part(
    //   numberOfPossibleEndPositionsWithInfiniteMap(100),
    //   "example" ->
    //     6536,
    // ),
    // Part(
    //   numberOfPossibleEndPositionsWithInfiniteMap(500),
    //   "example" ->
    //     167004,
    // ),
    // Part(
    //   numberOfPossibleEndPositionsWithInfiniteMap(1000),
    //   "example" -> 668697,
    // ),
    // Part(
    //   numberOfPossibleEndPositionsWithInfiniteMap(5000),
    //   "example" -> 16733044,
    // ),
    Part(
      numberOfPossibleEndPositionsWithInfiniteMap(26501365),
      "input" -> 617729401414635L,
    ),
  )
