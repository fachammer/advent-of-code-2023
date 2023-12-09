package day05.test
import test.*
import day05.*
given Day = Day(5)
class Test extends DayTest:
  def parts = Seq(
    Part(lowestLocationNumberOfAnySeed, "example" -> 35, "input" -> 486613012),
    Part(
      lowestLocationNumberOfAnySeedWithRangeInput,
      "example" -> 46,
      "input"   -> 56931769
    )
  )

  val exampleAlmanac = parseAlmanac(file("example"))
  "applyRange" - {
    ((x: AlmanacMap, range: Interval) => x.restrictTo(range)).tupled.testCases(
      "applyRange outside of given ranges" :>
        (exampleAlmanac.maps.head, Interval(30, 10))
        -> AlmanacMap(Seq(IntervalMap(30, 30, 10)))
    )
  }
