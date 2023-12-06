class Day05 extends DayTest(5):
  import day05.*
  def parts = Seq(
    Part(lowestLocationNumberOfAnySeed, "example" -> 35, "input" -> 486613012),
    Part(
      lowestLocationNumberOfAnySeedConsideringRanges,
      "example" -> 46,
      "input"   -> 56931769
    )
  )

  val exampleAlmanac = parseAlmanac(file("example"))
  "applyRange" - {
    ((x: AlmanacMap, range: Range) => x.restrictTo(range)).tupled.testCases(
      "applyRange outside of given ranges" :> (
        exampleAlmanac.maps.head,
        Range(30, 10)
      ) ->
        AlmanacMap("seed", "soil", Seq((Range(30, 10), Range(30, 10))))
    )
  }
