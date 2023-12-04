class Day01 extends DayTest(1) {
  import day01.*
  def parts = Seq(
    Part(sumOfCalibrationValues, "example1" -> 142, "input" -> 56042),
    Part(
      sumOfCalibrationValuesIncludingWords,
      "example2" -> 281,
      "input"    -> 55358
    )
  )

  "calibrationValue" - {
    calibrationValue.testCases("words are not significant" :> "12one" -> 12)
  }

  "calibrationValueWithWords" - {
    calibrationValueIncludingWords.testCases(
      "words are significant" :> "12one" -> 11
    )
  }
}
