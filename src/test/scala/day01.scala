package day01.test
import test.*
import day01.*
given Day = Day(1)
class Test extends DayTest {
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
