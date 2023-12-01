import scala.io.Source
// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class Suite extends munit.FunSuite {
  test("day01-1 example") {
    val lines = Source.fromFile("day01-1.example").mkString
    assertEquals(sumOfCalibrationValues(lines), 142)
  }

  test("day01-1 input") {
    val lines = Source.fromFile("day01-1.input").mkString
    assertEquals(sumOfCalibrationValues(lines), 56042)
  }

  test("day01-2 example") {
    val lines = Source.fromFile("day01-2.example").mkString
    assertEquals(sumOfCalibrationValuesWithDigitNames(lines), 281)
  }

  test("day01-2 input") {
    val lines = Source.fromFile("day01-2.input").mkString
    assertEquals(sumOfCalibrationValuesWithDigitNames(lines), 55358)
  }

  test("overlapping digit names") {
    assertEquals(
      calibrationValueWithDigitNames("9963onefourthree6oneightq"),
      98
    )
  }
}
