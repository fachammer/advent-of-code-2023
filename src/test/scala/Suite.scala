import scala.io.Source

abstract class DayPart(
    val day: Int,
    val part: 1 | 2,
    val example: Any,
    val input: Any
) extends munit.FunSuite:
  def dayTest(
      inputType: "example" | "input",
      expectedOutput: Any
  ) =
    test(f"day$day%02d-$part $inputType") {
      val lines = Source.fromResource(f"day$day%02d-$part.$inputType").mkString
      assertEquals(run(lines), expectedOutput)
    }

  dayTest("example", example)
  dayTest("input", input)

  def run(input: String): Any
