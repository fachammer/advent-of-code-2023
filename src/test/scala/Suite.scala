import scala.io.Source
import scala.quoted.Expr
import scala.quoted.Quotes
import scala.quoted.Type

abstract class DayPart[T](
    val day: Int,
    val part: Int
) extends munit.FunSuite:
  def dayTest(
      inputType: "example" | "input",
      input: String,
      expectedOutput: T
  ) =
    test(f"day$day%02d-$part $inputType") {
      assertEquals(run(input), expectedOutput)
    }

  def fileToString(inputType: "example" | "input") =
    Source.fromResource(f"day$day%02d-$part.$inputType").mkString

  def dayTestFile(inputType: "example" | "input", expectedOutput: T) =
    dayTest(inputType, fileToString(inputType), expectedOutput)

  def testExample(expectedOutput: T) = dayTestFile("example", expectedOutput)
  def testInput(expectedOutput: T) = dayTestFile("input", expectedOutput)

  inline def testFn[T, R](
      inline fn: T => R,
      expectedResults: (T, R)*
  ) =
    for (input, expectedOutput) <- expectedResults do
      test(s"${fn.exprString}($input) = $expectedOutput") {
        assertEquals(fn(input), expectedOutput)
      }

  def run(input: String): T

class NamedFunction[T, R](name: String, f: Function[T, R])
    extends Function[T, R]:
  def apply(v: T) = f(v)
  override def toString = name

def exprStringCode[T, R](f: Expr[T => R])(using Quotes): Expr[String] =
  Expr(f.show.split(" ").last.split("\\(").head.split("\\.").last)

extension [T, R](inline f: T => R)
  inline def exprString = ${ exprStringCode('f) }
