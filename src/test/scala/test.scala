package test

import scala.io.Source
import scala.quoted.Expr
import scala.quoted.Quotes
import scala.quoted.Type

abstract class DayPart[T](
    val day: Int,
    val part: Int
) extends munit.FunSuite:
  def testString(
      inputType: String,
      inputExpectedOuput: (String, T)
  ) =
    test(inputType) {
      assertEquals(run(inputExpectedOuput._1), inputExpectedOuput._2)
    }

  def testFile(inputTypeExpectedOutput: (String, T)) =
    testString(
      inputTypeExpectedOutput._1,
      (
        Source
          .fromResource(f"day$day%02d/${inputTypeExpectedOutput._1}")
          .mkString,
        inputTypeExpectedOutput._2
      )
    )

  inline def testFn[T, R](
      inline fn: T => R,
      expectedResults: (T, R)*
  ) =
    for (input, expectedOutput) <- expectedResults do
      test(s"${fn.exprString}($input) = $expectedOutput") {
        assertEquals(fn(input), expectedOutput)
      }

  def run(input: String): T

  extension [T, R](inline fn: T => R)
    inline def testCases(expectedResults: (T, R)*) =
      testFn(fn, expectedResults*)

class NamedFunction[T, R](name: String, f: Function[T, R])
    extends Function[T, R]:
  def apply(v: T) = f(v)
  override def toString = name

def exprStringCode[T, R](f: Expr[T => R])(using Quotes): Expr[String] =
  Expr(f.show.split(" ").last.split("\\(").head.split("\\.").last)

extension [T, R](inline f: T => R)
  inline def exprString = ${ exprStringCode('f) }
