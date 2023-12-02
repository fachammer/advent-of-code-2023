package test

import scala.io.Source
import scala.quoted.Expr
import scala.quoted.Quotes

abstract class Day(val day: Int) extends munit.FunSuite:
  def testFnWithNames[T, R](fn: T => R, expectedResults: (String, T, R)*) =
    for (testName, input, expectedOutput) <- expectedResults do
      test(testName) {
        assertEquals(fn(input), expectedOutput)
      }

  def file(name: String) = Source.fromResource(f"day$day%02d/$name").mkString

  extension [T, R](inline fn: T => R)
    inline def testCases(expectedResults: (T, R)*) =
      testFnWithNames(
        fn,
        expectedResults.map(x =>
          (s"${fn.exprString}(${x._1}) = ${x._2}", x._1, x._2)
        )*
      )

  extension [R](inline fn: String => R)
    inline def testCasesFromFile(expectedResults: (String, R)*) =
      testFnWithNames(
        fn,
        expectedResults.map((x) =>
          val input = file(x._1)
          (s"${fn.exprString}(file(${x._1})) = ${x._2}", input, x._2)
        )*
      )

def exprStringCode[T, R](f: Expr[T => R])(using Quotes): Expr[String] =
  Expr(f.show.split(" ").last.split("\\(").head.split("\\.").last)

extension [T, R](inline f: T => R)
  inline def exprString = ${ exprStringCode('f) }
