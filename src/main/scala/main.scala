package main

import scala.io.Source
import scala.quoted.Expr
import scala.quoted.Quotes
import utest.framework.Tree
import utest.framework.TestCallTree
import scala.collection.mutable
import utest.TestRunner
import utest.Tests
import utest.given

@main def main =
  import days.*
  day01
  day02
  TestRunner.runAndPrint(utests, "Advent of Code 2023")

case class Day(val day: Int)
given Conversion[Int, Day] with
  def apply(x: Int) = Day(x)

def file(name: String)(using day: Day) =
  Source.fromResource(f"day${day.day}%02d/$name").mkString

var tests = mutable.Map[Int, mutable.Map[String, () => Any]]()
def utests =
  val sortedTests =
    tests.toSeq.sortBy(_._1).map(x => (x._1, x._2.toSeq.sortBy(_._1)))
  val nameTree =
    Tree(
      "root",
      sortedTests
        .map(x =>
          Tree(
            f"Day ${x._1}%02d",
            x._2.toSeq
              .sortBy(_._1)
              .map(y =>
                Tree(
                  y._1.replace("\n", "\\n").substring(0, 50.min(y._1.length()))
                )
              )*
          )
        )*
    )
  val callTree = TestCallTree(
    Right(
      sortedTests
        .map(t =>
          TestCallTree(
            Right(t._2.map(x => TestCallTree(Left(x._2()))).toIndexedSeq)
          )
        )
        .toIndexedSeq
    )
  )
  Tests(nameTree, callTree)

extension [T, R](fn: T => R)(using day: Day)
  def testCasesWithNames(expectedResults: (String, (T, R))*) =
    var dayTests = tests.getOrElseUpdate(day.day, mutable.Map())
    for (testName, (input, expectedOutput)) <- expectedResults do
      dayTests.addOne(testName, () => fn(input) ==> expectedOutput)

extension [T, R](inline fn: T => R)(using day: Day)
  inline def testCases(expectedResults: (T, R)*) =
    fn.testCasesWithNames(
      expectedResults.map(x =>
        (s"${fn.exprString}(${x._1}) = ${x._2}", (x._1, x._2))
      )*
    )

extension [R](inline fn: String => R)(using day: Day)
  inline def testCasesFromFile(expectedResults: (String, R)*) =
    fn.testCasesWithNames(
      expectedResults.map((x) =>
        val input = file(x._1)
        (s"${fn.exprString}(file(${x._1})) = ${x._2}", (input, x._2))
      )*
    )

def exprStringCode[T, R](f: Expr[T => R])(using Quotes): Expr[String] =
  Expr(f.show.split(" ").last.split("\\(").head.split("\\.").last)

extension [T, R](inline f: T => R)
  inline def exprString = ${ exprStringCode('f) }
