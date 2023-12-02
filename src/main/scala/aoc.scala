import scala.io.Source
import scala.quoted.Expr
import scala.quoted.Quotes
import utest.framework.Tree
import utest.framework.TestCallTree
import scala.collection.mutable
import utest.TestRunner
import utest.Tests
import utest.given
import utest.test
import utest.framework.Formatter
import utest.framework.HTree
import utest.framework.Result
import utest.ufansi.Str
import scala.annotation.StaticAnnotation
import scala.language.experimental

object TestFormatter extends utest.framework.Formatter:
  override def formatColor: Boolean = true

  override def formatWrapWidth: Int = 40

  override def formatSummary(
      topLevelName: String,
      results: HTree[String, Result]
  ): Option[Str] = Some("hello")

class TestCases[T, R](val cases: (T, R)*)
    extends scala.annotation.StaticAnnotation:
  def transform(using Quotes)(
      tree: x$1.reflect.Definition
  ): List[x$1.reflect.Definition] = List(tree)

@main def main =
  day01
  day02
  val results = TestRunner.runAndPrint(
    utests(tests.map(x => (x._1, x._2.toMap)).toMap),
    "Advent of Code 2023"
  )
  val (summary, _, _) =
    TestRunner.renderResults(Seq("Advent of Code 2023" -> results))
  println(summary)

case class Day(val day: Int)
given Conversion[Int, Day] with
  def apply(x: Int) = Day(x)

def file(name: String)(using day: Day) =
  Source.fromResource(f"day${day.day}%02d/$name").mkString

var tests = mutable.Map[Int, mutable.Map[String, () => Any]]()
def utests(tests: Map[Int, Map[String, () => Any]]) =
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
                  y._1.replace("\n", "\\n").substring(0, y._1.length().min(80))
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
  def testCasesWithNames(expectedResults: (String, (T, R))*): Tests =
    val newTests =
      expectedResults.map(x => (x._1, () => fn(x._2._1) ==> x._2._2))
    tests.getOrElseUpdate(day.day, mutable.Map()).addAll(newTests)
    utests(Map(day.day -> newTests.toMap))

extension [T, R](inline fn: T => R)(using Day)
  inline def testCases(inline expectedResults: (T, R)*): Tests =
    fn.testCasesWithNames(
      expectedResults.map(x =>
        (s"${fn.exprString}(${x._1}) = ${x._2}", (x._1, x._2))
      )*
    )

extension [R](inline fn: String => R)(using Day)
  inline def testCasesFromFile(expectedResults: (String, R)*): Tests =
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
