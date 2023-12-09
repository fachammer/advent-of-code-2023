package test

import scala.io.Source
import org.scalatest.freespec.AnyFreeSpec

case class Day(val day: Int)

def file(name: String)(using day: Day) =
  Source.fromResource(f"day${day.day}%02d/$name").mkString

case class Part[T](val part: String => T, val fileTestCases: Array[(String, T)])

object Part:
  def apply[T](part: String => T, fileTestCases: (String, T)*): Part[T] =
    Part(part, fileTestCases.toArray)

abstract class DayTest(using day: Day) extends AnyFreeSpec {
  def testFn[T, R](f: T => R, cases: (String, (T, R))*) = {
    for (name, (input, expected)) <- cases do
      name in {
        assert(f(input) == expected)
      }
  }

  extension [T, R](f: T => R)
    inline def testCases(inline cases: => (String, (T, R))*) = testFn(f, cases*)

  extension [L, R](l: L) inline def :>(r: R) = (l, r)

  def parts: Seq[Part[Any]]

  s"day ${day.day}" - {
    for (Part(fn, testCases), i) <- parts.zipWithIndex do {
      s"part ${i + 1}" - {
        fn.testCases(
          testCases.map((fileName: String, output: Any) =>
            (fileName, (file(fileName), output))
          )*
        )
      }
    }
  }
}
