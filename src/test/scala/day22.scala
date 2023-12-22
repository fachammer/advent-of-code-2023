package day22.test
import test.*
import day22.*
given Day = Day(22)
class Test extends DayTest:
  def parts = Seq(
    Part(safelyDisintegrableBricks, "example"  -> 5, "input" -> 519),
    Part(sumOfSinglyUnsettledBricks, "example" -> 7, "input" -> 109531),
  )

import org.openjdk.jmh.annotations.*
@State(Scope.Benchmark)
class Bench:
  val input = file("input")

  @Benchmark
  def bench = sumOfSinglyUnsettledBricks(input)
