package day22.test
import test.*
import day22.*
given Day = Day(22)
class Test extends DayTest:
  def parts = Seq(
    Part(
      part1,
      "example" -> 5,
      // "input"   -> 519,
    ),
    Part(
      part2,
      "example" -> 7,
      "input"   -> 109531,
    ),
  )

import org.openjdk.jmh.annotations.*
@State(Scope.Benchmark)
class Bench:
  val input = file("input2")

  @Benchmark
  def bench = part1(input)
