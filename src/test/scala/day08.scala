package day08.test
import test.*
import day08.*
given Day = Day(8)
class Test extends DayTest:
  def parts = Seq(
    Part(
      requiredSteps,
      "example1" -> 2,
      "example2" -> 6,
      "input"    -> 20093
    ),
    Part(
      requiredStepsAsGhost,
      "example3" -> Some(6),
      "input"    -> Some(22103062509257L)
    )
  )

import org.openjdk.jmh.annotations.*
@State(Scope.Benchmark)
class Bench:
  val input          = file("input")
  val (steps, nodes) = parseInput(input)

  @Benchmark
  def benchWithParsedInput = requiredStepsAsGhostFromParsed(steps, nodes)

  @Benchmark
  def benchWithUnparsedInput = requiredStepsAsGhost(input)
