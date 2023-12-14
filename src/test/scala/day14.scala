package day14.test
import test.*
import day14.*
import org.openjdk.jmh.annotations.Benchmark
given Day = Day(14)
class Test extends DayTest:
  def parts =
    Seq(
      Part(totalLoadWhenTiltingNorth, "example" -> 136, "input" -> 108889),
      Part(
        totalLoadAfterCycles(1_000_000_000),
        "example" -> 64,
        "input"   -> 104671,
      ),
    )

class Bench:
  @Benchmark
  def bench = totalLoadAfterCycles(1_000_000_000)(file("input"))
