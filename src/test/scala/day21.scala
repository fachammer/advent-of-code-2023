package day21.test
import test.{Day, DayTest, Part}
import day21.*
given Day = Day(21)
class Test extends DayTest:
  def parts = Seq(
    Part(reachableGardenPlotsOnFiniteLand(6), "example"      -> 16),
    Part(reachableGardenPlotsOnFiniteLand(64), "input"       -> 3733),
    Part(reachableGardenPlotsOnInfiniteLand(6), "example"    -> 16),
    Part(reachableGardenPlotsOnInfiniteLand(10), "example"   -> 50),
    Part(reachableGardenPlotsOnInfiniteLand(50), "example"   -> 1594),
    Part(reachableGardenPlotsOnInfiniteLand(100), "example"  -> 6536),
    Part(reachableGardenPlotsOnInfiniteLand(500), "example"  -> 167004),
    Part(reachableGardenPlotsOnInfiniteLand(1000), "example" -> 668697),
    Part(reachableGardenPlotsOnInfiniteLand(5000), "example" -> 16733044),
    Part(
      reachableGardenPlotsWithSpecialInput((26501365 - 65) / 131),
      "input" -> 617729401414635L,
    ),
  )
