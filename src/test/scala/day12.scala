package day12.test
import test.*
import day12.*
given Day = Day(12)
class Test extends DayTest:
  def parts =
    Seq(Part(sumOfSpringConfigurations, "example" -> 21, "input" -> 8270))

  "number of spring configurations" - numberOfSpringConfigurations.testCases(
    "a" :> "???.### 1,1,3"             -> 1,
    "b" :> ".??..??...?##. 1,1,3"      -> 4,
    "c" :> "?#?#?#?#?#?#?#? 1,3,1,6"   -> 1,
    "d" :> "????.#...#... 4,1,1"       -> 1,
    "e" :> "????.######..#####. 1,6,5" -> 4,
    "f" :> "?###???????? 3,2,1"        -> 10,
  )
