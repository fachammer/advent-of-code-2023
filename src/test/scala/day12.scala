package day12.test
import test.*
import day12.*
given Day = Day(12)
class Test extends DayTest:
  def parts =
    Seq(
      Part(sumOfSpringConfigurations, "example" -> 21, "input" -> 8270),
      Part(
        sumOfUnfoldedSpringConfigurations,
        "example" -> 525152,
        "input"   -> 204640299929836L,
      ),
    )

  "number of spring configurations" - numberOfSpringConfigurations.testCases(
    "a" :> "???.### 1,1,3"                             -> 1,
    "b" :> ".??..??...?##. 1,1,3"                      -> 4,
    "c" :> "?#?#?#?#?#?#?#? 1,3,1,6"                   -> 1,
    "d" :> "????.#...#... 4,1,1"                       -> 1,
    "e" :> "????.######..#####. 1,6,5"                 -> 4,
    "f" :> "?###???????? 3,2,1"                        -> 10,
    "g" :> "?????? 1,1,1"                              -> 4,
    "a unfold" :> (unfold("???.### 1,1,3")             -> 1),
    "b unfold" :> (unfold(".??..??...?##. 1,1,3")      -> 16384),
    "c unfold" :> (unfold("?#?#?#?#?#?#?#? 1,3,1,6")   -> 1),
    "d unfold" :> (unfold("????.#...#... 4,1,1")       -> 16),
    "e unfold" :> (unfold("????.######..#####. 1,6,5") -> 2500),
    "f unfold" :> (unfold("?###???????? 3,2,1")        -> 506250),
  )
