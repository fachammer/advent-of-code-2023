package day19.test
import test.{Day, DayTest, Part}
import day19.*
given Day = Day(19)
class Test extends DayTest:
  def parts = Seq(
    Part(sumOfRatingsOfAcceptedParts, "example" -> 19114, "input" -> 409898),
    Part(
      acceptedCombinations,
      "example" -> 167409079868000L,
      "input"   -> 113057405770956L,
    ),
  )
