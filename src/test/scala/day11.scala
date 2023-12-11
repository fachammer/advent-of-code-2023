package day11.test

import test.*
import day11.*

given Day = Day(11)
class Test extends DayTest:
  def parts =
    Seq(Part(sumOfShortestPaths, "example" -> 374, "input" -> 9536038))
