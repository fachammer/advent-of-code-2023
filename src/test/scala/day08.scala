class Day08 extends DayTest(8):
  import day08.*
  def parts = Seq(
    Part(requiredSteps, "example1" -> 2, "example2" -> 6, "input" -> 20093),
    Part(
      requiredStepsAsGhost,
      "example3" -> Some(6),
      "input"    -> Some(22103062509257L)
    )
  )
