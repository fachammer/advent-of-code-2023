package day02

import test.*

class Test extends DayTest(2):
  val exampleGames = parseGames(file("example"))
  val inputGames   = parseGames(file("input"))

  def parts = Seq(
    Part(sumOfIdsOfPossibleGames, "example" -> 8, "input"    -> 2439),
    Part(sumOfCubePowers, "example"         -> 2286, "input" -> 63711)
  )

  "parseGame" - {
    parseGame.testCases(
      "example 1" :>
        "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green" ->
        Game(
          1,
          Map("blue"  -> 3, "red"   -> 4),
          Map("red"   -> 1, "green" -> 2, "blue" -> 6),
          Map("green" -> 2)
        ),
      "example 4" :>
        "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red" ->
        Game(
          4,
          Map("green" -> 1, "red"  -> 3, "blue" -> 6),
          Map("green" -> 3, "red"  -> 6),
          Map("green" -> 3, "blue" -> 15, "red" -> 14)
        )
    )
  }
  "isPossible" - {
    isPossible.testCases(
      "Game 1" :> exampleGames.byId(1) -> true,
      "Game 2" :> exampleGames.byId(2) -> true,
      "Game 3" :> exampleGames.byId(3) -> false,
      "Game 4" :> exampleGames.byId(4) -> false,
      "Game 5" :> exampleGames.byId(5) -> true
    )
  }

  "minimumRequiredCubes" - {
    minimumRequiredCubes.testCases(
      "Game 1" :> exampleGames
        .byId(1)
        .shownCubes -> Map("red" -> 4, "green" -> 2, "blue" -> 6)
    )
  }

  "cubeSetPower" - {
    cubeSetPower.testCases(
      "Game 1" :> exampleGames.byId(1).shownCubes -> 48,
      "Game 2" :> exampleGames.byId(2).shownCubes -> 12,
      "Game 3" :> exampleGames.byId(3).shownCubes -> 1560,
      "Game 4" :> exampleGames.byId(4).shownCubes -> 630,
      "Game 5" :> exampleGames.byId(5).shownCubes -> 36
    )
  }
