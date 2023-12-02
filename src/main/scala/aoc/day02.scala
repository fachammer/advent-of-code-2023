object day02:
  given Day              = 2
  val totalCubes         = Map("red" -> 12, "green" -> 13, "blue" -> 14)
  given Map[String, Int] = totalCubes
  val exampleGames       = parse(file("example"))
  val inputGames         = parse(file("input"))

  sumOfIdsOfPossibleGames.testCases(file("example") -> 8, file("input") -> 2439)
  def sumOfIdsOfPossibleGames(input: String) =
    val games = parse(input)
    games.games.values.filter(isPossible).map(_.id).sum

  case class Games(val games: Map[Int, Game]):
    def byId(id: Int) = games.get(id).get

  case class Game(val id: Int, val shownCubes: Seq[Map[String, Int]])

  def parse(input: String) =
    Games(input.linesIterator.map(parseGame).map(game => (game.id, game)).toMap)

  parseGame.testCases(
    "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green" -> Game(
      1,
      Seq(
        Map("blue"  -> 3, "red"   -> 4),
        Map("red"   -> 1, "green" -> 2, "blue" -> 6),
        Map("green" -> 2)
      )
    ),
    "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red" -> Game(
      4,
      Seq(
        Map("green" -> 1, "red"  -> 3, "blue" -> 6),
        Map("green" -> 3, "red"  -> 6),
        Map("green" -> 3, "blue" -> 15, "red" -> 14)
      )
    )
  )
  def parseGame(inputLine: String): Game = inputLine match
    case s"Game $id: $remainder" =>
      val cubeSets = remainder.split("; ")
      val shownCubes = cubeSets.map { cubeSet =>
        val colorCounts = cubeSet.split(", ")
        colorCounts.map { case s"$count $color" => (color, count.toInt) }.toMap
      }.toSeq
      Game(id.toInt, shownCubes)

  isPossible.testCases(
    exampleGames.byId(1) -> true,
    exampleGames.byId(2) -> true,
    exampleGames.byId(3) -> false,
    exampleGames.byId(4) -> false,
    exampleGames.byId(5) -> true
  )
  def isPossible(game: Game)(using totalCubes: Map[String, Int]): Boolean =
    minimumRequiredCubes(game.shownCubes).forall { (k, v) =>
      v <= totalCubes.getOrElse(k, 0)
    }

  minimumRequiredCubes.testCases(
    exampleGames
      .byId(1)
      .shownCubes -> Map("red" -> 4, "green" -> 2, "blue" -> 6)
  )
  def minimumRequiredCubes(cubeSet: Seq[Map[String, Int]]) =
    val noCubes = Map("red" -> 0, "green" -> 0, "blue" -> 0)
    cubeSet.fold(noCubes)((cubes, el) =>
      cubes.map((k, v) => (k, v.max(el.getOrElse(k, 0))))
    )

  cubeSetPower.testCases(
    exampleGames.byId(1).shownCubes -> 48,
    exampleGames.byId(2).shownCubes -> 12,
    exampleGames.byId(3).shownCubes -> 1560,
    exampleGames.byId(4).shownCubes -> 630,
    exampleGames.byId(5).shownCubes -> 36
  )
  def cubeSetPower(cubeSet: Seq[Map[String, Int]]) =
    minimumRequiredCubes(cubeSet).foldLeft(1)((x, y) => x * y._2)

  sumOfCubePowers.testCases(exampleGames -> 2286, inputGames -> 63711)
  def sumOfCubePowers(games: Games) =
    games.games.map((_, game) => cubeSetPower(game.shownCubes)).sum
