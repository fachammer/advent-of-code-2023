object day02:
  given Day      = 2
  val totalCubes = Map(Color.Red -> 12, Color.Green -> 13, Color.Blue -> 14)
  given Map[Color, Int] = totalCubes
  val exampleGames      = parse(file("example"))
  val inputGames        = parse(file("input"))

  sumOfIdsOfPossibleGames.testCases(file("example") -> 8, file("input") -> 2439)
  def sumOfIdsOfPossibleGames(input: String) =
    val games = parse(input)
    games.games.values.filter(isPossible).map(_.id).sum

  enum Color:
    case Red
    case Green
    case Blue

  extension (s: String)
    def toColor = s match
      case "red"   => Color.Red
      case "green" => Color.Green
      case "blue"  => Color.Blue

  case class Games(val games: Map[Int, Game]):
    def byId(id: Int) = games.get(id).get

  case class Game(val id: Int, val shownCubes: Seq[Map[Color, Int]])

  def parse(input: String) =
    Games(input.linesIterator.map(parseGame).map(game => (game.id, game)).toMap)

  parseGame.testCases(
    "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green" -> Game(
      1,
      Seq(
        Map(Color.Blue  -> 3, Color.Red   -> 4),
        Map(Color.Red   -> 1, Color.Green -> 2, Color.Blue -> 6),
        Map(Color.Green -> 2)
      )
    ),
    "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red" -> Game(
      4,
      Seq(
        Map(Color.Green -> 1, Color.Red  -> 3, Color.Blue -> 6),
        Map(Color.Green -> 3, Color.Red  -> 6),
        Map(Color.Green -> 3, Color.Blue -> 15, Color.Red -> 14)
      )
    )
  )
  def parseGame(inputLine: String): Game =
    val splits    = inputLine.split(": ", 2)
    val gamePart  = splits.head
    val remainder = splits.last
    val id        = gamePart.split(" ").last.toInt
    val shownCubes = remainder
      .split("; ")
      .map(
        _.split(", ")
          .map(colorCount => {
            val splits = colorCount.split(" ")
            val count  = splits.head.toInt
            val color  = splits.last.toColor
            (color, count)
          })
          .toMap
      )
      .toSeq
    Game(id, shownCubes)

  isPossible.testCases(
    exampleGames.byId(1) -> true,
    exampleGames.byId(2) -> true,
    exampleGames.byId(3) -> false,
    exampleGames.byId(4) -> false,
    exampleGames.byId(5) -> true
  )
  def isPossible(game: Game)(using totalCubes: Map[Color, Int]): Boolean =
    maxCubes(game.shownCubes).forall((k, v) => v <= totalCubes.getOrElse(k, 0))

  maxCubes.testCases(
    exampleGames
      .byId(1)
      .shownCubes -> Map(Color.Red -> 4, Color.Green -> 2, Color.Blue -> 6)
  )
  def maxCubes(cubeSet: Seq[Map[Color, Int]]) =
    cubeSet.fold(
      Map(Color.Red -> 0, Color.Green -> 0, Color.Blue -> 0)
    )((map, el) => map.map((k, v) => (k, v.max(el.getOrElse(k, 0)))))

  cubeSetPower.testCases(
    exampleGames.byId(1).shownCubes -> 48,
    exampleGames.byId(2).shownCubes -> 12,
    exampleGames.byId(3).shownCubes -> 1560,
    exampleGames.byId(4).shownCubes -> 630,
    exampleGames.byId(5).shownCubes -> 36
  )
  def cubeSetPower(cubeSet: Seq[Map[Color, Int]]) =
    maxCubes(cubeSet).foldLeft(1)((x, y) => x * y._2)

  sumOfCubePowers.testCases(exampleGames -> 2286, inputGames -> 63711)
  def sumOfCubePowers(games: Games) =
    games.games.map((_, game) => cubeSetPower(game.shownCubes)).sum
