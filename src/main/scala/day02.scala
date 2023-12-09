package day02

// part 1
given totalCubes: Map[String, Int] =
  Map("red" -> 12, "green" -> 13, "blue" -> 14)

case class Games(val games: Map[Int, Game]):
  def byId(id: Int) = games.get(id).get
case class Game(val id: Int, val shownCubes: Map[String, Int]*)

def sumOfIdsOfPossibleGames(input: String) =
  parseGames(input).games.values.filter(isPossible).map(_.id).sum

def parseGames(s: String) =
  Games(s.linesIterator.map(parseGame).map(g => (g.id, g)).toMap)

def parseGame(s: String): Game =
  s match
    case s"Game $id: $remainder" =>
      val cubeSets = remainder.split("; ")
      val shownCubes =
        for
          cubeSet <- cubeSets
          colorCounts = cubeSet.split(", ")
        yield colorCounts.map { case s"$count $color" => (color, count.toInt) }
          .toMap
      Game(id.toInt, shownCubes*)

def isPossible(game: Game)(using totalCubes: Map[String, Int]): Boolean =
  minimumRequiredCubes(game.shownCubes).forall { (k, v) =>
    v <= totalCubes.getOrElse(k, 0)
  }

// part 2
def minimumRequiredCubes(cubeSet: Seq[Map[String, Int]]) =
  val noCubes = Map("red" -> 0, "green" -> 0, "blue" -> 0)
  cubeSet.fold(noCubes)((cubes, el) =>
    cubes.map((k, v) => (k, v.max(el.getOrElse(k, 0)))),
  )

def cubeSetPower(cubeSet: Seq[Map[String, Int]]) =
  minimumRequiredCubes(cubeSet).foldLeft(1)((x, y) => x * y._2)

def sumOfCubePowers(input: String) =
  parseGames(input).games.map((_, game) => cubeSetPower(game.shownCubes)).sum
