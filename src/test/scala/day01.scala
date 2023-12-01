package day01

class part1 extends test.DayPart[Int](1, 1):
  testFile("example1", 142)
  testFile("input", 56042)
  def run(input: String) =
    input.linesIterator.map(calibrationValue).sum

  def calibrationValue(line: String): Int =
    val digits = line
      .toCharArray()
      .filter(Character.isDigit)
      .map(Character.digit(_, 10))

    s"${digits.head}${digits.last}".toInt

class part2 extends test.DayPart[Int](1, 2):
  testFile("example2", 281)
  testFile("input", 55358)
  def run(input: String) =
    input.linesIterator.map(calibrationValue).sum

  testFn(
    calibrationValue,
    "two1nine" -> 29,
    "eightwothree" -> 83,
    "abcone2threexyz" -> 13
  )
  def calibrationValue(line: String) =
    val digits = Map(
      "one" -> "1",
      "two" -> "2",
      "three" -> "3",
      "four" -> "4",
      "five" -> "5",
      "six" -> "6",
      "seven" -> "7",
      "eight" -> "8",
      "nine" -> "9"
    )

    extension (m: String) def toDigit = digits.get(m).getOrElse(m)

    val digitPattern = (digits.keys ++ Seq("[0-9]")).mkString("|");
    val firstDigit = digitPattern.r.findFirstIn(line).get.toDigit
    val lastDigit = s".*($digitPattern)".r
      .findFirstMatchIn(line)
      .get
      .group(1)
      .toDigit

    s"$firstDigit$lastDigit".toInt