object day01:
  given Day = 1

  def sumOfCalibrationValues(input: String) =
    input.linesIterator.map(calibrationValue).sum

  def calibrationValue(line: String): Int =
    val digits = line
      .toCharArray()
      .filter(Character.isDigit)
      .map(Character.digit(_, 10))

    s"${digits.head}${digits.last}".toInt

  def sumOfCalibrationValuesIncludingWords(input: String) =
    input.linesIterator.map(calibrationValueIncludingWords).sum

  def calibrationValueIncludingWords(line: String) =
    val digits = Map(
      "one"   -> "1",
      "two"   -> "2",
      "three" -> "3",
      "four"  -> "4",
      "five"  -> "5",
      "six"   -> "6",
      "seven" -> "7",
      "eight" -> "8",
      "nine"  -> "9"
    )

    extension (m: String) def toDigit = digits.get(m).getOrElse(m)

    val digitPattern = (digits.keys ++ Seq("[0-9]")).mkString("|");
    val firstDigit   = digitPattern.r.findFirstIn(line).get.toDigit
    val lastDigit = s".*($digitPattern)".r
      .findFirstMatchIn(line)
      .get
      .group(1)
      .toDigit

    s"$firstDigit$lastDigit".toInt

  sumOfCalibrationValues.testCases(
    file("example1") -> 142,
    file("input")    -> 56042
  )

  sumOfCalibrationValuesIncludingWords.testCases(
    file("example2") -> 281,
    "123123\n112"    -> 25,
    file("input")    -> 55358
  )

  calibrationValueIncludingWords.testCases(
    "two1nine"        -> 29,
    "eightwothree"    -> 83,
    "abcone2threexyz" -> 13
  )
