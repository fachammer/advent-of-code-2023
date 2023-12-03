package day01

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
