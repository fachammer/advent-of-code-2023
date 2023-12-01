def sumOfCalibrationValues(input: String) =
  input.linesIterator.map(calibrationValue).sum

def calibrationValue(line: String): Int =
  val digits = line
    .toCharArray()
    .filter(Character.isDigit)
    .map(Character.digit(_, 10))

  s"${digits.head}${digits.last}".toInt

def sumOfCalibrationValuesWithDigitNames(input: String) =
  input.linesIterator.map(calibrationValueWithDigitNames).sum

def calibrationValueWithDigitNames(line: String) =
  extension (m: String)
    def toDigit = m match
      case "one"   => 1
      case "two"   => 2
      case "three" => 3
      case "four"  => 4
      case "five"  => 5
      case "six"   => 6
      case "seven" => 7
      case "eight" => 8
      case "nine"  => 9
      case x       => x.toInt

  val firstDigit = "(one|two|three|four|five|six|seven|eight|nine|[0-9])".r
    .findFirstIn(line)
    .get
    .toDigit
  val lastDigit =
    "(eno|owt|eerht|ruof|evif|xis|neves|thgie|enin|[0-9])".r
      .findFirstIn(line.reverse)
      .get
      .reverse
      .toDigit

  s"$firstDigit$lastDigit".toInt
