def sumOfCalibrationValues(input: String): Int =
  input.lines().mapToInt(calibrationValue).sum()

def calibrationValue(line: String): Int =
  val firstDigit =
    line
      .toCharArray()
      .filter(Character.isDigit)
      .map(Character.digit(_, 10))
      .apply(0)
  val lastDigit = line
    .toCharArray()
    .filter(Character.isDigit)
    .map(Character.digit(_, 10))
    .lastOption

  (firstDigit.toString() + lastDigit.get).toInt

def sumOfCalibrationValuesWithDigitNames(input: String): Int =
  input.lines().mapToInt(calibrationValueWithDigitNames).sum()

def calibrationValueWithDigitNames(line: String): Int =
  val digitPattern =
    "(on(?=e)|tw(?=o)|thre(?=e)|four|fiv(?=e)|six|seve(?=n)|eigh(?=t)|nin(?=e)|[0-9])".r
  val digits = digitPattern
    .findAllIn(line)
    .map(_ match {
      case x if x.startsWith("on")   => 1
      case x if x.startsWith("tw")   => 2
      case x if x.startsWith("thre") => 3
      case x if x.startsWith("four") => 4
      case x if x.startsWith("fiv")  => 5
      case x if x.startsWith("six")  => 6
      case x if x.startsWith("seve") => 7
      case x if x.startsWith("eigh") => 8
      case x if x.startsWith("nin")  => 9
      case x                         => x.toInt
    })
    .toArray
  val firstDigit = digits.apply(0)
  val lastDigit = digits.last

  (firstDigit.toString() + lastDigit.toString()).toInt
