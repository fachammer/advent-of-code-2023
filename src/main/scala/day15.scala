package day15

// part 1
def hashValuesSum(input: String) = input.strip.split(',').map(hashValue).sum

def hashValue(input: String) =
  input
    .map(_.toInt)
    .fold(0): (current, next) =>
      ((current + next) * 17) % 256

// part 2
def focusingPowerSum(input: String) =
  initialize(input).zipWithIndex
    .map((box, boxNumber) =>
      box.zipWithIndex.map { case ((_, focalLength), slotIndex) =>
        (boxNumber + 1) * (slotIndex + 1) * focalLength
      }.sum,
    )
    .sum

def initialize(input: String): Array[Vector[(String, Int)]] =
  val steps = input.strip.split(',')
  val boxes = Array.fill(256)(Vector.empty[(String, Int)])
  for step <- steps
  do
    step match
      case s"$label-" =>
        val hash      = hashValue(label)
        val box       = boxes(hash)
        val lensIndex = box.indexWhere((l, _) => l == label)
        if lensIndex != -1 then
          boxes(hash) = box.take(lensIndex) ++ box.drop(lensIndex + 1)

      case s"$label=$focalLength" =>
        val hash             = hashValue(label)
        val focalLengthValue = focalLength.toInt
        val box              = boxes(hash)
        val lensIndex        = box.indexWhere((l, _) => l == label)
        if lensIndex != -1 then
          boxes(hash) = box.updated(lensIndex, (label, focalLengthValue))
        else boxes(hash) = box :+ (label, focalLengthValue)

  boxes
