package day15

// part 1
def hashValuesSum(input: String) = input.strip.split(',').map(hashValue).sum

def hashValue(input: String) =
  input.map(_.toInt).fold(0)((current, next) => ((current + next) * 17) % 256)

// part 2
def focusingPower(input: String) =
  import scala.collection.mutable.LinkedHashMap
  val boxes = Array.fill(256)(LinkedHashMap.empty[String, Int])
  val steps = input.strip.split(',')
  steps.foreach:
    case s"$label-"        => boxes(hashValue(label)).remove(label)
    case s"$label=$focLen" => boxes(hashValue(label))(label) = focLen.toInt

  val lensFocusingPowers = for
    (box, boxNumber)              <- boxes.zipWithIndex
    ((_, focalLength), slotIndex) <- box.zipWithIndex
  yield (boxNumber + 1) * (slotIndex + 1) * focalLength

  lensFocusingPowers.sum
