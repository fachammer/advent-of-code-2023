package day09

// part 1
def extrapolatedValuesSum(input: String) =
  val histories = input.linesIterator.map(_.split(" ").map(_.toInt).toSeq)
  histories.map(extrapolate).sum

def extrapolate(sequence: Seq[Int]): Int =
  if sequence.forall(_ == 0) then 0
  else extrapolate(differences(sequence)) + sequence.last

def differences(sequence: Seq[Int]): Seq[Int] =
  sequence.drop(1).zip(sequence).map(_ - _)
