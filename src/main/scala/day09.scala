package day09

// part 1
def extrapolatedSum(input: String) = parseHistories(input).map(extrapolate).sum

def extrapolate(seq: Array[Int]): Int =
  if seq.forall(_ == 0) then 0 else extrapolate(differences(seq)) + seq.last

// part 2
def extrapolatedPreviousSum(input: String) =
  parseHistories(input).map(extrapolatePrevious).sum

def extrapolatePrevious(seq: Array[Int]): Int =
  if seq.forall(_ == 0) then 0
  else seq.head - extrapolatePrevious(differences(seq))

def parseHistories(s: String) = s.linesIterator.map(_.split(" ").map(_.toInt))
def differences(seq: Array[Int]) = seq.drop(1).zip(seq).map(_ - _)
