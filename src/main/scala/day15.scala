package day15

import debug.*

// part 1
def hashValuesSum(input: String) = input.strip.split(',').map(hashValue).sum

def hashValue(input: String) =
  input
    .map(_.toInt)
    .fold(0): (current, next) =>
      ((current + next) * 17) % 256
