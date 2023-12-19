package day19

import debug.*

// part 1
def sumOfRatingsOfAcceptedParts(input: String) =
  val (workflowsString, partsString) = input.split("\n\n") match
    case Array(w, p) => (w, p)

  val workflows =
    workflowsString.linesIterator.map(Workflow.parse).map(w =>
      (w.name, w),
    ).toMap
  val parts = partsString.linesIterator.map(Part.parse).toSeq.d

  def accepted(part: Part): Boolean =
    var workflow = workflows("in")
    while true do
      workflow(part) match
        case RuleAction.Accept => return true
        case RuleAction.Reject => return false
        case RuleAction.NextWorkflow(next) =>
          workflow = workflows(next)
    false

  val acceptedParts = parts.filter(accepted)
  acceptedParts.map(_.map.values.sum).sum

type Condition = Function[Part, Boolean]
enum RuleAction:
  case Accept
  case Reject
  case NextWorkflow(workflow: String)
object RuleAction:
  def parse(input: String) = input match
    case "A"      => Accept
    case "R"      => Reject
    case workflow => NextWorkflow(workflow)
case class Rule(condition: Condition, action: RuleAction)
case class Workflow(name: String, rules: Seq[Rule]):
  def apply(part: Part) =
    rules.find(r => r.condition(part)).map(_.action).get
object Workflow:
  def parse(line: String) =
    val s"$name{$rulesString}" = line: @unchecked
    val rules = rulesString.split(",").map {
      case s"$category<$amount:$action" =>
        Rule(p => p(category.head) < amount.toInt, RuleAction.parse(action))
      case s"$category>$amount:$action" =>
        Rule(p => p(category.head) > amount.toInt, RuleAction.parse(action))
      case action => Rule(_ => true, RuleAction.parse(action))
    }
    Workflow(name, rules.toSeq)

case class Part(map: Map[Char, Int]):
  def apply(category: Char) = map(category)
object Part:
  def parse(input: String) =
    Part(input match
      case s"{$ratingsString}" =>
        ratingsString.split(",").map: ratingString =>
          val (key, value) = ratingString.span(_ != '=')
          (key.head, value.tail.toInt)
        .toMap,
    )
