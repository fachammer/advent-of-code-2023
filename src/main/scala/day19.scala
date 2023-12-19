package day19

// part 1
def sumOfRatingsOfAcceptedParts(input: String) =
  val (workflowsString, partsString) = input.split("\n\n") match
    case Array(w, p) => (w, p)

  val workflows =
    workflowsString.linesIterator.map(Workflow.parse).map(w =>
      (w.name, w),
    ).toMap
  val parts = partsString.linesIterator.map(Part.parse).toSeq

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

enum Condition:
  case AlwaysFalse
  case AlwaysTrue
  case CategoryGreaterThan(category: Char, amount: Int)
  case CategoryLessThan(category: Char, amount: Int)

  def categoryField = this match
    case AlwaysFalse                           => None
    case AlwaysTrue                            => None
    case CategoryGreaterThan(category, amount) => Some(category)
    case CategoryLessThan(category, amount)    => Some(category)

  def apply(part: Part) = this match
    case AlwaysFalse                           => false
    case AlwaysTrue                            => true
    case CategoryGreaterThan(category, amount) => part(category) > amount
    case CategoryLessThan(category, amount)    => part(category) < amount

  def unary_- = this match
    case AlwaysFalse => AlwaysTrue
    case AlwaysTrue  => AlwaysFalse
    case CategoryGreaterThan(category, amount) =>
      CategoryLessThan(category, amount + 1)
    case CategoryLessThan(category, amount) =>
      CategoryGreaterThan(category, amount - 1)

case class Interval(exclusiveMin: Int, exclusiveMax: Int):
  val isEmpty                = exclusiveMax <= exclusiveMin + 1
  val length                 = (exclusiveMax - exclusiveMin - 1).max(0)
  def contains(element: Int) = exclusiveMin < element && element < exclusiveMax
  def contains(other: Interval) = exclusiveMin <= other.exclusiveMin
    && other.exclusiveMax <= exclusiveMax
  def isDisjointTo(other: Interval) =
    Interval(exclusiveMin, other.exclusiveMax).isEmpty ||
      Interval(other.exclusiveMin, exclusiveMax).isEmpty

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
        Rule(
          Condition.CategoryLessThan(category.head, amount.toInt),
          RuleAction.parse(action),
        )
      case s"$category>$amount:$action" =>
        Rule(
          Condition.CategoryGreaterThan(category.head, amount.toInt),
          RuleAction.parse(action),
        )
      case action =>
        Rule(Condition.AlwaysTrue, RuleAction.parse(action))
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

def acceptedCombinations(input: String) =
  val workflowsString = input.split("\n\n").head

  val workflows = workflowsString.linesIterator
    .map(Workflow.parse)
    .map(w => (w.name, w))
    .toMap

  def acceptanceConditions(workflow: Workflow): Seq[Seq[Condition]] =
    val nonEmptyRulesPrefixes = workflow.rules.inits.take(workflow.rules.length)
    val conditions = nonEmptyRulesPrefixes.flatMap: rulesInits =>
      val rule            = rulesInits.last
      val previousNegated = rulesInits.init.map(r => -r.condition)
      rule.action match
        case RuleAction.Accept => Seq(previousNegated :+ rule.condition)
        case RuleAction.Reject => Seq.empty
        case RuleAction.NextWorkflow(workflow) =>
          acceptanceConditions(workflows(workflow)).map(s =>
            s ++ (previousNegated :+ rule.condition),
          )
    conditions.toSeq.map(_.filterNot(_ == Condition.AlwaysTrue))

  type CategoryConstraints = Map[Char, Interval]
  def simplify(conditions: Seq[Condition]): CategoryConstraints =
    val default = Interval(0, 4001)
    val defaultMap =
      Map('x' -> default, 'm' -> default, 'a' -> default, 's' -> default)

    val conditionsMap = conditions.groupBy(_.categoryField).map { (c, s) =>
      val (lessThan, greaterThan) = s.partitionMap {
        case Condition.CategoryLessThan(_, amount)    => Left(amount)
        case Condition.CategoryGreaterThan(_, amount) => Right(amount)
        case _                                        => ???
      }
      val lessThanMin    = lessThan.minOption.getOrElse(4001)
      val greaterThanMax = greaterThan.maxOption.getOrElse(0)

      (c.get, Interval(greaterThanMax, lessThanMin))
    }.toMap

    defaultMap.concat(conditionsMap)

  extension (c: CategoryConstraints)
    def acceptedParts = c.values.map(_.length.toLong).product

  val conditions = acceptanceConditions(workflows("in"))
  conditions.find(s => s.contains(Condition.AlwaysFalse))
  val asConstraints = conditions.map(simplify)
  asConstraints.map(_.acceptedParts).sum
