package day19

// part 1
def sumOfRatingsOfAcceptedParts(input: String) =
  val (workflowsString, partsString) = input.split("\n\n") match
    case Array(w, p) => (w, p)

  val workflows = Workflows.parse(workflowsString)
  val parts     = partsString.linesIterator.map(Part.parse).toSeq
  parts.filter(workflows.isAccepted).map(_.values.sum).sum

enum Condition:
  case GreaterThan(categoryName: Char, amount: Int)
  case LessThan(categoryName: Char, amount: Int)

  def category = this match
    case GreaterThan(category, _) => category
    case LessThan(category, _)    => category

  def negated = this match
    case GreaterThan(category, amount) => LessThan(category, amount + 1)
    case LessThan(category, amount)    => GreaterThan(category, amount - 1)
import Condition.*

case class Interval(exclusiveMin: Int, exclusiveMax: Int):
  val isEmpty                = exclusiveMax <= exclusiveMin + 1
  val length                 = (exclusiveMax - exclusiveMin - 1).max(0)
  def contains(element: Int) = exclusiveMin < element && element < exclusiveMax

enum RuleAction:
  case Accept, Reject
  case NextWorkflow(workflow: String)
import RuleAction.*
object RuleAction:
  def parse(input: String) = input match
    case "A"      => Accept
    case "R"      => Reject
    case workflow => NextWorkflow(workflow)

case class Rule(condition: Condition, action: RuleAction)

case class Workflow(name: String, fallbackAction: RuleAction, rules: Seq[Rule])
object Workflow:
  def parse(line: String) =
    val s"$name{$rulesString}" = line: @unchecked
    val rules                  = rulesString.split(",")
    val strictRules = rules.init.map:
      case s"$category<$amount:$action" =>
        Rule(LessThan(category.head, amount.toInt), RuleAction.parse(action))
      case s"$category>$amount:$action" =>
        Rule(GreaterThan(category.head, amount.toInt), RuleAction.parse(action))

    Workflow(name, RuleAction.parse(rules.last), strictRules.toSeq)

type Part = Map[Char, Int]
object Part:
  def parse(input: String) =
    val s"{$ratingsString}" = input: @unchecked
    def parseRating(input: String) =
      val (key, value) = input.span(_ != '=')
      (key.head, value.tail.toInt)

    ratingsString.split(",").map(parseRating).toMap

case class Workflows(workflows: Map[String, Workflow]):
  val constraints =
    acceptanceConditions(this, "in").map(constraintsByCategory)

  def isAccepted(part: Part) =
    constraints.exists: intervals =>
      part.forall: (category, value) =>
        intervals(category).contains(value)

  def apply(workflowName: String) = workflows(workflowName)

  def acceptedCombinations =
    def constraintSatisfyingCombinations(constraint: Map[Char, Interval]) =
      constraint.values.map(_.length.toLong).product

    // we can just sum them all up, since they are distjoint by construction
    constraints.map(constraintSatisfyingCombinations).sum

object Workflows:
  def parse(input: String): Workflows = Workflows:
    input.linesIterator.map(Workflow.parse).map(w => (w.name, w)).toMap

def acceptanceConditions(
    workflows: Workflows,
    workflowName: String,
): Iterable[Set[Condition]] =
  val workflow              = workflows(workflowName)
  val nonEmptyRulesPrefixes = workflow.rules.inits
  Iterable.from:
    nonEmptyRulesPrefixes.flatMap: rulesInits =>
      val action =
        if rulesInits.isEmpty then workflow.fallbackAction
        else rulesInits.last.action
      val ruleConditions =
        if rulesInits.isEmpty then None
        else Some(rulesInits.last.condition)
      val previousRules =
        if rulesInits.isEmpty then workflow.rules else rulesInits.init

      val prefixNegated = previousRules.map(_.condition.negated)
      action match
        case Accept => Seq(prefixNegated.toSet ++ ruleConditions)
        case Reject => Seq.empty
        case NextWorkflow(workflow) =>
          for conds <- acceptanceConditions(workflows, workflow)
          yield conds ++ prefixNegated ++ ruleConditions

def constraintsByCategory(conditions: Set[Condition]): Map[Char, Interval] =
  val minExclusive = 0
  val maxExclusive = 4001
  val default      = Interval(minExclusive, maxExclusive)
  val defaultMap =
    Map('x' -> default, 'm' -> default, 'a' -> default, 's' -> default)

  defaultMap.concat:
    for (category, categoryConditions) <- conditions.groupBy(_.category)
    yield
      val (upperBounds, lowerBounds) = categoryConditions.partitionMap:
        case LessThan(_, amount)    => Left(amount)
        case GreaterThan(_, amount) => Right(amount)

      val leastUpperBound    = upperBounds.minOption.getOrElse(maxExclusive)
      val greatestLowerBound = lowerBounds.maxOption.getOrElse(minExclusive)

      (category, Interval(greatestLowerBound, leastUpperBound))

// part 2
def acceptedCombinations(input: String) =
  val workflowsString = input.split("\n\n").head
  Workflows.parse(workflowsString).acceptedCombinations
