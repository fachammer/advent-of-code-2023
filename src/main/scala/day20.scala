package day20

import debug.*
import scala.collection.mutable

// part 1
def part1(input: String) =
  val pulseNetwork = PulseNetwork.parse(input)
  val (low, high) =
    (1 to 1000).map(_ => pulseNetwork.pushButton("")).foldLeft((0, 0)):
      case ((accLow, accHigh), (low, high)) => (accLow + low, accHigh + high)
      case _                                => ???
  low * high

enum Pulse:
  case Low, High
case class PulseNetwork(
    outputs: mutable.HashMap[String, mutable.Buffer[String]],
    inputs: mutable.HashMap[String, mutable.Buffer[String]],
    moduleTypes: mutable.HashMap[String, ModuleType],
):
  private val states = moduleTypes.map {
    case (name, ModuleType.FlipFlop) => (name, ModuleState.FlipFlop(false))
    case (name, ModuleType.Conjunction) =>
      (
        name,
        ModuleState.Conjunction(
          mutable.HashMap(inputs(name).map(i => (i, true)).toSeq*),
        ),
      )
    case (name, _) => (name, ModuleState.None)
  }

  val lastIncomingPulses = mutable.HashMap[String, Pulse]()

  def stateView = states.toMap.view

  def pushButton(notifyIfLowOn: String): (Int, Int) | "found" =
    val queue          = mutable.Queue(("button", true, "broadcaster"))
    var lowPulsesSent  = 0
    var highPulsesSent = 0
    while queue.nonEmpty do
      val (pulseSource, pulseIsLow, pulseDestination) = queue.dequeue()
      lastIncomingPulses(pulseDestination) =
        if pulseIsLow then Pulse.Low else Pulse.High

      // println:
      /* s"$pulseSource -${if pulseIsLow then "low" else "high"}->
       * $pulseDestination" */

      if pulseIsLow then lowPulsesSent += 1
      else highPulsesSent += 1

      if moduleTypes.contains(pulseDestination) then
        assert(states.contains(pulseDestination))
        (moduleTypes(pulseDestination), states(pulseDestination)) match
          case (ModuleType.FlipFlop, ModuleState.FlipFlop(on)) =>
            if pulseIsLow then
              states(pulseDestination) = ModuleState.FlipFlop(!on)
              for output <- outputs(pulseDestination) do
                queue.enqueue((pulseDestination, on, output))

          case (ModuleType.Conjunction, ModuleState.Conjunction(inputs)) =>
            inputs(pulseSource) = pulseIsLow
            val isOutgoingPulseLow = inputs.forall((_, isLow) => !isLow)
            for output <- outputs(pulseDestination) do
              queue.enqueue((pulseDestination, isOutgoingPulseLow, output))

          case (ModuleType.Broadcaster, ModuleState.None) =>
            for output <- outputs(pulseDestination) do
              queue.enqueue((pulseDestination, pulseIsLow, output))

          case _ => ???
      if pulseDestination == notifyIfLowOn && pulseIsLow then return "found"

    (lowPulsesSent, highPulsesSent)

  def buttonPressesUntilRxIsLow =
    assert(inputs("rx").size == 1)
    val rxInput = inputs("rx").head
    assert(moduleTypes(rxInput) == ModuleType.Conjunction)
    buttonPressesUntilStateFromDefault(
      Map(
        rxInput ->
          ModuleState.Conjunction(
            mutable.HashMap(inputs(rxInput).map(name => (name, false)).toSeq*),
          ),
      ),
    )

  def modules =
    inputs.map((name, inputs) =>
      Module(
        name,
        moduleTypes.get(name),
        states.get(name).getOrElse(ModuleState.None),
        inputs.toSeq,
        outputs.get(name).map(_.toSeq).getOrElse(Seq.empty),
      ),
    )

  def buttonPressesUntilStateFromDefault(states: Map[String, ModuleState]) =
    ???

  def printFrom(module: String, depth: Int = 0): Unit =
    if depth > 10 then return

    val indent = " ".repeat(depth)
    val stringBuilder = mutable.StringBuilder:
      s"$indent$module (last in pulse: ${lastIncomingPulses.get(module)})\n"
    println(stringBuilder.mkString)
    inputs.get(module).getOrElse(Seq.empty).foreach: input =>
      printFrom(input, depth + 1)
    // stringBuilder.mkString

object PulseNetwork:
  def parse(input: String) =
    val outputs     = mutable.HashMap[String, mutable.Buffer[String]]()
    val inputs      = mutable.HashMap[String, mutable.Buffer[String]]()
    val moduleTypes = mutable.HashMap[String, ModuleType]()
    for line <- input.linesIterator do
      def updateInputsOutputs(name: String, outputModules: String) =
        for module <- outputModules.split(", ") do
          outputs.getOrElseUpdate(name, mutable.Buffer[String]()).append(module)
          inputs.getOrElseUpdate(module, mutable.Buffer[String]()).append(name)
      line match
        case s"%$name -> $outputModules" =>
          updateInputsOutputs(name, outputModules)
          moduleTypes.put(name, ModuleType.FlipFlop)
        case s"&$name -> $outputModules" =>
          updateInputsOutputs(name, outputModules)
          moduleTypes.put(name, ModuleType.Conjunction)
        case s"broadcaster -> $outputModules" =>
          updateInputsOutputs("broadcaster", outputModules)
          moduleTypes.put("broadcaster", ModuleType.Broadcaster)

    PulseNetwork(outputs, inputs, moduleTypes)

enum ModuleType:
  case FlipFlop, Conjunction, Broadcaster

enum ModuleState:
  case FlipFlop(on: Boolean)
  case Conjunction(inputs: mutable.HashMap[String, Boolean])
  case None

case class Module(
    name: String,
    moduleType: Option[ModuleType],
    moduleState: ModuleState,
    inputs: Seq[String],
    outputs: Seq[String],
)

def part2(input: String): Long =
  val pulseNetwork = PulseNetwork.parse(input)
  pulseNetwork.inputs("broadcaster") = mutable.Buffer("button")
  pulseNetwork.moduleTypes("broadcaster") = ModuleType.Broadcaster
  pulseNetwork.d
  val nodes = pulseNetwork.inputs.keys.map: name =>
    val nodeName = pulseNetwork.moduleTypes.get(name) match
      case None                         => name
      case Some(ModuleType.Broadcaster) => "broadcaster"
      case Some(ModuleType.FlipFlop)    => s"$name [fontcolor=blue]"
      case Some(ModuleType.Conjunction) => s"$name [fontcolor=red]"
    nodeName
  .mkString("\n")
  val edges = pulseNetwork.inputs.keys.map: name =>
    if pulseNetwork.outputs.get(name).isDefined then
      s"$name -> ${pulseNetwork.outputs.get(name).getOrElse(Seq.empty).mkString(",")}"
    else ""
  .mkString("\n")

  println(s"\ndigraph {$nodes\n$edges}")

  // pulseNetwork.printFrom("rx")
  // println(pulseNetwork.mkStringFrom("rx"))
  // var maxHigh       = 0
  // var buttonPresses = 0L
  // while true do
  //   buttonPresses += 1
  //   // lcm(3793,3733,4057,3947)
  //   pulseNetwork.pushButton("zf") match
  //     case "found"     => return buttonPresses
  //     case (low, high) =>

  // buttonPresses
  3793L * 3733 * 4057 * 3947
