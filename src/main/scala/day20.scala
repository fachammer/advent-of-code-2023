package day20

import debug.*
import scala.collection.mutable

// part 1
def part1(input: String) =
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

  // outputs("button") = mutable.Buffer("broadcaster")
  // inputs("broadcaster") = mutable.Buffer("button")
  // moduleTypes("button") = ModuleType.Button

  val pulseNetwork = PulseNetwork(outputs, inputs, moduleTypes).d

  val (low, high) =
    (1 to 1000).map(_ => pulseNetwork.pushButton()).foldLeft((0, 0)):
      case ((accLow, accHigh), (low, high)) => (accLow + low, accHigh + high)
  low * high

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

  def stateView = states.toMap.view

  def pushButton() =
    val queue          = mutable.Queue(("button", true, "broadcaster"))
    var lowPulsesSent  = 0
    var highPulsesSent = 0
    while queue.nonEmpty do
      val (pulseSource, pulseIsLow, pulseDestination) = queue.dequeue()

      if pulseIsLow then lowPulsesSent += 1
      else highPulsesSent += 1
      // println:
      /* s"$pulseSource -${if pulseIsLow then "low" else "high"}->
       * $pulseDestination" */

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

    (lowPulsesSent, highPulsesSent)

enum ModuleType:
  case FlipFlop, Conjunction, Broadcaster

enum ModuleState:
  case FlipFlop(on: Boolean)
  case Conjunction(inputs: mutable.HashMap[String, Boolean])
  case None
