package day20

import scala.collection.mutable

// part 1
def part1(input: String) =
  val pulseNetwork = PulseNetwork.parse(input)
  val (low, high) =
    (1 to 1000).map(_ => pulseNetwork.pushButton()).foldLeft((0, 0)):
      case ((lowAcc, highAcc), sentPulses) =>
        val (lowPulses, highPulses) =
          sentPulses.partition((_, p, _) => p == Pulse.Low)
        (lowAcc + lowPulses.size, highAcc + highPulses.size)

  low * high

enum Pulse:
  case Low, High

case class PulseNetwork(
    outputs: mutable.HashMap[String, mutable.Buffer[String]],
    inputs: mutable.HashMap[String, mutable.Buffer[String]],
    moduleTypes: mutable.HashMap[String, ModuleType],
):
  def pushButton(): Seq[(String, Pulse, String)] =
    val queue      = mutable.Queue(("button", Pulse.Low, "broadcaster"))
    val pulsesSent = mutable.Buffer[(String, Pulse, String)]()
    while queue.nonEmpty do
      val p @ (pulseSource, pulse, pulseDestination) = queue.dequeue()
      pulsesSent.append(p)

      if moduleTypes.contains(pulseDestination) then
        assert(moduleTypes.contains(pulseDestination))
        moduleTypes(pulseDestination) match
          case ModuleType.FlipFlop(on) =>
            if pulse == Pulse.Low then
              moduleTypes(pulseDestination) = ModuleType.FlipFlop(!on)
              val outPulse = if on then Pulse.Low else Pulse.High
              for output <- outputs(pulseDestination) do
                queue.enqueue((pulseDestination, outPulse, output))

          case ModuleType.Conjunction(inputs) =>
            inputs(pulseSource) = pulse
            val outPulse =
              if inputs.forall((_, pulse) => pulse == Pulse.High) then Pulse.Low
              else Pulse.High
            for output <- outputs(pulseDestination) do
              queue.enqueue((pulseDestination, outPulse, output))

          case ModuleType.Broadcaster =>
            for output <- outputs(pulseDestination) do
              queue.enqueue((pulseDestination, pulse, output))

    pulsesSent.toSeq

  def reset() =
    moduleTypes.foreach:
      case (module, ModuleType.Broadcaster) =>
        moduleTypes(module) = ModuleType.Broadcaster
      case (module, ModuleType.FlipFlop(_)) =>
        moduleTypes(module) = ModuleType.FlipFlop(false)
      case (module, ModuleType.Conjunction(inputs)) =>
        moduleTypes(module) = ModuleType.Conjunction:
          mutable.HashMap(inputs.map((k, _) => (k, Pulse.Low)).toSeq*)
    this

  def buttonPressesUntilLow(module: String): Long =
    module match
      case "rx" =>
        /* we assume here that rx's only input is a conjunction whose inputs are
         * all inverters (we call them the inverter inputs).
         * furthermore, without checking, we also assume that the inverter
         * inputs are low after every number of button presses which is a
         * multiple of buttonPressesUntilLow(inverter input).
         * this makes sure that we can take the lcm of all the
         * buttonPressesUntilLow(inverter input) to get the correct result */
        assert(inputs("rx").size == 1)
        val finalConjunction = inputs("rx").head
        val disjointedButtonPresses = inputs(finalConjunction).map: input =>
          moduleTypes(input) match
            case ModuleType.Conjunction(_) =>
            case _                         => assert(false)
          assert(inputs(input).size == 1)
          buttonPressesUntilLow(input)
        disjointedButtonPresses.foldLeft(1L)(lcm)

      case _ =>
        var buttonPresses    = 0L
        val pulseNetworkCopy = this.copy().reset()
        var pulsesSent       = Seq.empty[(String, Pulse, String)]
        Iterator.iterate(Seq.empty): pulsesSent =>
          pulseNetworkCopy.pushButton()
        while !pulsesSent.exists((_, p, to) => to == module && p == Pulse.Low)
        do
          buttonPresses += 1
          pulsesSent = pulseNetworkCopy.pushButton()

        buttonPresses

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
          moduleTypes.put(name, ModuleType.FlipFlop(false))
        case s"&$name -> $outputModules" =>
          updateInputsOutputs(name, outputModules)
          moduleTypes.put(name, ModuleType.Conjunction(mutable.HashMap.empty))
        case s"broadcaster -> $outputModules" =>
          updateInputsOutputs("broadcaster", outputModules)
          moduleTypes.put("broadcaster", ModuleType.Broadcaster)

    inputs("broadcaster") = mutable.Buffer("button")
    for module <- moduleTypes.keysIterator do
      moduleTypes(module) match
        case ModuleType.Conjunction(i) =>
          val inputStates = inputs(module).map((_, Pulse.Low))
          inputStates.foreach((name, pulse) => i(name) = pulse)

        case _ =>

    PulseNetwork(outputs, inputs, moduleTypes).reset()

extension (pulseNetwork: PulseNetwork)
  def mkGraphvizString =
    val nodes = pulseNetwork.inputs.keys.map: name =>
      val nodeName = pulseNetwork.moduleTypes.get(name) match
        case None | Some(ModuleType.Broadcaster) => name
        case Some(ModuleType.FlipFlop(_))        => s"$name [fontcolor=blue]"
        case Some(ModuleType.Conjunction(_))     => s"$name [fontcolor=red]"
      nodeName
    .mkString("\n")
    val edges = pulseNetwork.inputs.keys.map: name =>
      if pulseNetwork.outputs.get(name).isDefined then
        val outputs =
          pulseNetwork.outputs.get(name).getOrElse(Seq.empty).mkString(",")
        s"$name -> $outputs"
      else ""
    .mkString("\n")

    s"\ndigraph {$nodes\n$edges}"

enum ModuleType:
  case FlipFlop(on: Boolean)
  case Conjunction(inputs: mutable.HashMap[String, Pulse])
  case Broadcaster

def part2(input: String): Long =
  val pulseNetwork = PulseNetwork.parse(input)
  // println(pulseNetwork.mkGraphvizString)
  pulseNetwork.buttonPressesUntilLow("rx")

def gcd(a: Long, b: Long): Long = if a == 0 then b else gcd(b % a, a)
def lcm(a: Long, b: Long)       = a * (b / gcd(a, b))
