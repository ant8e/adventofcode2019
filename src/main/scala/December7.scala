import IntCodeComputer.{IO, Program}

object December7 {

  object Amplifier {
    def run(amplifierControlSoftware: List[Int],
            phase: Int,
            input: Int): Option[Int] = {

      object io extends IntCodeComputer.IO {
        var inputs = List(phase, input)
        var output: Option[Int] = None

        override def read(): Option[Int] = {
          inputs.headOption match {
            case value @ Some(_) =>
              inputs = inputs.tail
              value
            case None => None
          }

        }

        override def write(i: Int): Unit = output = Some(i)
      }
      IntCodeComputer.runProgram(amplifierControlSoftware, 0, io)
      io.output
    }
  }

  def runAmplifierSeries(amplifierControlSoftware: List[Int],
                         phases: List[Int]) = {
    val initialInput = 0

    phases.foldLeft(Option(initialInput)) {
      case (Some(input), phase) =>
        Amplifier.run(amplifierControlSoftware, phase, input)
      case (None, _) => None
    }
  }

  def runAmplifiersSeriesWithFeedbackLoop(amplifierControlSoftware: List[Int],
                                          phases: List[Int]): Option[Int] = {
    val feedBackLoop = Pipe()
    val pipes = feedBackLoop :: List.fill(phases.length - 1)(Pipe()) ::: feedBackLoop :: Nil
    val programs = pipes
      .sliding(2)
      .zip(phases)
      .map {
        case (left :: right :: Nil, phase) =>
          left.write(phase)
          Program(amplifierControlSoftware, 0, PipeIO(left, right))
      }
      .toList
    feedBackLoop.write(0)
    IntCodeComputer.runPrograms(programs)
    feedBackLoop.read()
  }

  case class PipeIO(leftPipe: Pipe, rightPipe: Pipe) extends IO {
    override def read(): Option[Int] = leftPipe.read()

    override def write(i: Int): Unit = rightPipe.write(i)
  }

  case class Pipe() {
    private val q = collection.mutable.Queue.empty[Int]

    def write(i: Int): Unit = q.enqueue(i)
    def read(): Option[Int] =
      if (q.isEmpty) None else Some(q.dequeue())
  }

  def main(args: Array[String]): Unit = {
    val prg = List(3, 8, 1001, 8, 10, 8, 105, 1, 0, 0, 21, 34, 59, 68, 89, 102,
      183, 264, 345, 426, 99999, 3, 9, 102, 5, 9, 9, 1001, 9, 5, 9, 4, 9, 99, 3,
      9, 101, 3, 9, 9, 1002, 9, 5, 9, 101, 5, 9, 9, 1002, 9, 3, 9, 1001, 9, 5,
      9, 4, 9, 99, 3, 9, 101, 5, 9, 9, 4, 9, 99, 3, 9, 102, 4, 9, 9, 101, 3, 9,
      9, 102, 5, 9, 9, 101, 4, 9, 9, 4, 9, 99, 3, 9, 1002, 9, 5, 9, 1001, 9, 2,
      9, 4, 9, 99, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 101, 2, 9, 9, 4, 9, 3, 9,
      1001, 9, 2, 9, 4, 9, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9,
      3, 9, 1001, 9, 2, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 3, 9, 1001, 9, 2, 9,
      4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 99, 3, 9, 1001,
      9, 1, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9,
      101, 1, 9, 9, 4, 9, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3,
      9, 102, 2, 9, 9, 4, 9, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4,
      9, 3, 9, 1001, 9, 2, 9, 4, 9, 99, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 102, 2,
      9, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 3, 9,
      1001, 9, 2, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 101, 1, 9, 9, 4, 9,
      3, 9, 1001, 9, 1, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 102, 2, 9, 9,
      4, 9, 99, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 102,
      2, 9, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9,
      1002, 9, 2, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9,
      3, 9, 101, 1, 9, 9, 4, 9, 3, 9, 101, 2, 9, 9, 4, 9, 99, 3, 9, 1001, 9, 1,
      9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9, 102,
      2, 9, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9,
      101, 1, 9, 9, 4, 9, 3, 9, 1001, 9, 1, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9,
      3, 9, 1002, 9, 2, 9, 4, 9, 99)

    val highestSignal = (0 to 4).permutations
      .flatMap(phases => runAmplifierSeries(prg, phases.toList))
      .max
    println(highestSignal)

    val highestSignalWithFeedbackLoop = (5 to 9).permutations
      .flatMap(phases =>
        runAmplifiersSeriesWithFeedbackLoop(prg, phases.toList))
      .max
    println(highestSignalWithFeedbackLoop)
  }
}
