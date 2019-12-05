import December5.Parameter.ImmediateParameter
import December5.Parameter.PositionParameter
import December5.ParameterType.Immediate
import December5.ParameterType.Positional

import scala.annotation.tailrec
import scala.util.Try

object December5 {
  trait IO {
    def read(): Int
    def write(i: Int): Unit
  }

  sealed trait Parameter {
    def readValue(memory: List[Int]): Int
  }

  object Parameter {
    case class ImmediateParameter(pointer: Int) extends Parameter {
      override def readValue(memory: List[Int]): Int = memory(pointer)
    }
    case class PositionParameter(pointer: Int) extends Parameter {
      override def readValue(memory: List[Int]): Int = memory(memory(pointer))

      def writeValue(memory: List[Int], value: Int): List[Int] =
        memory.updated(memory(pointer), value)
    }
  }

  sealed trait ParameterType {
    def toParameter(pointer: Int): Parameter
  }
  object ParameterType {
    case object Positional extends ParameterType {
      override def toParameter(pointer: Int): PositionParameter =
        PositionParameter(pointer)
    }
    case object Immediate extends ParameterType {
      override def toParameter(pointer: Int): ImmediateParameter =
        ImmediateParameter(pointer)
    }
  }

  object OpCode {
    def toParameterType(i: Int): ParameterType =
      if (i == 0) Positional else Immediate

    def unapply(i: Int): Option[(Int, ParameterType, ParameterType)] =
      if (i <= 0) None
      else
        Some(
          (i % 100,
           toParameterType((i / 100) % 10),
           toParameterType((i / 1000) % 10)))
  }

  def `less-than`(p1: Parameter,
                  p2: Parameter,
                  p3: PositionParameter): (Option[Int], scala.List[Int]) = ???

  def evaluateInstruction(memory: List[Int],
                          pointer: Int,
                          io: IO): (Option[Int], List[Int]) = {
    val opcode = memory.lift(pointer)

    def performOperation2(op: (Int, Int) => Int)(
        p1: Parameter,
        p2: Parameter,
        p3: PositionParameter): (Some[Int], List[Int]) = {
      Some(pointer + 4) -> Try(p3.writeValue(memory,
                                             op(p1.readValue(memory),
                                                p2.readValue(memory)))).toOption
        .getOrElse(memory)
    }

    val add = performOperation2(_ + _) _
    val mul = performOperation2(_ * _) _

    def `jump-if`(condition: Int => Boolean)(p1: Parameter,
                                             p2: Parameter): Int =
      if (condition(p1.readValue(memory))) p2.readValue(memory) else pointer + 3

    val `jump-if-true` = `jump-if`(_ != 0) _
    val `jump-if-false` = `jump-if`(_ == 0) _

    def `less-than`(p1: Parameter,
                    p2: Parameter,
                    p3: PositionParameter): (Option[Int], scala.List[Int]) = {
      Some(pointer + 4) -> p3.writeValue(
        memory,
        if (p1.readValue(memory) < p2.readValue(memory)) 1 else 0)
    }
    def equals(p1: Parameter,
               p2: Parameter,
               p3: PositionParameter): (Option[Int], scala.List[Int]) = {
      Some(pointer + 4) -> p3.writeValue(
        memory,
        if (p1.readValue(memory) == p2.readValue(memory)) 1 else 0)
    }

    opcode match {
      case Some(OpCode(code, p1, p2)) =>
        code match {
          case 1 =>
            add(p1.toParameter(pointer + 1),
                p2.toParameter(pointer + 2),
                PositionParameter(pointer + 3))

          case 2 =>
            mul(p1.toParameter(pointer + 1),
                p2.toParameter(pointer + 2),
                PositionParameter(pointer + 3))

          case 3 =>
            Some(pointer + 2) -> PositionParameter(pointer + 1)
              .writeValue(memory, io.read())

          case 4 =>
            io.write(p1.toParameter(pointer + 1).readValue(memory))
            Some(pointer + 2) -> memory

          case 5 =>
            Some(
              `jump-if-true`(p1.toParameter(pointer + 1),
                             p2.toParameter(pointer + 2))) -> memory

          case 6 =>
            Some(
              `jump-if-false`(p1.toParameter(pointer + 1),
                              p2.toParameter(pointer + 2))) -> memory

          case 7 =>
            `less-than`(p1.toParameter(pointer + 1),
                        p2.toParameter(pointer + 2),
                        PositionParameter(pointer + 3))
          case 8 =>
            equals(p1.toParameter(pointer + 1),
                   p2.toParameter(pointer + 2),
                   PositionParameter(pointer + 3))

          case 99 => None -> memory
        }

      case None => None -> memory
    }
  }

  @tailrec
  def runProgram(memory: List[Int], pointer: Int, io: IO): List[Int] =
    evaluateInstruction(memory, pointer, io) match {
      case (Some(nextPointer), updatedMemory) =>
        runProgram(updatedMemory, nextPointer, io)
      case (None, value) => value
    }

  def main(args: Array[String]): Unit = {
    val initialMemory = List(3, 225, 1, 225, 6, 6, 1100, 1, 238, 225, 104, 0,
      1101, 81, 30, 225, 1102, 9, 63, 225, 1001, 92, 45, 224, 101, -83, 224,
      224, 4, 224, 102, 8, 223, 223, 101, 2, 224, 224, 1, 224, 223, 223, 1102,
      41, 38, 225, 1002, 165, 73, 224, 101, -2920, 224, 224, 4, 224, 102, 8,
      223, 223, 101, 4, 224, 224, 1, 223, 224, 223, 1101, 18, 14, 224, 1001,
      224, -32, 224, 4, 224, 1002, 223, 8, 223, 101, 3, 224, 224, 1, 224, 223,
      223, 1101, 67, 38, 225, 1102, 54, 62, 224, 1001, 224, -3348, 224, 4, 224,
      1002, 223, 8, 223, 1001, 224, 1, 224, 1, 224, 223, 223, 1, 161, 169, 224,
      101, -62, 224, 224, 4, 224, 1002, 223, 8, 223, 101, 1, 224, 224, 1, 223,
      224, 223, 2, 14, 18, 224, 1001, 224, -1890, 224, 4, 224, 1002, 223, 8,
      223, 101, 3, 224, 224, 1, 223, 224, 223, 1101, 20, 25, 225, 1102, 40, 11,
      225, 1102, 42, 58, 225, 101, 76, 217, 224, 101, -153, 224, 224, 4, 224,
      102, 8, 223, 223, 1001, 224, 5, 224, 1, 224, 223, 223, 102, 11, 43, 224,
      1001, 224, -451, 224, 4, 224, 1002, 223, 8, 223, 101, 6, 224, 224, 1, 223,
      224, 223, 1102, 77, 23, 225, 4, 223, 99, 0, 0, 0, 677, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 1105, 0, 99999, 1105, 227, 247, 1105, 1, 99999, 1005, 227,
      99999, 1005, 0, 256, 1105, 1, 99999, 1106, 227, 99999, 1106, 0, 265, 1105,
      1, 99999, 1006, 0, 99999, 1006, 227, 274, 1105, 1, 99999, 1105, 1, 280,
      1105, 1, 99999, 1, 225, 225, 225, 1101, 294, 0, 0, 105, 1, 0, 1105, 1,
      99999, 1106, 0, 300, 1105, 1, 99999, 1, 225, 225, 225, 1101, 314, 0, 0,
      106, 0, 0, 1105, 1, 99999, 8, 226, 677, 224, 1002, 223, 2, 223, 1006, 224,
      329, 1001, 223, 1, 223, 7, 226, 226, 224, 102, 2, 223, 223, 1006, 224,
      344, 101, 1, 223, 223, 108, 677, 677, 224, 1002, 223, 2, 223, 1006, 224,
      359, 101, 1, 223, 223, 1107, 226, 677, 224, 1002, 223, 2, 223, 1005, 224,
      374, 101, 1, 223, 223, 1008, 677, 226, 224, 1002, 223, 2, 223, 1005, 224,
      389, 101, 1, 223, 223, 1007, 677, 226, 224, 1002, 223, 2, 223, 1005, 224,
      404, 1001, 223, 1, 223, 1107, 677, 226, 224, 1002, 223, 2, 223, 1005, 224,
      419, 1001, 223, 1, 223, 108, 677, 226, 224, 102, 2, 223, 223, 1006, 224,
      434, 1001, 223, 1, 223, 7, 226, 677, 224, 102, 2, 223, 223, 1005, 224,
      449, 1001, 223, 1, 223, 107, 226, 226, 224, 102, 2, 223, 223, 1006, 224,
      464, 101, 1, 223, 223, 107, 677, 226, 224, 102, 2, 223, 223, 1006, 224,
      479, 101, 1, 223, 223, 1007, 677, 677, 224, 1002, 223, 2, 223, 1006, 224,
      494, 1001, 223, 1, 223, 1008, 226, 226, 224, 1002, 223, 2, 223, 1006, 224,
      509, 101, 1, 223, 223, 7, 677, 226, 224, 1002, 223, 2, 223, 1006, 224,
      524, 1001, 223, 1, 223, 1007, 226, 226, 224, 102, 2, 223, 223, 1006, 224,
      539, 101, 1, 223, 223, 8, 677, 226, 224, 1002, 223, 2, 223, 1006, 224,
      554, 101, 1, 223, 223, 1008, 677, 677, 224, 102, 2, 223, 223, 1006, 224,
      569, 101, 1, 223, 223, 1108, 677, 226, 224, 102, 2, 223, 223, 1005, 224,
      584, 101, 1, 223, 223, 107, 677, 677, 224, 102, 2, 223, 223, 1006, 224,
      599, 1001, 223, 1, 223, 1108, 677, 677, 224, 1002, 223, 2, 223, 1006, 224,
      614, 1001, 223, 1, 223, 1107, 677, 677, 224, 1002, 223, 2, 223, 1005, 224,
      629, 1001, 223, 1, 223, 108, 226, 226, 224, 1002, 223, 2, 223, 1005, 224,
      644, 101, 1, 223, 223, 8, 226, 226, 224, 1002, 223, 2, 223, 1005, 224,
      659, 101, 1, 223, 223, 1108, 226, 677, 224, 1002, 223, 2, 223, 1006, 224,
      674, 101, 1, 223, 223, 4, 223, 99, 226)

    case class PrintingIO(value: Int) extends IO {
      override def read(): Int = value
      override def write(i: Int): Unit = println(i)
    }

    println("P1")
    runProgram(initialMemory, 0, PrintingIO(1))

    println("P2")
    runProgram(initialMemory, 0, PrintingIO(5))
  }
}
