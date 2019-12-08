import IntCodeComputer.Parameter.{ImmediateParameter, PositionParameter}
import IntCodeComputer.ParameterType.{Immediate, Positional}

import scala.annotation.tailrec
import scala.util.Try

object IntCodeComputer {

  trait IO {
    def read(): Option[Int]
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
            io.read() match {
              case Some(value) =>
                Some(pointer + 2) -> PositionParameter(pointer + 1)
                  .writeValue(memory, value)
              case None => Some(pointer) -> memory
            }

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

  case class Program(memory: List[Int], pointer: Option[Int], io: IO)
  object Program {
    def apply(memory: List[Int], pointer: Int, io: IO): Program =
      Program(memory, Some(pointer), io)
  }

  @tailrec
  def runProgram(memory: List[Int], pointer: Int, io: IO): List[Int] =
    evaluateInstruction(memory, pointer, io) match {
      case (Some(nextPointer), updatedMemory) =>
        runProgram(updatedMemory, nextPointer, io)
      case (None, value) => value
    }

  @tailrec
  def runPrograms(programs: List[Program]): List[List[Int]] = {
    if (programs.forall(_.pointer.isEmpty))
      programs.map(_.memory)
    else
      runPrograms(programs.map {
        case Program(memory, Some(pointer), io) =>
          val (newPointer, newMem) = evaluateInstruction(memory, pointer, io)
          Program(newMem, newPointer, io)
        case p @ Program(_, None, _) => p
      })
  }
}
