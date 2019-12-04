import scala.annotation.tailrec
import scala.util.Try

object December2 {

  def evaluateInstruction(memory: List[Int],
                          pointer: Int): (Option[Int], List[Int]) = {
    val opcode = memory.lift(pointer)

    def performOperation(op: (Int, Int) => Int): (Some[Int], List[Int]) = {
      Some(pointer + 4) -> Try(
        memory.updated(memory(pointer + 3),
                       op(memory(memory(pointer + 1)),
                          memory(memory(pointer + 2))))).toOption
        .getOrElse(memory)
    }

    opcode match {
      case Some(1)  => performOperation(_ + _)
      case Some(2)  => performOperation(_ * _)
      case Some(99) => None -> memory
      case None     => None -> memory
    }
  }

  @tailrec
  def runProgram(memory: List[Int], pointer: Int): List[Int] =
    evaluateInstruction(memory, pointer) match {
      case (Some(nextPointer), value) => runProgram(value, nextPointer)
      case (None, value)              => value
    }

  val initialMemory = List(1, 0, 0, 3, 1, 1, 2, 3, 1, 3, 4, 3, 1, 5, 0, 3, 2, 1, 9, 19, 1,
    5, 19, 23, 1, 6, 23, 27, 1, 27, 10, 31, 1, 31, 5, 35, 2, 10, 35, 39, 1, 9,
    39, 43, 1, 43, 5, 47, 1, 47, 6, 51, 2, 51, 6, 55, 1, 13, 55, 59, 2, 6, 59,
    63, 1, 63, 5, 67, 2, 10, 67, 71, 1, 9, 71, 75, 1, 75, 13, 79, 1, 10, 79, 83,
    2, 83, 13, 87, 1, 87, 6, 91, 1, 5, 91, 95, 2, 95, 9, 99, 1, 5, 99, 103, 1,
    103, 6, 107, 2, 107, 13, 111, 1, 111, 10, 115, 2, 10, 115, 119, 1, 9, 119,
    123, 1, 123, 9, 127, 1, 13, 127, 131, 2, 10, 131, 135, 1, 135, 5, 139, 1, 2,
    139, 143, 1, 143, 5, 0, 99, 2, 0, 14, 0)

  def patchMemory(noun: Int, verb: Int) = initialMemory.updated(1, noun).updated(2, verb)

  def main(args: Array[String]): Unit = {
    println(runProgram(patchMemory(12, 2), 0).head)

    (for {
      noun <- 0 to 99
      verb <- 0 to 99
    } yield (noun, verb))
      .find(t => {
        val (noun, verb) = t
        runProgram(patchMemory(noun, verb), 0).head == 19690720
      })
      .foreach(t => {
        val (noun, verb) = t
        println(s"$noun$verb")
      })
  }
}
