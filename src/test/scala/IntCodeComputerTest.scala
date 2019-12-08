import IntCodeComputer.{
  IO,
  Program,
  evaluateInstruction,
  runProgram,
  runPrograms
}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class IntCodeComputerTest extends AnyFlatSpec with Matchers {

  class MemIo(r: Int = 42) extends IO {
    var out: List[Int] = List.empty
    override def read() = Some(r)
    override def write(i: Int): Unit = out = i :: out
  }

  object io extends MemIo

  "A operation evaluator" should "evaluate" in {
    evaluateInstruction(List(1, 0, 0, 0, 99), 0, io) should be(
      Some(4) -> List(2, 0, 0, 0, 99))

    evaluateInstruction(List(2, 3, 0, 3, 99), 0, io) should be(
      Some(4) -> List(2, 3, 0, 6, 99))

    evaluateInstruction(List(2, 4, 4, 5, 99, 0), 0, io) should be(
      Some(4) -> List(2, 4, 4, 5, 99, 9801))

    evaluateInstruction(List(102, 4, 4, 5, 99, 0), 0, io) should be(
      Some(4) -> List(102, 4, 4, 5, 99, 396))

    evaluateInstruction(List(1102, 4, 4, 5, 99, 0), 0, io) should be(
      Some(4) -> List(1102, 4, 4, 5, 99, 16))

    evaluateInstruction(List(101, 4, 4, 5, 99, 0), 0, io) should be(
      Some(4) -> List(101, 4, 4, 5, 99, 103))

    evaluateInstruction(List(1101, 4, 4, 5, 99, 0), 0, io) should be(
      Some(4) -> List(1101, 4, 4, 5, 99, 8))

    evaluateInstruction(List(3, 3, 99, 0), 0, io) should be(
      Some(2) -> List(3, 3, 99, 42))

    val testIO = new MemIo
    evaluateInstruction(List(4, 0, 99), 0, testIO) should be(
      Some(2) -> List(4, 0, 99))
    testIO.out should be(List(4))
  }

  "A program runner" should "run programs" in {
    runProgram(List(1, 0, 0, 0, 99), 0, io) should be(List(2, 0, 0, 0, 99))
    runProgram(List(2, 3, 0, 3, 99), 0, io) should be(List(2, 3, 0, 6, 99))
    runProgram(List(2, 4, 4, 5, 99, 0), 0, io) should be(
      List(2, 4, 4, 5, 99, 9801))
    runProgram(List(1, 1, 1, 4, 99, 5, 6, 0, 99), 0, io) should be(
      List(30, 1, 1, 4, 2, 5, 6, 0, 99))

    val `test-input-equals-8-position-mode` =
      List(3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8)

    {
      val testIO = new MemIo(1)
      runProgram(`test-input-equals-8-position-mode`, 0, testIO)
      testIO.out should be(List(0))
    }

    {
      val testIO = new MemIo(8)
      runProgram(`test-input-equals-8-position-mode`, 0, testIO)
      testIO.out should be(List(1))
    }

    val `test-input-equals-8-immediate-mode` =
      List(3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8)

    {
      val testIO = new MemIo(1)
      runProgram(`test-input-equals-8-immediate-mode`, 0, testIO)
      testIO.out should be(List(0))
    }

    {
      val testIO = new MemIo(8)
      runProgram(`test-input-equals-8-immediate-mode`, 0, testIO)
      testIO.out should be(List(1))
    }

    val `test-input-less-than-8-position-mode`: List[Int] =
      List(3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8)

    {
      val testIO = new MemIo(2)
      runProgram(`test-input-less-than-8-position-mode`, 0, testIO)
      testIO.out should be(List(1))
    }

    {
      val testIO = new MemIo(10)
      runProgram(`test-input-less-than-8-position-mode`, 0, testIO)
      testIO.out should be(List(0))
    }

    val `test-input-less-than-8-immediate-mode` =
      List(3, 3, 1107, -1, 8, 3, 4, 3, 99)

    {
      val testIO = new MemIo(2)
      runProgram(`test-input-less-than-8-immediate-mode`, 0, testIO)
      testIO.out should be(List(1))
    }

    {
      val testIO = new MemIo(10)
      runProgram(`test-input-less-than-8-immediate-mode`, 0, testIO)
      testIO.out should be(List(0))
    }

    val `jump-test-position-mode` =
      List(3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9)

    {
      val testIO = new MemIo(0)
      runProgram(`jump-test-position-mode`, 0, testIO)
      testIO.out should be(List(0))
    }

    {
      val testIO = new MemIo(5)
      runProgram(`jump-test-position-mode`, 0, testIO)
      testIO.out should be(List(1))
    }

    val `jump-test-immediate-mode` =
      List(3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1)

    {
      val testIO = new MemIo(0)
      runProgram(`jump-test-immediate-mode`, 0, testIO)
      testIO.out should be(List(0))
    }

    {
      val testIO = new MemIo(5)
      runProgram(`jump-test-immediate-mode`, 0, testIO)
      testIO.out should be(List(1))
    }
  }

  "A programs evaluator" should "run multiple programs" in {
    val (p1, expectedp1) = Program(List(1, 0, 0, 0, 99), 0, io) -> List(2,
                                                                        0,
                                                                        0,
                                                                        0,
                                                                        99)

    val (p2, expectedp2) = Program(List(2, 4, 4, 5, 99, 0), 0, io) -> List(2, 4,
      4, 5, 99, 9801)

    val (p3, expectedp3) = Program(List(1, 1, 1, 4, 99, 5, 6, 0, 99), 0, io) -> List(
        30, 1, 1, 4, 2, 5, 6, 0, 99)

    runPrograms(List(p1, p2, p3)) should be(
      List(expectedp1, expectedp2, expectedp3))

  }
}
