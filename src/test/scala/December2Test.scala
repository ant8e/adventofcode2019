import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class December2Test extends AnyFlatSpec with Matchers {

  "A operation evaluator" should "evaluate" in {
    December2.evaluateInstruction(List(1, 0, 0, 0, 99), 0) should be(
      Some(4) -> List(2, 0, 0, 0, 99))
    December2.evaluateInstruction(List(2, 3, 0, 3, 99), 0) should be(
      Some(4) -> List(2, 3, 0, 6, 99))
    December2.evaluateInstruction(List(2, 4, 4, 5, 99, 0), 0) should be(
      Some(4) -> List(2, 4, 4, 5, 99, 9801))
  }

  "A program evaluator" should "evaluate" in {
    December2.runProgram(List(1, 0, 0, 0, 99), 0) should be(List(2, 0, 0, 0, 99))
    December2.runProgram(List(2, 3, 0, 3, 99), 0) should be(List(2, 3, 0, 6, 99))
    December2.runProgram(List(2, 4, 4, 5, 99, 0), 0) should be(
      List(2, 4, 4, 5, 99, 9801))
    December2.runProgram(List(1, 1, 1, 4, 99, 5, 6, 0, 99), 0) should be(
      List(30, 1, 1, 4, 2, 5, 6, 0, 99))
  }
}
