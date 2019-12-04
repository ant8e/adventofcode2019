import scala.annotation.tailrec

object December4 {

  def hasSixDigit(pwd: Int): Boolean = pwd >= 100000 && pwd <= 999999

  def digits(n: Int): List[Int] = {
    @tailrec
    def digits0(n1: Int, acc: List[Int]): List[Int] = {
      if (n1 == 0) acc
      else
        digits0(n1 / 10, n1 % 10 :: acc)
    }
    if (n < 0) List.empty else if (n == 0) List(0) else digits0(n, List.empty)
  }

  def digitsGroups(n: Int): List[List[Int]] = {
    @tailrec
    def groups(acc: List[List[Int]],
               current: List[Int],
               digits: List[Int]): List[List[Int]] = {
      if (digits.isEmpty) current :: acc
      else if (current.isEmpty) groups(acc, List(digits.head), digits.tail)
      else if (current.head == digits.head)
        groups(acc, digits.head :: current, digits.tail)
      else groups(current :: acc, List(digits.head), digits.tail)
    }
    groups(List.empty, List.empty, digits(n)).filter(_.nonEmpty).reverse
  }

  def hasIncreasingDigits(pwd: Int): Boolean =
    digits(pwd)
      .sliding(2)
      .forall(l => l.tail.nonEmpty && l.head <= l.drop(1).head)

  def hasDoubleDigit(pwd: Int): Boolean =
    digits(pwd)
      .sliding(2)
      .exists(l => l.tail.nonEmpty && l.head == l.drop(1).head)

  def hasStrictlyDoubleDigit(pwd: Int): Boolean =
    digitsGroups(pwd).count(_.size == 2) > 0

  def passwordFilterPart1(pwd: Int): Boolean =
    hasSixDigit(pwd) & hasIncreasingDigits(pwd) && hasDoubleDigit(pwd)

  def passwordFilterPart2(pwd: Int): Boolean =
    hasSixDigit(pwd) & hasIncreasingDigits(pwd) && hasStrictlyDoubleDigit(pwd)

  def main(args: Array[String]): Unit = {
    val p1 = (273025 to 767253).count(passwordFilterPart1)
    println(p1)

    val p2 = (273025 to 767253).count(passwordFilterPart2)
    println(p2)
  }
}
