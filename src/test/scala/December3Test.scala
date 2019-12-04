import December3.Point
import December3.Zero
import December3.intersection
import December3.minDistance
import December3.toPath
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class December3Test extends AnyFlatSpec with Matchers {

  "A Point" should "be able to compute the next point in given a direction" in {
    Zero.next("U") shouldBe (Point(0, 1))
    Zero.next("D") shouldBe (Point(0, -1))
    Zero.next("L") shouldBe (Point(-1, 0))
    Zero.next("R") shouldBe (Point(1, 0))
  }

  "A path calculation" should "be able to compute a complete path" in {
    val path1 = toPath(List("R8", "U5", "L5", "D3"))
    path1.head should be(Zero)
    path1.last should be(Point(3, 2))
    path1.size should be(22)
  }

  "intersection compute" should "be able to compute path intersection" in {
    val path1 = toPath(List("R8", "U5", "L5", "D3"))
    val path2 = toPath(List("U7", "R6", "D4", "L4"))

    intersection(path1, path2)
      .filterNot(_ == Zero) should contain only (Point(3, 3), Point(6, 5))
  }

  "min distance" should "be able to compute minimum distance" in {
    val path1 = toPath(List("R8", "U5", "L5", "D3"))
    val path2 = toPath(List("U7", "R6", "D4", "L4"))
    minDistance(path1, path2) should be(Some(Point(3, 3)))

    val path3 = toPath("R75,D30,R83,U83,L12,D49,R71,U7,L72".split(",").toList)
    val path4 = toPath("U62,R66,U55,R34,D71,R55,D58,R83".split(",").toList)
    minDistance(path3, path4).map(_.distance) should be(Some(159))

    val path5 =
      toPath("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51".split(",").toList)
    val path6 = toPath("U98,R91,D20,R16,D67,R40,U7,R15,U6,R7".split(",").toList)
    minDistance(path5, path6).map(_.distance) should be(Some(135))
  }

  "min step" should "be able to compute minimum steps" in {
    val path1 = toPath(List("R8", "U5", "L5", "D3"))
    val path2 = toPath(List("U7", "R6", "D4", "L4"))
    December3.minSteps(path1, path2) should be(Some(30 -> Point(6, 5)))

    val path3 = toPath("R75,D30,R83,U83,L12,D49,R71,U7,L72".split(",").toList)
    val path4 = toPath("U62,R66,U55,R34,D71,R55,D58,R83".split(",").toList)
    December3.minSteps(path3, path4).map(_._1) should be(Some(610))

    val path5 =
      toPath("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51".split(",").toList)
    val path6 = toPath("U98,R91,D20,R16,D67,R40,U7,R15,U6,R7".split(",").toList)
    December3.minSteps(path5, path6).map(_._1) should be(Some(410))
  }

}
