import December8._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class December8Test extends AnyFlatSpec with Matchers {

  "A space image" should "should decode layers" in {
    val data =
      "123456789012".grouped(1).map(_.toInt).toList

    SpaceImage.decode(2, 3, data) should be(
      SpaceImage(List(Layer(List(Row(List(1, 2, 3)), Row(List(4, 5, 6)))),
                      Layer(List(Row(List(7, 8, 9)), Row(List(0, 1, 2)))))))
  }

  it should "count digits" in {
    Layer(List(Row(List(1, 2, 3)), Row(List(4, 5, 6))))
      .countDigits(2) should be(1)
    Layer(List(Row(List(1, 2, 3)), Row(List(4, 5, 2))))
      .countDigits(2) should be(2)
  }

  it should "compute checksum" in {
    SpaceImage(
      List(Layer(List(Row(List(1, 2, 3)), Row(List(4, 5, 6)))),
           Layer(List(Row(List(7, 8, 9)), Row(List(0, 1, 2))))))
      .checksum() should be(1)
  }

  it should "stack layers" in {
    Row.stackPixel(0, 0) should be(0)
    Row.stackPixel(0, 1) should be(0)
    Row.stackPixel(0, 2) should be(0)
    Row.stackPixel(1, 0) should be(1)
    Row.stackPixel(1, 1) should be(1)
    Row.stackPixel(1, 2) should be(1)
    Row.stackPixel(2, 0) should be(0)
    Row.stackPixel(2, 1) should be(1)
    Row.stackPixel(2, 2) should be(2)

    Row.stackRow(Row(List(0, 1, 2)), Row(List(1, 1, 1))).row should be(
      Row(List(0, 1, 1)).row)

    val l1 = Layer(List(Row(List(0, 2)), Row(List(2, 2))))
    val l2 = Layer(List(Row(List(1, 1)), Row(List(2, 2))))
    val l3 = Layer(List(Row(List(2, 2)), Row(List(1, 2))))
    val l4 = Layer(List(Row(List(0, 0)), Row(List(0, 0))))

    val expectedLayer = Layer(List(Row(List(0, 1)), Row(List(1, 0))))

    SpaceImage(List(l1, l2, l3, l4)).stack() should be(
      SpaceImage(List(expectedLayer)))

  }
}
