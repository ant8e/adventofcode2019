import scala.io.Source

object December8 {

  case class Row(row: List[Int]) extends AnyVal {
    def countDigits(digit: Int): Int = row.count(_ == digit)

    override def toString: String =
      row.map {
        case 0 => "   "
        case 1 => " * "
      }.mkString
  }

  object Row {

    val stackPixel = (top: Int, bottom: Int) =>
      (top, bottom) match {
        case (2, b) => b
        case _      => top
    }

    val stackRow = (top: Row, bottom: Row) =>
      Row(top.row.zip(bottom.row).map(stackPixel.tupled))
  }

  case class Layer(rows: List[Row]) extends AnyVal {

    def countDigits(digit: Int): Int = rows.foldLeft(0) {
      case (sum, row) => sum + row.countDigits(digit)
    }

    override def toString: String = rows.mkString("\n")
  }

  object Layer {

    def decode(rowLength: Int, data: List[Int]) =
      Layer(data.grouped(rowLength).map(Row.apply).toList)

    def stackLayer(top: Layer, bottom: Layer): Layer =
      Layer(top.rows.zip(bottom.rows).map(Row.stackRow.tupled))
  }

  case class SpaceImage(layers: List[Layer]) extends AnyVal {

    def checksum(): Int = {
      val (l, _) = layers.map(l => (l, l.countDigits(0))).minBy(_._2)
      l.countDigits(2) * l.countDigits(1)
    }

    def stack() =
      SpaceImage(List(layers.tail.foldLeft(layers.head) {
        case (top, bottom) => Layer.stackLayer(top, bottom)
      }))

    override def toString: String = layers.mkString
  }

  object SpaceImage {

    def decode(rows: Int, rowLength: Int, data: List[Int]): SpaceImage =
      SpaceImage(
        data.grouped(rows * rowLength).map(Layer.decode(rowLength, _)).toList)
  }

  def main(args: Array[String]): Unit = {
    val input =
      Source
        .fromResource("December8")
        .toList
        .flatMap(_.toString.toIntOption)

    val spaceImage = SpaceImage.decode(6, 25, input)
    println(spaceImage.checksum())

    println(spaceImage.stack())
  }
}
