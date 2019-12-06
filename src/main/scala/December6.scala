import scala.io.Source

object December6 {
  case class SpaceObject(name: String)

  val COM = SpaceObject("COM")

  case class Orbit(obj: SpaceObject, around: SpaceObject)

  def toOrbit(s: String): Option[Orbit] =
    s.split("\\)").toList match {
      case a :: b :: Nil if a == COM.name => Some(Orbit(SpaceObject(b), COM))
      case a :: b :: Nil                  => Some(Orbit(SpaceObject(b), SpaceObject(a)))
      case _                              => None
    }

  def parse(s: String): List[Orbit] =
    s.linesIterator.flatMap(toOrbit).toList

  def countAllOrbits(orbits: List[Orbit]): Int =
    orbits.map(_.obj).distinct.map(countOrbitsOf(_, orbits)).sum

  def countOrbitsOf(spaceObject: SpaceObject, orbits: List[Orbit]): Int =
    orbits.find(_.obj == spaceObject) match {
      case Some(o) => 1 + countOrbitsOf(o.around, orbits)
      case _       => 0
    }

  def pathToCOM(obj: SpaceObject, orbits: List[Orbit]): List[SpaceObject] =
    if (obj == COM) Nil
    else
      orbits
        .find(_.obj == obj)
        .map(o =>
          if (o.around == COM) pathToCOM(o.around, orbits)
          else o.around :: pathToCOM(o.around, orbits))
        .getOrElse(Nil)

  def minTransfer(a: SpaceObject, b: SpaceObject, orbits: List[Orbit]): Int = {
    val pathA = pathToCOM(a, orbits)
    val pathB = pathToCOM(b, orbits)

    // missing the common object is those lists, we dont want to count it twice
    val commonA = pathA.reverse.dropWhile(pathB.contains)
    val commonB = pathB.reverse.dropWhile(pathA.contains)

    commonA.length + commonB.length
  }

  def main(args: Array[String]): Unit = {
    val orbits =
      Source.fromResource("December6").getLines().flatMap(toOrbit).toList

    println(countAllOrbits(orbits))
    println(minTransfer(SpaceObject("YOU"), SpaceObject("SAN"), orbits))
  }

}
