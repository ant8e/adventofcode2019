import December6._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class December6Test extends AnyFlatSpec with Matchers {

  "An orbit parser" should "parse" in {
    parse("COM)B") should be(List(Orbit(SpaceObject("B"), COM)))

    val s = """COM)B
              |B)C
              |C)D
              """.stripMargin

    parse(s) should be(
      List(Orbit(SpaceObject("B"), COM),
           Orbit(SpaceObject("C"), SpaceObject("B")),
           Orbit(SpaceObject("D"), SpaceObject("C"))))

  }

  "An orbit counter" should "count orbits" in {
    val s = """COM)B
              |B)C
              |C)D
              |D)E
              |E)F
              |B)G
              |G)H
              |D)I
              |E)J
              |J)K
              |K)L
              |""".stripMargin

    val orbits = parse(s)

    countOrbitsOf(SpaceObject("B"), orbits) should be(1)
    countOrbitsOf(SpaceObject("L"), orbits) should be(7)
    countAllOrbits(orbits) should be(42)
  }

  "An orbit transfer optimizer" should "find minimal count of orbit transfer" in {
    val s = """COM)B
              |B)C
              |C)D
              |D)E
              |E)F
              |B)G
              |G)H
              |D)I
              |E)J
              |J)K
              |K)L
              |""".stripMargin

    val orbits = parse(s)
    pathToCOM(SpaceObject("L"), orbits) should be(
      List(SpaceObject("K"),
           SpaceObject("J"),
           SpaceObject("E"),
           SpaceObject("D"),
           SpaceObject("C"),
           SpaceObject("B")))

    val orbits2 = parse(s + "K)YOU\nI)SAN")
    minTransfer(SpaceObject("YOU"), SpaceObject("SAN"), orbits2) should be(4)
  }
}
