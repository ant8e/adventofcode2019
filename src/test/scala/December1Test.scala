import December1.fuelForMass
import December1.fuelForMassCompensated
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class December1Test extends AnyFlatSpec with Matchers {
  "fuelForMass " should "compute fuel for mass " in {
    fuelForMass(2) should be(0)
    fuelForMass(12) should be(2)
    fuelForMass(14) should be(2)
    fuelForMass(1969) should be(654)
    fuelForMass(100756) should be(33583)
  }

  "fuelForMassComp " should "compute  compensated fuel for mass " in {
    fuelForMassCompensated(2) should be(0)
    fuelForMassCompensated(12) should be(2)
    fuelForMassCompensated(14) should be(2)
    fuelForMassCompensated(1969) should be(966)
    fuelForMassCompensated(100756) should be(50346)
  }
}
