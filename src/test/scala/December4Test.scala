import December4.hasDoubleDigit
import December4.hasIncreasingDigits
import December4.hasSixDigit
import December4.hasStrictlyDoubleDigit
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class December4Test extends AnyFlatSpec with Matchers {

  "December 4" should "be able to detect six digits number" in {
    hasSixDigit(1) should be(false)
    hasSixDigit(1000000) should be(false)
    hasSixDigit(99999) should be(false)
    hasSixDigit(123456) should be(true)
    hasSixDigit(999999) should be(true)
  }

  it should "detect increasing digits" in {
    hasIncreasingDigits(1) should be(false)
    hasIncreasingDigits(12) should be(true)
    hasIncreasingDigits(122) should be(true)
    hasIncreasingDigits(1221) should be(false)
  }

  it should "detect double digits" in {
    hasDoubleDigit(1) should be(false)
    hasDoubleDigit(12) should be(false)
    hasDoubleDigit(122) should be(true)
    hasDoubleDigit(1221) should be(true)
  }

  it should "detect strictly double digits" in {
    hasStrictlyDoubleDigit(112233) should be(true)
    hasStrictlyDoubleDigit(111122) should be(true)
    hasStrictlyDoubleDigit(123444) should be(false)
  }
}
