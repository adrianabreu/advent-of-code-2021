package submarine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CrabsSpec extends AnyFlatSpec with Matchers {

  val input = List(
    "16,1,2,0,4,2,7,1,2,14"
  )

  "The Crabs spec" should "align them at the less cost point" in {
    val output = Crabs.alignLinearCost(input)

    assertResult(37)(output)
  }

  "The Crabs spec" should "align them including the exponential cost" in {
    val output = Crabs.alignAccCost(input)

    assertResult(168)(output)
  }
}
