package submarine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DepthIncreaseSpec extends AnyFlatSpec with Matchers {

  val input = List(199, 200, 208, 210, 200, 207, 240, 269, 260, 263)

  "The DepthIncrease object" should "detect increase in lists" in {

    val output = DepthIncrease.increase(input)

    assertResult(7)(output)
  }

  "The DepthIncrease object" should "detect increase in sum of lists" in {

    val output = DepthIncrease.increaseInSum(input)

    assertResult(5)(output)
  }
}
