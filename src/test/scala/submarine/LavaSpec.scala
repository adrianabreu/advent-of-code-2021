package submarine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LavaSpec extends AnyFlatSpec with Matchers {

  val input = List(
    "2199943210",
    "3987894921",
    "9856789892",
    "8767896789",
    "9899965678"
  )

  "The Lava spec" should "calculate the risk" in {
    val output = Lava.calculateRisk(input)

    assertResult(15)(output)
  }

  "The Lava spec" should "calculate the basins" in {
    val output = Lava.calculateBasins(input)

    assertResult(1134)(output)
  }

}
