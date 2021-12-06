package submarine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LanternfishSpec extends AnyFlatSpec with Matchers {

  val input = List(
    "3,4,3,1,2"
  )

  "The Lanternfish spec" should "calculate the growth rate" in {
    val output = Lanternfish.expectedPopulation(input, 18)

    assertResult(26L)(output)
  }

  "The Lanternfish spec" should "calculate the growth rate in future" in {
    val output = Lanternfish.expectedPopulation(input, 256)

    assertResult(26984457539L)(output)
  }

}

