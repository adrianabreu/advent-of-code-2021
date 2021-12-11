package submarine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OctopusSpec extends AnyFlatSpec with Matchers {

  val input = List(
    "5483143223",
    "2745854711",
    "5264556173",
    "6141336146",
    "6357385478",
    "4167524645",
    "2176841721",
    "6882881134",
    "4846848554",
    "5283751526"
  )

  "The Octopus spec" should "calculate the flashes" in {
    val output = Octopus.flashes(input)(100)

    assertResult(1656)(output)
  }

  "The Octopus spec" should "be able to know the step all octopus flash" in {
    val output = Octopus.sync(input)

    assertResult(195)(output)
  }
}
