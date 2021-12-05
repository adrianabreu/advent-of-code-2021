package submarine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class VentsSpec extends AnyFlatSpec with Matchers {

  val input = List(
    "0,9 -> 5,9",
    "8,0 -> 0,8",
    "9,4 -> 3,4",
    "2,2 -> 2,1",
    "7,0 -> 7,4",
    "6,4 -> 2,0",
    "0,9 -> 2,9",
    "3,4 -> 1,4",
    "0,0 -> 8,8",
    "5,5 -> 8,2"
  )

  "The Vents spec" should "find the overlapping lines" in {
    val output = Vents.overlappings(input)

    assertResult(5)(output)
  }

  "The Vents spec" should "find the overlapping lines with diagonals " in {
    val output = Vents.overlappingsWithDiagonals(input)

    assertResult(12)(output)
  }
}
