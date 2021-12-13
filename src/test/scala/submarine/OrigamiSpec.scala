package submarine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OrigamiSpec extends AnyFlatSpec with Matchers {

  val input = List(
    "6,10",
    "0,14",
    "9,10",
    "0,3",
    "10,4",
    "4,11",
    "6,0",
    "6,12",
    "4,1",
    "0,13",
    "10,12",
    "3,4",
    "3,0",
    "8,4",
    "1,10",
    "2,14",
    "8,10",
    "9,0",
    "",
    "fold along y=7",
    "fold along x=5"
  )

  "The Origami spec" should "calculate the numbers after folding" in {
    val output = Origami.fold(input)

    assertResult(17)(output)
  }

  "The Origami spec" should "print the number" in {
    val output = Origami.fold2(input)
  }

}
