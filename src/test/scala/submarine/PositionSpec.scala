package submarine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PositionSpec extends AnyFlatSpec with Matchers {
  val input = List(
    "forward 5",
    "down 5",
    "forward 8",
    "up 3",
    "down 8",
    "forward 2"
  )

  "The Position spec" should "calculate both the horizontal and depth properly" in {

    val output = Position.calculate(input)

    assertResult(Position(x = 15, y = 10))(output)
  }

  "The Position spec" should "calculate also the aim properly if needed" in {

    val output = Position.calculateWithAim(input)

    assertResult(Position(x = 15, y = 60, aim = 10))(output)
  }
}
