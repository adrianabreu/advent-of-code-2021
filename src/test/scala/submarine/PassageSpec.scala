package submarine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PassageSpec extends AnyFlatSpec with Matchers {

  val input = List(
    "start-A",
    "start-b",
    "A-c",
    "A-b",
    "b-d",
    "A-end",
    "b-end",
  )

  "The Passage spec" should "calculate the different paths" in {
    val output = Passage.paths(input)

    assertResult(10)(output)
  }

    "The Passage spec" should "calculate the different paths when visit a small cave is allowed" in {
    val output = Passage.pathsWithTwice(input)

    assertResult(36)(output)
  }
}
