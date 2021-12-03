package submarine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BinaryDiagnosticSpec extends AnyFlatSpec with Matchers {
  "The BinaryDiagnostic spec" should "calculate the gamma rate properly" in {
  
    val input = List(
        "00100",
        "11110",
        "10110",
        "10111",
        "10101",
        "01111",
        "00111",
        "11100",
        "10000",
        "11001",
        "00010",
        "01010"
      )

    val output = BinaryDiagnostic.calculateConsumptionPower(input)

    assertResult(198)(output)
  }

    "The BinaryDiagnostic spec" should "calculate the life support rate properly" in {
  
    val input = List(
        "00100",
        "11110",
        "10110",
        "10111",
        "10101",
        "01111",
        "00111",
        "11100",
        "10000",
        "11001",
        "00010",
        "01010"
      )

    val output = BinaryDiagnostic.calculateLifeSupportRating(input)

    assertResult(230)(output)
  }
  
}
