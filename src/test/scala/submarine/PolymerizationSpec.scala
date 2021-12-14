package submarine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PolymerizationSpec extends AnyFlatSpec with Matchers {

  val input = List(
    "NNCB",
    "",
    "CH -> B",
    "HH -> N",
    "CB -> H",
    "NH -> C",
    "HB -> C",
    "HC -> B",
    "HN -> C",
    "NN -> C",
    "BH -> H",
    "NC -> B",
    "NB -> B",
    "BN -> B",
    "BB -> N",
    "BC -> B",
    "CC -> N",
    "CN -> C"
  )

  "The Polymerization spec" should "calculate the difference between the mce and lce at n step" in {
    val output = Polymerization.calculateDiff(input)(10)

    assertResult(1588)(output)
  }

  "The Polymerization spec" should "calculate the difference between the mce and lce at large n step" in {
    val output = Polymerization.calculateDiff(input)(40)

    assertResult(2188189693529L)(output)
  }

}
