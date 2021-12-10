package submarine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SyntaxSpec extends AnyFlatSpec with Matchers {

  val input = List(
      "[({(<(())[]>[[{[]{<()<>>",
      "[(()[<>])]({[<{<<[]>>(",
      "{([(<{}[<>[]}>{[]{[(<()>",
      "(((({<>}<{<{<>}{[]{[]{}",
      "[[<[([]))<([[{}[[()]]]",
      "[{[{({}]{}}([{[{{{}}([]",
      "{<[[]]>}<{[{[{[]{()[[[]",
      "[<(<(<(<{}))><([]([]()",
      "<{([([[(<>()){}]>(<<{{",
      "<{([{{}}[<[[[<>{}]]]>[]]"
  )

  "The Syntax spec" should "calculate the risk" in {
    val output = Syntax.calculateErrors(input)

    assertResult(26397)(output)
  }

  "The Syntax spec" should "calculate the incomplete score" in {
    val output = Syntax.calculateIncomplete(input)

    assertResult(288957)(output)
  }
}
