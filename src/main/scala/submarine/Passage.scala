package submarine
import scala.io.Source

object Passage {

  def main(args: Array[String]): Unit = {
    val filename            = "input12.txt"
    val lines: List[String] = Source.fromResource(filename).getLines.toList

    val f = paths(lines)
    println(f)

    val n = pathsWithTwice(lines)
    println(n)
  }

  def paths(xs: List[String]): Int = {

    def loop(point: String, xs: Map[String, Seq[String]], visited: Set[String]): Int = {
      if (point == "end") {
        1
      } else if (point.toLowerCase == point && !visited.find(v => v == point).isEmpty) {
        0
      } else {
        xs(point).map(x => loop(x, xs, visited ++ Set(point))).sum
      }
    }

    loop("start", inputToMap(xs), Set())
  }

  def pathsWithTwice(xs: List[String]): Int = {
    def loop(
        point: String,
        xs: Map[String, Seq[String]],
        visited: List[String],
        twice: Set[String]
    ): Int = {
      if (point == "end") {
        1
      } else if (
        point.toLowerCase == point && !visited.find(v => v == point).isEmpty && twice.size > 0
      ) {
        0
      } else {

        xs(point)
          .map(x => {
            val vst =
              if (point.toLowerCase() == point && !visited.find(v => v == point).isEmpty) Set(point)
              else Set[String]()
            loop(x, xs, visited ++ List(point), if (twice.size == 0) vst else twice)
          })
          .sum
      }
    }

    loop("start", inputToMap(xs), List(), Set())
  }

  def inputToMap(xs: List[String]): Map[String, List[String]] =
    xs
      .flatMap(x =>
        List((x.split("-")(0), x.split("-")(1)), (x.split("-").reverse(0), x.split("-").reverse(1)))
      )
      .filter(p => p._2 != "start")
      .filter(p => p._1 != "end")
      .groupBy(p => p._1)
      .mapValues(f => f.map(_._2))
      .toMap
}
