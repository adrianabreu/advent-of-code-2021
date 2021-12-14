package submarine
import scala.io.Source

object Polymerization {

  def main(args: Array[String]): Unit = {
    val filename            = "input14.txt"
    val lines: List[String] = Source.fromResource(filename).getLines.toList

    val part1 = calculateDiff(lines)(10)

    println(part1)

    val part2 = calculateDiff(lines)(40)

    println(part2)
  }

  def calculateDiff(xs: List[String])(steps: Int): Long = {
    val map = grow(xs)(steps)

    map.values.max - map.values.min
  }

  def grow(xs: List[String])(steps: Int): Map[String, Long] = {
    val start = xs.head
    val chainMap = xs
      .drop(2)
      .map(f => {
        val c = f.split(" -> ")
        (c(0), c(1))
      })
      .toMap

    def loop(
        polymers: Map[String, Long],
        letters: Map[String, Long],
        step: Int
    ): Map[String, Long] = {
      if (step == 0) letters
      else {
        val g = polymers.toList
          .flatMap {
            case (k, v) => {
              val a = k(0)
              val b = k(1)
              val c = chainMap(k)

              List(
                (a + c, v),
                (c + b, v)
              )
            }
          }
          .groupBy(_._1)
          .map { case (k, v) => (k, v.map(_._2).sum) }

        val lettersAdded = polymers.toList
          .map {
            case (k, v) => {
              (chainMap(k), v)
            }
          }
          .groupBy(_._1)
          .map { case (k, v) => (k, v.map(_._2).sum) }

        loop(g, letters.map { case (k, v) => (k, v + lettersAdded(k)) }, step - 1)
      }
    }

    val pairs = start.split("").sliding(2).map(k => k.mkString).toList.groupBy(identity).map {
      case (k, v) => (k, v.size)
    }

    val startingLetters = start.split("").groupBy(identity).map { case (k, v) => (k, v.size) }
    val letters = chainMap.values.groupBy(identity).map { case (k, v) =>
      if (!startingLetters.keys.find(p => p == k).isEmpty) (k, startingLetters(k).toLong)
      else (k, 0L)
    }

    loop(
      chainMap.map { case (k, v) =>
        if (!pairs.keys.find(p => p == k).isEmpty) (k, pairs(k).toLong) else (k, 0)
      },
      letters,
      steps
    )

  }

}
