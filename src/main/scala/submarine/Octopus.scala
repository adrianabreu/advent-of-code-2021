package submarine
import scala.io.Source

object Octopus {

  def main(args: Array[String]): Unit = {
    val filename            = "input11.txt"
    val lines: List[String] = Source.fromResource(filename).getLines.toList

    val f = flashes(lines)(100)
    println(f)

    val s = sync(lines)
    println(s)
  }

  def flashes(xs: List[String])(steps: Int): Int = {

    def loop(xs: Map[(Int, Int), Int], step: Int, acc: Int): Int = {
      if (step == 0) acc
      else {
        val increased = xs.map { case (k, v) => (k, v + 1) }

        val ff = increaseNeighbours(increased, Set())
        loop(ff._1, step - 1, acc + ff._2)
      }
    }

    loop(inputToMap(xs), steps, 0)
  }

  def sync(xs: List[String]): Int = {

    def loop(xs: Map[(Int, Int), Int], flashed: Int, step: Int): Int = {
      if (xs.keys.size == flashed) step
      else {
        val increased = xs.map { case (k, v) => (k, v + 1) }

        val ff = increaseNeighbours(increased, Set())
        loop(ff._1, ff._2, step + 1)
      }
    }

    loop(inputToMap(xs), 0, 0)
  }

  def increaseNeighbours(
      ys: Map[(Int, Int), Int],
      alreadyFlashed: Set[(Int, Int)]
  ): (Map[(Int, Int), Int], Int) = {
    val flashed = ys.filter { case (k, v) => v > 9 }
    if (flashed.size == 0) {
      (ys, alreadyFlashed.size)
    } else {
      val toIncrease = flashed.keys.toList
        .flatMap { k =>
          {
            List(
              (1, 0),
              (-1, 0),
              (0, 1),
              (0, -1),
              (-1, 1),
              (-1, -1),
              (1, -1),
              (1, 1)
            ).map(p => (k._1 + p._1, k._2 + p._2))
          }
        }
        .filter(p =>
          !ys.keys.find(k => k == p).isEmpty && (alreadyFlashed union flashed.keys.toSet)
            .find(k => k == p)
            .isEmpty
        )

      def go(zs: Map[(Int, Int), Int], xs: List[(Int, Int)]): Map[(Int, Int), Int] = {
        xs match {
          case Nil => zs
          case x :: next =>
            go(
              zs.map {
                case (k, v) => {
                  if (k == x) (k, v + 1)
                  else (k, v)
                }
              },
              xs.tail
            )
        }
      }
      increaseNeighbours(
        go(ys, toIncrease.toList).map { case (k, v) =>
          if (!(alreadyFlashed union flashed.keys.toSet).find(p => p == k).isEmpty) (k, 0)
          else (k, v)
        },
        alreadyFlashed union flashed.keys.toSet
      )
    }

  }

  def inputToMap(xs: List[String]) =
    xs
      .map(x => x.split("").map(_.toInt).zipWithIndex)
      .zipWithIndex
      .flatMap(r => r._1.map(c => (r._2, c._2) -> c._1))
      .toMap

}
