package submarine
import scala.io.Source

object Lanternfish {

  def main(args: Array[String]): Unit = {
    val filename            = "input6.txt"
    val lines: List[String] = Source.fromResource(filename).getLines.toList

    val fishes = expectedPopulation(lines, 80)
    println(fishes)
    val takingOcean = expectedPopulation(lines, 256)
    println(takingOcean)

  }

  def expectedPopulation(xs: List[String], days: Int): Long = {
    val input = xs.flatMap(_.split(",").map(_.toLong)).groupBy(identity).mapValues(_.size.toLong)
    val states = 0L.to(8L).groupBy(identity).map { case (k, v) =>
      if (!input.filterKeys(p => p == k).isEmpty) k -> input(k) else k -> 0L
    }

    def loop(states: Map[Long, Long], days: Int): Long = {
      if (days == 0) states.values.reduce(_ + _)
      else {
        val newBorns = states(0)
        val resets   = states(0)
        val updated = 0L.to(8L).groupBy(identity).map { case (k, v) =>
          k match {
            case 6 => k -> (states(7) + resets)
            case 8 => k -> newBorns
            case _ => k -> states(k + 1)
          }
        }
        loop(updated, days - 1)
      }
    }

    loop(states, days)
  }

}
