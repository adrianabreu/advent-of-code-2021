package submarine
import scala.io.Source

object Crabs {

  def main(args: Array[String]): Unit = {
    val filename            = "input7.txt"
    val lines: List[String] = Source.fromResource(filename).getLines.toList

    val cost = alignLinearCost(lines)
    println(cost)

    val expCost = alignAccCost(lines)
    println(expCost)

  }

  def alignCost(xs: List[String], costFn: (Int, Int) => Int): Int = {
    val input = xs.flatMap(_.split(",").map(_.toInt)).groupBy(identity).mapValues(_.size.toInt)

    input
      .minBy(_._1)
      ._1
      .to(input.maxBy(_._1)._1)
      .map(pos => input.map { case (k, v) => costFn(k, pos) * v }.toList.sum)
      .min
  }

  def alignLinearCost(xs: List[String]): Int = alignCost(xs, (k, pos) => (k - pos).abs)
  def alignAccCost(xs: List[String]): Int    = alignCost(xs, (k, pos) => 1.to((k - pos).abs).sum)

}
