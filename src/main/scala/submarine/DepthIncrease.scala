package submarine
import scala.io.Source

object DepthIncrease {

  def main(args: Array[String]): Unit = {
    val filename = "input.txt"
    val lines: List[Int] =
      Source.fromResource(filename).getLines.toList.map(_.toInt)

    val increases = increase(lines)
    println(increases)

    val sumIncreases = increaseInSum(lines)
    println(sumIncreases)
  }

  def increaseInSum(xs: List[Int]): Int =
    increase(windowSum(xs))

  def increase(xs: List[Int]): Int =
    xs.sliding(2).count { case List(a, b) =>
      a < b
    }

  def windowSum(xs: List[Int]): List[Int] =
    xs.sliding(3).map(_.sum).toList
}
