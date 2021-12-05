package submarine
import scala.io.Source
import scala.collection.MapView

object BinaryDiagnostic {

  def main(args: Array[String]): Unit = {
    val filename            = "input3.txt"
    val lines: List[String] = Source.fromResource(filename).getLines.toList

    println(calculateConsumptionPower(lines))

    println(calculateLifeSupportRating(lines))

  }

  def extractOcurrencesForAllNumbers(
      xs: List[String]
  ): Map[Int, MapView[String, Int]] =
    xs.flatMap(l => l.split("").zipWithIndex)
      .groupBy(_._2)
      .map { case (k, v) =>
        k -> v.map(_._1).groupBy(identity).mapValues(_.size)
      }

  def calculateRate(
      xs: Map[Int, MapView[String, Int]],
      mapFunction: MapView[String, Int] => String
  ): Int =
    Integer.parseInt(
      xs.map { case (k, v) => k -> mapFunction(v) }
        .toList
        .sortBy(_._1)
        .map(_._2)
        .mkString,
      2
    )

  def calculateConsumptionPower(xs: List[String]): Int = {
    val m       = extractOcurrencesForAllNumbers(xs)
    val gamma   = calculateRate(m, (v) => v.maxBy(_._2)._1)
    val epsilon = calculateRate(m, (v) => v.minBy(_._2)._1)
    gamma * epsilon
  }

  def calculateOxigen(xs: List[String]): Int = {
    def loop(ys: List[String], index: Int): Int = {
      if (ys.size == 1) Integer.parseInt(ys.head, 2)
      else {
        val occ = extractOcurrencesForAllNumbers(ys)
        loop(
          ys.filter(c =>
            c(index).toString == occ(index)
              .reduce((a, b) => {
                if (a._2 == b._2) if (a._1 == "1") a else b
                else if (a._2 < b._2) b
                else a
              })
              ._1
          ),
          index + 1
        )
      }

    }
    loop(xs, 0)
  }

  def calculateCo2(xs: List[String]): Int = {
    def loop(ys: List[String], index: Int): Int = {
      if (ys.size == 1) Integer.parseInt(ys.head, 2)
      else {
        val occ = extractOcurrencesForAllNumbers(ys)
        loop(
          ys.filter(c =>
            c(index).toString == occ(index)
              .reduce((a, b) => {
                if (a._2 == b._2) if (a._1 == "0") a else b
                else if (a._2 < b._2) a
                else b
              })
              ._1
          ),
          index + 1
        )
      }

    }
    loop(xs, 0)
  }
  def calculateLifeSupportRating(xs: List[String]): Int =
    calculateOxigen(xs) * calculateCo2(xs)

}
