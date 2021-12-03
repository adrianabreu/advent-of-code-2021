package submarine
import scala.io.Source
import scala.collection.MapView


object BinaryDiagnostic {

  def main(args: Array[String]): Unit = {
    val filename = "input3.txt"
    val lines: List[String] = Source.fromResource(filename).getLines.toList

    println(calculateConsumptionPower(lines))

  }

  def extractOcurrencesForAllNumbers(xs: List[String]): Map[Int, MapView[String, Int]] = 
    xs.flatMap(l => l.split("").zipWithIndex)
  .groupBy(_._2).map { case (k, v) => k -> v.map(_._1).groupBy(identity).mapValues(_.size)}

  def calculateGammaRate(xs:Map[Int, MapView[String, Int]]): Int =
    Integer.parseInt(xs.map { case (k, v) => k -> v.maxBy(_._2)._1}.toList.sortBy(_._1).map(_._2).mkString, 2)

  def calculateEpsilonRate(xs:Map[Int, MapView[String, Int]]): Int =
    Integer.parseInt(xs.map { case (k, v) => k -> v.minBy(_._2)._1}.toList.sortBy(_._1).map(_._2).mkString, 2)

  def calculateConsumptionPower(xs: List[String]): Int = {
    val m = extractOcurrencesForAllNumbers(xs)
    val x = calculateGammaRate(m)
    val z = calculateEpsilonRate(m)
    x * z
  }

  def calculateLifeSupportRating(xs: List[String]): Int = {
    val m = extractOcurrencesForAllNumbers(xs)
    val a = Integer.parseInt(Range(0, xs.head.size).map(p => xs.filter(c => c(p).toString == m(p).maxBy(_._2)._1)).toList.reduce((a: List[String], b: List[String]) => a.intersect(b)).head)
    
    val b = Integer.parseInt(Range(0, xs.head.size).map(p => xs.filter(c => c(p).toString == m(p).minBy(_._2)._1)).toList.reduce((a: List[String], b: List[String]) => a.intersect(b)).head)
    a * b
    // *
    // Range(0, xs.head.size).map(p => xs.filter(c => c.charAt(p) == m(p).minBy(_._2)._1).reduce(_.intersect_))
  }
    
}